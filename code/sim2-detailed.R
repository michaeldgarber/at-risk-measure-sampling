#Title: A more involved example inducing correlation of variables between groups
#       in which we correct for selection bias and confounding and construct
#       confidence intervals using bootstrapping

#Author: Michael Garber
#Date revised: July 6, 2020

#    In this code, we
#1.  Produce a multi-level dataset

#2.   Check to confirm variables are produced as expected.

#     Conduct analyses on the simulated data:
#3.   Calculate unadjusted point estimates of the incidence rate ratio in the cohort 
#     and in the sample

#4.   Adjust for selection bias using
#4.1. Inverse probability of selection weighting via an events-trials logistic regression
#     in a validation subsample.
#4.2. A summary bias 'breaker' given data in a validation subsample.

#5.   Adjust for confounding using a weighted geometric mean
#6.   Multiple bias analyses: first selection bias, then confounding
#7.   Produce confidence intervals for each of the above using the percentile method from
#     hierarchical bootstrapping

library(tidyverse)
library(truncnorm) #for simulating truncated normal distribution

#------1. Simulate multi-level data------------------------####
n_groups_df2 = 1000
df_sim2 = as_tibble(1:n_groups_df2) %>% 
  #----------------1.1. Group-level information-------------####
  rename(group_id = value) %>% 
  #-----Create the segments nested within street------------#
  #n, segments per street: mean=7, SD=14; right skew so square root it
  mutate(
    #these are empirically informed.
    n_segments_sqrt = rtruncnorm(a=1, mean=2.207, sd=1.5, n=n()),
    n_segments = round(n_segments_sqrt^2) ,
    
    #Define variables at the group (street) level 
    #so that then segments can take on something similar to those values and be correlated.
    #_g for group
    L_g=rtruncnorm(a=1, mean=10, sd=10, n=n()), 
    #number of trips over segment, weakly negatively associated with length.
    #round to make it an integer
    N_g=     round( rtruncnorm(a=2, mean=100 + -1*L_g, sd=50, n=n())), 
    PD_g=L_g*N_g, #person-distance over segment
    
    #Define a binary confounder variable.
    C_g = rbinom(n=n(), size=1, prob=.3),
    
    #Define exposure probability to be slightly higher when confounder present
    E_g=rbinom(n=n(),
               size = 1,
               prob = plogis(qlogis(.3) + log(1.4)*C_g)),

    #Another predictor variable for the sampling fraction to be used in IPSW
    #Could be proportion commutes as in the empirical model in the paper.
    #call it ind_g for group-level independent variable
    ind_g = rbeta(n=n(), shape1=10, shape2=20), #beta distribution for a proportion
    #scale each mean so easier to use in defining the beta distribution
    E_g_sd = sd(E_g),
    L_g_sd = sd(L_g),
    N_g_sd = sd(N_g),
    ind_g_sd = sd(ind_g),
    mean_f_predictors = mean(E_g+L_g+N_g+ind_g),
    scale_E_g = mean_f_predictors/E_g_sd,
    scale_L_g = mean_f_predictors/L_g_sd,
    scale_N_g = mean_f_predictors/N_g_sd,
    scale_ind_g = mean_f_predictors/ind_g_sd,
    #so the scale is a multiplier to make them all equal with respect to the total 
    #additive mean if I want to indicate 1 of their standard deviations

    #Define sampling fraction (f) to be positively associated with number of trips (N)
    #and negatively associated with length (L). Use beta dist'n because it's a proportion.
    #And induce an association with exposure so there is selection bias.
    f_g = rbeta(  
      n=n(),
      shape1 = mean_f_predictors + scale_N_g*.5*N_g  - .1*scale_L_g*L_g,
      shape2= 6*mean_f_predictors - scale_N_g*.5*N_g + 1*scale_E_g*E_g - .2*ind_g_sd 
      ),

    n_g = round(N_g*f_g), #sampled trips over segment  
    pd_g = n_g*L_g, #sampled person-distance over segment  
    
    #Define the number of crashes on the segment as a binomial r.v.
    #Number of trials is the number of trips over segment
    Y_g = rbinom(n=n(),
                 size = N_g,
                 prob = plogis(qlogis(0.001) +log(2)*E_g + log(1.2)*C_g)),
    
    dummy = 1 #a dummy counter variable for calculations
  ) %>% 

  #Now 'uncount' the data so it's at the segment level
  uncount(n_segments, .remove=FALSE) %>% 
  
  #----------------1.2. Segment-level information-------------####
  #create a segment ID within group (e.g., street)
  group_by(group_id) %>% 
  mutate(nested_segment_id = row_number()) %>% 
  #and an overall segment count
  ungroup() %>% 
  mutate( segment_id = row_number()) %>% 
  #considered grouping before defining variables.
  #decided not to because it induces too much random error since group size
  #is not that big
# group_by(group_id) %>%
  mutate(
    #--------Define the segment-level variables-----##
    #_m for segment (m is the index from the manuscript)
    
    #the length varies normally (mean=0 from group-level value) within group
    L_m = L_g + rnorm(n=n_segments, mean=0, sd=L_g_sd/2),
    #then truncate it at 1
    L_m =case_when(
      L_m < 1 ~ 1,
      TRUE ~ L_m ),
    
    #segment-level N
    N_m = N_g + rnorm(n=n_segments, mean=0, sd=N_g_sd/2),
    N_m =case_when(
      N_m < 1 ~ 2,
      TRUE ~ N_m ),
    
    #segment-level sampling fraction
    f_m = f_g + rnorm(n=n_segments, mean=0, sd=.002),
    f_m = case_when(
      f_m < 0.0001 ~ 0.0001,
      f_m > 0.9999 ~ 0.9999,
      TRUE ~ f_m),
    
    #for E and C, define a new binomial r.v. whose prob. depends on the group-level value
    C_m = rbinom(n=n_segments, 
                 size=1, 
                 prob= case_when(
                   C_g > 0.5 ~ .95, #if group value is 1, segment value is usually 1
                   C_g < 0.5 ~ .05 #if group value is 1, segment value is usually 0
                 )),
    E_m = rbinom(n=n_segments, 
                 size=1, 
                 prob= case_when(
                   E_g >= 0.5 ~ .95,
                   E_g < 0.5 ~ .05
                 )),
    
    #Set the cases as a poisson r.v. with the group-level value as mu
    Y_m = rpois(n=n_segments, lambda=Y_g)
    
    ) %>% 
    ungroup() %>% 
    mutate(
    n_m = round(f_m*N_m), #number sampled at segment level

    #number not sampled; for use in event-trial logistic regression
    n_not_m = round(N_m - n_m),

    #calculate person-distance in sample and in cohort at segment level
    PD_m = L_m*N_m,
    pd_m = L_m*n_m,

    #Suppose validation data are available at 5% of the segments.
    #Assume a simple random sample of segments for illustration - perhaps not realistic.
    #V for validation
    V_m = rbinom(n=n(), size=1, prob=0.05),
    
    #for the eventual IPSW model, try some square-root transformations
    L_m_sqrt = L_m^(1/2),
    N_m_sqrt = N_m^(1/2),
    n_m_sqrt = n_m^(1/2)  
    )
    


#---------2. Checks and descriptives------------------------------------#####
#confirm n_m is never higher than N_m
df_sim2 %>% mutate(error = case_when(
  n_m > N_m ~ 1,
  TRUE ~ 0 )) %>% 
  filter(error==1) 
nrow(df_sim2)
summary(df_sim2$n_segments)
summary(df_sim2$L_g)
summary(df_sim2$N_g)
summary(df_sim2$f_m)
summary(df_sim2$ind_g)
sd(df_sim2$N_g)
table(df_sim2)
table(df_sim2$C_g)
table(df_sim2$E_g)
table(df_sim2$C_g, 
      df_sim2$E_g)
table(df_sim2$Y_g)
table(df_sim2$Y_m)
table(df_sim2$Y_m,
      df_sim2$C_m)

table(df_sim2$Y_m,
      df_sim2$E_m)
table(df_sim2$V_m)
cor(df_sim2$ind_g,
    df_sim2$f_m)

#--how are length and N associated with f?--#
# df_sim2 %>% 
#   ggplot(aes(x=N_m, y=f_m)) + 
#   geom_point(aes(color = group_id)) + geom_smooth(method = 'lm')
# 
# df_sim2 %>% 
#   group_by(group_id) %>% 
#   summarise(
#     f_m_group_mean = mean(f_m, na.rm=TRUE),
#     N_g= mean(N_g, na.rm=TRUE)
#   ) %>% 
#   ungroup() %>% 
#   ggplot(aes(x=N_g, y=f_m_group_mean, color = group_id)) + 
#   geom_point() + geom_smooth()
# 
# df_sim2 %>% 
#   ggplot(aes(x=N_g, y=f_m, color = group_id)) + 
#   geom_point() + geom_smooth(se=F)
# 
# df_sim2 %>% 
#   ggplot(aes(x=N_g, y=f_g)) + 
#   geom_point() + geom_smooth(method = 'lm')
# 
# df_sim2 %>% 
#   ggplot(aes(x=n_m, y=f_m)) + 
#   geom_point() + geom_smooth(method = 'lm')
# df_sim2 %>% 
#   ggplot(aes(x=L_m, y=f_m)) + 
#   geom_point() + geom_smooth(method = 'lm')
# df_sim2 %>% 
#   ggplot(aes(x=ind_g, y=f_m)) + 
#   geom_point() + geom_smooth(method = 'lm')

#Prevalence of confounder within categories of exposure
df_sim2 %>% 
  group_by(E_g) %>% 
  summarise(
    sum_C = sum(C_g),
    total = sum(dummy)) %>% 
  ungroup() %>% 
  mutate(prop_C = sum_C/total)

#sampling fraction by exposure
df_sim2 %>% 
  group_by(E_m) %>% 
  summarise( f_m_mean = mean(f_m, na.rm=TRUE))

#Incidence rate and sampling fraction by exposure 
df_sim2 %>% 
  group_by(E_m) %>% 
  summarise(
    f_m_mean = mean(f_m, na.rm=TRUE),
    Y_m = sum(Y_m, na.rm=TRUE), #sample all cases, so this is same either way
    PD_m = sum(PD_m, na.rm=TRUE), #in cohort
    pd_m = sum(pd_m, na.rm=TRUE), #in sample
  ) %>% 
  ungroup() %>% 
  mutate(
    IR_m = Y_m/PD_m , #incidence rate in cohort
    IR_m_pseudo = Y_m/pd_m #incidence rate in sample
  )

#f: 26% unexposed; 21% exposed. ok, will induce bias in ratio. attempt to correct via IPSW


#Examine distribution of variables by group to confirm within-group correlation
# df_sim2 %>% ggplot(aes(n_segments)) + geom_histogram()   
# df_sim2 %>% 
#   ggplot(aes(x=n_segments, y=L_m)) + 
#   geom_point(aes(color = group_id))

#----- Analysis - point estimates----------------------#####

#--------------------------------------------------------#
#---3. No adjustment-----#####
#--------------------------------------------------------#
results_unadj= df_sim2 %>% 
  group_by(E_m) %>% 
  summarise(
    f_m_mean = mean(f_m, na.rm=TRUE), #for reference.
    Y_m = sum(Y_m), #sample all cases, so this is same either way
    PD_m = sum(PD_m), #in cohort
    pd_m = sum(pd_m), #in sample
  ) %>% 
  ungroup() %>% 
  mutate(
    IR_m = Y_m/PD_m , #incidence rate in cohort
    IR_m_pseudo = Y_m/pd_m, #incidence rate in sample
    #overall sampling fraction for summary bias calculations = pd/PD
    f_summary = pd_m/PD_m #summary rather than mean segment-specific
  )

#cases
Y_1 = results_unadj %>% filter(E_m==1) %>% dplyr::select(Y_m) %>% pull()
Y_0 = results_unadj %>% filter(E_m==0) %>% dplyr::select(Y_m) %>% pull()
Y_r = Y_1/Y_0

#person-distance in cohort
PD_1 = results_unadj %>% filter(E_m==1) %>% dplyr::select(PD_m) %>% pull()
PD_0 = results_unadj %>% filter(E_m==0) %>% dplyr::select(PD_m) %>% pull()
PD_r = PD_1/PD_0 #ratio of exposed to unexposed

#person-distance in sample
pd_1 = results_unadj %>% filter(E_m==1) %>% dplyr::select(pd_m) %>% pull()
pd_0 = results_unadj %>% filter(E_m==0) %>% dplyr::select(pd_m) %>% pull()
pd_r = pd_1/pd_0 #ratio of exposed to unexposed

IRR= Y_r/PD_r #true IRR
irr= Y_r/pd_r #lowercase for sample
IRR
irr
    #Some bias up and away from the null because of the association induced 
    #between f and E.
    #Note f_summary in 
    results_unadj

#--------------------------------------------------------#    
#4. Adjust for selection bias-------------------------####
#--------------------------------------------------------#
#-------------------------------------------------------------#
#4.1 Use inverse-probability-of-selection weighting-------#####
#------------------------------------------------------------#
    
#-----4.1.1. Estimate sampling fraction using all of data first------####
#    For proof of concept, estimate sampling fraction in all of the data first,
    #and then do so in the sub-sample.

# #confirm distribution of predictors.
# df_sim2  %>% ggplot(aes(L_m)) + geom_histogram()
# df_sim2  %>% ggplot(aes(L_m_sqrt)) + geom_histogram() 
# df_sim2  %>% ggplot(aes(N_m)) + geom_histogram()
# df_sim2  %>% ggplot(aes(N_m_sqrt)) + geom_histogram()
# df_sim2  %>% ggplot(aes(n_m)) + geom_histogram()
# df_sim2  %>% ggplot(aes(n_m_sqrt)) + geom_histogram()
# df_sim2  %>% ggplot(aes(predictor_f)) + geom_histogram()

#--------Run event-trails logistic regression model-----------#
#Outcome is sampled to not sampled.
etm_f = glm(formula = cbind(n_m, n_not_m) ~  
       ind_g +  L_m + n_m_sqrt  ,
       family = binomial,
       na.action = "na.exclude", 
       data = df_sim2,
       maxit = 1000 )

summary(etm_f)

df_pred_etm_f = as_tibble(stats::predict(etm_f,
          newdata = df_sim2, #it's actually not "newdata" in this instance.
                type = "response",
                se.fit = TRUE,  
                na.action = "na.exclude")) %>% 
  rename(
    f_pred=fit,
    f_pred_se = se.fit)    %>% 
  #append the old data to it to compare with the true sampling fraction
  bind_cols(df_sim2) %>% 
  mutate(
  resid_f = f_pred - f_m,   #calculate a simple absolute residual
  residual.scale=NULL,
  #calculate percentiles to consider trimming weights
  f_pred_1th = quantile(f_pred, prob=.01),
  f_pred_99th = quantile(f_pred, prob=.99),
  f_pred_trim = case_when(
    f_pred <=f_pred_1th ~ f_pred_1th,
    f_pred >=f_pred_99th ~f_pred_99th,
    TRUE ~ f_pred 
      ),
  
  #predict N using inverse-probability-of-selection weighting
  N_pred = N_m*(1/f_pred),
  N_pred_trim = N_m*(1/f_pred_trim),
  PD_pred_IPSW = N_pred*L_m,
  PD_pred_IPSW_trim = N_pred_trim*L_m
  )


#-----examine distribution of predicted values and residuals-------#
# summary(df_pred_etm_f$f_pred)
# summary(df_pred_etm_f$resid_f)
# df_pred_etm_f  %>% ggplot(aes(resid_f)) + geom_histogram() 
#   #left skew from zero, meaning predicted f usually smaller, meaning it will overestimate N.
#   #the model fit could perhaps be improved.
# df_pred_etm_f  %>% ggplot(aes(f_pred)) + geom_histogram()
# df_pred_etm_f  %>% ggplot(aes(f_m)) + geom_histogram()
# df_pred_etm_f  %>% ggplot(aes(N_m)) + geom_histogram()
# df_pred_etm_f  %>% ggplot(aes(N_pred)) + geom_histogram()
# df_pred_etm_f %>% 
#   ggplot(aes(x=N_pred, y=N_m)) + 
#   geom_point() + geom_smooth(method = 'lm')

# #overlay a density plot with the true vs. predicted number of rides over segment (N)
# N_m_df = df_pred_etm_f %>% rename(N = N_m) %>% mutate(type = "truth")
# N_pred_df = df_pred_etm_f %>% rename(N = N_pred) %>% mutate(type = "pred")
# N_pred_trim_df = df_pred_etm_f %>% rename(N = N_pred_trim) %>% mutate(type = "pred-trim")
# N_m_df %>% 
#   bind_rows(N_pred_df) %>% 
#   bind_rows(N_pred_trim_df) %>% 
#   ggplot(aes(x=N)) +geom_density(aes(color=type))

#Note: due to some model misspecification,
#the IPSW-predicted N has a more rightward distribution, meaning the model
# over-estimated the total N because the predicted sampling fraction was 
#slightly downwardly biased, in this case. 
#The model could perhaps be improved with some alternative modeling strategies.
#Ultimately, this could reflect reality of the difficulty of predicting the 
#sampling fraction given available information. Further modeling considerations
#are considered out of scope of this demo.

#------4.1.2. Calculate point estimates using IPSW-estimated data----#
results_SB_IPSW = df_pred_etm_f %>% 
  group_by(E_m) %>% 
  summarise(
    Y_m = sum(Y_m), 
    PD_pred_IPSW = sum(PD_pred_IPSW), 
  ) %>% 
  ungroup() %>% 
  mutate(
    IR_pred = Y_m/PD_pred_IPSW
  )

#person-distance in sample
PD_pred_IPSW_1 = results_SB_IPSW %>% filter(E_m==1) %>% dplyr::select(PD_pred_IPSW) %>% pull()
PD_pred_IPSW_0 = results_SB_IPSW %>% filter(E_m==0) %>% dplyr::select(PD_pred_IPSW) %>% pull()
PD_pred_IPSW_r = PD_pred_IPSW_1/PD_pred_IPSW_0 #ratio of exposed to unexposed

IRR_IPSW= Y_r/PD_pred_IPSW_r  
IRR_IPSW
IRR
irr
IRR_IPSW/IRR
irr/IRR

#In this simulated data, the IPSW-adjusted result is biased down and towards the null, whereas
#the unadjusted result was biased up and away from the null. An improvement, but still biased.

#------4.1.2. Same procedure using validation subset---------####
df_sim2_v = df_sim2 %>% filter(V_m==1)
    #Note, v is a random sample. In reality, it probably wouldn't be random.

etm_f_v = glm(formula = cbind(n_m, n_not_m) ~   #Fit same model as above
              ind_g +  L_m + n_m_sqrt  ,
            family = binomial,
            na.action = "na.exclude", 
            data = df_sim2_v,
            maxit = 1000 )

summary(etm_f_v)
df_pred_etm_f_v = as_tibble(stats::predict(etm_f_v,
           newdata = df_sim2, #Now, the "newdata" argument is used.
           type = "response",
           se.fit = TRUE,  
           na.action = "na.exclude")) %>% 
  rename(
    f_pred=fit,
    f_pred_se = se.fit)    %>% 
  #append the old data to it to compare with the true sampling fraction
  bind_cols(df_sim2) %>% 
  mutate(
    resid_f = f_pred - f_m,   #calculate a simple absolute residual
    residual.scale=NULL,
    #predict N using inverse-probability-of-selection weighting
    N_pred = N_m*(1/f_pred),
    PD_pred_IPSW = N_pred*L_m,
  )


#------4.2.2. Calculate point estimates using IPSW-estimated data based on validation data model----#
results_SB_IPSW_v = df_pred_etm_f_v %>% 
  group_by(E_m) %>% 
  summarise(
    Y_m = sum(Y_m), 
    PD_pred_IPSW = sum(PD_pred_IPSW), 
  ) %>% 
  ungroup() %>% 
  mutate(
    IR_pred = Y_m/PD_pred_IPSW
  )

#person-distance in sample
PD_pred_IPSW_v_1 = results_SB_IPSW_v %>% filter(E_m==1) %>% dplyr::select(PD_pred_IPSW) %>% pull()
PD_pred_IPSW_v_0 = results_SB_IPSW_v %>% filter(E_m==0) %>% dplyr::select(PD_pred_IPSW) %>% pull()
PD_pred_IPSW_v_r = PD_pred_IPSW_v_1/PD_pred_IPSW_v_0 #ratio of exposed to unexposed

IRR_IPSW_v= Y_r/PD_pred_IPSW_v_r  
IRR_IPSW_v
IRR
irr
IRR_IPSW_v/IRR
IRR/IRR_IPSW_v
irr/IRR

#Less biased than the crude estimate on the ratio scale but still biased because the model
#to predict the sampling fraction had some bias.

#--------------------------------------------------------#
#4.2. Use a summary selection bias breaker-----------#####
#--------------------------------------------------------#

# 4.2.1. First, for proof of concept, use the full dataset. It's a simulation, 
#so we know the sampling fraction everywhere.)

  #Use Equation S3.1. from the Appendix. 
    
    #With the variable names in this code, it is:
    
    # pd_r_SB_adj_summary = pd_r *f_summary_0/f_summary_1
    #where pd_r_SB_adj_summary is the adjusted ratio of exposed to unexposed person-distance
    #so irr_SB_adj_summary = Y_r/pd_r_SB_adj_summary

#overall sampling fraction by exposure
f_summary_1 = results_unadj %>% filter(E_m==1) %>% dplyr::select(f_summary) %>% pull()
f_summary_0 = results_unadj %>% filter(E_m==0) %>% dplyr::select(f_summary) %>% pull()
f_summary_r = f_summary_1/f_summary_0 

pd_r_SB_adj_summary = pd_r*(1/f_summary_r)
irr_SB_adj_summary = Y_r/pd_r_SB_adj_summary

#Does the overall summary correct the selection bias?
IRR #truth
irr_SB_adj_summary #estimate corrected for selection bias

#Yes, they are equivalent.

#-----4.2.2. Now, use the random sample, indicated by the _v indicator.---------#
f_summary_v= df_sim2 %>% 
  filter(V_m==1) %>% #restrict to the validation subsample
  group_by(E_m) %>% 
  summarise(
    PD_m = sum(PD_m), #in cohort
    pd_m = sum(pd_m), #in sample
  ) %>% 
  ungroup() %>% 
  mutate(
    f_summary_v = pd_m/PD_m
  )

f_summary_v_1 = f_summary_v %>% filter(E_m==1) %>% dplyr::select(f_summary_v) %>% pull()
f_summary_v_0 = f_summary_v %>% filter(E_m==0) %>% dplyr::select(f_summary_v) %>% pull()
f_summary_v_r = f_summary_v_1/f_summary_v_0 

#Again, use it to estimate the incidence rate ratio.
#Because V is a 5% random sample, it should differ a small amount due to random error.
pd_r_SB_adj_summary_v = pd_r*(1/f_summary_v_r)
irr_SB_adj_summary_v = Y_r/pd_r_SB_adj_summary_v

#How does it differ from the selection bias where all data were available?
irr_SB_adj_summary #all data available
irr_SB_adj_summary_v #only the 5% subset available
IRR #And from the truth?


#-----------------------------------------------------------------------------#
#5. Adjust for possible confounding with a weighted geometric mean -------#####
#-----------------------------------------------------------------------------#
#Joint distribution of cases and person-distance by E, C
joint_EC = df_sim2 %>%  
  group_by(E_m, C_m) %>% 
  summarise(
    Y = sum(Y_m, na.rm=TRUE),  
    pd = sum(pd_m, na.rm=TRUE) ) %>% 
  ungroup() %>% 
  mutate(ir  = Y/pd) #note, the ir is actually a "pseudo-ir" b/c denom is sampled

#marginal probability of person-distance in confounder
marg_C = df_sim2 %>%  
  group_by(C_m) %>% 
  summarise(pd = sum(pd_m, na.rm=TRUE) ) %>% 
  mutate(
    pd_sum = sum(pd),
    prop_pd = pd/pd_sum 
  )

#grab the joint pseudo-IR
ir_E1_C0 = joint_EC %>% filter(E_m==1 & C_m==0) %>% dplyr::select(ir) %>% pull()
ir_E1_C1 = joint_EC %>% filter(E_m==1 & C_m==1) %>% dplyr::select(ir) %>% pull()
ir_E0_C0 = joint_EC %>% filter(E_m==0 & C_m==0) %>% dplyr::select(ir) %>% pull()
ir_E0_C1 = joint_EC %>% filter(E_m==0 & C_m==1) %>% dplyr::select(ir) %>% pull()

prop_pd_C0 = marg_C %>% filter(C_m==0) %>% dplyr::select(prop_pd) %>% pull()
prop_pd_C1 = marg_C %>% filter(C_m==1) %>% dplyr::select(prop_pd) %>% pull()


#The geometric mean could be calculated as either the
#ratio of exposure ratios or the ratio of pseudo-IRs. Or could average stratum-specific IRR.
#Here, use average of stratum-specific pseudo-IRs.
#See page 113 Modern Epidemiology 3rd Edition for discussion of psuedo-IR.

ir_E1_geo = exp(prop_pd_C1*log(ir_E1_C1)+prop_pd_C0*log(ir_E1_C0))
ir_E0_geo = exp(prop_pd_C1*log(ir_E1_C0)+prop_pd_C0*log(ir_E0_C0))
irr_geo = ir_E1_geo/ir_E0_geo

#How does it compare to the sampled and overall estimates?
IRR
irr
irr_geo

#-----------------------------------------------------------------------------#
#6. Multiple bias analyses: IPSW then standardization              -------#####
#-----------------------------------------------------------------------------#
#Use df_pred_etm_f_v because it already has the IPSW estimates
names(df_pred_etm_f_v)
#Joint distribution of cases and person-distance by E, C
joint_IPSW_EC = df_pred_etm_f_v %>%  
  group_by(E_m, C_m) %>% 
  summarise(
    Y = sum(Y_m, na.rm=TRUE), 
    PD_pred_IPSW = sum(PD_pred_IPSW, na.rm=TRUE),
    PD = sum(PD_m, na.rm=TRUE)  #curious about the truth, too, for comparison
    ) %>% 
  ungroup() %>% 
  mutate(
    IR_IPSW  = Y/PD_pred_IPSW, #here, it's a predicted incidence rate (not pseudo)
    IR = Y/PD
    )

#marginal probability of person-distance in levels of confounder
marg_IPSW_C = df_pred_etm_f_v %>%  
  group_by(C_m) %>% 
  summarise(
    PD_IPSW = sum(PD_pred_IPSW, na.rm=TRUE),
    PD = sum(PD_m, na.rm=TRUE)  #curious about the truth, too
    ) %>% 
  mutate(
    PD_IPSW_sum = sum(PD_IPSW),
    prop_PD_IPSW = PD_IPSW/PD_IPSW_sum, 
    PD_sum = sum(PD),
    prop_PD = PD/PD_sum #curious about the truth, too
  )

IR_IPSW_E1_C0 = joint_IPSW_EC %>% filter(E_m==1 & C_m==0) %>% dplyr::select(IR_IPSW) %>% pull()
IR_IPSW_E1_C1 = joint_IPSW_EC %>% filter(E_m==1 & C_m==1) %>% dplyr::select(IR_IPSW) %>% pull()
IR_IPSW_E0_C0 = joint_IPSW_EC %>% filter(E_m==0 & C_m==0) %>% dplyr::select(IR_IPSW) %>% pull()
IR_IPSW_E0_C1 = joint_IPSW_EC %>% filter(E_m==0 & C_m==1) %>% dplyr::select(IR_IPSW) %>% pull()

prop_PD_IPSW_C0 = marg_IPSW_C %>% filter(C_m==0) %>% dplyr::select(prop_PD_IPSW) %>% pull()
prop_PD_IPSW_C1 = marg_IPSW_C %>% filter(C_m==1) %>% dplyr::select(prop_PD_IPSW) %>% pull()

IR_E1_IPSW_geo = exp(prop_pd_C1*log(IR_IPSW_E1_C1)+prop_PD_IPSW_C0*log(IR_IPSW_E1_C0))
IR_E0_IPSW_geo = exp(prop_pd_C1*log(IR_IPSW_E1_C0)+prop_PD_IPSW_C0*log(IR_IPSW_E0_C0))
IRR_IPSW_geo = IR_E1_IPSW_geo/IR_E0_IPSW_geo

#How does it compare to the sampled and overall estimates?
IR_E1_C0 = joint_IPSW_EC %>% filter(E_m==1 & C_m==0) %>% dplyr::select(IR) %>% pull()
IR_E1_C1 = joint_IPSW_EC %>% filter(E_m==1 & C_m==1) %>% dplyr::select(IR) %>% pull()
IR_E0_C0 = joint_IPSW_EC %>% filter(E_m==0 & C_m==0) %>% dplyr::select(IR) %>% pull()
IR_E0_C1 = joint_IPSW_EC %>% filter(E_m==0 & C_m==1) %>% dplyr::select(IR) %>% pull()
prop_PD_C0 = marg_IPSW_C %>% filter(C_m==0) %>% dplyr::select(prop_PD) %>% pull()
prop_PD_C1 = marg_IPSW_C %>% filter(C_m==1) %>% dplyr::select(prop_PD) %>% pull()
IR_E1_geo = exp(prop_pd_C1*log(IR_E1_C1)+prop_PD_C0*log(IR_E1_C0))
IR_E0_geo = exp(prop_pd_C1*log(IR_E1_C0)+prop_PD_C0*log(IR_E0_C0))
IRR_geo = IR_E1_geo/IR_E0_geo

IRR
IRR_geo #the true geometric weighted mean is less than the crude IRR
IRR_IPSW_geo #IPSW geometric weighted mean is close to and, in this replicate, smaller than IRR_geo
IRR_IPSW_v
irr
irr_geo

#Comparison with arithmetic standardization, out of curiosity.
IR_E1_IPSW_arith =  IR_IPSW_E1_C1*prop_PD_IPSW_C1 + IR_IPSW_E1_C0*prop_PD_IPSW_C0  
IR_E0_IPSW_arith =  IR_IPSW_E0_C1*prop_PD_IPSW_C1 + IR_IPSW_E0_C0*prop_PD_IPSW_C0  
IRR_IPSW_arith = IR_E1_IPSW_arith/IR_E0_IPSW_arith #it doesn't adjust it downward as much as geom

IR_E1_arith =  IR_E1_C1*prop_PD_C1 + IR_E1_C0*prop_PD_C0  
IR_E0_arith =  IR_E0_C1*prop_PD_C1 + IR_E0_C0*prop_PD_C0  
IRR_arith = IR_E1_arith/IR_E0_arith 


#--------------------------------------------------------------------------------------#
#--7. Confidence intervals using percentile method and hierarchical bootstrap-----######
#--------------------------------------------------------------------------------------#
#Get a vector of the residuals
names(df_pred_etm_f_v)
resid_v = as.vector(df_pred_etm_f_v$resid_f)

#nested_boot <-function(s_id) {
names(df_pred_etm_f_v)

bootstrap_func <-function(s_id_val){

  #----------7.1 Re-sample groups first-------------------####
  #sample the groups
  by_group_s =   df_sim2 %>% 
    group_by(group_id) %>% 
    summarise( n=n()) %>% 
    dplyr::select(group_id) %>% 
    ungroup() %>% 
    sample_frac(size=1, replace=TRUE)  %>% 
    mutate(in_sample_group= 1)

  #sample the number of times each segment was covered restricted to the sampled groups
  by_seg_s = df_pred_etm_f_v %>% 
    left_join(by_group_s, by = "group_id") %>% 
    filter(in_sample_group==1) %>% #restrict to the groups that were sampled
    #don't need each of these for each calculation 
    #but to keep it all in one step, use them all
    dplyr::select(segment_id, group_id, E_m, L_m, n_m, C_m, f_pred) %>% 
    uncount(n_m, .remove=FALSE) %>% #so that every observation is a time the segment was traveled
    group_by(group_id) %>% 
    sample_frac(size=1, replace=TRUE, weight=n_m) %>%  #sample segments by group
    group_by(segment_id) %>% 
    summarise(
      n_s = n(), #the number of observations at the segment is the number of times it was covered in the sample
      #easier to get the original value this way. median is original value. a bit of a hack.
      L_m = median(L_m, na.rm=TRUE), 
      E_m = median(E_m, na.rm=TRUE),
      C_m = median(C_m, na.rm=TRUE),
      f_pred = median(f_pred, na.rm=TRUE)
    ) %>% 
    mutate(
      pd_s = n_s*L_m,
      #re-sampling residuals. subtract b/c resid was defined as predicted-truth, so t=pred-r
      f_pred_s = f_pred - sample(resid_v, size=1, replace=TRUE ), 
      N_pred_s  = as.integer((1/f_pred_s)*n_s),
      PD_pred_IPSW_s = L_m*N_pred_s
    )

  #sample the crashes
  Y_by_seg_s = df_sim2 %>% 
    left_join(by_group_s, by = "group_id") %>% 
    filter(in_sample_group==1) %>% #restrict to the groups that were sampled
    uncount(Y_m, .remove=FALSE) %>%  #some rows have several cases, so sample every case.
    #implicit weighting by number of cases b/c uncounted.
    sample_frac(size=1, replace=TRUE) %>% 
    group_by(segment_id) %>% 
    summarise(
      Y_s=n(),
      E_m = median(E_m, na.rm=TRUE),
      C_m = median(C_m, na.rm=TRUE)
      )
  
  #----------7.2 Calculate summary measures-------------------####
  # make it a long-form dataset by measure-------#
  #----crashes--------#
  Y_by_E_s = Y_by_seg_s %>%
    group_by(E_m) %>% 
    summarise(Y_s=sum(Y_s, na.rm=TRUE)) %>% 
    rename(value=Y_s)
  
  Y_1_s = Y_by_E_s %>% filter(E_m==1) %>% dplyr::select(value) %>% pull()
  Y_0_s = Y_by_E_s %>% filter(E_m==0) %>% dplyr::select(value) %>% pull()
  Y_r_s  = Y_1_s/Y_0_s 
  Y_1_tib = Y_1_s %>% as_tibble() %>% mutate(measure = "Y_1_s", E_or_R= "E1") 
  Y_0_tib = Y_0_s %>% as_tibble() %>% mutate(measure = "Y_0_s", E_or_R= "E0") 
  Y_r_tib = Y_r_s %>% as_tibble() %>% mutate(measure = "Y_r_s",  E_or_R= "R") 
  Y_tib_s = Y_1_tib %>% bind_rows(Y_0_tib, Y_r_tib)  %>% mutate( scenario = "1-unadj")
  
  #----7.2.1 Unadjusted and IPSW-estimated person-distance--------####
  pd_by_E_s = by_seg_s %>% 
  group_by(E_m) %>% 
    summarise(
      pd_s = sum(pd_s, na.rm=FALSE),
      PD_pred_IPSW_s = sum(PD_pred_IPSW_s, na.rm=FALSE)
    ) %>% 
    #the first argument selects all but E_m.
    pivot_longer(-E_m, values_to = "value", names_to = "measure") %>% 
    mutate(
      scenario = case_when(
        measure == "pd_s" ~ "1-unadj",
        measure == "PD_pred_IPSW_s" ~ "2-IPSW",
        ),
      #this is weird coding. oh well.
      measure = case_when(
        E_m==1 & scenario == "1-unadj" ~ "pd_1_s",
        E_m==0 & scenario == "1-unadj" ~ "pd_0_s",
        E_m==1 & scenario == "2-IPSW" ~ "PD_pred_IPSW_1_s",
        E_m==0 & scenario == "2-IPSW" ~ "PD_pred_IPSW_0_s"
      ),
    E_or_R = case_when(
      E_m==1 ~ "E1",
      E_m==0 ~ "E0",
    )) %>% 
    dplyr::select(-E_m)  
    
  
  #no IPSW
  pd_1_s = pd_by_E_s %>% filter(measure=="pd_1_s" ) %>% dplyr::select(value) %>% pull()
  pd_0_s = pd_by_E_s %>% filter(measure=="pd_0_s" ) %>% dplyr::select(value) %>% pull()
  pd_r_s = pd_1_s/pd_0_s  
  irr_s = Y_r_s/pd_r_s  
  
  #with IPSW
  PD_pred_IPSW_1_s = pd_by_E_s %>% 
    filter(measure=="PD_pred_IPSW_1_s") %>%  dplyr::select(value) %>% pull()
  PD_pred_IPSW_0_s = pd_by_E_s %>% 
    filter(measure=="PD_pred_IPSW_0_s") %>%  dplyr::select(value) %>% pull()
  PD_pred_IPSW_r_s = PD_pred_IPSW_1_s/PD_pred_IPSW_0_s  
  IRR_IPSW_s = Y_r_s/PD_pred_IPSW_r_s
  
  #make them into tibbles
  pd_r_s_tib = pd_r_s %>% as_tibble() %>% 
    mutate(measure = "pd_r_s",
            E_or_R= "R",
           scenario = "1-unadj")
  irr_s_tib = irr_s %>% as_tibble() %>% 
    mutate(measure = "irr_s", 
           E_or_R= "R",
           scenario = "1-unadj")
  PD_pred_IPSW_r_s_tib = PD_pred_IPSW_r_s %>% as_tibble() %>% 
    mutate(measure = "PD_pred_IPSW_r_s", 
           E_or_R= "R",
           scenario = "2-IPSW")
  IRR_IPSW_s_tib = IRR_IPSW_s %>% as_tibble() %>% 
    mutate(measure = "IRR_IPSW_s", 
           E_or_R= "R",
           scenario = "2-IPSW")
  
  #stack them
  tib_unadj_SB = Y_tib_s %>%  
    bind_rows(pd_by_E_s, pd_r_s_tib, irr_s_tib, PD_pred_IPSW_r_s_tib, IRR_IPSW_s_tib)

  #----7.2.2. Adjusted for confounding and multiple bias analyses--------#######
  #Joint distribution of cases and person-distance by E, C
  joint_EC_Y_s = Y_by_seg_s %>% 
    group_by(E_m, C_m) %>% 
    summarise(Y_s = sum(Y_s, na.rm=TRUE)) %>% 
    ungroup()
  
  joint_EC_s  = by_seg_s %>%    
    group_by(E_m, C_m) %>% 
    summarise(
      pd_s = sum(pd_s, na.rm=TRUE),
      PD_pred_IPSW_s = sum(PD_pred_IPSW_s, na.rm=TRUE)
      ) %>% 
    ungroup() %>% 
    left_join(joint_EC_Y_s, by = c("E_m", "C_m")) %>% 
    mutate(
      ir_s  = Y_s/pd_s,
      IR_IPSW_s = Y_s/PD_pred_IPSW_s
      ) 
  
  #marginal probability of person-distance in confounder
  names(by_seg_s)
  marg_C_s = by_seg_s %>%  
    group_by(C_m) %>% 
    summarise(
      pd = sum(pd_s, na.rm=TRUE),
      PD_IPSW = sum(PD_pred_IPSW_s, na.rm=TRUE)
      ) %>% 
    ungroup() %>% 
    mutate(
      pd_sum = sum(pd),
      prop_pd_s = pd/pd_sum,
      PD_IPSW_sum = sum(PD_IPSW),
      prop_PD_IPSW_s = PD_IPSW/PD_IPSW_sum, 
    )
  
  #Calculate weighted geometric mean
  #No IPSW
  ir_E1_C0_s = joint_EC_s %>% filter(E_m==1 & C_m==0) %>% dplyr::select(ir_s) %>% pull()
  ir_E1_C1_s = joint_EC_s %>% filter(E_m==1 & C_m==1) %>% dplyr::select(ir_s) %>% pull()
  ir_E0_C0_s = joint_EC_s %>% filter(E_m==0 & C_m==0) %>% dplyr::select(ir_s) %>% pull()
  ir_E0_C1_s = joint_EC_s %>% filter(E_m==0 & C_m==1) %>% dplyr::select(ir_s) %>% pull()
  
  prop_pd_C0_s = marg_C_s %>% filter(C_m==0) %>% dplyr::select(prop_pd_s) %>% pull()
  prop_pd_C1_s = marg_C_s %>% filter(C_m==1) %>% dplyr::select(prop_pd_s) %>% pull()

  ir_E1_geo_s = exp(prop_pd_C1_s*log(ir_E1_C1_s)+prop_pd_C0_s*log(ir_E1_C0_s))
  ir_E0_geo_s = exp(prop_pd_C1_s*log(ir_E1_C0_s)+prop_pd_C0_s*log(ir_E0_C0_s))
  irr_geo_s = ir_E1_geo_s/ir_E0_geo_s
  
  #IPSW
  IR_IPSW_E1_C0_s = joint_EC_s %>% filter(E_m==1 & C_m==0) %>% dplyr::select(IR_IPSW_s) %>% pull()
  IR_IPSW_E1_C1_s = joint_EC_s %>% filter(E_m==1 & C_m==1) %>% dplyr::select(IR_IPSW_s) %>% pull()
  IR_IPSW_E0_C0_s = joint_EC_s %>% filter(E_m==0 & C_m==0) %>% dplyr::select(IR_IPSW_s) %>% pull()
  IR_IPSW_E0_C1_s = joint_EC_s %>% filter(E_m==0 & C_m==1) %>% dplyr::select(IR_IPSW_s) %>% pull()
  
  prop_PD_IPSW_C0_s = marg_C_s %>% filter(C_m==0) %>% dplyr::select(prop_PD_IPSW_s) %>% pull()
  prop_PD_IPSW_C1_s = marg_C_s %>% filter(C_m==1) %>% dplyr::select(prop_PD_IPSW_s) %>% pull()
  
  IR_E1_IPSW_geo_s = exp(prop_pd_C1_s*log(IR_IPSW_E1_C1_s)+prop_PD_IPSW_C0_s*log(IR_IPSW_E1_C0_s))
  IR_E0_IPSW_geo_s = exp(prop_pd_C1_s*log(IR_IPSW_E1_C0_s)+prop_PD_IPSW_C0_s*log(IR_IPSW_E0_C0_s))
  IRR_IPSW_geo_s = IR_E1_IPSW_geo_s/IR_E0_IPSW_geo_s
  
  #make them into tibbles
  ir_E1_geo_tib = ir_E1_geo_s %>% as_tibble() %>% 
    mutate(measure = "ir_E1_geo_s", E_or_R= "E1") %>% mutate(scenario = "3-geo-mean")
  ir_E0_geo_tib = ir_E0_geo_s %>% as_tibble() %>% 
    mutate(measure = "ir_E0_geo_s", E_or_R= "E0") %>% mutate(scenario = "3-geo-mean")
  irr_geo_tib =  irr_geo_s  %>% as_tibble() %>% 
    mutate(measure = "irr_geo_s", E_or_R= "R") %>% mutate(scenario = "3-geo-mean")
  
  IR_E1_IPSW_geo_tib = IR_E1_IPSW_geo_s %>% as_tibble() %>%  
    mutate(measure = "IR_E1_IPSW_geo_s", E_or_R= "E1") %>% mutate(scenario = "4-mult")
  IR_E0_IPSW_geo_tib = IR_E0_IPSW_geo_s %>% as_tibble() %>%  
    mutate(measure = "IR_E1_IPSW_geo_s", E_or_R= "E0") %>% mutate(scenario = "4-mult")
  IRR_IPSW_geo_s = IRR_IPSW_geo_s %>% as_tibble() %>%  
    mutate(measure = "IRR_IPSW_geo_s", E_or_R= "R") %>% mutate(scenario = "4-mult")

  tib_conf_mult = ir_E1_geo_tib %>% 
    bind_rows(ir_E0_geo_tib, irr_geo_tib, IR_E1_IPSW_geo_tib, IR_E0_IPSW_geo_tib , IRR_IPSW_geo_s)
  
  tib_s = tib_unadj_SB %>% bind_rows(tib_conf_mult) %>% 
    mutate( 
      s_id=s_id_val
    )
  
  return(tib_s)

}

#-----7.3. Run the function and summarize results----------####
n_boot_reps = 100
s_id_val_list <- seq(from = 1, to = n_boot_reps, by = 1)

#This runs the function and then rbinds results
bootstrap_df = s_id_val_list %>% map_dfr(bootstrap_func) 

bootstrap_summary = bootstrap_df %>% 
  filter(value  < 100000000000000) %>%  #exclude infinities
  group_by(scenario, measure, E_or_R) %>% 
  summarise(
    s_median = median(value),
    s_ll = quantile(value, probs = c(0.025)),
    s_ul = quantile(value, probs = c(0.975)),
#    pt = median(pt)  
  ) %>% 
  ungroup() %>%
  mutate( 
    ratio_width = s_ul/s_ll,
    reps = n_boot_reps
  ) %>% 
  
  #add point estimates for comparison
  mutate(
    pt = case_when(
      measure=="Y_r_s" ~ Y_r,
      measure=="Y_1_s" ~ as.numeric(Y_1),
      measure=="Y_0_s" ~ as.numeric(Y_0),
      measure=="irr_s"  ~ irr,
      measure=="pd_r_s"  ~ pd_r,
      measure=="pd_1_s" ~ as.numeric(pd_1),
      measure=="pd_0_s" ~ as.numeric(pd_0),
      measure=="IRR_IPSW_s" ~ as.numeric(IRR_IPSW),
      measure=="PD_pred_IPSW_1_s" ~ as.numeric(PD_pred_IPSW_v_1),
      measure=="PD_pred_IPSW_0_s" ~ as.numeric(PD_pred_IPSW_v_0),
      measure=="PD_pred_IPSW_r_s" ~ as.numeric(PD_pred_IPSW_v_r),
      measure=="IRR_IPSW_s" ~ as.numeric(IRR_IPSW_v),
      measure== "ir_E1_geo_s" ~ ir_E1_geo,
      measure== "ir_E0_geo_s" ~ ir_E0_geo,
      measure== "irr_geo_s" ~ irr_geo,
      measure== "IR_E1_IPSW_geo_s" ~ IR_E1_IPSW_geo,
      measure== "IR_E0_IPSW_geo_s" ~ IR_E0_IPSW_geo,
      measure== "IRR_IPSW_geo_s" ~ IRR_IPSW_geo
    ),
  pt_in_95CI = case_when(
    pt >=s_ll & pt <= s_ul ~ "YES",
    TRUE ~ "NO"
  )
  )

options(scipen=7)
View(bootstrap_summary)
