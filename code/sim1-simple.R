
#Title: A simple example with no correlation of segments within 
#       groups to illustrate the condition

#Author: Michael Garber
#Date revised: July 3, 2020

library(tidyverse)
library(truncnorm)

#------Simulate the data----------------######
#To show the condition (Equation 3 from the manuscript) is satisfied.
n_segments_df1 = 10000
df_sim1 = as_tibble(1:n_segments_df1) %>% 
  rename(segment_id=value) %>%
  mutate(
    #length (L) of segment 
    L=rtruncnorm(a=3, mean=20, sd=20, n=n()), 
    #number of trips (N) over segment. negatively associated with length
    N=rtruncnorm(a=5, mean=20 + -0.1*L, sd=20, n=n()), 
    PD=L*N, #person-distance over segment

    #Define sampling fraction (f) to be positively associated with number of trips (N)
    #and negatively associated with length (L).
    #Use beta dist'n because it's a proportion.
    f=rbeta(n=n(),
            shape1= 15*(mean(L)+ mean(N))+10*N , 
            shape2= 30*(mean(L)+ mean(N)) + (L*50) 
    ),
    n = N*f, #sampled trips over segment  
    pd = n*L, #sampled person-distance over segment  

    #Define exposure without regard to any of the above.
    E = rbinom(n=n(), size=1, prob=.4)
    )


#------Summarize results---------####
#What is the distribution of the sampling fraction?
summary(df_sim1$f)

#Summarize person-distance in sample and in cohort by E
df_sim1_by_E = df_sim1 %>% 
  group_by(E) %>% 
  summarise(
    mean_f = mean(f, na.rm=TRUE),
    pd=sum(pd, na.rm=TRUE),
    PD = sum(PD, na.rm=TRUE)
    )

#does mean sampling fraction differ by exposure?
df_sim1_by_E #no

#Person-distance in cohort by exposure
PD_1 = df_sim1_by_E %>% filter(E==1) %>% dplyr::select(PD) %>% pull()
PD_0 = df_sim1_by_E %>% filter(E==0) %>% dplyr::select(PD) %>% pull()

#Ratio of exposed to unexposed person-distance in cohort
PD_E_r =PD_1/PD_0

#person-distance in sample by exposure
pd_1 = df_sim1_by_E %>% filter(E==1) %>% dplyr::select(pd) %>% pull()
pd_0 = df_sim1_by_E %>% filter(E==0) %>% dplyr::select(pd) %>% pull()

#ratio of exposed to unexposed sampled person-distance
pd_E_r =pd_1/pd_0

#The two exposure ratios should be about the same (differ due to random error only)
PD_E_r
pd_E_r

#Suppose the ratio of exposed to unexposed cases is 1.5
cases_E_r = 1.5

#Calculate the IRR in the cohort and in the sample
IRR_cohort = cases_E_r/PD_E_r
IRR_sample = cases_E_r/pd_E_r

IRR_cohort
IRR_sample

#They are very similar, and approach equivalence as sample size grows,
#i.e., statistical consistency.


#Note, this is in spite of a correlation between 
#sampling fraction and number of trips over segment
cor(df_sim1$N,
     df_sim1$f)
cor(df_sim1$L,
    df_sim1$f)
