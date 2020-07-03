
#----------Aim2 Figures and simulations-------######

#The contents of this code:

#1. Code to produce the two figures
#2. Code to simulate a simple example to illustrate Condition 3
#3. A more complicated but more realistic simulate dataset where there is
#   correlation of variables within groups of segments (streets).

    #In this dataset, the following procedures are illustrated:
      #A. Variance estimation via hierarchical bootstrapping
      #B. Inverse probability of selection weighting
      #C. Confounding control via a weighted geometric mean
    

#
library(truncnorm)
#------1. A simple example with no correlation of segments within groups to illustrate the concept------####
#The condition (Equation 3) is satisfied.
n_segments_df1 = 100000
df_sim1 = as_tibble(1:n_segments_df1) %>% 
  rename(segment_id=value) %>%
  mutate(
    #length of segment
    L=rtruncnorm(a=3, mean=20, sd=20, n=n()), 
    #number of trips over segment. negatively associated with length
    N=rtruncnorm(a=5, mean=20 + -0.1*L, sd=20, n=n()), 
    PD=L*N, #person-distance over segment

    #Define sampling fraction to be positively associated with number of trips (N)
    #and negatively associated with length (L)
    f=rbeta(n=n(),
            shape1= 15*(mean(L)+ mean(N))+100*N , 
            shape2= mean(L)+ mean(N) + 5*(L*100)
    ),
    n = N*f, #sampled trips over segment  
    pd = n*L, #sampled person-distance over segment  
    
    #define exposure independently of the above
    e = rbinom(n=n(), size=1, prob=.4)
    )


#------Summarize results---------#
df_sim1_by_e = df_sim1 %>% 
  group_by(e) %>% 
  summarise(
    pd=sum(pd, na.rm=TRUE),
    PD = sum(PD, na.rm=TRUE)
    )

#Person-distance in cohort by exposure
PD_1 = df_sim1_by_e %>% filter(e==1) %>% dplyr::select(PD) %>% pull()
PD_0 = df_sim1_by_e %>% filter(e==0) %>% dplyr::select(PD) %>% pull()

#ratio of exposed to unexposed person-distance in cohort
PD_r =PD_1/PD_0

#person-distance in sample by exposure
pd_1 = df_sim1_by_e %>% filter(e==1) %>% dplyr::select(pd) %>% pull()
pd_0 = df_sim1_by_e %>% filter(e==0) %>% dplyr::select(pd) %>% pull()

#ratio of exposed to unexposed sampled person-distance
pd_r =pd_1/pd_0

#The two exposure ratios should be about the same (differ due to random error only)
PD_r
pd_r

#Suppose the ratio of exposed to unexposed cases is 1.5
cases_E_ratio = 1.5

#Calculate the IRR in the cohort and in the sample
IRR_cohort = cases_E_ratio/PD_r
IRR_sample = cases_E_ratio/pd_r




#Consider 2,000 streets
#each has on average how many segments within it?
edge_id_osm_lookup_aim2 %>% 
  group_by(OSM_NAME, edge_id) %>% 
  summarise(n=n())


edges_per_street = edge_id_osm_lookup_aim2 %>% 
  group_by(OSM_NAME) %>% 
  summarise(n=n()) %>% 
  mutate(
    log_n = log(n),
    n_sqrt = n^(1/2)
         )

edges_per_street  %>% ggplot(aes(n)) + geom_histogram()
edges_per_street  %>% ggplot(aes(log_n)) + geom_histogram()
edges_per_street  %>% ggplot(aes(n_sqrt)) + geom_histogram()

summary(edges_per_street$n)
summary(edges_per_street$n_sqrt)
sd(edges_per_street$n)
sd(edges_per_street$n_sqrt)

names(edge_eco_a_aim2)
edge_eco_a_aim2 %>% 
  mutate(tactcnt_sqrt)
  group_by(major_or_res) %>% 
  ggplot(aes(tactcnt)) + geom_histogram()

n_streets = 2000
segments_df = as_tibble(1:n_streets) %>% 
  
  #-----Create the segments nested within street------------#
  rename(street_id = value) %>% 
  #n, segments per street: mean=7, SD=14; right skew so square root it
  mutate(n_segments_sqrt = 
           rtruncnorm(a=1, mean=2.207, sd=1.5, n=n()),
         n_segments = n_segments_sqrt^2,

  #-----Define variables at the street level to induce within-group correlation--------#
  #(Variables for segments within groups should be correlated)
  #Define exposure as a binomial r.v.
  E_group =rbinom(n=n(), size=1, prob=.4),

  #Create an observation for every segment using uncount()
  uncount(n_segments, .remove=FALSE) %>% 
  
  #create a segment ID within street
  group_by(street_id) %>% 
  mutate(nested_segment_id = row_number()) %>% 
  #and an overall segment count
  ungroup() %>% 
  mutate(
    segment_id = row_number(),

  #-------Define variables at the segment level--------------#

    #Define exposure as a binomial r.v.
    E=rbinom(n=n(), size=1, prob=.4),
  
  8959/(8959+11317)

 
View(segments_df)
streets_df %>% ggplot(aes(n_segments)) + geom_histogram()        

  mutate(row)