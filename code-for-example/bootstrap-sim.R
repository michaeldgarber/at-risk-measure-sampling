library(tidyverse)
library(truncnorm)
n=100
x=1:n
df=as_tibble(x) %>%
  mutate(nest_id = row_number(),
         n_segments_per_nest = rpois(n=n(), lambda = 10),
         e  = rbinom(n=n(), size=1, prob=.2 )) %>%  #binary exposure variable, nest level
         
  uncount(n_segments_per_nest,
          .remove=FALSE)  %>% 
  mutate(segment_id = row_number(),
         length = rpois(n=n(), lambda = 50),
         n_rides_per_segment = as.integer(
                                      rtruncnorm(
                                          n=n(), a=0,   #truncated at zero
                                              mean = .5*(1/n)*nest_id, 
                                              sd = 10+10*e*nest_id
                                                )
                                          )
   )
     
View(df)                

df_uncount = df %>% uncount(n_rides_per_segment, .remove=FALSE) %>% mutate(obs_id = row_number())
ggplot(df, aes(length)) + geom_histogram()
ggplot(df, aes(n_segments_per_nest)) + geom_histogram()
ggplot(df, aes( n_rides_per_segment)) + geom_histogram()

#Now, I want to bootstrap this - test a multi-level bootstrap
#n for nested
boot_sim_n <-function(s_id_val) {
  boot_s = df_uncount %>%
    group_by(nest_id) %>%
    sample_frac(size=.5,  replace=TRUE) %>%
    group_by(e) %>%
    summarise(dist = sum(length))

  bd_s_e1= boot_s %>% filter(e==1) %>% dplyr::select(dist) %>% pull() %>% as_tibble()
  bd_s_e0= boot_s %>% filter(e==0) %>% dplyr::select(dist) %>% pull() %>% as_tibble()
  
  bd_s_ratio = bd_s_e1/bd_s_e0
  

  #I want all three measures in one row
  bd_s_n_ratio_tibble = as_tibble(bd_s_ratio) %>%
    bind_cols(bd_s_e1) %>%
    bind_cols(bd_s_e0) %>%
    dplyr::rename(ratio=value, e1=value1, e0=value2 ) %>%
    mutate(s_id=s_id_val)
  
  return(bd_s_n_ratio_tibble)
  
}

#check to make sure it's samplig 0.5 from every nest_id
boot_s %>% group_by(nest_id) %>% summarise(n=n())
df %>% group_by(nest_id) %>% summarise(n_rides = sum(n_rides_per_segment))

#yes, it is.
nreps = 10
nreps_list <-seq(1, nreps, by =1)
boot_sim_n_df = nreps_list %>% map_dfr(boot_sim_n) 
View(boot_sim_n_df)
options(scipen = 999)
var(boot_sim_n_df$ratio)