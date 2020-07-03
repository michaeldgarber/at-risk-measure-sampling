---
title: "Code for Figures"
author: "Michael Garber"
date: "7/2/2020"
output:
  html_document:
    df_print: paged
    keep_md: true #This creates a md file which can then be displayed on github. See https://happygitwithr.com/rmd-test-drive.html
---

This code produces Figures 1 and 2 in the manuscript.

First, load the necessary libraries.

```r
library(tidyverse) 
library(truncnorm)
```


### Simulate data for the figures


```r
fig1_df = 1:20 %>% 
  as_tibble() %>% 
  rename(id=value) %>%
  mutate(
    L=rtruncnorm(a=3,
                 mean=20, sd=20, n=n()),
    #induce a slight negative association between segment length (L) and
    #the number of times the segment was traveled upon (N)
    N=rtruncnorm(a=5, mean=20 + -0.1*L, sd=20, n=n())) %>% 
  arrange(desc(N)) %>% 
  mutate(
    L_cum_right = cumsum(L),
    BD=L*N,
    L_cum_left =L_cum_right-L,
    L_cum_mid = L_cum_left+((L_cum_right-L_cum_left)/2), #because I want to plot at midpoint
  
    #define the sampling fraction (f) with a beta distribution because it's a proportion
    f=rbeta(n=n(),
            #more likely to be sampled if higher traffic segment (higher N)
            shape1= 15*(mean(L)+ mean(N))+100*N , 
            
            #more likely to be sampled if longer segment (higher L)
            shape2= mean(L)+ mean(N) + 5*(L*100)
            ),
    N_f = N*f,
    BD_f = N_f*L,
    N_f_not = N-N_f,
    BD_f_not = BD - BD_f,
    e = rbinom(n=n(), size=1, prob=.4),
    e_char = case_when(
      e==1 ~ "E=1",
      e==0 ~ "E=0"
    )
  )


# Create a long-form dataset. (Could do this with pivot-longer but not yet intuitive to me)
fig1_df_long_s1 = fig1_df %>% 
  dplyr::select(id, L, N, L_cum_mid, N_f, BD_f, e_char, e) %>% 
  rename(n_long = N_f, BD_long = BD_f) %>% 
  arrange(L) %>% 
  mutate(sampled=1) %>% 
  group_by(e) %>% 
  mutate(
    L_cum_right_alt = cumsum(L), #alt
    L_cum_left_alt =L_cum_right_alt-L,
    L_cum_mid_alt = L_cum_left_alt+((L_cum_right_alt-L_cum_left_alt)/2)
  )

fig1_df_long_s0 = fig1_df %>% 
  dplyr::select(id, L, N, L_cum_mid, N_f_not, BD_f_not, e_char, e) %>% 
  rename(n_long = N_f_not, BD_long = BD_f_not) %>% 
  arrange(L) %>% 
  mutate(sampled=0) %>% 
  group_by(e) %>% 
  mutate(
    L_cum_right_alt = cumsum(L), #alt
    L_cum_left_alt =L_cum_right_alt-L,
    L_cum_mid_alt = L_cum_left_alt+((L_cum_right_alt-L_cum_left_alt)/2)
  )


fig1_df_long = fig1_df_long_s1 %>% 
  bind_rows(fig1_df_long_s0)
```

### Figure 1: person-distance sampling at the segment level


```r
fig1_df_long %>% 
  ggplot(aes(x=L_cum_mid, y=n_long, width=L, fill =as.character(sampled))) + #set coordinate to the cumulative midpoint
  geom_bar(
    stat="identity", 
    position = "stack",
    colour = "black",
    lwd=.5) +
  facet_grid(rows = vars(e_char)) +

  ylab("Number of trips over segment") +
  xlab("Segment") +
  scale_fill_manual(
    name = "Sampled",
    labels = c(
      "No",
      "Yes"
    ),
    values = c("gray90",
               "gray57"

    )
  ) +
   theme_bw(base_size = 14)+
  theme(
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y = element_blank()
    )+
  theme( strip.background =element_rect(fill="white"))+
  theme(  strip.text =element_text(colour =   "black"))
```

![](code-for-figures_files/figure-html/figure1-1.png)<!-- -->

### Figure 2: person-event sampling at the intersection level


```r
fig1_df_long %>% 
  group_by(sampled) %>%  
  arrange(desc(N)) %>% 
  mutate(new_id = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x=new_id, y=n_long, 
             fill =as.character(sampled))) + #set coordinate to the cumulative midpoint
  geom_bar(
    stat="identity", 
    position = "stack",
    colour = "black") +
  facet_grid(rows = vars(e_char)) +
  ylab("Number of trips over intersection") +
  xlab("Intersection") +
  scale_fill_manual(
    name = "Sampled",
    labels = c(
      "No",
      "Yes"
    ),
    values = c("gray90",
               "gray57"
               
    )
  ) +
  theme_bw(base_size = 14)+
  theme(
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y = element_blank()
  )+
  theme( strip.background =element_rect(fill="white"))+
  theme(  strip.text =element_text(colour =   "black"))
```

![](code-for-figures_files/figure-html/figure2-1.png)<!-- -->


