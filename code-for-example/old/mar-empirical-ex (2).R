
#------Aim 2 Example------------#
#Began 2/1/2020
#Idea: Show how density sampling could have lower variance than measure-at-risk sampling
#First, classify the basemap by highway class. Either above or below residential.
#Exclude trails.
#Revised 4/17/2020

#---0.000 Load packages--------#
library(tidyverse)
library(tmap)
library(mapview)
library(sf)
library(RColorBrewer)
library(tmaptools)


#--------0.0 Initial data prep------###########
#load the 5-mile radius and restrict to it
setwd(file.path("C:","Users","mdg71","Dropbox","EMORY", "General research",
                "Dissertation", "R_data", "buffer radii around areas"))

load(file = "mp_sf_t.RData")
load(file = "mp_sf_5halfmi.RData")
mapview(mp_sf_5halfmi)
mp_sf_5halfmi = mp_sf_t %>%
  st_buffer(26400) %>%
  mutate(radius = "5-mi radius, Monroe & Ponce") %>%
  st_transform(4326)

setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "Strava 2018")) 
load(file = "edge_rollup_aim3.RData")
load(file = "edge_rollup_aim3_inczeros.RData")
 

dim(edge_rollup_aim3)
dim(edge_rollup_elig_speednomiss)
table(edge_rollup_elig_speednomiss$highway_6cat)
table(edge_rollup_aim3$highway_6cat)

st_crs(edge_rollup_elig_speednomiss)
st_crs(mp_sf_5halfmi)
#dichotomize as primary-tertiary or residential


#0.1. Exposure data classification-------------##########
#note, this is a 5.5 mile radius
#Updating 5/16/20 to allow for segments without any Strava data for use in the bootstrapping
aim2_ex_strava_5halfmi = edge_rollup_aim3_inczeros %>%
  
  #restrict to only residential or major streets
  filter(highway_6cat == "primary-tertiary road" |
           highway_6cat == "residential road"
           ) %>%
  #the beltline was erroneously coded as a primary road. remove..
  filter(OSM_NAME != "Beltline") %>%
  
  # Before, I limited to segments with non-missing values of bike distance.
  #Here (5/16/20), I am not doing that.
  # filter(bike_distance_km_tactcnt > 0) %>%
  
  #"Exposed" should be non-reidential road, acutally, because
  #it will be omre dangerous.
  mutate(major_or_res = case_when(
    highway_6cat == "primary-tertiary road" ~ 1,
    highway_6cat == "residential road" ~ 0,
  )) 

options(viewer = NULL) #send viewer to the browser
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

mapview(aim2_ex_strava_5halfmi, zcol = "major_or_res") 
#visualize eco-counters with exposure for the example
mapview(eco_counter_ft, col.regions = "red") + mapview(aim2_ex_strava_5halfmi, zcol = "major_or_res")

n_unique_name = aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL) %>% 
  group_by(name) %>% 
  summarise(n=n())
n_unique_osm_name = aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL) %>% 
  group_by(OSM_NAME) %>% 
  summarise(n=n())

nrow(n_unique_name)  #3256 rows
nrow(n_unique_osm_name)  #3277 unique osm_name
View(n_unique_name)
#look at the missings
aim2_ex_strava_5halfmi %>% filter(is.na(OSM_NAME)==TRUE) %>% mapview() #no missings!
aim2_ex_strava_5halfmi %>% filter(is.na(name)==TRUE) %>% mapview() #no missings!


#-----0.1.1 Modeled exposure data---------######


#0.2. Create a buffer area. One for exposed and one for unexposed.-------------##########


#-------Exposed - including intersections with unexposed-------------------------##
buffer_e1 = aim2_ex_strava_5halfmi %>% filter(major_or_res == 1) %>%
#this should make it all one thing so it's faster in anaylsis?
   st_union(by_feature=FALSE) %>%
  #and create a little buffer around it
  #first, for the buffer make it feet.
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  #now the buffer
  st_buffer(50) %>% #30 feet
    #back to 4326
 st_transform(4326)


#-------Unexposed - including intersections with exposed-------------------------##
buffer_e0 = aim2_ex_strava_5halfmi %>% filter(major_or_res == 0) %>%
  #this should make it all one thing so it's faster in anaylsis?
  st_union(by_feature=FALSE) %>%
  #and create a little buffer around it
  #first, for the buffer make it feet.
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  #now the buffer
  st_buffer(50) %>% #30 feet
  #back to 4326
  st_transform(4326)

#---------Exposed, excluding intersections with unexposed
buffer_e1_noint = buffer_e1 %>%
  st_difference(buffer_e0) #nice, that worked as expected.

#---------Unexposed, excluding intersections with exposed
buffer_e0_noint = buffer_e0 %>%
  st_difference(buffer_e1) #nice, that worked as expected.


# mapview(buffer_e0_noint)
# mapview(buffer_e1_noint)

#intersection between exposed and unexposed
e1_e0_int = buffer_e0 %>% 
  st_intersection(buffer_e1) #nice, that worked as expected.


#hmm, another thing I might need to do is just exclude all intersections...
buffer_not_unioned = aim2_ex_strava_5halfmi %>% 
  #limit to wherever there are either exposed or unexposed
  filter(major_or_res >= 0) %>%
  #and create a little buffer around it
  #first, for the buffer make it feet.
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  #now the buffer
  st_buffer(40) #okay, so there are lots of intersections

any_intersection = buffer_not_unioned %>%
  st_union(by_feature=FALSE) %>%
  st_intersection()

mapview(intersections)

mapview(exposed_to_unexposed_int)  

#-----save these buffers for speed----------##
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
save(buffer_e1_noint, file = "buffer_e1_noint.RData")
save(buffer_e0_noint, file = "buffer_e0_noint.RData")
save(aim2_ex_strava_5halfmi, file = "aim2_ex_strava_5halfmi.RData")

#--0.3 Load collision data---------###########
#for a first try, since I already have it loaded, use the GEARS data I already had.
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "gears_sf_5.RData")
load( file = "gears_dave_bikes.RData")
load(file = "buffer_e1_noint.RData")
load(file = "buffer_e0_noint.RData")

#update 2/16/2020 I am using Dave's GEARS data

#now, find intersection between gears and buffer
gears_dave_bikes_e1_noint = gears_dave_bikes %>% 
  st_intersection(buffer_e1_noint) %>% 
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E1",
    intersection_with_other_allowed = "NO"
    ) 

gears_dave_bikes_e0_noint = gears_dave_bikes %>% 
  st_intersection(buffer_e0_noint)  %>% dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E0",
    intersection_with_other_allowed = "NO"
  ) 

gears_dave_bikes_e1_any  = gears_dave_bikes %>% st_intersection(buffer_e1) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E1",
    intersection_with_other_allowed = "YES"
  ) 

gears_dave_bikes_e0_any = gears_dave_bikes %>% st_intersection(buffer_e0) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E0",
    intersection_with_other_allowed = "NO"
  ) 

#mapview(gears_dave_bikes_e1_any)

#out of curiosity, how many collisions at exposed vs unexposed intersections?
gears_sf_e1_e0_int = gears_dave_bikes %>%
  st_intersection(e1_e0_int) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "BOTH",
    intersection_with_other_allowed = "YES"
  ) 




dim(gears_dave_bikes_e1_noint)
dim(gears_dave_bikes_e0_noint)
dim(gears_sf_e1_e0_int)
dim(gears_dave_bikes)

#so there are 4 that aren't on either one. find them.
#stack
#of the four above, you need to left join them into the main file
#remove the geometry from them

gears_dave_bikes_e1_noint_nogeo = gears_dave_bikes_e1_noint %>% st_set_geometry(NULL)
gears_dave_bikes_e0_noint_nogeo = gears_dave_bikes_e0_noint %>% st_set_geometry(NULL)
gears_sf_e1_e0_int_nogeo = gears_sf_e1_e0_int %>% st_set_geometry(NULL)

gears_dave_whether_exposed = gears_sf_e1_e0_int_nogeo %>%
  bind_rows(gears_dave_bikes_e1_noint_nogeo) %>%
  bind_rows(gears_dave_bikes_e0_noint_nogeo) 


View(gears_dave_whether_exposed)
dim(gears_dave_whether_exposed)
dim(gears_dave_bikes)
mapview(gears_sf_e1_e0_int)

gears_dave_bikes_wrangle = gears_dave_bikes %>%
  left_join(gears_dave_whether_exposed, by = "mdg_id") %>%
  mutate(
    major_or_res = case_when(
      is.na(major_or_res) ~ "NEITHER",
      TRUE ~ major_or_res)
  ) %>%
  #alternate exposure definition based on my review of the case reports
  #If a crash occurred at an intersection between exposure types
  #(n=49; e.g., a primary road intersecting a residential road), then the case was 
  #classified as occurring on the road from which the bicyclist entered the intersection 
  #as noted on the police report. 
  
  mutate(
    #I'd like this to be numeric. easier to work with below.
    exposure_revised = 
           case_when(
              major_or_res_revised == "Major" ~ "E1",
              major_or_res_revised == "Res" ~ "E0",
              major_or_res_revised == "NA" ~ NA_character_,
              major_or_res == "BOTH" ~ NA_character_,
              major_or_res == "NEITHER" ~ NA_character_,
              TRUE ~ major_or_res
          ),
    
    #the number of cases occuring at intersections
    #between both exposure types for which I couldn't figure out the
    #origin street of the cyclist from the police report
    both_exposure_no_info = 
      case_when(
        major_or_res == "BOTH" &
          is.na(exposure_revised) == TRUE ~ 1,
        TRUE ~ 0
      
    )
  )

table(gears_dave_bikes_wrangle$both_exposure_no_info)
table(gears_dave_bikes_wrangle$exposure_revised)
names(gears_dave_bikes_wrangle)
gears_dave_bikes_wrangle %>%
  dplyr::select(major_or_res, major_or_res_revised, AccidentNo, mdg_id,
                exposure_revised) %>% View() #checks out


both_exposures =  gears_dave_bikes_wrangle %>% filter(major_or_res=="BOTH") %>%
  dplyr::select(mdg_id, major_or_res, AccidentNo, geometry) 

mapview(both_exposures,  col.regions = "red") +
  mapview(buffer_e1_noint)
gears_dave_bikes_wrangle %>% 
  filter(major_or_res=="BOTH") %>%
  dplyr::select(mdg_id, major_or_res, AccidentNo, geometry) %>% mapview()

mapview(gears_dave_bikes_wrangle)
#how many were classified as both - exposed vs unexposed intersections?
gears_dave_bikes_wrangle %>% filter(major_or_res=="BOTH") %>% 
  st_set_geometry(NULL) %>%
  mutate(dummy=1) %>%
  group_by(dummy) %>% summarise(n=n())

neither = gears_dave_bikes_wrangle %>% filter(major_or_res == "NEITHER") 
mapview(neither) + mapview(buffer_e1_noint) + mapview(buffer_e0_noint)
mapview(gears_dave_bikes) + mapview(buffer_e1_noint) + mapview(buffer_e0_noint)
#strange, angiers count is not coded as anything. exclude it from this analysis.
#it doesn't count in the bike-distance so it shouldn't count in the cases. OSM issue I guess

View(gears_dave_bikes_wrangle)
View(gears_dave_bikes_e0_noint)

#how many anywhere?


#-----Save stuff--------------##########
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 

save(gears_dave_bikes_wrangle, file = "gears_dave_bikes_wrangle.RData")
save(gears_dave_bikes, file = "gears_dave_bikes.RData")
save(gears_dave_whether_exposed, file = "gears_dave_whether_exposed.RData")
save(crashes_ratio, file = "crashes_ratio.RData")
load(file = "gears_dave_bikes_wrangle.RData")
load(file = "gears_dave_bikes.RData")

#exposure ratio in cases
e1_vs_e0 = gears_dave_bikes_wrangle %>%
  st_drop_geometry()%>%
  group_by(exposure_revised) %>%
  summarise(n=n())

crashes_e1 = e1_vs_e0 %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% pull()
crashes_e0 = e1_vs_e0 %>% filter(exposure_revised == "E0") %>% dplyr::select(n) %>% pull()
24+18+89
24+89
crashes_ratio = crashes_e1/crashes_e0 


crash_bootstrap_df_summary = crash_bootstrap_df %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(
    
    #exposed value
    e1_med = median(e1),
    e1_ll = quantile(e1, probs = c(0.025)),
    e1_ul = quantile(e1, probs = c(0.975)),
    
    #unexposed value
    e0_med = median(e0),
    e0_ll = quantile(e0, probs = c(0.025)),
    e0_ul = quantile(e0, probs = c(0.975)),
    
    #exposure ratio
    ratio_med = median(ratio),
    ratio_ll = quantile(ratio, probs = c(0.025)),
    ratio_ul = quantile(ratio, probs = c(0.975)),
 
  ) %>%
  
  ungroup() %>%
  mutate( 
    ratio_width = ratio_ul/ratio_ll,
    sample_type = "crashes",
    nreps_value = nreps,
  )


View(crash_bootstrap_df_summary)
mapview(gears_sf_e1_e0_int)
mapview(gears_sf_mixed_intersections)
dim(gears_sf_mixed_intersections) #63
mapview(gears_sf_5_unexposed, col.regions = "green") + mapview(buffer_prim_0)

#------0. Map of ridership by exposure status-----------####
bd_km_0 = edge_eco_a_aim2 %>% filter(major_or_res==0)
bd_km_1 = edge_eco_a_aim2 %>% filter(major_or_res==1)


summary(aim2_ex_strava_5halfmi_res$bike_distance_km_tactcnt)
summary(aim2_ex_strava_5halfmi_nores$bike_distance_km_tactcnt)

summary(edge_eco_a_aim2$length_km)
ggplot(aim2_ex_strava_5halfmi_res, aes(length_mdg_km)) + geom_histogram()
sd(edge_eco_a_aim2$length_km)

#a stratified map
library(tmap)
library(tmaptools)
tmap_mode("view")
#palette_explorer()
tm_basemap(leaflet::providers$CartoDB.Positron) +
tm_shape(bd_km_1) +
  tm_lines(
    #note, this could be useful if you don't like the 0.8 implied upper bound
    #https://geocompr.github.io/post/2019/tmap-color-scales/
    col = "bd_km",
    alpha = .9,
     # style = "fisher", #try a continuous scale
  style = "fixed",
     breaks = c(0, 250, 750, 1500, 3000, 5000, 8100), #set this as max
  lwd=2.5,
    stretch.palette = TRUE,
    textNA = NA,
    showNA = FALSE,
    title.col = "Bicycle distance (km) in Strava on primary through tertiary roads",
    palette = get_brewer_pal("YlOrRd",  contrast = c(0.5, 0.96)))  +
  tm_view(view.legend.position = c("right","bottom")) +
tm_shape(bd_km_0) +
  tm_lines(
    #note, this could be useful if you don't like the 0.8 implied upper bound
    #https://geocompr.github.io/post/2019/tmap-color-scales/
    col = "bd_km",
    alpha = .9,
    # style = "fisher", #try a continuous scale
    style = "fixed",
    breaks = c(0, 250, 750, 1500, 3000, 5000, 7200), #set this as max
    
    lwd=2.5, #It was 2.5
    stretch.palette = TRUE,
    textNA = NA,
    showNA = FALSE,
    title.col = "Bicycle distance (km) in Strava on residential roads             ",
    palette = get_brewer_pal("PuBuGn",   contrast = c(0.5, 0.96))) +
  tm_view(view.legend.position = c("right","bottom")) 
  

mapviewOptions(vector.palette = colorRampPalette(brewer.pal(2, "Set1")))
mapview(aim2_ex_strava_5halfmi, zcol = "major_or_res")

dim(aim2_ex_strava_5halfmi)

#------Point estimates for IRR----------######
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "aim2_ex_strava_5halfmi.RData")
aim2_ex_strava_5halfmi_summary = aim2_ex_strava_5halfmi %>%
  st_set_geometry(NULL) %>%

  group_by(major_or_res) %>%
  summarise(bd = sum(bike_distance_km_tactcnt, na.rm=TRUE)) 

#View(aim2_ex_strava_5halfmi_summary)
bd_e1= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==1) %>% dplyr::select(bd) %>% pull()
bd_e0= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==0) %>% dplyr::select(bd) %>% pull()

bd_e_ratio = bd_e1/bd_e0

e1_vs_e0_s = gears_dave_bikes_wrangle %>% 
  st_drop_geometry()%>%
#  sample_frac(size=1, replace=TRUE) %>% 
  group_by(exposure_revised) %>%
  summarise(n=n())
crashes_e1_s = e1_vs_e0_s %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% pull()
crashes_e0_s = e1_vs_e0_s %>% filter(exposure_revised == "E0") %>% dplyr::select(n)%>% pull()
crashes_ratio_pt_estimate = crashes_e1_s/crashes_e0_s
estimated_irr_unadjusted = crashes_ratio_pt_estimate/bd_e_ratio
rm(estimated_irr)
rm(e1_vs_e0_s)
rm(crashes_e0_s)
rm(crashes_e1_s)
#-------1. estimate variance of measure-at-risk sample-------------####
#-----to get variance in MAR, uncount it and then sample it with replacement---------------#
# bike_distance_km_tactcnt = tactcnt*length_mdg_km
names(aim2_ex_strava_5halfmi)
dim(aim2_ex_strava_5halfmi)
#Updating this 5/16/20 using the new dataset that contains all segments including those with no data
#Having memory issues. Break it in 2.
#re-sample these and calculate a new ratio

#test this
crash_bootstrap <- function(crash_s_id_val) {
  
  e1_vs_e0_s = gears_dave_bikes_wrangle %>% 
    st_drop_geometry()%>%
    sample_frac(size=1, replace=TRUE) %>% 
    group_by(exposure_revised) %>%
    summarise(n=n())
  
  crashes_e1_s = e1_vs_e0_s %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% rename(crashes_e1_s=n)
  crashes_e0_s = e1_vs_e0_s %>% filter(exposure_revised == "E0") %>% dplyr::select(n)%>% rename(crashes_e0_s=n)

  #I want all three measures in one row
  crashes_ratio_tibble =crashes_e1_s  %>% bind_cols(crashes_e0_s) %>%
    mutate(
        crashes_ratio_s = crashes_e1_s/crashes_e0_s, 
        crash_s_id = crash_s_id_val
           )
  
  return(crashes_ratio_tibble)
  
  
}
nreps = 100
nreps_list <-seq(1, nreps, by =1)
crash_bootstrap_df = nreps_list %>% map_dfr(crash_bootstrap)

#-----multi-level sampling--------#########
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "aim2_ex_strava_5halfmi.RData")
load(file = "crashes_ratio.RData")
load(file = "gears_dave_bikes_wrangle.RData")

n_unique_osm_name = aim2_ex_strava_5halfmi %>%
  st_set_geometry(NULL) %>% 
  group_by(OSM_NAME) %>% 
  summarise(
    n=n(), 
    bd=sum(bike_distance_km_tactcnt, na.rm=TRUE),
    tactcnt=sum(tactcnt, na.rm=TRUE),
    ) 

sum(n_unique_osm_name$bd, na.rm=TRUE)
mean(n_unique_osm_name$bd, na.rm=TRUE)*3352
mean(n_unique_osm_name$tactcnt, na.rm=TRUE)*3352
n_distinct(n_unique_osm_name)

n_unique_osm_name_e = aim2_ex_strava_5halfmi %>%
  st_set_geometry(NULL) %>% 
  group_by(OSM_NAME, major_or_res) %>% summarise(n=n()) #3352
n_distinct(n_unique_osm_name_e)

#so there are only 75 where exposure varies within osm group
n_distinct(n_unique_osm_name_e)-n_distinct(n_unique_osm_name)
n_distinct(n_unique_osm_name)/n_distinct(n_unique_osm_name_e)

#how many edges per group? about 6.5
n_distinct(aim2_ex_strava_5halfmi)/n_distinct(n_unique_osm_name)

rm(crash_s_id_val)
rm(s_id_val)
crash_s_id_val=5
s_id_val=5

#-----update 5/21/20 - I think you can recode this to be a binomial r.v.
nested_bootstrap <-function(s_id_val,
                            crash_s_id_val) {
  
  crashes_s = gears_dave_bikes_wrangle %>% 
    st_drop_geometry()%>%
    sample_frac(size=1, replace=TRUE) %>% 
    group_by(exposure_revised) %>%
    summarise(n=n()) %>% 
    #update 5/18/20, I want to keep this in long form. easier to summarize below.
    #this isn't always how I'd do this
    ungroup() %>% 
    rename(value=n) %>% 
    mutate(
      e_or_ratio = exposure_revised,
      measure = "n_crashes"
    )

  crashes_e1_s =   crashes_s %>% filter(e_or_ratio ==  "E1") %>% dplyr::select(value) %>% pull()
  crashes_e0_s =   crashes_s %>% filter(e_or_ratio == "E0") %>% dplyr::select(value) %>% pull()
  
  crashes_tibble = (crashes_e1_s/crashes_e0_s) %>% as_tibble() %>% 
    mutate(e_or_ratio =  "ratio",
           measure = "n_crashes") %>% 
    bind_rows(crashes_s) %>% 
    dplyr::select(-exposure_revised) %>% 
    filter(is.na(e_or_ratio)==FALSE) 


  osm_name_sample_weight = aim2_ex_strava_5halfmi %>% 
    st_set_geometry(NULL) %>% 
    #sampling will only work if there are non-zeros, so filter this to non-zeros,
    #so that the without-replacement sampling works
    filter(tactcnt > 0) %>% 
    #there are a few where exposure varies within osm_name so group by both
    #but we're linking the edge_id below anyway so will separate it then
    #this worked to make the variance a bit higher (more honest)
    group_by(OSM_NAME) %>% 
    summarise(
      bike_distance_km_tactcnt = sum(bike_distance_km_tactcnt, na.rm=TRUE), 
      length_mdg_km = sum(length_mdg_km, na.rm=TRUE),
      tactcnt = sum(tactcnt, na.rm=TRUE)) %>% 
    ungroup() %>% 
    #define sampling weights based on # times ridden in study period
    mutate(
      wt = tactcnt/sum(tactcnt, na.rm=TRUE)) %>% 
    arrange(-wt) %>% #sort descending
    mutate( nest_id = row_number()  ) %>% #to keep track 
    #so that we don't double count, don't sample these w/ replacement.
    #that way, the weighting by total count can be accomplished in the next step
    #but it only works if with replacement, so stick to that list, then uncount
    sample_frac(size=1, replace=FALSE, weight=wt)   %>% 
    #I have to do this b/c it's too big to do all in one so I have to split it up
    mutate(nest_id_s = row_number())  
    
  osm_name_sample_weight_4join = osm_name_sample_weight %>%  
    dplyr::select(OSM_NAME, nest_id, nest_id_s, wt)
  
  #I'm creating a new dataset rather than continuing with the piping
  #because I want to track which edges, too, and the above collapses over edges
  rides_in_nested_sample = aim2_ex_strava_5halfmi %>% 
    st_set_geometry(NULL) %>% 
    left_join(osm_name_sample_weight_4join, by = c("OSM_NAME")) %>% 
    filter(nest_id_s > 0) %>%    #where the other one is not missing
    filter(tactcnt > 0) %>%
    mutate(edge_id_s = row_number()) %>%   #note it doesn't correspond. just a number for splitting and combining
    #just keep a few variables to keep the size down for speed
    dplyr::select(edge_id_s, edge_id, OSM_NAME, major_or_res, tactcnt, wt, nest_id, length_mdg_km) %>% 
    uncount(tactcnt,  .remove=FALSE) #works without splitting
  
  #now sample that with replacement
sampled_rides_in_nested_sample = rides_in_nested_sample %>% 
    group_by(OSM_NAME) %>% 
    sample_frac(size=1, replace=TRUE)  %>%  #no weighting, but group by osm_name
    group_by(major_or_res) %>% 
    summarise(bd = sum(length_mdg_km)) %>% 
  #update 5/18/20, I want to keep this in long form. easier to summarize below.
  ungroup() %>% 
    rename(value=bd) %>% 
    mutate(
      e_or_ratio = case_when(
        major_or_res == 1 ~ "E1",
        major_or_res == 0 ~ "E0"
      ),
      measure = "bd"
    )

  bd_e1_s =   sampled_rides_in_nested_sample %>% 
    filter(e_or_ratio ==  "E1") %>% dplyr::select(value) %>% pull()
  bd_e0_s =   sampled_rides_in_nested_sample %>% 
    filter(e_or_ratio == "E0") %>% dplyr::select(value) %>% pull()
  
  irr_s_val = ((crashes_e1_s/crashes_e0_s)/(bd_e1_s /bd_e0_s )) %>% as_tibble() %>% 
    mutate( measure = "irr",   
            e_or_ratio = "ratio")

  table = (bd_e1_s/bd_e0_s) %>% as_tibble() %>% 
    mutate(e_or_ratio =  "ratio",
           measure = "bd") %>% 
    bind_rows(sampled_rides_in_nested_sample) %>% 
    bind_rows(irr_s_val) %>% 
    dplyr::select(-major_or_res) %>% 
    bind_rows(crashes_tibble) %>% #add the crashes data above
    mutate( 
      s_id=s_id_val,
      crash_s_id =crash_s_id_val
      )

  return(table)

}


#--------set the number of repetitions for both crashes and the nested bike-distance sample----#
s_id_reps = 10 
crash_id_reps = 10 
s_id_val_list <- seq(from = 1, to = s_id_reps, by = 1)
crash_s_id_val_list<- seq(from = 1, to = crash_id_reps, by = 1)


c_s_list_parallel = list(
  s_id_val =  s_id_val_list, 
  crash_s_id_val = crash_s_id_val_list)

#FInd all unique permutations of the lists, the result of which will be a data frame.
c_s_list_unique = expand.grid(c_s_list_parallel)

boot_df =  c_s_list_unique   %>% pmap_dfr(nested_bootstrap)

boot_df_summary =boot_df %>% 
  group_by(measure, e_or_ratio) %>% 
  summarise(
    median_value = median(value),
    ll = quantile(value, probs = c(0.025)),
    ul = quantile(value, probs = c(0.975))
  ) %>% 
  ungroup() %>%
  mutate( 
    ratio_width = ul/ll,
    n_s_id_reps = s_id_reps,
    n_crash_id_reps = crash_id_reps
  )
View(boot_df_summary)
options(scipen = 999)
var(nested_bootstrap_df$ratio)
#roll it back up because you will then apply your regression model to this dataset
#note, the regression model has the time component, so you're going to have to address that separately.


#--------Eventually I would have to add the estiated values here from my sf model----###########
dim(aim2_ex_strava_5halfmi)
aim2_ex_strava_5halfmi_uncount_1 = aim2_ex_strava_5halfmi  %>%
  st_set_geometry(NULL) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum < 10000) %>%
  #this restriction *should* stay in place because otherwise you will be re-sampling obs without bike data
  filter(tactcnt > 0) %>% 
  uncount(tactcnt,
          .remove=FALSE) %>%
  dplyr::select(
    edge_id,  tactcnt, bike_distance_km_tactcnt, length_mdg_km, major_or_res, name, OSM_NAME
  ) %>%
  #the uncount value should be:
  mutate( tactcnt_calc = bike_distance_km_tactcnt/ length_mdg_km,
          obs_id = row_number()) #for tracking 

aim2_ex_strava_5halfmi_uncount_2 = aim2_ex_strava_5halfmi  %>%
  st_set_geometry(NULL) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum >= 10000) %>%
  filter(tactcnt > 0) %>%
  uncount(tactcnt,
          .remove=FALSE) %>%
  dplyr::select(
    edge_id,  tactcnt, bike_distance_km_tactcnt, length_mdg_km, major_or_res, name, OSM_NAME
  ) %>%
  #the uncount value should be:
  mutate( tactcnt_calc = bike_distance_km_tactcnt/ length_mdg_km,
          obs_id = row_number()) #for tracking 

aim2_ex_strava_5halfmi_uncount = aim2_ex_strava_5halfmi_uncount_1 %>%
  bind_rows(aim2_ex_strava_5halfmi_uncount_2)

rm(aim2_ex_strava_5halfmi_uncount_1)
rm(aim2_ex_strava_5halfmi_uncount_2)

dim(aim2_ex_strava_5halfmi_uncount)
head(aim2_ex_strava_5halfmi_uncount)

#----save this since it takes a while------#
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",
                "Dissertation", "R_data", "analysis data"))
save(aim2_ex_strava_5halfmi_uncount, file = "aim2_ex_strava_5halfmi_uncount.RData")
load(file = "aim2_ex_strava_5halfmi_uncount.RData")

#--checks
ggplot(aim2_ex_strava_5halfmi , aes(length_mdg_km)) + geom_histogram() 
ggplot(aim2_ex_strava_5halfmi , aes(bike_distance_km_tactcnt)) + geom_histogram() 
summary(aim2_ex_strava_5halfmi$bike_distance_km_tactcnt)
options(scipen = 999)


#a histogram by exposure
ggplot(aim2_ex_strava_5halfmi , aes(bike_distance_km_tactcnt)) + geom_histogram() +
facet_grid(cols =  vars(major_or_res))

aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL) %>%
  group_by(major_or_res) %>% 
  summarise(
    dist_bd_90 = quantile(bike_distance_km_tactcnt, probs = c(0.90)),
    dist_bd_95 = quantile(bike_distance_km_tactcnt, probs = c(0.95)),
    
    dist_length_90 = quantile(length_mdg_km, probs = c(0.90)),
    dist_length_95 = quantile(length_mdg_km, probs = c(0.95))
    
    )

aim2_ex_strava_5halfmi %>% filter(bike_distance_km_tactcnt > 8000) %>% mapview()
names(aim2_ex_strava_5halfmi)


#re-sample the huge dataset and calculate the variance in the ratio estimator
#(#this is bootstrapping)

mar_var_calc <-function(s_id_val) {
mar_sample = aim2_ex_strava_5halfmi_uncount %>%

#  group_by(name) %>%
  sample_frac(size=1,
              replace=TRUE) %>%
  group_by(major_or_res) %>%
  summarise(bd = sum(length_mdg_km))

bd_s_e1= mar_sample %>% filter(major_or_res==1) %>% dplyr::select(bd) %>% pull() %>% as_tibble()
bd_s_e0= mar_sample %>% filter(major_or_res==0) %>% dplyr::select(bd) %>% pull() %>% as_tibble()

bd_s_ratio = bd_s_e1/bd_s_e0

irr_s = crashes_ratio/bd_s_ratio


#I want all three measures in one row
bd_s_ratio_tibble = as_tibble(bd_s_ratio) %>%
  bind_cols(bd_s_e1) %>%
  bind_cols(bd_s_e0) %>%
  bind_cols(irr_s) %>%
  dplyr::rename(ratio=value, e1=value1, e0=value2, irr=value3) %>%
  mutate(s_id=s_id_val)

return(bd_s_ratio_tibble)

}

View(mar_sample)

#nested version

mar_var_calc_n <-function(s_id_val) {
  mar_sample = aim2_ex_strava_5halfmi_uncount %>%
    mutate(obs_id = row_number()) %>% 
    filter(obs_id <10000) %>% 
    group_by(name) %>%
    sample_frac(size=1,
                replace=TRUE) %>%
    group_by(major_or_res) %>%
    summarise(bd = sum(length_mdg_km))
  
  bd_s_e1= mar_sample %>% filter(major_or_res==1) %>% dplyr::select(bd) %>% pull() %>% as_tibble()
  bd_s_e0= mar_sample %>% filter(major_or_res==0) %>% dplyr::select(bd) %>% pull() %>% as_tibble()
  
  bd_s_ratio = bd_s_e1/bd_s_e0
  
  irr_s = crashes_ratio/bd_s_ratio
  
  
  #I want all three measures in one row
  bd_s_ratio_tibble = as_tibble(bd_s_ratio) %>%
    bind_cols(bd_s_e1) %>%
    bind_cols(bd_s_e0) %>%
    bind_cols(irr_s) %>%
    dplyr::rename(ratio=value, e1=value1, e0=value2, irr=value3) %>%
    mutate(s_id=s_id_val)
  
  return(bd_s_ratio_tibble)
  
}

nreps = 10
nreps_list <-seq(1, nreps, by =1)
#iterate nreps_list through the values of s_id. there's only one argument, so use map_dfr.
#don't need pmap
mar_var_df = nreps_list %>% map_dfr(mar_var_calc) %>% mutate(nested= 0)
var(mar_var_df$ratio)
mar_var_df_n = nreps_list %>% map_dfr(mar_var_calc_n) %>% mutate(nested= 1)
var(mar_var_df_n$ratio)
mar_var_df_both = mar_var_df %>% rbind(mar_var_df_n)

View(mar_var_df_both)
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",
                "Dissertation", "R_data", "analysis data"))
save(mar_var_df_both, file = "mar_var_df_both")



#-------1.1. calculate summary statistics for the bootstrapped samples----##########
mar_var_df_summary = mar_var_df %>% rbind(mar_var_df_n ) %>% 
  group_by(nested) %>%
  summarise(
    
    #exposed value
    e1_med = median(e1),
    e1_ll = quantile(e1, probs = c(0.025)),
    e1_ul = quantile(e1, probs = c(0.975)),
    
    #unexposed value
    e0_med = median(e0),
    e0_ll = quantile(e0, probs = c(0.025)),
    e0_ul = quantile(e0, probs = c(0.975)),

    #exposure ratio
    ratio_med = median(ratio),
    ratio_ll = quantile(ratio, probs = c(0.025)),
    ratio_ul = quantile(ratio, probs = c(0.975)),
    
    #irr
    irr_med = median(irr),
    irr_ll = quantile(irr, probs = c(0.025)),
    irr_ul = quantile(irr, probs = c(0.975))

    
  ) %>%
  
  ungroup() %>%
  mutate( 
    ratio_width = ratio_ul/ratio_ll,
    sample_type = "MAR",
    nreps_value = nreps,
  )


View(mar_var_df_summary)







#4-----when you run your sampling-fraction model through, you have to figure out which --------##########
#edges are sampled when each osm_name is sampled. build off of this code.

nested_bootstrap_bymo <-function(s_id_val) {
  
  osm_name_sample_weight = aim2_ex_strava_5halfmi %>% 
    st_set_geometry(NULL) %>% 
    #sampling will only work if there are non-zeros, so filter this to non-zeros,
    #so that the without-replacement sampling works
    filter(tactcnt > 0) %>% 
    group_by(OSM_NAME, major_or_res) %>% #there are a few where exposure varies within osm_name so group by both
    summarise(
      bike_distance_km_tactcnt = sum(bike_distance_km_tactcnt, na.rm=TRUE), 
      length_mdg_km = sum(length_mdg_km, na.rm=TRUE),
      tactcnt = sum(tactcnt, na.rm=TRUE)) %>% 
    ungroup() %>% 
    #define sampling weights based on # times ridden in study period
    mutate(
      wt = tactcnt/sum(tactcnt, na.rm=TRUE)) %>% 
    arrange(-wt) %>% #sort descending
    mutate( nest_id = row_number()  ) %>% #to keep track 
    #so that we don't double count, don't sample these w/ replacement.
    #that way, the weighting by total count can be accomplished in the next step
    #but it only works if with replacement, so stick to that list, then uncount
    sample_frac(size=1, replace=FALSE, weight=wt)   %>% 
    #I have to do this b/c it's too big to do all in one so I have to split it up
    mutate(nest_id_s = row_number())
  
  osm_name_sample_weight_4join = osm_name_sample_weight %>%  dplyr::select(OSM_NAME, major_or_res, nest_id, nest_id_s, wt)
  
  #I'm creating a new dataset rather than continuing with the piping
  #because I want to track which edges, too, and the above collapses over edges
  rides_in_nested_sample = aim2_ex_strava_5halfmi %>% 
    st_set_geometry(NULL) %>% 
    left_join(osm_name_sample_weight_4join, by = c("OSM_NAME", "major_or_res")) %>% 
    filter(nest_id_s > 0) %>%    #where the other one is not missing
    filter(tactcnt > 0) %>%
    mutate(edge_id_s = row_number()) %>%   #note it doesn't correspond. just a number for splitting and combining
    #just keep a few variables to keep the size down for speed
    dplyr::select(edge_id_s, edge_id, OSM_NAME, major_or_res, tactcnt, wt, nest_id, length_mdg_km) %>% 
    uncount(tactcnt,  .remove=FALSE) #works without splitting
  
  #now sample that with replacement
  names(sampled_rides_in_nested_sample)
  sampled_rides_in_nested_sample = rides_in_nested_sample %>% 
    group_by(OSM_NAME) %>% 
    sample_frac(size=1, replace=TRUE)  %>%  #no weighting, but group by osm_name
    group_by(major_or_res) %>% 
    summarise(bd = sum(length_mdg_km))
  
  
  #n for nested
  bd_s_e1_n = sampled_rides_in_nested_sample %>% filter(major_or_res==1) %>% 
    dplyr::select(bd) %>% pull() %>% as_tibble()
  bd_s_e0_n = sampled_rides_in_nested_sample %>% filter(major_or_res==0) %>% 
    dplyr::select(bd) %>% pull() %>% as_tibble()
  
  bd_s_ratio_n = bd_s_e1_n/bd_s_e0_n
  
  irr_s_n = crashes_ratio/bd_s_ratio_n #nested sample
  
  bd_s_ratio_tibble_n = as_tibble(bd_s_ratio_n) %>%
    bind_cols(bd_s_e1_n) %>%
    bind_cols(bd_s_e0_n) %>%
    bind_cols(irr_s_n) %>%
    dplyr::rename(ratio=value, e1=value1, e0=value2, irr=value3) %>%
    mutate(s_id=s_id_val)
  
  return(bd_s_ratio_tibble_n)
  
}

















#----------2.  density sample--------------------------#########
#First, some checks
#are there any dupes in this?
dupes =  aim2_ex_strava_5halfmi %>%
  st_set_geometry(NULL) %>%
  filter(tactcnt > 0) %>%
  group_by(edge_id) %>%
  summarise(edge_id_count = n()) %>%
  mutate(
    dupe = case_when(
      edge_id_count > 1 ~ 1,
      TRUE ~ 0)) %>%
  filter(dupe==1)#none now, good. ugh, that took too long.
#none

#need to figure out if there are super high values 
quantile(aim2_ex_strava_5halfmi$bike_distance_km_tactcnt, probs = c(0, 0.01, .9, .95, .975, .98, .99))
summary(aim2_ex_strava_5halfmi$length_mdg_km)
summary(aim2_ex_strava_5halfmi$bike_distance_km_tactcnt)

# #note,
# #----------bike distance--------------------------#
# bike_distance_km_tactcnt = tactcnt*length_mdg_km

dens_var_calc <-function(s_id_val) {
  dens_sample =aim2_ex_strava_5halfmi  %>% 
    st_set_geometry(NULL) %>%
    filter(tactcnt > 0) %>%
    as_tibble() %>%
    #does it need to be an interger? wtf is going on?
    # mutate(  bike_distance_km_integer = as.integer(bike_distance_km_tactcnt)) %>%
    sample_frac( weight = bike_distance_km_tactcnt,
                 size = 1,
                 replace = TRUE) %>%
    #keep only a few variables.
    dplyr::select(edge_id,
                  # bike_distance_km_integer,
                  bike_distance_km_tactcnt,
                  length_mdg_km,
                  major_or_res
    ) %>%
    group_by(major_or_res) %>%
    summarise(n = n())
  
  n_s_e1= dens_sample %>% filter(major_or_res==1) %>% dplyr::select(n) %>% pull()  %>% as_tibble()
  n_s_e0= dens_sample %>% filter(major_or_res==0) %>% dplyr::select(n) %>% pull()  %>% as_tibble()
  
  n_s_ratio = n_s_e1/n_s_e0
  
  
  #I want all three measures in one row
  n_s_ratio_tibble = as_tibble(n_s_ratio) %>%
    bind_cols(n_s_e1) %>%
    bind_cols(n_s_e0) %>%
    dplyr::rename(ratio=value, e1=value1, e0=value2) %>%
    mutate(s_id=s_id_val)
  
  return(n_s_ratio_tibble)
}


nreps = 1000
nreps_list <-seq(1, nreps, by =1)
#iterate nreps_list through the values of s_id. there's only one argument, so use map_dfr.
#don't need pmap

dens_var_df = nreps_list %>% map_dfr(dens_var_calc) #here the function is the object.

#-------2.1. calculate summary statistics for the bootstrapped samples----##########
dens_var_df_summary = dens_var_df %>%
  #create a dummy variable over which to summarise
  mutate(dummy = 1) %>%
  group_by(dummy) %>%
  summarise(
    
    #exposed value
    e1_med = median(e1),
    e1_ll = quantile(e1, probs = c(0.025)),
    e1_ul = quantile(e1, probs = c(0.975)),
    
    #unexposed value
    e0_med = median(e0),
    e0_ll = quantile(e0, probs = c(0.025)),
    e0_ul = quantile(e0, probs = c(0.975)),
    
    #ratio
    ratio_med = median(ratio),
    ratio_ll = quantile(ratio, probs = c(0.025)),
    ratio_ul = quantile(ratio, probs = c(0.975))
  ) %>%
  
  ungroup() %>%
  #width of ratio
  mutate( 
    ratio_width = ratio_ul/ratio_ll,
    sample_type = "Density 1",
    nreps_value = nreps,
  )

#View(dens_var_df_summary)

#------density sampling truncated @ 97.5th-------#####

#2/2/2020 deleted this. can re-do it if you want.

quantile(aim2_ex_strava_5halfmi$bike_distance_km_tactcnt, probs = c(0, 0.01, .9, .95, .975, .98, .99))

#------3. density sampling cyclist-segment combinations-------#####
dens_v2_var_calc <-function(s_id_val) {
  dens_v2_sample = aim2_ex_strava_5halfmi_uncount %>%
    sample_frac(size=1,
                replace=TRUE,
                weight = length_mdg_km
    ) %>%
    group_by(major_or_res) %>%
    summarise(n = n())
  
  n_v2_s_e1= dens_v2_sample %>% filter(major_or_res==1) %>% dplyr::select(n) %>% pull() %>% as_tibble()
  n_v2_s_e0= dens_v2_sample %>% filter(major_or_res==0) %>% dplyr::select(n) %>% pull() %>% as_tibble()
  
  n_v2_s_ratio = n_v2_s_e1/n_v2_s_e0
  
  #I want all three measures in one row
  n_v2_s_ratio_tibble = as_tibble(n_v2_s_ratio) %>%
    bind_cols(n_v2_s_e1) %>%
    bind_cols(n_v2_s_e0) %>%
    dplyr::rename(ratio=value, e1=value1, e0=value2) %>%
    mutate(s_id=s_id_val)
  
  return(n_v2_s_ratio_tibble)
}

nreps = 1000
nreps_list <-seq(1, nreps, by =1)

dens_v2_var_df = nreps_list %>% map_dfr(dens_v2_var_calc) 

dens_v2_var_df_summary = dens_v2_var_df %>%
  #create a dummy variable over which to summarise
  mutate(dummy = 1) %>%
  group_by(dummy) %>%
  summarise(
    
    #exposed value
    e1_med = median(e1),
    e1_ll = quantile(e1, probs = c(0.025)),
    e1_ul = quantile(e1, probs = c(0.975)),
    
    #unexposed value
    e0_med = median(e0),
    e0_ll = quantile(e0, probs = c(0.025)),
    e0_ul = quantile(e0, probs = c(0.975)),
    
    #ratio
    ratio_med = median(ratio),
    ratio_ll = quantile(ratio, probs = c(0.025)),
    ratio_ul = quantile(ratio, probs = c(0.975))
  ) %>%
  
  ungroup() %>%
  #width of ratio
  mutate(
    ratio_width = ratio_ul/ratio_ll,
    sample_type = "Density 2",
    nreps_value = nreps
  )

#stack results on top of one another for table 1
aim2_empirical_table1 = mar_var_df_summary %>%
  bind_rows(dens_var_df_summary) %>%
  bind_rows(dens_v2_var_df_summary) 

View(aim2_empirical_table1)

#----save this since it takes a while------#
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",
                "Dissertation", "R_data", "analysis data"))
save(aim2_empirical_table1, file = "aim2_empirical_table1.RData")
load(file = "aim2_empirical_table1t.RData")

