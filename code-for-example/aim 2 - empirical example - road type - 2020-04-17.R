
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
 

dim(edge_rollup_aim3)
dim(edge_rollup_elig_speednomiss)
table(edge_rollup_elig_speednomiss$highway_6cat)
table(edge_rollup_aim3$highway_6cat)

st_crs(edge_rollup_elig_speednomiss)
st_crs(mp_sf_5halfmi)
#dichotomize as primary-tertiary or residential


#0.1. Exposure data classification-------------##########
#note, this is a 5.5 mile radius
aim2_ex_strava_5halfmi = edge_rollup_aim3 %>%
  
  #restrict to only residential or major streets
  filter(highway_6cat == "primary-tertiary road" |
           highway_6cat == "residential road"
           ) %>%
  #apparently the beltline is a primary road?? get rid of it
  filter(OSM_NAME != "Beltline") %>%
  
  #limit to non-missing values of bike distnace, I guess?
  filter(bike_distance_km_tactcnt > 0) %>%
  
  #"Exposed" should be non-reidential road, acutally, because
  #it will be omre dangerous.
  mutate(major_or_res = case_when(
    highway_6cat == "primary-tertiary road" ~ 1,
    highway_6cat == "residential road" ~ 0,
  )) 

names(aim2_ex_strava_5halfmi)


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

#--0.3 Load collision data---------###########
#for a first try, since I already have it loaded, use the GEARS data I already had.
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "gears_sf_5.RData")
load( file = "gears_dave_bikes.RData")

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

mapview(gears_sf_e1_e0_int)
mapview(gears_sf_mixed_intersections)
dim(gears_sf_mixed_intersections) #63
mapview(gears_sf_5_unexposed, col.regions = "green") + mapview(buffer_prim_0)

#-----Save stuff--------------##########
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
save(gears_dave_bikes_wrangle, file = "gears_dave_bikes_wrangle.RData")
save(gears_dave_bikes, file = "gears_dave_bikes.RData")
save(gears_dave_whether_exposed, file = "gears_dave_whether_exposed.RData")
load(file = "gears_dave_bikes_wrangle.RData")
load(file = "gears_dave_bikes.RData")

#------0. Map of ridership by exposure status-----------####


aim2_ex_strava_5halfmi_res = aim2_ex_strava_5halfmi %>% filter(major_or_res==0)
aim2_ex_strava_5halfmi_nores = aim2_ex_strava_5halfmi %>% filter(major_or_res==1)

summary(aim2_ex_strava_5halfmi_res$major_or_res)
summary(aim2_ex_strava_5halfmi_nores$major_or_res)

summary(aim2_ex_strava_5halfmi_res$bike_distance_km_tactcnt)
summary(aim2_ex_strava_5halfmi_nores$bike_distance_km_tactcnt)

summary(aim2_ex_strava_5halfmi_res$length_mdg_km)
ggplot(aim2_ex_strava_5halfmi_res, aes(length_mdg_km)) + geom_histogram()
sd(aim2_ex_strava_5halfmi_res$length_mdg_km)

#a stratified map
tmap_mode("view")
#palette_explorer()
tm_basemap(leaflet::providers$CartoDB.Positron) +
tm_shape(aim2_ex_strava_5halfmi_nores) +
  tm_lines(
    #note, this could be useful if you don't like the 0.8 implied upper bound
    #https://geocompr.github.io/post/2019/tmap-color-scales/
    col = "bike_distance_km_tactcnt",
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
tm_shape(aim2_ex_strava_5halfmi_res) +
  tm_lines(
    #note, this could be useful if you don't like the 0.8 implied upper bound
    #https://geocompr.github.io/post/2019/tmap-color-scales/
    col = "bike_distance_km_tactcnt",
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
aim2_ex_strava_5halfmi_summary = aim2_ex_strava_5halfmi %>%
  st_set_geometry(NULL) %>%

  group_by(major_or_res) %>%
  summarise(bd = sum(bike_distance_km_tactcnt, na.rm=TRUE)) 

#View(aim2_ex_strava_5halfmi_summary)
bd_e1= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==1) %>% dplyr::select(bd) %>% pull()
bd_e0= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==0) %>% dplyr::select(bd) %>% pull()

bd_e_ratio = bd_e1/bd_e0
estimated_ir = crashes_ratio/bd_e_ratio

#-------1. estimate variance of measure-at-risk sample-------------####
#-----to get variance in MAR, uncount it and then sample it with replacement---------------#
# bike_distance_km_tactcnt = tactcnt*length_mdg_km
names(aim2_ex_strava_5halfmi)
dim(aim2_ex_strava_5halfmi)
aim2_ex_strava_5halfmi_uncount = aim2_ex_strava_5halfmi  %>%
  st_set_geometry(NULL) %>%
  mutate(rownum = row_number()) %>%
  # filter(rownum < 10) %>%
  filter(tactcnt > 0) %>%
  uncount(tactcnt,
          .remove=FALSE) %>%
  dplyr::select(
    edge_id,  tactcnt, bike_distance_km_tactcnt, length_mdg_km, major_or_res
  ) %>%
  #the uncount value should be:
  mutate( tactcnt_calc = bike_distance_km_tactcnt/ length_mdg_km)

dim(aim2_ex_strava_5halfmi_uncount)
View(aim2_ex_strava_5halfmi_uncount)
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


nreps = 1000
nreps_list <-seq(1, nreps, by =1)
#iterate nreps_list through the values of s_id. there's only one argument, so use map_dfr.
#don't need pmap
mar_var_df = nreps_list %>% map_dfr(mar_var_calc) 
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY", "General research",
                "Dissertation", "R_data", "analysis data"))
save(mar_var_df, file = "mar_var_df.RData")

#-------1.1. calculate summary statistics for the bootstrapped samples----##########
mar_var_df_summary = mar_var_df %>%
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



