
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

setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "edge_rollup_aim3.RData")
load(file = "edge_rollup_aim3_inczeros.RData")
 
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
    highway_6cat == "residential road" ~ 0
  ),
    
    #is the street more of an east west street or more of a north south street?
    x_abs_diff = abs(X1-X2),
    y_abs_diff = abs(Y1-Y2),
  #north-south street if y_abs is greater than x_abs
    north_south = case_when(
      y_abs_diff > x_abs_diff ~ 1,
      TRUE ~ 0
    )
    
  ) 


# aim2_ex_strava_5halfmi %>% dplyr::select(edge_id, X1, X2, Y1, Y2, x_abs_diff, y_abs_diff, major_or_res, north_south ) %>% View()
# #check how I did
# aim2_ex_strava_5halfmi %>% 
#   filter(north_south==1) %>%
#   mutate(rownum = row_number()) %>% 
#   filter(rownum < 50) %>% 
#   mapview()
# aim2_ex_strava_5halfmi %>% 
#   filter(north_south==0) %>%
#   mutate(rownum = row_number()) %>% 
#   filter(rownum < 50) %>% 
#   mapview() #worked

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
#look at the missings
# aim2_ex_strava_5halfmi %>% filter(is.na(OSM_NAME)==TRUE) %>% mapview() #no missings!
# aim2_ex_strava_5halfmi %>% filter(is.na(name)==TRUE) %>% mapview() #no missings!

setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
save(aim2_ex_strava_5halfmi, file = "aim2_ex_strava_5halfmi.RData")
#-----0.1.1 Modeled exposure data---------######
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "aim2_ex_strava_5halfmi.RData")


#0.2. Create a buffer area. One for exposed and one for unexposed.-------------##########

#update 5/25/2020 - creating versions that are both a union (single polygon) and edge level

#-------Exposed - including intersections with unexposed-------------------------##
names(aim2_ex_strava_5halfmi)
buffer_e1 = aim2_ex_strava_5halfmi %>% 
  filter(major_or_res == 1) %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(50 ) %>% #30 feet
  st_transform(4326) %>% 
  dplyr::select(edge_id, geometry, major_or_res, north_south)


#-------Unexposed - including intersections with exposed-------------------------##
buffer_e0 = aim2_ex_strava_5halfmi %>% 
  filter(major_or_res == 0) %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(50 ) %>% #30 feet
  st_transform(4326) %>% 
  dplyr::select(edge_id, geometry, major_or_res, north_south)

#-Each with a unary union  
#(the shorter version was giving me inconsistent results, so going through this long way)
buffer_e1_union = aim2_ex_strava_5halfmi %>% 
  filter(major_or_res == 1) %>%
  st_union(by_feature=FALSE) %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(50 ) %>% #30 feet
  st_transform(4326)  

buffer_e0_union = aim2_ex_strava_5halfmi %>% 
  filter(major_or_res == 0) %>%
  st_union(by_feature=FALSE) %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 +x_0=699999.9999999999
             +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0") %>%
  st_buffer(50 ) %>% #30 feet
  st_transform(4326)  


#-----------no intersection versions----------------------------#
#---------Exposed, excluding intersections with unexposed
buffer_e1_noint = buffer_e1 %>% st_difference(buffer_e0_union)  
buffer_e1_noint_union = buffer_e1_union %>% st_difference(buffer_e0_union )

#---------Unexposed, excluding intersections with exposed
buffer_e0_noint = buffer_e0 %>% st_difference(buffer_e1_union)  #careful! takes a while. load below
buffer_e0_noint_union = buffer_e0_union %>% st_difference(buffer_e1_union )

#intersection between exposed and unexposed
e1_e0_int = buffer_e0_union %>%  st_intersection(buffer_e1_union) 




#---------mapview checks--------------------------------------#
options(viewer = NULL) #send viewer to the browser
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

#the by edge versions, excluding intersections
mapview(buffer_e0_noint, col.regions = "red", alpha=0) + 
  mapview(buffer_e1_union, col.regions = "blue", alpha=0)

#the unioned versions, excluding intersections
mapview(buffer_e1_noint_union,
        col.regions = "red", alpha=0) + 
  mapview(buffer_e0_noint_union, col.regions = "blue", alpha=0)

mapview(any_intersection)
mapview(e1_e0_int)

#-----save these buffers for speed----------##
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
save(buffer_e1, file = "buffer_e1.RData")
save(buffer_e0, file = "buffer_e0.RData")
save(buffer_e1_noint, file = "buffer_e1_noint.RData")
save(buffer_e0_noint, file = "buffer_e0_noint.RData")
save(e1_e0_int, file = "e1_e0_int.RData")
save(buffer_e1_union, file = "buffer_e1_union.RData")
save(buffer_e0_union, file = "buffer_e0_union.RData")
save(buffer_e1_noint_union, file = "buffer_e1_noint_union.RData")
save(buffer_e0_noint_union, file = "buffer_e0_noint_union.RData")
save(buffer_not_unioned, file = "buffer_not_unioned.RData")


#-------Load buffer data-------------#####
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "buffer_e1_noint.RData")
load(file = "buffer_e0_noint.RData")
load(file = "buffer_e1_union.RData")
load(file = "buffer_e0_union.RData")
load(file = "buffer_e1_noint_union.RData")
load(file = "buffer_e0_noint_union.RData")
load(file = "buffer_e1.RData")
load(file = "buffer_e0.RData")
load(file = "e1_e0_int.RData")

#--0.3 Load collision data---------###########
#for a first try, since I already have it loaded, use the GEARS data I already had.
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "gears_sf_5.RData")
load( file = "gears_dave_bikes.RData")

#update 2/16/2020 I am using Dave's GEARS data

#now, find intersection between gears and buffer
#View(gears_dave_bikes_e1_noint)

#--------------------0.3.1. Summarize By collision------------------------------------------------------######
gears_dave_bikes_e1_noint = gears_dave_bikes %>% 
  st_intersection(buffer_e1_noint_union) %>% 
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E1",
    intersection_with_other_allowed = "NO"
    ) 


gears_dave_bikes_e0_noint = gears_dave_bikes %>% 
  st_intersection(buffer_e0_noint_union)  %>% 
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E0",
    intersection_with_other_allowed = "NO"
  ) 

gears_dave_bikes_e1_any  = gears_dave_bikes %>% 
  st_intersection(buffer_e1_union) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E1",
    intersection_with_other_allowed = "YES"
  ) 

gears_dave_bikes_e0_any = gears_dave_bikes %>% 
  st_intersection(buffer_e0_union) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "E0",
    intersection_with_other_allowed = "NO"
  ) 



#out of curiosity, how many collisions at exposed vs unexposed intersections?
gears_sf_e1_e0_int = gears_dave_bikes %>%
  st_intersection(e1_e0_int) %>%
  dplyr::select(mdg_id, geometry) %>%
  mutate(
    major_or_res= "BOTH",
    intersection_with_other_allowed = "YES"
  ) 


#mapview(gears_sf_e1_e0_int)

nrow(gears_dave_bikes_e1_noint)
nrow(gears_dave_bikes_e0_noint)
nrow(gears_sf_e1_e0_int)
names(gears_dave_bikes)

mapview(gears_dave_bikes)
nrow(gears_dave_bikes_e1_noint) + nrow(gears_dave_bikes_e0_noint) + nrow(gears_sf_e1_e0_int)

gears_dave_bikes_e1_noint_nogeo = gears_dave_bikes_e1_noint %>% st_set_geometry(NULL)
gears_dave_bikes_e0_noint_nogeo = gears_dave_bikes_e0_noint %>% st_set_geometry(NULL)
gears_sf_e1_e0_int_nogeo = gears_sf_e1_e0_int %>% st_set_geometry(NULL)

gears_dave_whether_exposed = gears_sf_e1_e0_int_nogeo %>%
  bind_rows(gears_dave_bikes_e1_noint_nogeo) %>%
  bind_rows(gears_dave_bikes_e0_noint_nogeo) 

names(gears_dave_bikes)
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
              mdg_id == "325" ~ "E1", #hardcode. not sure why it's not getting picked up. 5/25/20
              major_or_res_revised == "NA" ~ NA_character_,
              major_or_res == "BOTH" ~ NA_character_,
              major_or_res == "NEITHER" ~ NA_character_,
              TRUE ~ major_or_res
          ),
    
    #the number of cases occcuring at intersections
    #between both exposure types for which I couldn't figure out the
    #origin street of the cyclist from the police report
    both_exposure_no_info = 
      case_when(
        major_or_res == "BOTH" &
          is.na(exposure_revised) == TRUE ~ 1,
        TRUE ~ 0
      
    ),
    
    #determine the bicycle direction...for classifying to edge...this might work
    bike_dir = case_when(
      veh1_bike == 1 ~ DirVeh1,
      veh2_bike == 1 ~ DirVeh2,
      TRUE ~ NA_character_
    )
  ) %>% 
  
  #5/25/20 whittle down to things I know
  dplyr::select(AccidentNo, Date, Time, Route, VehType1, VehType2, DirVeh1, DirVeh2,
                veh2_bike, veh1_bike, after_oct2016, gtech_pd, Ferst_Fowler, exclude,
                exclude_why, major_or_res, both_exposure, origin_street_mdg, 
                crash_check_comment, crash_check_comment2, mdg_id, bike_dir,
                exposure_revised, both_exposure_no_info, geometry) %>% 
  filter(is.na(exclude)==TRUE) #exclude the exclusions 

names(gears_dave_bikes_wrangle)
#5/25/20 - good, the numbers are growing a bit. using more of the data.
nrow(gears_dave_bikes_wrangle)
#None!
aim2_ex_strava_5halfmi_nogeo = aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL)
gears_dave_bikes_wrangle_nogeo = gears_dave_bikes_wrangle %>% 
  st_set_geometry(NULL)


gears_dave_bikes_wrangle %>% 
  dplyr::select(exposure_revised, major_or_res, both_exposure_no_info, major_or_res, mdg_id, AccidentNo,
                bike_dir, crash_check_comment, origin_street_mdg, exclude_why, crash_check_comment2, gtech_pd,
                origin_street_mdg) %>% 
  View()

mapview_missinggears = gears_dave_bikes_wrangle %>% filter(is.na(exposure_revised)==TRUE) %>% mapview(col.regions = "orange")
mapview_e1 = buffer_e1_noint_union %>% mapview(col.regions="red", alpha=0)
mapview_e0 = buffer_e0_noint_union %>% mapview(col.regions="blue", alpha=0)

table(gears_dave_bikes_wrangle$exposure_revised)
#-----Save stuff--------------##########
aim2_ex_strava_5halfmi_nogeo = aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL)
gears_dave_bikes_wrangle_nogeo = gears_dave_bikes_wrangle %>% 
  st_set_geometry(NULL)
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 

save(gears_dave_bikes_wrangle, file = "gears_dave_bikes_wrangle.RData")
save(gears_dave_bikes, file = "gears_dave_bikes.RData")
save(gears_dave_bikes_wrangle_nogeo, file = "gears_dave_bikes_wrangle_nogeo.RData")

#--------------------0.3.2. Summarize By Edge (so can link with edge vars) -------------------------------------------######
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "gears_dave_bikes_wrangle.RData")
load(file = "gears_dave_bikes_wrangle_nogeo.RData")

#Approach:
# 1. Create a dataset each of exposed cases and unexposed cases.
# 2. Get the intersection of the buffered non-unioned layer including intersections for a given exposure value

table(gears_dave_bikes_wrangle$exposure_revised)
gears_e1 = gears_dave_bikes_wrangle %>%  filter(exposure_revised=="E1") %>% st_transform(4326)
gears_e0 = gears_dave_bikes_wrangle %>%  filter(exposure_revised=="E0") %>% st_transform(4326)

names(gears_e1)
#----------------------3.2.E1----------------------##########
buffer_e1_wgears0 = buffer_e1 %>% 
  st_transform(4326) %>% 
  #With this code, the same crash is getting assigned to many IDs. I just need it to pick one.
  #As long as it's assigned to one of them, it doesn't really matter which.
  st_intersection(gears_e1) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(edge_id, mdg_id, north_south, bike_dir,) %>% 
  #match on east-west vs noth-south street
  mutate(
    direction_match = case_when(
      (bike_dir ==  "North" | bike_dir == "South") & north_south==1 ~ 1,
      (bike_dir == "East" | bike_dir == "West") & north_south==0 ~ 1,
      TRUE ~0
    )
  ) 

#Does everyone have a match?
e1_dir_match_list =  buffer_e1_wgears0 %>% 
    group_by(mdg_id, direction_match) %>% 
    summarise(n=n()) %>% 
    filter(direction_match==1) %>%  
    mutate(any_match= 1) %>% 
    dplyr::select(-n, -direction_match) %>% 
    ungroup()

#so this code, where possible picks an edge ID that matches the stated direction the bicycle was going
#there are a few where the match didn't work ... no big deal
buffer_e1_wgears = buffer_e1_wgears0 %>% 
  left_join(e1_dir_match_list, by = "mdg_id") %>% 
  group_by(mdg_id) %>% 
  mutate(keep = case_when(
    any_match == 1 ~ direction_match, #if it has any matches, take the one with the direction match
    TRUE ~ 1 #if not just add a 1
        )
  ) %>% #now group by mdg_id and take the first row
  #now keep only the matches and then keep only the first obs within the matches
  filter(keep == 1) %>% 
  group_by(mdg_id) %>% 
  slice(1) %>% 
  dplyr::select(-north_south, -bike_dir) %>%  #drop before the join
  left_join(aim2_ex_strava_5halfmi, by = "edge_id") %>% 
  dplyr::select(-exclude, -major_or_res) %>% 
  left_join(gears_dave_bikes_wrangle_nogeo, by = "mdg_id") %>% 
  st_as_sf() 

nrow(buffer_e1_wgears)
table(gears_dave_bikes_wrangle$exposure_revised)
names(gears_dave_bikes_wrangle)
names(buffer_e1_wgears2)

#confirmed correct number of crashes. great. how to handle duplciate edge is?
buffer_e1_wgears_by_edge = buffer_e1_wgears %>% 
  group_by(edge_id) %>% 
  summarise(n_crashes=n()) %>% 
  ungroup() %>% 
  mutate(major_or_res=1)

table(aim2_ex_strava_5halfmi$major_or_res)

#----------------------3.2.E0----------------------##########
buffer_e0_wgears0 = buffer_e0 %>% 
  st_transform(4326) %>% 
  #With this code, the same crash is getting assigned to many IDs. I just need it to pick one.
  #As long as it's assigned to one of them, it doesn't really matter which.
  st_intersection(gears_e0) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(edge_id, mdg_id, north_south, bike_dir,) %>% 
  #match on east-west vs noth-south street
  mutate(
    direction_match = case_when(
      (bike_dir ==  "North" | bike_dir == "South") & north_south==1 ~ 1,
      (bike_dir == "East" | bike_dir == "West") & north_south==0 ~ 1,
      TRUE ~0
    )
  ) 

#Does everyone have a match?
e0_dir_match_list =  buffer_e0_wgears0  %>% 
  group_by(mdg_id, direction_match) %>% 
  summarise(n=n()) %>% 
  filter(direction_match==1) %>%  
  mutate(any_match= 1) %>% 
  dplyr::select(-n, -direction_match) %>% 
  ungroup()

#so this code, where possible picks an edge ID that matches the stated direction the bicycle was going
#there are a few where the match didn't work ... no big deal
buffer_e0_wgears = buffer_e0_wgears0 %>% 
  left_join(e0_dir_match_list, by = "mdg_id") %>% 
  group_by(mdg_id) %>% 
  mutate(keep = case_when(
    any_match == 1 ~ direction_match, #if it has any matches, take the one with the direction match
    TRUE ~ 1 #if not just add a 1
  )
  ) %>% #now group by mdg_id and take the first row
  #now keep only the matches and then keep only the first obs within the matches
  filter(keep == 1) %>% 
  group_by(mdg_id) %>% 
  slice(1) %>% 
  dplyr::select(-north_south, -bike_dir) %>%  #drop before the join
  left_join(aim2_ex_strava_5halfmi, by = "edge_id") %>% 
  dplyr::select(-exclude, -major_or_res) %>% 
  left_join(gears_dave_bikes_wrangle_nogeo, by = "mdg_id") %>% 
  st_as_sf() 

nrow(buffer_e0_wgears)
table(gears_dave_bikes_wrangle$exposure_revised)
names(buffer_e0_wgears2)

buffer_e0_wgears_by_edge = buffer_e0_wgears %>% 
  group_by(edge_id) %>% 
  summarise(n_crashes=n()) %>% 
  ungroup() %>% 
  mutate(major_or_res=0)



#--------3.2. stack those into your major edge-level dataset---------------------------##########
gears_by_edge = rbind(buffer_e0_wgears_by_edge, buffer_e1_wgears_by_edge) %>% st_as_sf() %>% mutate(edge_has_gears = 1)
gears_by_edge_nogeo = gears_by_edge %>% st_set_geometry(NULL)

class(gears_by_edge)
View(gears_by_edge)
table(gears_by_edge$major_or_res)
mapview(gears_by_edge, zcol = "major_or_res", color = c("blue", "red"), lwd=4) #excellent.
names(gears_by_edge)

edge_no_gears = aim2_ex_strava_5halfmi %>% 
  left_join(gears_by_edge_nogeo, by = "edge_id") %>% 
  filter(is.na(edge_has_gears)==TRUE) %>% 
  dplyr::select(edge_id, n_crashes, edge_has_gears) %>% 
  mutate(edge_has_gears=0) %>% 
  st_set_geometry(NULL)

edge_yes_gears = gears_by_edge_nogeo %>%
  dplyr::select(edge_id, n_crashes, edge_has_gears) 

nrow(edge_no_gears) + nrow(gears_by_edge)
nrow(aim2_ex_strava_5halfmi) #same. great.
edge_gears_combined_nogeo = edge_no_gears %>% bind_rows(edge_yes_gears)

#geometry lookup
aim2_ex_strava_5halfmi_geolookup = aim2_ex_strava_5halfmi %>% dplyr::select(edge_id, geometry)
edge_gears_combined = edge_gears_combined_nogeo %>% left_join(aim2_ex_strava_5halfmi_geolookup, by = "edge_id")


#-----Save stuff--------------##########
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 

save(aim2_ex_strava_5halfmi_geolookup, file = "aim2_ex_strava_5halfmi_geolookup.RData")
save(edge_gears_combined, file = "edge_gears_combined.RData")
save(edge_gears_combined_nogeo, file = "edge_gears_combined_nogeo.RData")


#-------------checks-------------------------------------------------------------------------#
options(viewer = NULL) #send viewer to the browser
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

filter(direction_match == 1)
arrange(desc(length_prop_in_bg)) %>% 
  slice(1) %>%  
  summarise(n=n())
View(e1_direction_match_list)  
    View()

View(buffer_e1_wgears)
#see what these look like: does it matter which one I pick? no. It doesn't. Just pick one.
#Ideally it would be the same
buffer_e1_wgears %>%
#  filter(mdg_id == 63) %>% 
  dplyr::select(edge_id, mdg_id) %>% 
  st_set_geometry(NULL) %>% 
  left_join(aim2_ex_strava_5halfmi, by = "edge_id") %>% 
  st_as_sf() %>% 
  mapview(zcol = "mdg_id")

nrow(buffer_e1_wgears) #got them all
View(buffer_e1_wgears)
mapview(buffer_e1_wgears)

#Must begin with the edge-level data, because that is the format in which you'd like the data. Then, you can group by edge and the number of times
#an edge appears in the data will be its number of cases for that exposure value.

View(buffer_e1_wgears)


#---------------------analyze gears data for IRR-------------------------##########

#exposure ratio in cases
e1_vs_e0 = gears_dave_bikes_wrangle %>%
  st_drop_geometry()%>%
  group_by(exposure_revised) %>%
  summarise(n=n())


crashes_e1 = e1_vs_e0 %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% pull()
crashes_e0 = e1_vs_e0 %>% filter(exposure_revised == "E0") %>% dplyr::select(n) %>% pull()
crashes_ratio = crashes_e1/crashes_e0 


#exclude tech?
table(gears_dave_bikes_wrangle$gtech_pd)
e1_vs_e0_noGT = gears_dave_bikes_wrangle %>%
  filter(is.na(gtech_pd)==TRUE) %>% 
  st_drop_geometry()%>%
  group_by(exposure_revised) %>%
  summarise(n=n())

93/24

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
#pt for point estimate suffix to differentiate from that used throughout the bootstrapping 5/22/20
bd_e1_pt= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==1) %>% dplyr::select(bd) %>% pull()
bd_e0_pt= aim2_ex_strava_5halfmi_summary %>% filter(major_or_res==0) %>% dplyr::select(bd) %>% pull()

bd_e_ratio_pt = bd_e1_pt/bd_e0_pt

e1_vs_e0_pt = gears_dave_bikes_wrangle %>% 
  st_drop_geometry()%>%
#  sample_frac(size=1, replace=TRUE) %>% 
  group_by(exposure_revised) %>%
  summarise(n=n())
crashes_e1_pt = e1_vs_e0_pt %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% pull()
crashes_e0_pt = e1_vs_e0_pt %>% filter(exposure_revised == "E0") %>% dplyr::select(n)%>% pull()

crashes_ratio_pt = crashes_e1_pt/crashes_e0_pt
estimated_irr_unadjusted_pt = crashes_ratio_pt/bd_e_ratio_pt

round(crashes_ratio_pt, 2)
round(bd_e_ratio_pt, 2)
round(estimated_irr_unadjusted_pt, 2)


#-------1. estimate variance of measure-at-risk sample-------------####
#-----to get variance in MAR, uncount it and then sample it with replacement---------------#
# bike_distance_km_tactcnt = tactcnt*length_mdg_km
names(aim2_ex_strava_5halfmi)
dim(aim2_ex_strava_5halfmi)


crash_bootstrap <- function(crash_s_id_val) {
  
  e1_vs_e0_s = gears_dave_bikes_wrangle %>% 
    st_drop_geometry()%>%
    sample_frac(size=1, replace=TRUE) %>% 
    group_by(exposure_revised) %>%
    summarise(n=n())
  
  crashes_e1_s = e1_vs_e0_s %>% filter(exposure_revised == "E1") %>% dplyr::select(n) %>% rename(crashes_e1_s=n)
  crashes_e0_s = e1_vs_e0_s %>% filter(exposure_revised == "E0") %>% dplyr::select(n)%>% rename(crashes_e0_s=n)
  crashes_ratio_s = crashes_e1_s/crashes_e0_s
  
  #I want all three measures in one row
  crashes_ratio_tibble =crashes_e1_s  %>% bind_cols(crashes_e0_s) %>%
    mutate(
        crashes_ratio_s = crashes_e1_s/crashes_e0_s, 
        crash_s_id = crash_s_id_val
           )

  #Following here, subtract the estimated point estimate overall from the bootsrapped estimate
  #https://stats.stackexchange.com/questions/355781/is-it-true-that-the-percentile-bootstrap-should-never-be-used
  #b for bootstrapped difference
  crashes_e1_b = (crashes_e1_s- crashes_e1_pt) %>% as_tibble() %>% rename(crashes_e1_b=crashes_e1_s)
  crashes_e0_b = (crashes_e0_s - crashes_e0_pt) %>% as_tibble() %>% rename(crashes_e0_b=crashes_e0_s)
  crashes_ratio_b = (crashes_ratio_s - crashes_ratio_pt) %>% as_tibble() %>% rename(crashes_ratio_b=crashes_e1_s)
  
  crashes_ratio_tib = crashes_ratio_tibble %>% 
    bind_cols(crashes_e1_b, crashes_e0_b, crashes_ratio_b)
  
  return(crashes_ratio_tib)
  
  
}
nreps = 10
nreps_list <-seq(1, nreps, by =1)
crash_bootstrap_df = nreps_list %>% map_dfr(crash_bootstrap)
View(crash_bootstrap_df)
ggplot(crash_bootstrap_df, aes(crashes_ratio_b)) + geom_histogram()
crashes_e1_b_ul = crashes_e1_pt + quantile(crash_bootstrap_df$crashes_e1_b, probs = c(0.975))
crashes_e1_b_ll = crashes_e1_pt + quantile(crash_bootstrap_df$crashes_e1_b, probs = c(0.025))

crashes_e0_b_ul = crashes_e0_pt + quantile(crash_bootstrap_df$crashes_e0_b, probs = c(0.975))
crashes_e0_b_ll = crashes_e0_pt + quantile(crash_bootstrap_df$crashes_e0_b, probs = c(0.025))

crashes_ratio_b_ul = crashes_ratio_pt + quantile(crash_bootstrap_df$crashes_ratio_b, probs = c(0.975))
crashes_ratio_b_ll = crashes_ratio_pt + quantile(crash_bootstrap_df$crashes_ratio_b, probs = c(0.025))

#-----multi-level sampling--------#########
setwd(file.path("C:", "Users", "mdg71", "Dropbox", "EMORY",
                "General research",  "Dissertation", "R_data", "analysis data")) 
load(file = "aim2_ex_strava_5halfmi.RData")
load(file = "crashes_ratio.RData")
load(file = "gears_dave_bikes_wrangle.RData")
library(tidyverse)
library(sf)
aim2_ex_strava_5halfmi_nogeo = aim2_ex_strava_5halfmi %>% 
  st_set_geometry(NULL)
gears_dave_bikes_wrangle_nogeo = gears_dave_bikes_wrangle %>% 
  st_set_geometry(NULL)

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


nested_bootstrap <-function(s_id_val,
                            crash_s_id_val) {
  
  crashes_s = gears_dave_bikes_wrangle_nogeo %>% 
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

  osm_name_sample_weight = aim2_ex_strava_5halfmi_nogeo %>% 
    filter(tactcnt > 0) %>% 
    group_by(OSM_NAME) %>% 
    summarise(
      bike_distance_km_tactcnt = sum(bike_distance_km_tactcnt, na.rm=TRUE), 
      length_mdg_km = sum(length_mdg_km, na.rm=TRUE),
      tactcnt = sum(tactcnt, na.rm=TRUE)) %>% 
    ungroup() %>% 
    #this is your weight
    #compromise: log your weights so they're not so extreme.
    #it will be less extreme b/c it's logged. should help balance bias and variance
    mutate(log_tactcnt_osm = log(tactcnt)) %>% 
    mutate( nest_id = row_number()) %>% 

    sample_frac(size=1, replace=TRUE)   %>% 
    mutate(nest_id_s = row_number())  
    
  osm_name_sample_weight_4join = osm_name_sample_weight %>%  
    dplyr::select(OSM_NAME, nest_id, nest_id_s, log_tactcnt_osm)
  
  n_distinct(osm_name_sample_weight_4join$OSM_NAME)/nrow(osm_name_sample_weight)
  #set a row number to be random and sample from it so that you're not grabbing
  #all the data in one go

  
  #I'm creating a new dataset rather than continuing with the piping
  #because I want to track which edges, too, and the above collapses over edges
  edges_in_nested_sample = aim2_ex_strava_5halfmi_nogeo %>% 
    left_join(osm_name_sample_weight_4join, by = c("OSM_NAME")) %>% 
    filter(nest_id_s > 0) %>%    #where the other one is not missing
    filter(tactcnt > 0) %>%
    mutate(edge_id_s = row_number()) %>%   #note it doesn't correspond. just a number for splitting and combining
    #just keep a few variables to keep the size down for speed
    dplyr::select(edge_id_s, edge_id, OSM_NAME, major_or_res, tactcnt, log_tactcnt_osm, nest_id, length_mdg_km) %>% 
    #shuffle these up for easier split-apply-combine
    #https://stackoverflow.com/questions/6422273/how-to-randomize-or-permute-a-dataframe-rowwise-and-columnwise
    sample_frac(1L) %>% 
    mutate(edge_id_s_shuffled = row_number()) %>% 
    uncount(tactcnt,  .remove=FALSE) %>% 
    
    #now sample those observations with replacement
    group_by(OSM_NAME) %>% 
    #I didn't weight above for computational efficiency (originally I thought to)
    #so weighting here instead...
    sample_frac(size=1, replace=TRUE, weight = tactcnt)  %>%  
    group_by(major_or_res) %>% 
    summarise(bd = sum(length_mdg_km)) %>% 
  #update 5/18/20, I want to keep this in long form. easier to summarize below.
    #my hunch is the lack of weighting will make the 95% CIs downwardly biased but we shall see
    
  ungroup() %>% 
    rename(value=bd) %>% 
    mutate(
      e_or_ratio = case_when(
        major_or_res == 1 ~ "E1",
        major_or_res == 0 ~ "E0"
      ),
      measure = "bd"
    )

  bd_e1_s =   edges_in_nested_sample %>% 
    filter(e_or_ratio ==  "E1") %>% dplyr::select(value) %>% pull()
  bd_e0_s =   edges_in_nested_sample %>% 
    filter(e_or_ratio == "E0") %>% dplyr::select(value) %>% pull()
  
  irr_s_val = ((crashes_e1_s/crashes_e0_s)/(bd_e1_s /bd_e0_s )) %>% as_tibble() %>% 
    mutate( measure = "irr",   
            e_or_ratio = "ratio")

  table = (bd_e1_s/bd_e0_s) %>% as_tibble() %>% 
    mutate(e_or_ratio =  "ratio",
           measure = "bd") %>% 
    bind_rows(edges_in_nested_sample) %>% 
    bind_rows(irr_s_val) %>% 
    dplyr::select(-major_or_res) %>% 
    bind_rows(crashes_tibble) %>% #add the crashes data above
    mutate( 
      s_id=s_id_val,
      crash_s_id =crash_s_id_val
      )

  return(table)

}

options(scipen = 999)
View(table)

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

