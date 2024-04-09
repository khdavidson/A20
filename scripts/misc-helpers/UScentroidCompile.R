# Spatial data join source code for CWT maps
# Apr 2024


library(rgdal)                  # for mapping readOGR()
library(readxl)




# ============= LOAD US spatial =============

# 1 Load available US District spatial files and extract centroids of Districts -----------------------
# Cook Inlet
ADFGcook <- rgdal::readOGR(dsn=here("data", "fisheries", "spatial"), 
                           layer="Commercial_Fisheries_Upper_Cook_Inlet_Salmon_Districts", verbose=F)
ADFGcook_cent <- as.data.frame(cbind(st_point_on_surface(st_as_sf(ADFGcook)), 
                                     do.call(rbind, st_point_on_surface(st_as_sf(ADFGcook))$geometry)))
# Prince William Sound
ADFGpws <- rgdal::readOGR(dsn=here("data", "fisheries", "spatial"), 
                          layer="PVB_PWS_Districts_2018_Present_GCS_WGS1984", verbose=F)
ADFGpws_cent <- as.data.frame(cbind(st_point_on_surface(st_as_sf(ADFGpws)), 
                                    do.call(rbind, st_point_on_surface(st_as_sf(ADFGpws))$geometry)) )
# SEAK
ADFGseak <- rgdal::readOGR(dsn=here("data", "fisheries", "spatial"), 
                           layer="Southeast_Salmon_Districts_GCS_WGS1984", verbose=F)
ADFGseak_cent <- as.data.frame(cbind(st_point_on_surface(st_as_sf(ADFGseak)), 
                                     do.call(rbind, st_point_on_surface(st_as_sf(ADFGseak))$geometry)) )


# 2 Create a manual aux centroid file for unavailable US spatial data -----------------------
UScentroidAux <- readxl::read_excel(here("data", "fisheries", "spatial", "Centroid auxiliary file US.xlsx"), sheet=1)


# 3 Join US centroids, join to aux centroid file -----------------------  
UScentroids <- full_join(ADFGcook_cent %>% 
                           mutate(across(everything(), as.character)),
                         ADFGpws_cent %>%
                           mutate(across(everything(), as.character))) %>%
  full_join(., 
            ADFGseak_cent %>%
              mutate(across(everything(), as.character))) %>% 
  mutate(DISTRICT_C = case_when(DISTRICT_N == "District 15" & X1 == -135.292221133256 ~ "115",
                                TRUE ~ DISTRICT_C),
         `R (RC) Recovery PSC Location-L4` = DISTRICT_C) %>% 
  full_join(., 
            UScentroidAux %>% 
              mutate(across(everything(), as.character)))


# Remove unneccessary temp files 
rm(list = c('ADFGcook','ADFGcook_cent', "ADFGpws", "ADFGpws_cent", "ADFGseak", "ADFGseak_cent", "UScentroidAux"))
