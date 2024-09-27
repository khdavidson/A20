
# misc code to help with DNA subsampling


library(tidyverse)
library(ggridges)
library(leaflet)


# ============================ LOAD DATA ============================

# PURSE SEINE DATA -------------------------
form1 <- read.csv(file="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/PUrse Seining/form-1__rst-seining-survey_MASTER_2024.csv") %>%
  mutate(X2_Date = lubridate::dmy(X2_Date)) %>% 
  rename(ec5_form1_uuid = ec5_uuid)
form2 <- read.csv(file="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/PUrse Seining/form-2__fishing-sets_MASTER_2024.csv") %>%
  rename(ec5_form2_uuid = ec5_uuid,
         ec5_form1_uuid = ec5_parent_uuid)
form3 <- read.csv(file="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/PUrse Seining/form-3__catch-sampling_MASTER_2024.csv") %>%
  rename(ec5_form3_uuid = ec5_uuid,
         ec5_form2_uuid = ec5_parent_uuid) %>%
  mutate(date = lubridate::ymd(stringr::str_sub(created_at, 1,10)))

# BEACH SEINE DATA -------------------------
bs <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan Juveniles/Data management/2024 Misc Data/SanJuan_BeachSeine_Data_2024/SanJuan_BeachSeine_Data_2024.xlsx",
                         sheet="2024 Beach Seine Data") %>% 
  mutate(date = lubridate::ymd(`Date (YY-MM-DD)`)) %>% 
  left_join(.,
            readxl::read_excel(path=here::here("data", "juvenile", "estuary", "Export_Fish_Sample_Sites.xlsx"), sheet="Export_Fish_Sample_Sites") %>% 
              mutate(as.data.frame(oce::utm2lonlat(Easting, Northing, zone=10, hemisphere = "N", km=F)[1])) %>%
              mutate(as.data.frame(oce::utm2lonlat(Easting, Northing, zone=10, hemisphere = "N", km=F)[2])) %>% 
              mutate(Name = str_to_upper(Name)) %>%
              rename(`Site Name` = Name) 
  ) %>% 
  mutate(arm = case_when(grepl("San Juan", WaterCours, ignore.case=T) ~ "San Juan",
                         grepl("(Gordon)|(Brown's)|(N Arm)", WaterCours, ignore.case=T) ~ "Gordon",
                         TRUE ~ "FLAG"))


#########################################################################################################################################################

# Joins to export ------
tt12 <- left_join(form2,
                  form1 %>%
                    select(ec5_form1_uuid, X2_Date, X5_Survey_Location_ri),
                  by="ec5_form1_uuid") %>%
  select(ec5_form1_uuid, ec5_form2_uuid, X2_Date, X5_Survey_Location_ri, UTM_Northing_99_Coordinates, UTM_Easting_99_Coordinates)

tt123 <- left_join(form3,
                   tt12,
                   by="ec5_form2_uuid")


tt123 <- sf::st_as_sf(tt123,
             coords = c("UTM_Easting_99_Coordinates", "UTM_Northing_99_Coordinates"),
             remove=F,
             crs = "+proj=utm +zone=10 +north +datum=WGS84") %>%
  sf::st_transform(., crs="+proj=longlat +datum=WGS84") %>% 
  mutate(lat = sf::st_coordinates(.)[,2],
         long = sf::st_coordinates(.)[,1])




write.csv(file=paste0(here::here("outputs"), "/R_OUT - 2024 purse seine joined data (forms1+2+3).csv"), x=tt123, row.names=F)

prs.dna <- read.csv(file=here::here("outputs", "R_OUT - 2024 purse seine joined data (forms1+2+3) - DNA submission version.csv"))

#########################################################################################################################################################

#                                                                purse seine 

# Density plot: length ~ date + clip status 
pdf(file = here::here("outputs", "figures", "2024 Purse Seine chinook length ~ date + clip status (density).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_density_ridges(data=form3 %>% filter(grepl("Chinook", title, ignore.case=T) | X242_Species=="Chinook"), 
                      aes(x=X249_Fork_Length_mm, y=as.character(date), group=interaction(as.character(date), X248_Adipose_Clip_Sta), 
                          fill=ordered(X248_Adipose_Clip_Sta, c("Clipped", "Not clipped")), colour=ordered(X248_Adipose_Clip_Sta, c("Clipped", "Not clipped"))), 
                      alpha=0.4, scale=1) +
  labs(x="Fork length (mm)", y="", fill="Ad clip?", colour="Ad clip?") +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values=c("purple", "orange", "gray80")) +
  scale_colour_manual(values=c("purple", "orange", "gray80")) +
  scale_x_continuous(breaks=seq(0,250,by=10)) +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        legend.title = element_text(face="bold"))

dev.off()


# Scatter plot: length ~ date + clip status
pdf(file = here::here("outputs", "figures", "2024 Purse Seine chinook length ~ date + clip status (scatter).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_point(data=form3 %>% filter(grepl("Chinook", title, ignore.case=T) | X242_Species=="Chinook"), 
             aes(x=date, y=X249_Fork_Length_mm, 
                 fill=ordered(X248_Adipose_Clip_Sta, c("Clipped", "Not clipped")), colour=ordered(X248_Adipose_Clip_Sta, c("Clipped", "Not clipped")),
                 size=X250_Height_mm), 
             alpha=0.5,  shape=21, stroke=1.5) +
  labs(x="Fork length (mm)", y="", fill="Ad clip?", colour="Ad clip?") +
  scale_y_continuous(breaks=seq(0,250,by=10)) +
  scale_fill_manual(values=c("purple", "orange", "gray80")) +
  scale_colour_manual(values=c("purple", "orange", "gray80")) +
  scale_size_continuous(breaks=seq(5,250,by=25)) +
  scale_x_date(date_labels = "%b %d", date_breaks="5 day") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_text(face="bold"))

dev.off()




ggplot()+
  geom_point(data=form3 %>% filter(grepl("Chinook", title, ignore.case=T) | X242_Species=="Chinook"), 
             aes(x=X249_Fork_Length_mm, y=X250_Height_mm, fill=X248_Adipose_Clip_Sta, colour=X248_Adipose_Clip_Sta),
             size=3, shape=21, alpha=0.7) +
  theme_bw()



# MAP
leaflet() %>% 
  # ---- 1. Structure BASEMAP: 
  #addProviderTiles(providers$Stamen.TerrainBackground) %>%
  #addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  #addProviderTiles(providers$OpenStreetMap.HOT) %>%
  #addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  setView(lng=-124.445266, lat= 48.556015, zoom=13) %>%
  hideGroup(c('Place names')) %>%
  addCircleMarkers(data=prs.dna %>% 
                     filter(X242_Species=="Chinook", X266_Is_this_a_lethal=="No", X248_Adipose_Clip_Sta=="Not clipped", DNA.submission.status=="") %>%
                     group_by(X5_Survey_Location_ri, lat, long) %>%
                     summarize(n_samples = n()),
                   lat= ~lat,
                   lng = ~long,
                   radius= ~ifelse(n_samples%in%c(1:5), 3,
                                   ifelse(n_samples%in%c(6:12), 5, 10))#,
                   #color=~ifelse(X248_Adipose_Clip_Sta=="Clipped", "red",
                    #             ifelse(X248_Adipose_Clip_Sta=="Not clipped", "blue", "gray70")), stroke=F, fillOpacity=0.7
                  )




#########################################################################################################################################################

#                                                                           beach seine 

# Density plot: length ~ date + clip status 
pdf(file = here::here("outputs", "figures", "2024 Beach Seine chinook length ~ date + clip status (density).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_density_ridges(data=bs, 
                      aes(x=`Fork Length (mm)`, y=as.character(date), group=interaction(`AD Clipped (Y/N)`, as.character(date)), 
                          fill=ordered(`AD Clipped (Y/N)`, c("Y", "N", "NA")), colour=ordered(`AD Clipped (Y/N)`, c("Y", "N", "NA"))), 
                       alpha=0.3, scale=1) +
  labs(x="Fork length (mm)", y="", fill="Ad clip?", colour="Ad clip?") +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values=c("purple", "orange", "gray80")) +
  scale_colour_manual(values=c("purple", "orange", "gray80")) +
  scale_x_continuous(breaks=seq(0,200,by=10)) +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        legend.title = element_text(face="bold"))

dev.off()



# Scatter plot: length ~ date + clip status
pdf(file = here::here("outputs", "figures", "2024 Beach Seine chinook length ~ date + clip status (scatter).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_point(data=bs, 
              aes(x=date, y=`Fork Length (mm)`, 
                  fill=ordered(`AD Clipped (Y/N)`, c("Y", "N", "NA")), colour=ordered(`AD Clipped (Y/N)`, c("Y", "N", "NA"))), 
              alpha=0.5, size=5, shape=21, stroke=1.5) +
  labs(y="Fork length (mm)", x="", fill="Ad clip?", colour="Ad clip?") +
  scale_y_continuous(breaks=seq(0,200,by=10)) +
  scale_fill_manual(values=c("purple", "orange", "gray80")) +
  scale_colour_manual(values=c("purple", "orange", "gray80")) +
  scale_x_date(date_labels = "%b %d", date_breaks="5 day") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.title = element_text(face="bold")) +
  facet_wrap(~`Site Name`)

dev.off()



# SAMPLING MAP 

leaflet() %>% 
  # ---- 1. Structure BASEMAP: 
  #addProviderTiles(providers$Stamen.TerrainBackground) %>%
  #addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  #addProviderTiles(providers$OpenStreetMap.HOT) %>%
  #addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  setView(lng=-124.396857, lat=48.573119, zoom=16) %>%
  hideGroup(c('Place names')) %>%
  addCircleMarkers(data=bs %>% 
                     filter(`Lethal Sample (Y/N)`=="Y", is.na(`DNA submission status`)) %>%
                     group_by(`Site Name`, latitude, longitude, `AD Clipped (Y/N)`) %>%
                     summarize(n_samples = n()),
                   lat= ~latitude,
                   lng = ~longitude,
                   radius= ~ifelse(n_samples%in%c(1:5), 3,
                                   ifelse(n_samples%in%c(6:12), 5, 10)),
                   color=~ifelse(`AD Clipped (Y/N)`=="Y", "red",
                                 ifelse(`AD Clipped (Y/N)`=="N", "blue", "gray70")), stroke=F, fillOpacity=0.7)



leaflet() %>% 
  # ---- 1. Structure BASEMAP: 
  #addProviderTiles(providers$Stamen.TerrainBackground) %>%
  #addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  #addProviderTiles(providers$OpenStreetMap.HOT) %>%
  #addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  setView(lng=-124.396857, lat=48.573119, zoom=16) %>%
  hideGroup(c('Place names')) %>%
  addCircleMarkers(data=bs %>% 
                     filter(`Lethal Sample (Y/N)`=="N", is.na(`DNA submission status`)) %>%
                     group_by(`Site Name`, latitude, longitude, `AD Clipped (Y/N)`) %>%
                     summarize(n_samples = n()),
                   lat= ~latitude,
                   lng = ~longitude,
                   radius= ~ifelse(n_samples%in%c(1:5), 3,
                                   ifelse(n_samples%in%c(6:12), 5, 10)),
                   color=~ifelse(`AD Clipped (Y/N)`=="Y", "red",
                                 ifelse(`AD Clipped (Y/N)`=="N", "blue", "gray70")), stroke=F, fillOpacity=0.7)


#########################################################################################################################################################


# Random selection process 


# ======================= BEACH SEINE =======================
# Dates/situations chosen for sub-sampling are: 
# NOT CLIPPED
# NOT LETHAL 
# June 17 BS09 --> n=3
# July 17 BS21 --> n=3
# July 2 BS09 --> n=3
# July 2 BS13 --> n=3

set.seed(1)

bs.subsamp <- bs %>% 
  filter((`Date (YY-MM-DD)`=="24-06-17" & `Site Name`%in%c("BS09", "BS21")) |  
         (`Date (YY-MM-DD)`=="24-07-02" & `Site Name` %in% c("BS09", "BS13"))) %>% 
  filter(`AD Clipped (Y/N)`=="N", `Lethal Sample (Y/N)`=="N") %>%
  mutate(subsamp_group = case_when(`Date (YY-MM-DD)`=="24-06-17" & `Site Name`=="BS09" ~ "group1",
                                   `Date (YY-MM-DD)`=="24-06-17" & `Site Name`=="BS21" ~ "group2",
                                   `Date (YY-MM-DD)`=="24-07-02" & `Site Name`=="BS09" ~ "group3",
                                   `Date (YY-MM-DD)`=="24-07-02" & `Site Name`=="BS13" ~ "group4")) %>%
  group_by(subsamp_group) %>%
  slice_sample(n = 3) %>%
  print()


# ======================= PURSE SEINE =======================
# Dates/situations chosen for sub-sampling are: 
# NOT CLIPPED
# NOT LETHAL 
# July 15 PRCD --> n=1
# Sept 3 Offshore B --> n=3

set.seed(2)

prs.subsamp <- full_join(
  prs.dna %>% 
    filter(X242_Species=="Chinook", X248_Adipose_Clip_Sta=="Not clipped",  X266_Is_this_a_lethal=="No") %>%
    filter(date=="7/15/2024" & grepl("PRCD", X5_Survey_Location_ri, ignore.case=T)) %>% 
    slice_sample(n = 1),
  prs.dna %>% 
    filter(X242_Species=="Chinook", X248_Adipose_Clip_Sta=="Not clipped",  X266_Is_this_a_lethal=="No") %>%
    filter(date=="9/3/2024" & grepl("Offshore B", X5_Survey_Location_ri, ignore.case=T)) %>% 
    slice_sample(n = 3)
) %>% 
  print()
    
    




