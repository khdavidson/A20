# Water data compile
# Mar 2025




# ============================= WATER DATA ============================

SJ.water <- full_join(
  read.csv(file = here::here("data", "enviro", "SANJUAN_historical_daily.csv"), skip=1) %>%
    mutate(PARAM = case_when(PARAM==1 ~ "Discharge (cms)",
                             PARAM==2 ~ "Level (m)")) %>% 
    mutate(across(everything(), as.character)),
  
  do.call("rbind", lapply(list.files(here::here("data", "enviro"), 
                                     pattern="SANJUAN_REALTIME", full.names=T), 
                          function(x) {
                            df <- read.csv(x, skip=9)  
                            names(df) <- ifelse(grepl("Value", names(df)), "Value", ifelse(grepl("Date", names(df)), "Date", names(df)))  # Rename columns
                            return(df)
                          })) %>%
    rename(PARAM=Parameter) %>%
    mutate(PARAM = case_when(PARAM==5 ~ "Temperature (C)",
                             PARAM==6 ~ "Discharge (cms)",
                             PARAM==3 ~ "Level (m)")) %>%
    mutate(across(everything(), as.character))
) %>%
  mutate(Date2 = lubridate::ymd(Date),
         year = lubridate::year(Date),
         month = lubridate::month(Date, label=T, abbr=T),
         DOY = lubridate::yday(Date))
  

    # ***** here next day
         
         
         
         
          %>%
    rename(Value = Value..m..s.,
           Date = Date..PST.,
           PARAM = Parameter) #,
  
  # read.csv(file=list.files(path = here::here("data", "enviro"),
  #                          pattern = "SANJUAN_REALTIME_08HA010_HG*",     # water level realtime
  #                          full.names = TRUE),
  #          skip=9) %>%
  #   rename(Value = Value..m.,
  #          Date = Date..PST.,
  #          PARAM = Parameter)
)
%>%
  mutate(PARAM = case_when(PARAM==46 ~ "Level (m)",
                           PARAM=="47" ~ "Discharge (cms)"),
         Date = lubridate::ymd(stringr::str_sub(string=Date, start=1, end=10)),
         time = stringr::str_sub(string=Date, start=12, end=19),
         year = lubridate::year(Date),
         month = lubridate::month(Date, label=T, abbr=T),
         DOY = lubridate::yday(Date)) 
)