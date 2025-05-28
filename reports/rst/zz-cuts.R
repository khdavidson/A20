
# CUTS


# REMOVED CPUE CALCULATIONS FOR NOW AS DON'T LIKE IT. 
# Calculate hours fished per day, and calculate CPUE (catch per hour) and expand across full 24-hr period. (see reservations below)
# AND for the overnight window
#mutate(hrs_fished = as.numeric(datetime_stop-datetime_start),
#       CPUE_per24 = (set_total/hrs_fished)*24,
#       earliest_start = case_when(grepl("Night", usid, ignore.case=T) ~ format(min(as.POSIXct(format(datetime_start, 
#                                                                                                     format = "%H:%M:%S"), 
#                                                                                              format = "%H:%M:%S", tz = "UTC")), 
#                                                                               "%H:%M:%S"),
#                                  TRUE ~ NA))
#arrange(datetime_start) %>%
#mutate(datetime_start_CLEAN = round_date(datetime_start, "30 minute"),
#        datetime_stop_CLEAN = round_date(datetime_stop, "30 minute")) %>% 




# =============== !STOP POINT! Manual intervention required =============== 
#  Could not figure out how to expand unfished times while leaving fished periods so did it manually

# ---- Original export:
# writexl::write_xlsx(eventMeta_totals %>% 
#                       select(year, usid, datetime_start_CLEAN, datetime_stop_CLEAN) %>%
#                       arrange(datetime_start_CLEAN) %>%
#                       group_by(year, usid) %>%
#                       pivot_longer(cols=c(datetime_start_CLEAN, datetime_stop_CLEAN), names_to = "interval", values_to = "datetime") %>%
#                       mutate(interval = case_when(interval=="datetime_stop_CLEAN" ~ "stop",
#                                                   TRUE ~ "start")) %>%
#                       ungroup() %>%
#                       complete(datetime = seq.POSIXt(min(datetime), max(datetime), by="30 min")) , 
#                     "C:/Users/DAVIDSONKA/Desktop/datetime series test.xlsx")





# ---- Read in new expanded fishing period time series after manual assignment of fishing periods: 
fishing_periods <- 
  readxl::read_excel(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/RST 2023-2024 fishing periods expanded.xlsx",
                     sheet="2023-2024 fishing periods") %>% 
  # This part just consolidates/reformats the datetime_start and _stop across multiple rows and then removes the leftover duplicate row
  mutate(datetime_notfished_CLEAN = case_when(`trap status`=="not fished" ~ datetime, 
                                              TRUE ~ NA),
         datetime_start_CLEAN = case_when(interval=="start" ~ datetime,
                                          TRUE ~ NA),
         datetime_stop_CLEAN = case_when(interval=="stop" ~ datetime,
                                         TRUE~ NA)) %>%
  select(-c(datetime, interval)) %>%
  group_by(year, usid) %>%
  mutate(datetime_start_CLEAN = if (all(`trap status` == "fished")) coalesce(datetime_start_CLEAN[1], datetime_start_CLEAN[2]) else datetime_start_CLEAN,
         datetime_stop_CLEAN  = if (all(`trap status` == "fished")) coalesce(datetime_stop_CLEAN[1], datetime_stop_CLEAN[2]) else datetime_stop_CLEAN) %>%
  ungroup()  %>%
  group_by(usid, `trap status`) %>%
  filter(!(all(`trap status` == "fished") & duplicated(usid))) %>%
  ungroup()



# =============== JOIN METATOTALS + FISHING PERIOD EXPANSION =============== 

intersect(colnames(fishing_periods), colnames(eventMeta_totals))

eventMeta_totals_expanded = full_join(fishing_periods,
                                      eventMeta_totals) %>%
  mutate(set_type = case_when(format(datetime_notfished_CLEAN, "%H:%M:%S") >= "06:00:00" & 
                                format(datetime_notfished_CLEAN, "%H:%M:%S") < "16:00:00" ~ "Day",
                              
                              TRUE ~ "Night"),
         date_notfished = case_when(!is.na(datetime_notfished_CLEAN) ~ as.Date(datetime_notfished_CLEAN),
                                    TRUE ~ NA),
         DOY = case_when(is.na(DOY) ~ lubridate::yday(date_notfished),
                         TRUE ~ DOY)) %>%
  relocate(date_notfished, .after=datetime_notfished_CLEAN) %>%
  relocate(set_type, .after=usid) %>%
  relocate(gear, .before=usid) %>%
  relocate(c(date_start, datetime_start, date_stop, datetime_stop, DOY), .after = last_col()) %>%
  rowwise() %>%
  mutate(total_catch = rowSums(across(c(`chinook fry`:`chinook smolt (hatchery)`)), na.rm=T)) 








ggplot() +
  geom_bar(data=eventMeta_totals_expanded%>%
             filter(year==2024),
           aes(x=datetime_stop_CLEAN, y=total_catch),   colour="gray70", fill="gray70", stat="identity") +
  geom_point(data=eventMeta_totals_expanded %>%
               filter(year==2024),
             aes(x=datetime_notfished_CLEAN, y=-20, fill=set_type, colour=set_type),  alpha=0.3, size=2, shape=22)




# PLOT: CPUE expanded for the full 24hr period ------------ 
# This assumes missed daytime catch is the same "intensity" as night time, which I don't think is necessarily true based on juvenile biology

# ggplot()+
#   geom_bar(data=eventMeta_totals %>% filter(!is.na(species_stage_simple)), 
#            aes(x=DOY, y=CPUE_per24, group=DOY), 
#            stat="identity", position="dodge", na.rm=T, size=1, alpha=0.5, fill="gray80") +
#   geom_bar(data=setTotals %>% filter(species %in% c("chinook", "chum", "coho"), !is.na(species)), 
#            aes(x=DOY, y=total_caught_excl_recaps, fill=species_stage_simple, group=DOY), 
#            stat="identity", position="dodge", na.rm = T, size=1, alpha=0.5) +
#   theme_bw() +
#   facet_wrap(~year+species_stage_simple, scales="free_y")