# GSI compile 
# Feb 2024


# Load libraries ------------------------------------
library(tidyverse)
#library(here)
#library(readxl)
library(writexl)




# Load and join MGL files ------------------------------------
gsi.IDs <- full_join(
  # GSI part 1: Read in extraction sheet which has Vial and individual ID and catch metadata
  readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15_new-format.xlsx"), 
                     sheet="extraction_sheet") %>% 
    select(indiv, Vial, CatchJulDate, CatchYear),
  # GSI part 1: Read in GSI table results
  readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15_new-format.xlsx"), 
                     sheet="repunits_table_ids") %>%
    select(indiv, ID_Source:PBT_brood_group, repunit.1:associated_collection_prob)) %>% 

  full_join(.,
            full_join(
              # GSI part 2: read in extraction sheet which has Vial and individual ID and catch metadata 
              readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20240039(2)_WCVI_FTF(22-23)_sc578_2024-07-11_NF_JB -- 2023 SJ purse seine part 2.xlsx"),
                                 sheet = "extraction_sheet") %>% 
                select(indiv, CatchYear, CatchJulDate, Vial),
              # GSI part 2: read in GSI table results
              readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20240039(2)_WCVI_FTF(22-23)_sc578_2024-07-11_NF_JB -- 2023 SJ purse seine part 2.xlsx"),
                                 sheet="repunits_table_ids") %>%
                select(indiv, `Vial Number`:associated_collection_prob) %>%
                rename(Vial=`Vial Number`))
  ) %>%
  left_join(.,
            readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15_new-format.xlsx"), 
                               sheet="species_ID")) %>%
  left_join(., 
            readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15.xlsx"), 
                               sheet="sex_ID") %>%
              select(indiv, sex_ID, notes)) %>% 

  left_join(.,
            readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20240039(2)_WCVI_FTF(22-23)_sc578_2024-07-11_NF_JB -- 2023 SJ purse seine part 2.xlsx"),
                               sheet = "species_ID")) %>%   
  left_join(.,
            readxl::read_excel(here::here("data", "juvenile", "GSI", "PID20240039(2)_WCVI_FTF(22-23)_sc578_2024-07-11_NF_JB -- 2023 SJ purse seine part 2.xlsx"),
                               sheet = "sex_ID") %>%
              select(indiv, sex_ID, notes)) %>%  
  setNames(paste0('MGL_', names(.))) %>%
  rename(DNA_vial = MGL_Vial) %>% 
  print()
            



# Link juvi biodata + metadata + MGL IDs ------------------------------------
juvi.bioMeta.GSI <- left_join(readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"), sheet="biosamples", trim_ws=T),
                          readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"), sheet="sample_event_metadata", trim_ws=T),
                          na_matches="never",
                          by="usid") %>% 
  left_join(.,
            gsi.IDs,
            by="DNA_vial", na_matches="never") %>% 
  # mutate(`(R) ORIGIN` = case_when(ID_Source=="PBT" ~ "Hatchery",
  #                                 ad_clip=="Y" ~ "Hatchery",
  #                                 prob.1 >= 75 & grepl("HATCHERY", collection.1) ~ "Hatchery",
  #                                 grepl("HAT", usid) ~ "Hatchery",
  #                                 grepl("RST", gear) ~ "Natural",
  #                                 ID_Source=="GSI" & ad_clip=="N" & !grepl("RST", gear) ~ "Natural (assumed)",
  #                                 ID_Source=="Failed to amplify" ~ "Unknown",
  #                                 
  #                                 is.na(DNA_vial) | is.na(indiv) ~ NA,
  #                                 
  #                                 TRUE ~ "FLAG"),
  #        # `(R) STOCK ID` = case_when(prob.1 >= 75 ~ str_to_title(gsub(collection.1, pattern="_", replacement=" ")),
  #        #                            grepl("RST", gear) & is.na(collection.1) ~ "San Juan River",
  #        #                            grepl("HAT", usid) ~ "San Juan River",
  #        #                            ID_Source=="Failed to amplify" ~ "Unknown",
  #        #                            prob.1 < 75 & (repunit.1%in%c("SWVI", "NWVI") | repunit.1%in%c("SWVI", "NWVI") | repunit.1%in%c("SWVI", "NWVI")) ~ 
  #        #                              "Uncertain WCVI origin",
  #        #                            is.na(DNA_vial) | is.na(indiv) ~ NA,
  #        #                            TRUE ~ "FLAG")) %>%
  # unite(col=`(R) STOCK-ORIGIN`, c(`(R) ORIGIN`, `(R) STOCK ID`), sep=" ", remove=F, na.rm=T) %>% 
  #mutate(yday = lubridate::yday(date_end)) %>%
  print()




# ================= EXPORT ================= 
# To github ------------------------------------
writexl::write_xlsx(juvi.bioMeta.GSI, 
                    path = here::here("outputs", 
                                      paste0("R_OUT - PFN_DFO_FTFjuvi_2023_verified_with-GSI-Results_",
                                             Sys.Date(),
                                             ".xlsx")))


