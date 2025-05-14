# San Juan mark history compilation - all mark types 
# Shifted out of rmarkdown script May 2025


# Load packages ------------------------
library(tidyverse)

# Helpers ------------------------
"%notin%" <- Negate("%in%")
options(scipen = 999999)
analysis_year <- 2023



# ============================== LOAD MARKS ==============================

# PBT ------------------------

SJPBT <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/R_OUT - PBT_Tag_Rates_WCVIStocks-All BYs.xlsx",
                            sheet="Sheet1") %>% 
  filter(grepl("san juan", Brood, ignore.case=T))



# Load otolith reference specimens output ------------------------
# Note, if new ref specimens have been added, have to run OtoCompile.R in run reconstruction repo: https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/OtoCompile.R

SJotoRef <- full_join(
  SJomRef <-
    lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/2-Export-from-R/", 
                      pattern="^R_OUT - OtoManager_CN_REFERENCEspecimens.*\\.xlsx$", 
                      full.names=T), 
           function(x) {
             readxl::read_excel(path=x, sheet="Sheet1", guess_max=20000)
           })  %>% 
    do.call("cbind",.) %>%
    filter(grepl("SAN JUAN", FACILITY)) %>% 
    mutate(data_source = "reference specimen (Oto Manager)"),
  
  
  SJnpafcRef <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                                   pattern = "^All CN Marks",   
                                                   full.names = TRUE), 
                                   sheet=1) %>% 
    filter(grepl("SAN JUAN", STOCK), BROOD_YEAR %notin% unique(SJomRef$`BROOD YEAR`)) %>% 
    mutate(data_source = "NPAFC mark records") %>% 
    rename(`BROOD YEAR` = BROOD_YEAR,
           `RELEASE YEAR` = RELEASE_YEAR,
           `INTEND HATCH CODE` = HATCH_CODE,
           `USER COMMENT` = MARK_COMMENT) %>% 
    group_by(`BROOD YEAR`, FACILITY, SPECIES, `RELEASE YEAR`, `INTEND HATCH CODE`, data_source, STAGE) %>% 
    summarize(`USER COMMENT` = paste(`USER COMMENT`, collapse=" / ")) %>% 
    mutate(`RF READ STATUS` = case_when(grepl("not thermally marked", `USER COMMENT`, ignore.case=T) ~ "Not Marked",
                                        TRUE ~ "Marked"),
           QUALITY = case_when(`RF READ STATUS`=="Not Marked" ~ NA,
                               TRUE ~ "Unknown"))
) %>% 
  arrange(`BROOD YEAR`) %>%
  mutate(TM_application_flag = case_when(`INTEND HATCH CODE`!=`ACT ASSIGN HATCH CODE` ~ "mark application issue: actual mark applied was not the intended mark applied",
                                         is.na(`INTEND HATCH CODE`) | is.na(`ACT ASSIGN HATCH CODE`) ~ NA,
                                         TRUE ~ "ok")) %>%
  print()
remove(SJomRef)
remove(SJnpafcRef)


# CWT/AD ------------------------
source(here::here("scripts", "functions", "pullA20ChinookReleases.R"))








# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# MARK HISTORY/QUALITY ~ BROOD YEAR ----------------------
SJ_mark_history_BYrollup <- full_join(
  full_join(
    # --- Otolith release # from NPAFC file
    readxl::read_excel(here("data", "biosamples", "All CN Marks from NPAFC Otolith Database to December 6, 2023.xlsx"),
                       sheet="AC087805 (1)") %>% 
      filter(grepl("SAN JUAN", STOCK)) %>% 
      group_by(BROOD_YEAR) %>% 
      summarize(TM_release = sum(NUMBER_RELEASED)) %>%
      rename(`BROOD YEAR`=BROOD_YEAR),
    # --- Otolith mark quality from OtoCompile.R
    SJotoRef %>% 
      filter(`RF READ STATUS`%notin%c("Destroyed")) %>%
      group_by(`BROOD YEAR`, `RF READ STATUS`, QUALITY) %>% 
      summarize(TM_application_flag = unique(TM_application_flag), 
                n_submitted_refSpec_by_quality = n(),
                comments = case_when(`BROOD YEAR` %notin% SJomRef$`BROOD YEAR` ~ paste(`USER COMMENT`, collapse = ", "))) %>%
      group_by(`BROOD YEAR`, `RF READ STATUS`, QUALITY) %>% 
      summarize(TM_application_flag = unique(TM_application_flag), 
                n_submitted_refSpec_by_quality = unique(n_submitted_refSpec_by_quality),
                comments = unique(comments)) %>%
      mutate(n_submitted_refSpec_by_quality = case_when(`BROOD YEAR`==2001 ~ 16,
                                                        `BROOD YEAR` %notin% SJomRef$`BROOD YEAR` ~ 0,
                                                        TRUE ~ n_submitted_refSpec_by_quality))  %>% 
      
      group_by(`BROOD YEAR`) %>% 
      mutate(n_submitted_refSpec_by_BY = sum(n_submitted_refSpec_by_quality),
             oto_propn_by_rfStatQuality = case_when(`BROOD YEAR` %notin% SJomRef$`BROOD YEAR` ~ 1.00,
                                                    TRUE ~ round(n_submitted_refSpec_by_quality/n_submitted_refSpec_by_BY,2))
      ) %>%
      select(-c(n_submitted_refSpec_by_quality)) %>%
      
      
      
      
      pivot_wider(names_from = QUALITY, values_from = oto_propn_by_rfStatQuality, names_prefix = "TM Quality (%): ") %>%
      group_by(`BROOD YEAR`) %>% 
      summarise(across(`RF READ STATUS`, ~ max(., na.rm=T)),
                across(TM_application_flag:n_submitted_refSpec_by_BY, ~ unique(., na.rm = T)),
                across(`TM Quality (%): Unknown`:`TM Quality (%): Poor`, ~ sum(., na.rm = T))) %>% 
      relocate(`TM Quality (%): NA`, .before = `TM Quality (%): Unknown`)
  ),
  # --- CWT/AD release #s
  SJRelSEP %>% 
    filter(SPECIES_NAME=="Chinook") %>% 
    group_by(BROOD_YEAR) %>%
    summarize(SEP_released = sum(TotalRelease,na.rm=T),
              `CWT` = round(sum(TaggedNum,na.rm=T)/SEP_released,3),
              `AD-only` = round((sum(NoTagNum,na.rm=T)+sum(ShedTagNum,na.rm=T))/SEP_released,3),
              `mark rate` = round((sum(NoTagNum,na.rm=T)+sum(ShedTagNum,na.rm=T)+sum(TaggedNum,na.rm=T))/SEP_released,3),
              `unmark/untag` = round(sum(UnmarkedNum,na.rm=T)/SEP_released,3)) %>% 
    rename(`BROOD YEAR`=BROOD_YEAR) 
) %>% 
  mutate(total_rel_check = TM_release==SEP_released) %>%
  left_join(.,
            SJPBT %>%
              rename(`BROOD YEAR`=BROOD_YEAR)) %>%
  arrange(`BROOD YEAR`) %>%
  rename(TM_read_status = `RF READ STATUS`) %>%
  mutate(TM_comments = coalesce(TM_application_flag, comments)) %>% 
  select(-c(TM_application_flag, comments)) %>%
  print()


# EXPORT ----------------------
#writexl::write_xlsx(SJ_mark_history_BYrollup, here("outputs", "R_OUT - San Juan mark history by BROOD YEAR.xlsx"))













