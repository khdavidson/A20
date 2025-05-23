# San Juan mark history compilation - all mark types 
# Shifted out of rmarkdown script May 2025


# Load packages ------------------------
library(tidyverse)

# Helpers ------------------------
"%notin%" <- Negate("%in%")
options(scipen = 999999)




# ============================== LOAD MARKS ==============================

# PBT ------------------------

SJPBT <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/15-DNA_Results/PBT/Chinook/R_OUT - PBT_Tag_Rates_WCVIStocks-All BYs.xlsx",
                            sheet="Sheet1") %>% 
  filter(grepl("san juan", Brood, ignore.case=T))



# Load Oto Manager reference specimens output ------------------------
# Note, if new ref specimens have been added, have to run OtoCompile.R in run reconstruction repo: https://github.com/SCA-stock-assess/WCVI_CN_TermRunRecon/blob/main/scripts/misc-helpers/OtoCompile.R
SJoto_OM <-
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/OtoCompile_base-files/ReferenceSpecimens/2-Export-from-R/", 
                    pattern="^R_OUT - OtoManager_CN_REFERENCEspecimens.*\\.xlsx$", 
                    full.names=T), 
         function(x) {
           readxl::read_excel(path=x, sheet="Sheet1", guess_max=20000)
         })  %>% 
  do.call("cbind",.) %>%
  filter(grepl("SAN JUAN", FACILITY)) %>% 
  mutate(data_source = "reference specimen (Oto Manager)") %>%
  mutate(`USER COMMENT` = stringr::str_to_lower(`USER COMMENT`))


# Load NPAFC mark file ------------------------
SJoto_NPAFC <- readxl::read_excel(path=list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/Spec_Projects/Thermal_Mark_Project/Marks/",
                                                  pattern = "^All CN Marks",   
                                                  full.names = TRUE), 
                                  sheet=1) %>%
  mutate(data_source = "NPAFC mark records") 



# CWT/AD ------------------------ (slow-ish but not bad)
source(here::here("scripts", "functions", "pullA20Releases.R"))
# saves as a20Releases


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# 1. JOIN OM + NPAFC ========================================== 

SJoto_OM.NPAFC <- full_join(
  # Summarize Oto Manager reference specimens: --------------
  SJoto_OM %>%
    filter(`RF READ STATUS`%notin%c("Destroyed")) %>%
    group_by(`BROOD YEAR`, `RF READ STATUS`, QUALITY) %>% 
    summarize(n_submitted_refSpec_by_quality = n(),
              `USER COMMENT` = paste(unique(na.omit(`USER COMMENT`)), collapse=" / ")) %>%
    mutate(n_submitted_refSpec_by_quality = case_when(`BROOD YEAR`==2001 ~ 16,
                                                      `BROOD YEAR` %notin% SJoto_OM$`BROOD YEAR` ~ 0,
                                                      TRUE ~ n_submitted_refSpec_by_quality))  %>% 
    group_by(`BROOD YEAR`) %>% 
    mutate(total_refspecimens_BY = sum(n_submitted_refSpec_by_quality),
           propn_quality = case_when(`BROOD YEAR` %notin% SJoto_OM$`BROOD YEAR` ~ 1.00,
                                     TRUE ~ round(n_submitted_refSpec_by_quality/total_refspecimens_BY,2))) %>%
    select(-c(n_submitted_refSpec_by_quality)) %>%
    pivot_wider(names_from = QUALITY, values_from = propn_quality, names_prefix = "TM Quality (%): ") %>%
    group_by(`BROOD YEAR`) %>% 
    summarise(across(`RF READ STATUS`:`USER COMMENT`, ~ max(., na.rm=T)) ,
              across(total_refspecimens_BY, ~ unique(., na.rm = T)),
              across(`TM Quality (%): Good`:`TM Quality (%): Poor`, ~ sum(., na.rm = T))) %>% 
    relocate(`TM Quality (%): NA`, .before = `TM Quality (%): Good`),
  
  # Summarize NPAFC extra records: --------------
  SJoto_NPAFC %>% 
    filter(grepl("SAN JUAN", STOCK), BROOD_YEAR %notin% unique(SJoto_OM$`BROOD YEAR`)) %>% 
    rename(NPAFC_COMMENT = MARK_COMMENT) %>% 
    group_by(BROOD_YEAR) %>% 
    summarize(NPAFC_COMMENT = paste(na.omit(NPAFC_COMMENT), collapse=" / ")) %>% 
    mutate(`RF READ STATUS` = case_when(grepl("not thermally marked", NPAFC_COMMENT, ignore.case=T) ~ "Not Marked",
                                        TRUE ~ "Marked"),
           QUALITY = case_when(`RF READ STATUS`=="Not Marked" ~ NA,
                               TRUE ~ "Unknown"),
           propn_quality=1) %>% 
    pivot_wider(names_from = QUALITY, values_from = propn_quality, names_prefix = "TM Quality (%): ") 
  
  ,
  by=c("BROOD YEAR"="BROOD_YEAR",
       "RF READ STATUS", "TM Quality (%): NA")
) %>% 
  relocate("TM Quality (%): Unknown", .after="TM Quality (%): NA") %>%
  arrange(`BROOD YEAR`) %>%
  left_join(.,
            SJoto_NPAFC %>% 
              filter(grepl("SAN JUAN", STOCK)) %>% 
              rename(NPAFC_COMMENT = MARK_COMMENT) %>% 
              group_by(BROOD_YEAR) %>% 
              summarize(NPAFC_COMMENT = paste(na.omit(NPAFC_COMMENT), collapse=" / ")) %>%
              mutate(TM_intended_flag = case_when(grepl("intended", NPAFC_COMMENT, ignore.case=T) ~ "mark application issue: actual mark applied was not the intended mark applied",
                                                  TRUE ~ "ok")) %>%
              select(BROOD_YEAR, NPAFC_COMMENT, TM_intended_flag),
            by=c("BROOD YEAR"="BROOD_YEAR")
  ) %>%
  select(-c(NPAFC_COMMENT.x)) %>%
  unite(NPAFC_COMMENT.y, `USER COMMENT`, col=TM_COMMENT, na.rm=T, sep=" / ", remove=T) %>%
  relocate(TM_COMMENT, .before=TM_intended_flag) %>%
  print()








# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# MARK HISTORY/QUALITY ~ BROOD YEAR ----------------------

SJ_mark_history_BYrollup <- full_join(
  full_join(
    # --- Otolith release # from NPAFC file
    SJoto_NPAFC %>% 
      filter(grepl("SAN JUAN", STOCK)) %>% 
      group_by(BROOD_YEAR) %>% 
      summarize(TM_release = sum(NUMBER_RELEASED),
                TM_COMMENT = paste(na.omit(MARK_COMMENT), collapse=" / ")) %>% 
      ungroup(),
    
    # --- Otolith mark quality from OtoCompile.R
     SJoto_OM.NPAFC #%>% 
  #     filter(`RF READ STATUS`%notin%c("Destroyed")) %>%
  #     group_by(`BROOD YEAR`, `RF READ STATUS`, QUALITY) %>% 
  #     summarize(TM_intended_flag = unique(TM_intended_flag), 
  #               n_submitted_refSpec_by_quality = n(),
  #               TM_COMMENT = paste(na.omit(TM_COMMENT), collapse=" / ")) %>%
  #     mutate(n_submitted_refSpec_by_quality = case_when(`BROOD YEAR`==2001 ~ 16,
  #                                                       `BROOD YEAR` %notin% SJoto_OM$`BROOD YEAR` ~ 0,
  #                                                       TRUE ~ n_submitted_refSpec_by_quality))  %>% 
  #     group_by(`BROOD YEAR`) %>% 
  #     mutate(total_refspecimens_BY = sum(n_submitted_refSpec_by_quality),
  #            oto_propn_by_rfStatQuality = case_when(`BROOD YEAR` %notin% SJoto_OM$`BROOD YEAR` ~ 1.00,
  #                                                   TRUE ~ round(n_submitted_refSpec_by_quality/total_refspecimens_BY,2))
  #     ) %>%
  #     select(-c(n_submitted_refSpec_by_quality)) %>%
  #     pivot_wider(names_from = QUALITY, values_from = oto_propn_by_rfStatQuality, names_prefix = "TM Quality (%): ") %>%
  #     group_by(`BROOD YEAR`) %>% 
  #     summarise(across(`RF READ STATUS`, ~ max(., na.rm=T)),
  #               across(TM_intended_flag:total_refspecimens_BY, ~ unique(., na.rm = T)),
  #               across(`TM Quality (%): Unknown`:`TM Quality (%): Poor`, ~ sum(., na.rm = T))) %>% 
  #     relocate(`TM Quality (%): NA`, .before = `TM Quality (%): Unknown`),
  #   
  #   by=c("BROOD_YEAR" = "BROOD YEAR",
  #        "TM_COMMENT")
  # 
  ),
  # --- CWT/AD release #s
  a20Releases %>% 
    filter(`Species Name`=="Chinook") %>% 
    rowwise() %>%
    mutate(CWT = sum(`Num WithCWT Adclip`, `Num WithCWT NoAdclip`, `Num WithCWT UnknAD`, na.rm=T),
           `AD-clip` = sum(`Num NoCWT Adclip`, `Num WithCWT Adclip`, na.rm=T),
           unmark_untag = sum(`Num NoCWT NoAdclip`, na.rm=T)) %>%
    group_by(`Brood Year`) %>%
    summarize(SEP_released = sum(`Total Released`,na.rm=T),
              CWT_rate = round(sum(CWT, na.rm=T)/SEP_released,3),
              AD_rate = round(sum(`AD-clip`, na.rm=T)/SEP_released,3),
              #`mark rate` = round((sum(NoTagNum,na.rm=T)+sum(ShedTagNum,na.rm=T)+sum(TaggedNum,na.rm=T))/SEP_released,3),
              unmark_untag = round(sum(unmark_untag, na.rm=T)/SEP_released,3)),
  
  by=c("BROOD_YEAR"="Brood Year")
) %>% 
  mutate(total_rel_check = TM_release==SEP_released) %>%
  left_join(.,
            SJPBT %>% 
              select(BY_tagrate, Brood_Year) %>%
              rename(PBT_tagrate = BY_tagrate),
            by=c("BROOD_YEAR"="Brood_Year")) %>%
  arrange(BROOD_YEAR) %>%
  rename(TM_read_status = `RF READ STATUS`) %>%
  #mutate(TM_comments = coalesce(TM_intended_flag, comments)) %>% 
  #select(-c(TM_intended_flag, comments)) %>%
  print()


# EXPORT ----------------------
writexl::write_xlsx(SJ_mark_history_BYrollup, here::here("outputs", "R_OUT - San Juan mark history by BROOD YEAR.xlsx"))













