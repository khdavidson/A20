
# Dump and combine age data for all SJ Species


# Load libraries ------------------------
library(tidyverse)


# Helper ------------------------
"%notin%" <- Negate("%in%")




# ============================ 1: MRP AGES (~2020-present) ============================

# 1.1. Dump MRP age batch metadata ------------------------
  # This output has broad metadata like river etc., but no results
SJ_ageBatchMeta.MRP <- saaWeb::getAgeBatchList2021toCurrent() %>% 
  filter(Sector=="SC", Location=="San Juan River") %>% 
  mutate_at("Id", as.character)


# 1.2. Dump MRP age results ------------------------ (slow)
  # This output has the age results, but can't be traced back to specific river etc. without joining to dataframe above
SJ_scaleAges.MRP <- saaWeb::getAgeBatchScaleResults(c(SJ_ageBatchMeta.MRP$Id)) %>% 
  rename(SampleYear=RecoveryYear) %>% 
  mutate_at("SampleYear", as.numeric) %>% 
  filter_all(any_vars(!is.na(.)))


# 1.3. Join MRP batch metadata to results ------------------------
intersect(colnames(SJ_ageBatchMeta.MRP), colnames(SJ_scaleAges.MRP))

SJ_scaleAgesMeta.MRP <- left_join(SJ_scaleAges.MRP, SJ_ageBatchMeta.MRP) %>%
  #filter(!grepl("Georgia Str|Sooke", ProjectName)) %>%
  setNames(paste0('PADS_', names(.))) %>%
  mutate(`(R) SCALE BOOK NUM` = PADS_ContainerId,
         `(R) SCALE CELL NUM` = PADS_FishNumber,
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) SCALE BOOK NUM`) & !is.na(`(R) SCALE CELL NUM`) ~
                                                    paste0(`(R) SCALE BOOK NUM`,sep="-",`(R) SCALE CELL NUM`)),
         `(R) scale data source` = "MRP") %>%
  rename(`(R) SAMPLE YEAR` = PADS_SampleYear) %>%
  select(PADS_Species, `(R) SAMPLE YEAR`, PADS_LifeHistory, PADS_Region, PADS_Area, PADS_ProjectName, PADS_Location, PADS_GearMrpName,
         PADS_EuAge, PADS_GrAge, PADS_FishEdge, PADS_ScaleCondition, PADS_ContainerId, PADS_FishNumber, `(R) SCALE BOOK NUM`, `(R) SCALE CELL NUM`, 
         `(R) SCALE BOOK-CELL CONCAT`, `(R) scale data source`, PADS_Id, PADS_Structure) %>%
  mutate_at(c("(R) SCALE CELL NUM","(R) SAMPLE YEAR"), as.character) %>%
  mutate_at("(R) SAMPLE YEAR", as.numeric) %>%
  print()





# ============================ 2: NuSEDS AGES (historical-2021) ============================

# 3.1. Define NuSEDS query function (not yet in saaWeb) ------------------------
runNuSEDSQuery <- function (query_doc, config_file = here::here("saaWeb.config"), user_name = Sys.getenv("username"), password = NULL) 
{
  config_list <- saaWeb:::loadConfigFile(config_file)
  nuseds_usage_url <- config_list$NusedsExtractorUsageUrl
  nuseds_query_url <- config_list$NusedsExtractorQueryUrl
  query_result <- saaWeb:::runExtractorQuery(query_doc, nuseds_usage_url, 
                                             nuseds_query_url, user_name, password)
  return(query_result)
}



# 3.2. Dump NuSEDS ages & reformat ------------------------
SJ_ages.NuSEDS <- runNuSEDSQuery(here::here("scripts", "json", "nuseds_ages_SanJuan_allSpp-allYrs.json")) %>%
  select(`Fiscal Year`, Project, Location, Species, `Sample Source`, `Gear Code`, `Container Label`, `Container Address`, `Sample Number`, 
         `Sample Start Date`, `Sample End Date`, `Part Age Code`, `GR Age`, `EU Age`) %>%
  setNames(paste0('PADS_', names(.))) %>%
  rename(PADS_ProjectName = PADS_Project,
         PADS_Location = PADS_Location,
         PADS_Species = PADS_Species,
         PADS_GearMrpName = `PADS_Gear Code`,
         PADS_CntStartDate = `PADS_Sample Start Date`,
         PADS_CntEndDate = `PADS_Sample End Date`,
         PADS_ScaleCondition = `PADS_Part Age Code`,
         PADS_GrAge = `PADS_GR Age`,
         PADS_EuAge = `PADS_EU Age`) %>%
  mutate(`(R) SAMPLE YEAR` = `PADS_Fiscal Year`,
         `(R) SCALE BOOK NUM` = `PADS_Container Label`,
         `(R) SCALE CELL NUM` = `PADS_Container Address`,
         `(R) scale data source` = "NuSEDs",
         `(R) SCALE BOOK-CELL CONCAT` = case_when(!is.na(`(R) SCALE BOOK NUM`) & !is.na(`(R) SCALE CELL NUM`) ~ paste0(`(R) SCALE BOOK NUM`, sep="-",
                                                                                                                       `(R) SCALE CELL NUM`)),
         PADS_CntStartDate = lubridate::ymd(PADS_CntStartDate),
         PADS_CntEndDate = lubridate::ymd(PADS_CntEndDate)) %>%
  mutate_at(c("PADS_GearMrpName", "(R) SCALE CELL NUM"), as.character) %>%
  print()





# ============================ 3: NuSEDS + MRP ages ============================

# Join all ages for complete dataset (NuSEDS and MRP PADS) ------------------------
intersect(colnames(SJ_ages.NuSEDS), colnames(SJ_scaleAgesMeta.MRP))

SJ_allAgesMaster <- full_join(SJ_scaleAgesMeta.MRP %>% 
                                # currently exclude the years from NuSEDS as the MRP database is in a state of transitioning the data over and is sometimes incomplete for those historical years
                                filter(`(R) SAMPLE YEAR` %notin% SJ_ages.NuSEDS$`(R) SAMPLE YEAR`),
                              SJ_ages.NuSEDS) %>%
  arrange(`(R) SAMPLE YEAR`)
 



# ============================ 4: EXPORT ============================

# 4.1. Export to StA drive, WCVI Term Run folder ------------------------ (no)
# writexl::write_xlsx(SC_allAgesMaster, 
#                     paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/R_OUT - ALL South Coast Chinook Age results ", min(SC_allAgesMaster$`(R) SAMPLE YEAR`), "-", max(SC_allAgesMaster$`(R) SAMPLE YEAR`), ".xlsx"))



# 4.2. Export to github repo ------------------------
writexl::write_xlsx(SJ_allAgesMaster, here::here("outputs", 
                                           paste0("R_OUT - SanJuan_Ages_allSpp-allYrs ", min(SJ_allAgesMaster$`(R) SAMPLE YEAR`), "-", max(SJ_allAgesMaster$`(R) SAMPLE YEAR`), ".xlsx")))



# 4.3. Export to Sharepoint ------------------------ (no)
# writexl::write_xlsx(SC_allAgesMaster, 
#                     paste0("C:/Users/",
#                            Sys.info()["login"],
#                            "/DFO-MPO/PAC-SCA Stock Assessment (STAD) - Terminal CN Run Recon/",
#                            analysis_year,
#                            "/Communal data/Misc biodata dumps",
#                            "/R_OUT - ALL South Coast Chinook Age results ", 
#                            min(SC_allAgesMaster$`(R) SAMPLE YEAR`), 
#                            "-", 
#                            max(SC_allAgesMaster$`(R) SAMPLE YEAR`), 
#                            ".xlsx"))




