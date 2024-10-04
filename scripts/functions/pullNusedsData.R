# pullNusedsData.R 
# based on saaWeb package by N. Komick
# Feb 2024


# Load library -----------------------
library(saaWeb)




# ============================ 1. DEFINE FUNCTION ============================
runNuSEDSQuery <- function (query_doc, config_file = "saaWeb.config", user_name = Sys.getenv("username"), password = NULL) 
{
  config_list <- saaWeb:::loadConfigFile(config_file)
  extractor_usage_url <- config_list$NusedsExtractorUsageUrl
  extractor_query_url <- config_list$NusedsExtractorQueryUrl
  query_result <- saaWeb:::runExtractorQuery(query_doc, extractor_usage_url, 
                                             extractor_query_url, user_name, password)
  return(query_result)
}


# ============================ 2. EXTRACT ESCAPEMENT DATA ============================
a20_nuseds_escapement <- runNuSEDSQuery(query_doc=here::here("scripts", "json", "nuseds_esc_query_A20.json"), 
                                 config_file=here::here("saaWeb.config")) %>%
  filter(!grepl("GEORGIA STRAIT", `Conservation Unit Name`),
         `Waterbody Name` %notin% c("DE MAMIEL CREEK", "ROCKY CREEK", "AYUM CREEK", "SOOKE RIVER", "MAIDENHAIR CREEK", 
                                    "MUIR CREEK", "FALLS CREEK", "UGLOW CREEK"), 
         !grepl("Charters|Jordan", `Waterbody Name`, ignore.case = T)) %>%
  mutate(across(c(`Analysis Year`, `Max Estimate`:`Natural Adult Spawners`, 
                  `Other Adults Removals`:`Total Jack Return River`), as.numeric)) %>%
  mutate(`Total Adult Return River` = case_when(is.na(`Total Adult Return River`) ~ `Max Estimate`,
                                                TRUE ~ `Total Adult Return River`),
         `Adult Broodstock Removals` = case_when(is.na(`Adult Broodstock Removals`) ~ `Total Broodstock Removals`,
                                                 TRUE ~ `Adult Broodstock Removals`)) %>% 
  pivot_longer(cols=c("Max Estimate":"Unspecified Return", "Other Removals":"Natural Adult Spawners", 
                      "Other Adults Removals":"Total Jack Return River"), 
               names_to = "est_type", values_to = "estimate") %>% 
  rename(waterbody_name=`Waterbody Name`,
         year=`Analysis Year`) %>% 
  mutate(source="NuSEDS")



# ============================ 3: EXPORT ============================

# Export to github repo ------------------------
writexl::write_xlsx(a20_nuseds_escapement, here::here("outputs", 
                                           paste0("R_OUT - Area20WCVI_Escapement_allSpp-allYrs ", min(a20_nuseds_escapement$year), "-", max(a20_nuseds_escapement$year), ".xlsx")))

