# GSI compile 
# Feb 2024


# Load libraries ------------------------------------
library(tidyverse)



# ========================= LOAD MGL FILES =========================

# Load and compile repunit_table_ids tabs all years ------------------------------------
gsi.repunits_table_ids.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         })
)

# Rename, convert to data frame: 
names(gsi.repunits_table_ids.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                            pattern=".xlsx", full.names=F),
                                   list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                              pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.repunits_table_ids <- do.call("rbind", gsi.repunits_table_ids.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.repunits_table_ids.LL)



# Load and compile extraction_sheet tabs all years ------------------------------------
gsi.extraction_sheets.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         })
)

# Rename, convert to data frame: 
names(gsi.extraction_sheets.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                                 pattern=".xlsx", full.names=F),
                                      list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.extraction_sheets <- do.call("rbind", gsi.extraction_sheets.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.extraction_sheets.LL)


# Load and compile species_ID tabs all years ------------------------------------
gsi.species_ID.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         })
)

# Rename, convert to data frame: 
names(gsi.species_ID.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                                 pattern=".xlsx", full.names=F),
                                      list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.species_ID <- do.call("rbind", gsi.species_ID.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.species_ID.LL)




# ========================= JOIN MGL INTO 1 MASTER FILE =========================

# Join into a master dataframe ------------------------------------
gsi.master <- full_join(
  gsi.repunits_table_ids %>% 
    select(indiv, mixture_collection:prob.2, top_collection, associated_collection_prob),
  gsi.extraction_sheets %>% 
    select(indiv, CatchJulDate, Vial, Comments, ID_Source),
  by=c("indiv", "ID_Source")
) %>%
  full_join(.,
            gsi.species_ID %>%
              select(indiv:neg_sp_confirmed),
            by=c("indiv")) %>%
  print()


# Export in case needed ------------------------------------
writexl::write_xlsx(gsi.master, path=paste0(here::here("data", "juvenile", "GSI"), 
                                            "/San Juan MGL master file ",
                                            Sys.Date(),
                                            ".xlsx"))














  
  
  
  
  
  


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


