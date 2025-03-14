# San Juan 2024 RST 
# 2024/2025


# Load libraries -------------------
library(tidyverse)

# Load helpers -------------------
"%notin%" <- Negate("%in%")



# ========================= LOAD AREA 20 JUVI "DATABASE" =========================

# Sample event metadata/environmentals ----------------- 

eventMeta <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/test run master DB development.xlsx",
  #                  sheet="sample_event_meta")
  readxl::read_excel(path=here::here("data", "juvenile", "test run master DB development.xlsx"),
                     sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear))



# Catch totals ----------------- 

setTotals <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/test run master DB development.xlsx",
  #                    sheet="set_totals")
  readxl::read_excel(path=here::here("data", "juvenile", "test run master DB development.xlsx"),
                     sheet="set_totals") %>% 
  filter(grepl("RST|IPT", gear)) %>%
  mutate(species_stage_simple = case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow parr",
                                          grepl("cutthroat", species, ignore.case=T) ~ "Cutthroat parr",
                                          !is.na(life_stage) ~ paste0(species, " ", life_stage),
                                          grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                          grepl("lamprey|sculpin|stickleback", species, ignore.case=T) ~ "Other non-salmonid",
                                          
                                          TRUE ~ species))


# Mark-release ----------------- 

release <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/test run master DB development.xlsx",
  #                    sheet="mark-release")
  readxl::read_excel(path=here::here("data", "juvenile", "test run master DB development.xlsx"),
                     sheet="mark-release")



# Biosampling ----------------- 
biosamp <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/test run master DB development.xlsx",
  #                  sheet="biosampling")
  readxl::read_excel(path=here::here("data", "juvenile", "test run master DB development.xlsx"),
                     sheet="biosampling") %>% 
  filter(grepl("RST|IPT", gear))




# ========================= LOAD ENVIRONMENTAL =========================
# Load hydromet data eventually  - can i call in existing code from the compendium?


# ========================= LOAD HATCHERY DATA =========================
# Load hatchery releases - can i call in existing code from the compendium?


# ========================= LOAD 2024 EPICOLLECT DATA =========================  ***DELETE ALL OF THIS SOON! 
# Form 1: Metadata/survey details -------------------
# rst24.1 <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-1_RSTmaster - verified.xlsx",
#                               guess_max=20000) %>% 
#   mutate(R_date = lubridate::dmy(stringr::str_sub(title, start=1, end=10))#,
#          #R_time = stringr::str_sub()
#          ) %>% 
#   rename(ec5_parent_uuid=ec5_uuid)
# 
# 
# # Form 2: Enumeration -------------------
# rst24.2 <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-2_RSTmaster - verified.xlsx",
#                               guess_max=20000) 
# 
# rst24.releases <- rst24.2 %>% 
#   select(X217_Are_you_marking_:Comments)
# 
# rst24.2 <- rst24.2 %>% 
#   select(ec5_uuid:Species_etc)
# 
# 
# # Form 3: Catch sampling -------------------
# rst24.3 <- readxl::read_excel(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-3_RSTmaster - verified.xlsx",
#                               guess_max=20000)





# ========================= JOIN 2024 EPICOLLECT DATA =========================
# rst24.metaEnum <- left_join(rst24.1, rst24.2,
#                             by="ec5_parent_uuid") %>% 
#   mutate(DOY = lubridate::yday(R_date),
#          across(c(CN_smlt_cl_nobis:Anchovy), ~case_when(.=="NA" ~ NA,
#                                                         TRUE ~ as.numeric(.)))) %>% 
#   pivot_longer(cols=c(CN_smlt_cl_nobis:Anchovy), names_to="spp_stage_condition", values_to="count") %>% 
#   mutate(species = case_when(grepl("CO", spp_stage_condition) ~ "Coho",
#                              grepl("CN", spp_stage_condition) ~ "Chinook",
#                              grepl("SK", spp_stage_condition) ~ "Sockeye",
#                              grepl("CM", spp_stage_condition) ~ "Chum",
#                              grepl("Pk|PK", spp_stage_condition) ~ "Pink",
#                              grepl("SH", spp_stage_condition) ~ "Steelhead",
#                              grepl("other", spp_stage_condition) ~ "Other"),
#          stage = case_when(grepl("fry", spp_stage_condition) ~ "Fry",
#                            grepl("smlt|smolt", spp_stage_condition) ~ "Smolt",
#                            TRUE ~ "FLAG"),
#          condition = case_when(grepl("mort|mrt", spp_stage_condition) ~ "Mort",
#                                TRUE ~ "Live"),
#          tag_status = case_when(grepl("nobi", spp_stage_condition) ~ "Untagged",
#                                 grepl("_bis", spp_stage_condition) ~ "Tag recovery",
#                                 TRUE ~ "FLAG"),
#          clip_status = case_when(grepl("_cl_", spp_stage_condition) ~ "Ad-clipped",
#                                  TRUE ~ "Unclipped"),
#          count = case_when(is.na(count) ~ 0,
#                            TRUE ~ as.numeric(count)))


# export for use later
# writexl::write_xlsx(rst24.metaEnum, here::here("outputs", "R_OUT - RST 2024 metadata and catch totals.xlsx"))
# 
# 
# 
# writexl::write_xlsx(left_join(rst24.2, rst24.1,
#                               by="ec5_parent_uuid"), here::here("outputs", "R_OUT - RST 2024 form 1+2.xlsx"))
# writexl::write_xlsx(left_join(rst24.3, rst24.1,
#                               by="ec5_parent_uuid"), here::here("outputs", "R_OUT - RST 2024 form 1+3.xlsx"))






         
############################################################################################################################################################

#                                                                 CATCH SECTION (2023-2025)


# ======================= ALL SPECIES, ALL YEARS =======================

# Species slightly simplified ---------------------
rstCatchFig <- 
ggplot() +
  geom_bar(data=setTotals %>%
             filter(!is.na(species) & species!="unknown") %>%
             group_by(year, DOY, species_stage_simple) %>%
             summarize(n=sum(total_caught_excl_recaps, na.rm=T)) ,
           aes(x=as.Date(DOY, origin="2023-12-31"), y=n,
               fill=stringr::str_to_sentence(species_stage_simple), colour=stringr::str_to_sentence(species_stage_simple)),
           stat="identity", alpha=0.7,
           position=position_stack(vjust=0.5)) +
  # ggrepel::geom_text_repel(data=setTotals %>%
  #                            filter(!is.na(species) & species!="unknown") %>%
  #                            group_by(year, DOY, species_stage_simple) %>%
  #                            summarize(n=sum(total_caught_excl_recaps, na.rm=T)) %>%
  #                            filter(grepl("coho|chum|chinook", species_stage_simple, ignore.case=T), n<10),
  #                          aes(label=stringr::str_to_sentence(paste0(species_stage_simple, " (", n, ")")),
  #                              x=as.Date(DOY, origin="2023-12-31"),
  #                              y=n-10,
  #                              colour=species_stage_simple),
  #                          position=position_stack(vjust=1),
  #                          size=2.5,
  #                          direction="y",
  #                          hjust=-0.5,
  #                          segment.size=1, segment.curvature=-0.1, segment.ncp=3, segment.angle=45, min.segment.length=unit(0, 'lines'),
  #                          box.padding=2,
  #                          force=0.5, force_pull=1,
  #                          na.rm=T, show.legend=F) +
  scale_x_date(date_labels="%b %d", date_breaks="3 day") +
  labs(x="", y="Total catch") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text = element_text(colour="black", size=8),
        axis.title = element_text(face="bold", size=9),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(2.5, "mm"), 
        legend.title = element_blank(),
        legend.position = c(0.8, 0.3),
        legend.background = element_rect(colour="black"),
        strip.text = element_text(size=8)) +
  guides(fill=guide_legend(ncol=2)) +
  facet_wrap(~year, ncol=1, scales="free_y")
  

# Save as PDF ---------------------
pdf(file = here::here("outputs", "figures", "juvenile", "RST raw catch (bars).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

rstCatchFig

dev.off()






# ========================= MORTS =========================

# Count of mortalities -------------------
# ggplot(data=rst24.metaEnum %>% 
#          filter(condition=="Mort") %>% 
#          group_by(DOY, species, stage) %>%
#          summarize(n=sum(count, na.rm=T)) %>%
#          filter(species%in%c("Chinook", "Chum", "Coho") & n>0), 
#        aes(x=as.Date(DOY, origin="2023-12-31"), y=n, group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" "))) +
#   geom_bar(stat="identity", width=1, alpha=0.6) +
#   scale_x_date(date_labels="%b %d", date_breaks="3 day") +
#   labs(x="", y="Mortalities (count)", group="Species/stage", fill="Species/stage", colour="Species/stage") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=45, hjust=1),
#         axis.text = element_text(colour="black"),
#         axis.title = element_text(face="bold"),
#         legend.title = element_text(face="bold"),
#         legend.position = c(0.8,0.8),
#         legend.background = element_rect(colour="black"))


# Mort rate -------------------
# ggplot() +
#   geom_bar(data=rst24.metaEnum %>% 
#              filter(!is.na(species)) %>%
#              group_by(DOY, species, stage, condition) %>%
#              summarize(n=sum(count, na.rm=T)) %>%
#              group_by(DOY) %>% 
#              mutate(total=sum(n)) %>%
#              filter(condition=="Mort" & total>0) %>% 
#              mutate(mort_rate=n/total) %>%
#              filter(mort_rate>0),
#            aes(x=as.Date(DOY, origin="2023-12-31"), y=mort_rate, 
#                group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" "), 
#                label=paste0("n=", n)), 
#            stat="identity", width=1, alpha=0.5, position=position_stack(vjust=0.5)) +
#   # geom_text(data=rst24.metaEnum %>% 
#   #             filter(!is.na(species)) %>%
#   #             group_by(DOY, species, stage, condition) %>%
#   #             summarize(n=sum(count, na.rm=T)) %>%
#   #             group_by(DOY) %>% 
#   #             mutate(total=sum(n)) %>%
#   #             filter(condition=="Mort" & total>0) %>% 
#   #             mutate(mort_rate=n/total) %>%
#   #             filter(mort_rate>0),
#   #           aes(x=as.Date(DOY, origin="2023-12-31"), y=mort_rate+0.007, 
#   #               group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" "),
#   #               label=paste0("n=", n)), position=position_stack(vjust=0.5)) +
#   labs(x="", y="Mortality rate", group="Species/stage", fill="Species/stage", colour="Species/stage") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=45, hjust=1),
#         axis.text = element_text(colour="black"),
#         axis.title = element_text(face="bold"),
#         legend.title = element_text(face="bold"),
#         legend.position = c(0.8,0.8),
#         legend.background = element_rect(colour="black"))




# ========================= CORRECTING FOR MIS-ID CHINOOK/COHO?? =========================




# ========================= CHINOOK, COHO ONLY =========================

# Cumulative probability figure -----------------------




# ========================= CHINOOK ONLY =========================

# Smolts & fry -----------------------






############################################################################################################################################################

#                                             INFILLING FOR UNFISHED DAYS (2023-2025) & MARK RECAPTURE ESTIMATE 


# ========================= INFILLING =========================
# Do for chum, coho, chinook only (?)




# ========================= MARK-RECAP ESTIMATE (2024, 2025) =========================
# Do for chum, coho, chinook only (?)



############################################################################################################################################################

#                                                                     BIOSAMPLING



# ========================= SUMMARY STATS, ALL SPECIES, ALL YEARS =========================

# Length x weight x condition factor






############################################################################################################################################################














