# Purse seine source script for Tech Report - copied over from data compendium 




# Load libraries ------------------------------------
library(tidyverse)


# Helpers ------------------------------------
"%notin%" <- Negate("%in%")



# ===================== LOAD 2023 DATA =====================

# 2023 Set totals and environmentals ------------------------------------
juvi.enviroMeta <- left_join(readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"), 
                                                sheet="enviro_ocgy", trim_ws=T),
                             readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"),
                                                sheet="sample_event_metadata", trim_ws=T), 
                             na_matches="never",
                             by="usid")

juvi.enumMeta <- left_join(readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"),
                                              sheet="set_totals", trim_ws=T),
                           readxl::read_excel(here::here("data", "juvenile", "PFN_DFO_FTFjuvi_2023_verified.xlsx"),
                                              sheet="sample_event_metadata", trim_ws=T), 
                           na_matches="never",
                           by="usid")



# 2023 Biodata ------------------------------------

# OPTION 1: Only need to do if new 2023 GSI after Aug 2024: 
#juvi.bioMeta.GSI <- source(here("scripts","misc-helpers","gsiCompile.R")) %>% 
# mutate(`(R) ORIGIN` = case_when(ad_clip=="Y" ~ "Hatchery",
#                                 MGL_ID_Source=="PBT" ~ "Hatchery",
#                                 ad_clip=="N" & MGL_ID_Source=="GSI" ~ "Natural",
#                                 TRUE ~ "Unknown"))

# OPTION 2: all other times
juvi.bioMeta.GSI <- readxl::read_excel(path=here::here("outputs", list.files(path=here::here("outputs"),
                                                                             pattern = "^R_OUT - PFN_DFO_FTFjuvi_2023_verified_with-GSI-Results_")),
                                       sheet="Sheet1", guess_max=1500) %>% 
  mutate(`(R) ORIGIN` = case_when(species%in%c("chinook", "coho", "chum", "pink", "sockeye") & 
                                    ad_clip=="Y" ~ "Hatchery",
                                  species%in%c("chinook", "coho", "chum", "pink", "sockeye") & 
                                    MGL_ID_Source=="PBT" ~ "Hatchery",
                                  species%in%c("chinook", "coho", "chum", "pink", "sockeye") & 
                                    ad_clip=="N" & MGL_ID_Source=="GSI" ~ "Natural (assumed)",
                                  species%in%c("chinook", "coho", "chum", "pink", "sockeye") & 
                                    ad_clip=="N" & MGL_ID_Source%notin%c("GSI", "PBT") ~ "Unknown",
                                  species%in%c("chinook", "coho", "chum", "pink", "sockeye") & 
                                    ad_clip%notin%c("Y", "N") & MGL_ID_Source%notin%c("GSI", "PBT") ~ "Unknown",
                                  TRUE ~ NA),
         `(R) RESOLVED STOCK ID` = case_when(is.na(DNA_vial) ~ "Unknown (No DNA sample)",
                                             !is.na(MGL_top_collection) & MGL_associated_collection_prob>=0.75 ~
                                               str_to_title(gsub(pattern="_", replacement=" ", MGL_top_collection)),
                                             grepl("failed|loci", MGL_ID_Source, ignore.case=T) |
                                               MGL_associated_collection_prob<0.75 ~ "Unknown (DNA did not amplify)",
                                             MGL_ID_Source=="non-target_species" ~ "Not chinook"),
         `(R) ORIGIN-ID` = paste0(`(R) ORIGIN`, sep=" ", `(R) RESOLVED STOCK ID`),
         `(R) ORIGIN-ID-GROUP` = case_when(`(R) RESOLVED STOCK ID`=="Not chinook" ~ `(R) RESOLVED STOCK ID`,
                                           is.na(MGL_PBT_brood_group) ~ `(R) ORIGIN-ID`,
                                           !is.na(MGL_PBT_brood_group) ~ 
                                             paste0(`(R) ORIGIN-ID`, sep=" - ", 
                                                    str_to_title(gsub(pattern="_",
                                                                      replacement=" ",
                                                                      MGL_PBT_brood_group)))))




# ===================== LOAD OTHER DATA =====================

# J. King WCVI Trawl Data ------------------------------------
swvi.trawl <- read.csv(here::here("data", "juvenile", "marine", "juvenile_chinook_AOI_20240412.csv")) %>%
  mutate(stock_group = case_when(above_stock_prob_limit_yn=="Y" & grepl("NITINAT", stock_final) ~ "SWVI",
                                 above_stock_prob_limit_yn=="Y" & grepl("JUAN", stock_final) ~ "San Juan",
                                 above_stock_prob_limit_yn=="Y" & 
                                   grepl("NECHAKO|CLEARWATER|CHILLIWACK|CHEHALIS|THOMPSON", stock_final) ~ "Fraser",
                                 above_stock_prob_limit_yn=="Y" & grepl("WENATCHEE|SOOS|ABERNATHY|NOOKSACK", stock_final) |
                                   reporting_agency=="WDFW" ~ "Washington",
                                 TRUE ~ "No GSI/below limit"))





#######################################################################################################################################################

#                                                      DAILY CATCH TOTAL/SUMMARY + FIG

# ========================= DAILY CATCH SUMMARY =========================

daily_catch_summary <- juvi.enumMeta %>% 
  filter(grepl("purse seine", gear)) %>% 
  group_by(date_start, species) %>% 
  summarize(n = sum(total_caught)) %>% 
  group_by(date_start) %>% 
  mutate(daily_total = sum(n),
         catch_propn = n/daily_total,
         label_group1 = case_when(species=="chinook" ~ paste0(species, sep=" (", n, ")" ),
                                  TRUE ~ NA),
         label_group2 = case_when(species%in%c("coho","chum") ~ paste0(species, sep=" (", n, ")" ),
                                  TRUE ~ NA),
         label_group3 = case_when(date_start!=as.Date("2023-06-28") & species%in%c("chinook", "coho", "chum") ~ paste0(species, sep=" (", n, ")"),
                                  TRUE ~ NA),
         label_group4 = case_when(date_start==as.Date("2023-06-28") & species%in%c("chinook", "coho", "chum") ~ paste0(species, sep=" (", n, ")"),
                                  TRUE ~ NA),
         species_group = case_when(species%in%c("shiner perch", "sardine", "juvenile pacific sandfish", "greenling", "anchovy") ~ "other misc non-salmon",
                                   TRUE ~ species)) %>%
  print()



# ========================= FIGURES =========================

# Dodged bar graph - not great for x axis date, but easier to interpret abundances -----------------
PRS_daily_catch_fig_dodge <- 
  ggplot(data=daily_catch_summary %>% filter(species_group%notin%c("crab", "dogfish")), 
         aes(x=as.Date(date_start), y=n, fill=str_to_title(species_group), colour=str_to_title(species_group))) +
  geom_bar(stat="identity", position=position_dodge(width=7), alpha=0.7, size=0.5) +
  ggrepel::geom_text_repel(aes(label=str_to_title(label_group1)), position=position_dodge(width=7), size=2.5,
                           vjust=-6, hjust=1,
                           segment.angle=90, segment.curvature=-0.1, segment.ncp=3, segment.size=0.5,
                           force_pull=0, force=10, na.rm=T, show.legend = F) +
  ggrepel::geom_text_repel(aes(label=str_to_title(label_group2)), position=position_dodge(width=7), size=2.5,
                           ylim=c(0,-Inf),
                           vjust=2, hjust=1,
                           segment.angle=45, segment.curvature=0.1, segment.ncp=3, segment.size=0.5,
                           force_pull=0, force=10, na.rm=T, show.legend = F) +
  # geom_text(inherit.aes = F, data=daily_catch_summary%>%group_by(date_start)%>%summarize(total=unique(daily_total)),
  #           aes(x=as.Date(date_start), y=2200, label=total),
  #           stat="identity", size=3.5) +
  scale_y_continuous(breaks=seq(0,3000,by=500), expand = expansion(mult=0.1)) +
  scale_x_date(date_labels="%b %d") +
  #xlim(as.Date(c("2023-06-25", "2023-08-26"))) +
  labs(x="", y="Daily catch", fill="", colour="") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8),
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.position = c(0.85,0.75),
        legend.background = element_rect(colour="black"))



# Stacked bar graph - not great for x axis date, but easier to interpret abundances -----------------
PRS_daily_catch_fig_stack <- 
  ggplot(data=daily_catch_summary %>% filter(species_group%notin%c("crab", "dogfish")), 
         aes(x=as.Date(date_start), y=n, fill=str_to_title(species_group), colour=str_to_title(species_group))) +
  geom_bar(stat="identity", position=position_stack(), alpha=0.7, size=0.5, width=2) +
  ggrepel::geom_text_repel(aes(label=str_to_title(label_group3), x=as.Date(date_start)+0.95), position=position_stack(vjust=0.5), size=2.5,
                           direction="y", hjust=-0.3,
                           segment.size=0.5, segment.curvature=-0.1, segment.ncp=3, segment.angle=45, min.segment.length=unit(0, 'lines'), 
                           #box.padding=1,
                           #force=0.5, force_pull=1,
                           na.rm=T, show.legend=F) +
  ggrepel::geom_text_repel(aes(label=str_to_title(label_group4), x=as.Date(date_start)-0.95), position=position_stack(vjust=0.5), size=2.5,
                           direction="y", hjust=1.5,
                           segment.size=0.5, segment.curvature=-0.1, segment.ncp=3, segment.angle=45, min.segment.length=unit(0, 'lines'), 
                           #box.padding=1,
                           #force=0.5, force_pull=1,
                           na.rm=T, show.legend=F) +
  geom_text(inherit.aes = F, data=daily_catch_summary%>%group_by(date_start)%>%summarize(total=unique(daily_total)),
            aes(x=as.Date(date_start), y=total+50, label=total),
            stat="identity", size=2.5, colour="gray50") +
  scale_y_continuous(breaks=seq(0,3000,by=500)) +
  scale_x_date(date_labels="%b %d", date_breaks="5 day") +
  xlim(as.Date(c("2023-06-20", "2023-08-31"))) +
  labs(x="", y="Daily catch", fill="", colour="") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8),
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.position = c(0.85,0.75),
        legend.background = element_rect(colour="black"))




# avg_per_set <- juvi.enumMeta %>% 
#   filter(grepl("purse seine", gear)) %>% 
#   group_by(date_start, site_name, species) %>% 
#   summarize(n = sum(total_caught)) %>% 
#   group_by(date_start, site_name) %>% 
#   mutate(daily_set_total = sum(n),
#          daily_set_propn = n/daily_set_total) %>%
#   group_by(date_start, species) %>% 
#   mutate(spp_avg_propn = mean(daily_set_propn, na.rm=T),
#          spp_sd_propn = sd(daily_set_propn, na.rm=T))
# label_group1 = case_when(species%in%c("coho", "chum") ~ species,
#                          TRUE ~ "")) %>%
#   print()


################################################################################################################################################

#                                                                CUML PROB/DEN + FIG

cuml_prob_sum <- juvi.enumMeta %>% 
  filter(species=="chinook", grepl("purse seine", gear)) %>% 
  group_by(date_start, species) %>% 
  summarize(n=sum(total_caught)) %>%
  ungroup() %>%
  mutate(cuml_sum = cumsum(as.numeric(n)),
         cuml_prob = cuml_sum/sum(n)) %>%
  print()


cuml_prob_fig <- 
  ggplot(data=cuml_prob_sum, aes(x=as.Date(date_start), y=cuml_prob)) +
  geom_line(size=0.8) +
  geom_point(size=3, shape=21, fill="gray80", stroke=1) +
  geom_hline(yintercept=0.9, colour="red", linetype="dashed", size=1) +
  scale_x_date(date_breaks = "5 day", date_labels="%b %d") +
  labs(x="", y="Cumulative probability") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8))


################################################################################################################################################


















