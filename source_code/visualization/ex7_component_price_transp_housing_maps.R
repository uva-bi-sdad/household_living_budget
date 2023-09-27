library(ggrepel)
library(BAMMtools)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(scales)
library(Rttf2pt1)
options(tigris_use_cache=TRUE)

#V2021<-load_variables(2021, "acs5", cache=TRUE)
#View(V2021)

#Set the color palette for 7 categories
SEQ <- c("#F9F1CB","#2686A0")
SEQturquoise <- colorRampPalette(SEQ)(42)
show_col(SEQturquoise)
show_col(SEQturquoise[c(1,7,14,21,28,35,42)])
SEQ_turquoise<-SEQturquoise[c(1,7,14,21,28,35,42)]; show_col(SEQ_turquoise)

#################################################################################################
#S1702POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES 2021: ACS 5-Year Estimates Subject Tables
#################################################################################################

PRvar<-c(
  #Poverty Ratio
  "C17002_001E",
  "C17002_002E",
  "C17002_003E",
  "C17002_004E",
  "C17002_005E",
  "C17002_006E",
  "C17002_007E")

PR <- get_acs(geography="tract", 
              county=c("Fairfax County"),
              state="VA", 
              variables=PRvar, #Poverty Ratio
              year=2021, 
              survey="acs5", 
              output="wide", 
              geometry=TRUE,
              cache_table=TRUE) 
View(PR); dim(PR)
#create a census tract variable from GEOID
PR$tract <- as.factor(substring(PR$GEOID, 6))

FINAL <- read_csv(archive_read("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/derived_variables/fairfax_va_hlb_2021.csv.zip"))     
FINAL <- FINAL[,-1]; dim(FINAL); str(FINAL) 
dim(FINAL) #408648x27
#change the variables hh_comb and serialno to factors
#insert the leading 0 in the hh_comb with only 5-digits
  FINAL$hh_comb <- as.factor(FINAL$hh_comb)
  levels(FINAL$hh_comb)[levels(FINAL$hh_comb)=="10000"] <- "010000"
  levels(FINAL$hh_comb)[levels(FINAL$hh_comb)=="11000"] <- "011000"
  levels(FINAL$hh_comb)[levels(FINAL$hh_comb)=="20000"] <- "020000"
View(data.frame(table(FINAL$hh_comb, useNA="always"))) 
FINAL$serialno <- as.factor(FINAL$serialno); View(data.frame(table(FINAL$serialno, useNA="always"))) 
str(FINAL)

#look at component variation across the census tracts
#household composition: 2 adults + schooler + toddler
#housing cost/month
FC201010 <- FINAL[FINAL$hh_comb=="201010",]

breaks_housing <- round(getJenksBreaks(FC201010$housing_cost_month, 8, subset = NULL), 0)  
breaks_housing

        FC201010$GEOID <-   paste0("51059",FC201010$tract)
FC201010$group_housing <- rep(NA, length=length(FC201010$housing_cost_month))
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[1] & FC201010$housing_cost_month>=breaks_housing[2])]="[2060, 2130)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[2] & FC201010$housing_cost_month>=breaks_housing[3])]="[2130, 2415)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[3] & FC201010$housing_cost_month>=breaks_housing[4])]="[2415, 2551)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[4] & FC201010$housing_cost_month>=breaks_housing[5])]="[2551, 2698)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[5] & FC201010$housing_cost_month>=breaks_housing[6])]="[2698, 2875)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[6] & FC201010$housing_cost_month>=breaks_housing[7])]="[2875, 3180)"
FC201010$group_housing[which(FC201010$housing_cost_month>=breaks_housing[7])]="[3180, 3450]"
group_housing <- factor(FC201010$group_housing, 
                        levels=c("[2060, 2130)","[2130, 2415)","[2415, 2551)","[2551, 2698)",
                                 "[2698, 2875)","[2875, 3180)","[3180, 3450]"))
FC201010$group_housing <- factor(FC201010$group_housing, 
                                 levels=c("[2060, 2130)","[2130, 2415)","[2415, 2551)","[2551, 2698)",
                                          "[2698, 2875)","[2875, 3180)","[3180, 3450]"))
evpalette_turquoise_housing <- c("[2060, 2130)"="#F9F1CB", "[2130, 2415)"="#DAE1C4",
                                 "[2415, 2551)"="#B6CFBD", "[2551, 2698)"="#92BCB6",
                                 "[2698, 2875)"="#6EAAAE", "[2875, 3180)"="#4A98A7",
                                 "[3180, 3450]"="#2686A0")

FC201010_map_housing <- merge(PR, FC201010, by="GEOID", all.x=TRUE, all.y=TRUE)
dim(FC201010_map_housing); names(FC201010_map_housing)

#map housing costs by census tracts for two adults + schooler + infant
ex7_fairfax_housing_201010_map <- ggplot() + 
  geom_sf(data=FC201010_map_housing, aes(fill=group_housing), color="grey", lwd=0.65) + 
  scale_fill_manual(values=evpalette_turquoise_housing, 
                    na.value="grey",
                    name="Monthly Housing Costs: 2 Adults+Schooler+Toddler",
                    drop=FALSE, 
                    labels=c("2060      2130"," 2415"," 2551"," 2698"," 2875"," 3180"," 3450"),
                    na.translate=FALSE,
                    guide=guide_legend(
                      direction="horizontal",
                      keyheight=unit(4, units="mm"),
                      keywidth=unit(15/length(labels), units="mm"),
                      title.position="top",
                      title.hjust=0.5,
                      label.hjust=1,
                      nrow=1,
                      byrow=TRUE,
                      reverse=FALSE,
                      label.position="bottom"
                    )) +
  #  labs(title="Yearly Household Living Budget for a Single Parent with and Infant", 
  #       subtitle="Census Tracts in Fairfax County, Virginia",
  #       caption="Synthetic Population using IPUMS and\n ACS 2021 5-YR Tables B11016 and S1906") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=1.0),
        plot.subtitle=element_text(size=12, hjust=0.5, vjust=0.5),
        plot.caption=element_text(size=9, hjust=0.5),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  theme_SDAD()

ggsave("ex7_fairfax_housing_201010_map.pdf", width=10, height=11)
ex7_fairfax_housing_201010_map


#household composition: 2 adults + schooler + toddler
#transportation costs
breaks_transp <- round(getJenksBreaks(FC201010$transp_cost_month, 8, subset = NULL), 0)  
breaks_transp

FC201010$group_transp <- rep(NA, length=length(FC201010$transp_cost_month))
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[1] & FC201010$transp_cost_month>=breaks_transp[2])]="[586, 751)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[2] & FC201010$transp_cost_month>=breaks_transp[3])]="[751, 856)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[3] & FC201010$transp_cost_month>=breaks_transp[4])]="[856, 931)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[4] & FC201010$transp_cost_month>=breaks_transp[5])]="[931, 997)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[5] & FC201010$transp_cost_month>=breaks_transp[6])]="[997, 1059)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[6] & FC201010$transp_cost_month>=breaks_transp[7])]="[1059, 1129)"
FC201010$group_transp[which(FC201010$transp_cost_month>=breaks_transp[7])]="[1129, 1260]"
group_housing <- factor(FC201010$group_transp, 
                        levels=c("[586, 751)","[751, 856)","[856, 931)","[931, 997)",
                                 "[997, 1059)","[1059, 1129)","[1129, 1260]"))
FC201010$group_transp <- factor(FC201010$group_transp, 
                                levels=c("[586, 751)","[751, 856)","[856, 931)","[931, 997)",
                                         "[997, 1059)","[1059, 1129)","[1129, 1260]"))
evpalette_turquoise_transp <- c("[[586, 751)"="#F9F1CB", "[751, 856)"="#DAE1C4",
                                "[856, 931)"="#B6CFBD", "[931, 997)"="#92BCB6",
                                "[997, 1059)"="#6EAAAE", "[1059, 1129)"="#4A98A7",
                                "[1129, 1260]"="#2686A0")
FC201010_map_transp <- merge(PR, FC201010, by="GEOID", all.x=TRUE, all.y=TRUE)
dim(FC201010_map_transp); names(FC201010_map_transp)

#map yearly HLB by census tracts for two adults
ex7_fairfax_transp_201010_map <- ggplot() + 
  geom_sf(data=FC201010_map_transp, aes(fill=group_transp), color="grey", lwd=0.65) + 
  scale_fill_manual(values=evpalette_turquoise_transp, 
                    na.value="grey",
                    name="Monthly Transportation Costs: 2 Adults+Schooler+Toddler",
                    drop=FALSE, 
                    labels=c("586      751"," 856"," 931"," 997"," 1059"," 1129"," 1260"),
                    na.translate=FALSE,
                    guide=guide_legend(
                      direction="horizontal",
                      keyheight=unit(4, units="mm"),
                      keywidth=unit(15/length(labels), units="mm"),
                      title.position="top",
                      title.hjust=0.5,
                      label.hjust=1,
                      nrow=1,
                      byrow=TRUE,
                      reverse=FALSE,
                      label.position="bottom"
                    )) +
  #  labs(title="Yearly Household Living Budget for a Single Parent with and Infant", 
  #       subtitle="Census Tracts in Fairfax County, Virginia",
  #       caption="Synthetic Population using IPUMS and\n ACS 2021 5-YR Tables B11016 and S1906") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=1.0),
        plot.subtitle=element_text(size=12, hjust=0.5, vjust=0.5),
        plot.caption=element_text(size=9, hjust=0.5),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  theme_SDAD()

ggsave("ex7_fairfax_transp_201010_map.pdf", width=10, height=11)
ex7_fairfax_transp_201010_map





