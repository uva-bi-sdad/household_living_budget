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
SEQ<-c("#F9F1CB","#4E5827")
SEQolive<-colorRampPalette(SEQ)(42)
show_col(SEQolive)
show_col(SEQolive[c(1,7,14,21,28,35,42)])
SEQ_OLIVE<-SEQolive[c(1,7,14,21,28,35,42)]; show_col(SEQ_OLIVE)

#Set the color palette for 7 categories
SEQ<-c("#F9F1CB","#D55E00")
SEQorange<-colorRampPalette(SEQ)(42)
show_col(SEQorange)
show_col(SEQorange[c(1,7,14,21,28,35,42)])
SEQ_orange<-SEQorange[c(1,7,14,21,28,35,42)]; show_col(SEQ_orange)

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

#data wrangling
#create ratio of the hh income versus hlb 
#categorize the ratios to compare to the poverty ratios
  FINAL$HLB_ratio <- FINAL$hh_income/(FINAL$hlb_year)
FINAL$HLB_ratio_1 <- ifelse(FINAL$HLB_ratio < 0.50, 1, 0) #economically vulnerable
FINAL$HLB_ratio_2 <- ifelse((FINAL$HLB_ratio >= 0.50 & FINAL$HLB_ratio < 0.99), 1, 0)
FINAL$HLB_ratio_3 <- ifelse((FINAL$HLB_ratio >= 1.00 & FINAL$HLB_ratio < 1.24), 1, 0)
FINAL$HLB_ratio_4 <- ifelse((FINAL$HLB_ratio >= 1.24 & FINAL$HLB_ratio < 1.49), 1, 0)
FINAL$HLB_ratio_5 <- ifelse((FINAL$HLB_ratio >= 1.49 & FINAL$HLB_ratio < 1.84), 1, 0)
FINAL$HLB_ratio_6 <- ifelse((FINAL$HLB_ratio >= 1.84 & FINAL$HLB_ratio < 1.99), 1, 0)
FINAL$HLB_ratio_7 <- ifelse(FINAL$HLB_ratio >= 2.00, 1, 0) #hh income is 2x hlb
      FINAL$tally <- as.numeric(rep(1, dim(FINAL)[1]))
            FINAL <- as.data.frame(FINAL)
       FINAL$puma <- factor(FINAL$puma)
      FINAL$tract <- factor(FINAL$tract)

    tract_ev_prev <- aggregate(cbind(tally,HLB_ratio_1,HLB_ratio_2,HLB_ratio_3,
                                     HLB_ratio_4,HLB_ratio_5,HLB_ratio_6,HLB_ratio_7)
                                     ~puma+tract, data=FINAL, FUN=sum)
  tract_ev_prev$prev1 <- round(tract_ev_prev$HLB_ratio_1/tract_ev_prev$tally, 3)
  tract_ev_prev$prev2 <- round(tract_ev_prev$HLB_ratio_2/tract_ev_prev$tally, 3)
  tract_ev_prev$prev3 <- round(tract_ev_prev$HLB_ratio_3/tract_ev_prev$tally, 3)
  tract_ev_prev$prev4 <- round(tract_ev_prev$HLB_ratio_4/tract_ev_prev$tally, 3)
  tract_ev_prev$prev5 <- round(tract_ev_prev$HLB_ratio_5/tract_ev_prev$tally, 3)
  tract_ev_prev$prev6 <- round(tract_ev_prev$HLB_ratio_6/tract_ev_prev$tally, 3)
  tract_ev_prev$prev7 <- round(tract_ev_prev$HLB_ratio_7/tract_ev_prev$tally, 3)
View(tract_ev_prev); dim(tract_ev_prev)

#Use a Jenks Breaks
breaks_ev1 <- round(getJenksBreaks(tract_ev_prev$prev1, 8, subset = NULL), 3)  
breaks_ev1

tract_ev_prev$group_ev1 <- rep(NA, length=length(tract_ev_prev$prev1))
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[1] & tract_ev_prev$prev1<breaks_ev1[2])]="[0.000, 0.058)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[2] & tract_ev_prev$prev1<breaks_ev1[3])]="[0.058, 0.096)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[3] & tract_ev_prev$prev1<breaks_ev1[4])]="[0.096, 0.136)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[4] & tract_ev_prev$prev1<breaks_ev1[5])]="[0.136, 0.196)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[3] & tract_ev_prev$prev1<breaks_ev1[4])]="[0.196, 0.268)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[4] & tract_ev_prev$prev1<breaks_ev1[5])]="[0.268, 0.378)"
tract_ev_prev$group_ev1[which(tract_ev_prev$prev1>=breaks_ev1[5])]="[0.378, 0.488]"
group_ev1<-factor(tract_ev_prev$group_ev1, 
                  levels=c("[0.000, 0.058)","[0.058, 0.096)","[0.096, 0.136)","[0.136, 0.196)","[0.196, 0.268)",
                           "[0.268, 0.378)","[0.378, 0.488]"))

tract_ev_prev$group_ev1 <- factor(tract_ev_prev$group_ev1, 
                                  levels=c("[0.000, 0.058)","[0.058, 0.096)","[0.096, 0.136)",
                                           "[0.136, 0.196)","[0.196, 0.268)","[0.268, 0.378)",
                                           "[0.378, 0.488]"))
View(tract_ev_prev); dim(tract_ev_prev)

#bring in data on the percent below the poverty level
#S1701POVERTY STATUS IN THE PAST 12 MONTHS ACS 1-YR
#S1701 <- read.csv("~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/S1701_2021.csv")
#names(S1701); dim(S1701); View(S1701)
#  colnames(S1701)[1] <- "GEOID"
#         S1701$GEOID <- substr(S1701$GEOID, 10, 20) 
#S1701$S1701_C03_001E <- as.numeric(S1701$S1701_C03_001E) 
#S1701$S1701_C03_001M <- as.numeric(S1701$S1701_C03_001M) 
#           S1701$ind <- round((S1701$S1701_C03_001E/100), 3)
#                 ind <- S1701[, c(1,5)]
                 
 evpalette_olive <- c("[0.000, 0.058)"="#F9F1CB", "[0.058, 0.096)"="#DFDAB3",
                      "[0.096, 0.136)"="#C2C097", "[0.136, 0.196)"="#A5A67B",
                      "[0.196, 0.268)"="#888C5E", "[0.214, 0.295)"="#6B7242",
                      "[0.378, 0.488]"="#4E5827")
evpalette_orange <- c("[0.000, 0.058)"="#F9F1CB", "[0.058, 0.096)"="#F3DBAD",
                      "[0.096, 0.136)"="#EDC28A", "[0.136, 0.196)"="#E7A967",
                      "[0.196, 0.268)"="#E19045", "[0.268, 0.378)"="#DB7722",
                      "[0.378, 0.488]"="#D55E00")

              ev_plot <- merge(PR, tract_ev_prev, by="tract", all.x=TRUE, all.y=TRUE)
              ev_plot <- ev_plot[-c(272:274),] #remove none hh tracts
              #ev_plot <- merge(ev_plot, ind, by="tract", all.x=TRUE, all.y=TRUE)
              dim(ev_plot); View(ev_plot); names(ev_plot)
              
     ev_plot$pov_prev <- round((ev_plot$C17002_002E+ev_plot$C17002_003E)/ev_plot$C17002_001E, 2)
ev_plot$pov_prev_plus <- round((ev_plot$C17002_002E+ev_plot$C17002_003E+ev_plot$C17002_004E+
                                ev_plot$C17002_005E+ev_plot$C17002_006E+ev_plot$C17002_007E)/ev_plot$C17002_001E, 2)
     ev_plot$HLB_prev <- round((ev_plot$HLB_ratio_1+ev_plot$HLB_ratio_2)/ev_plot$tally, 2)
ev_plot$HLB_prev_plus <- round((ev_plot$HLB_ratio_1+ev_plot$HLB_ratio_2+ev_plot$HLB_ratio_3+
                                ev_plot$HLB_ratio_4+ev_plot$HLB_ratio_5+ev_plot$HLB_ratio_6)/ev_plot$tally, 2)

#estimate the correlation between the poverty versus hlb ratios
cor(ev_plot$pov_prev, ev_plot$HLB_prev, use="complete.obs")  
cor(ev_plot$pov_prev_plus, ev_plot$HLB_prev_plus, use="complete.obs")  
cor(ev_plot$pov_prev_plus, ev_plot$HLB_prev, use="complete.obs")  

cor((ev_plot$C17002_002E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_1/ev_plot$tally), use="complete.obs")  
cor((ev_plot$C17002_003E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_2/ev_plot$tally), use="complete.obs")  
cor((ev_plot$C17002_004E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_3/ev_plot$tally), use="complete.obs")  
cor((ev_plot$C17002_005E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_4/ev_plot$tally), use="complete.obs")  
cor((ev_plot$C17002_006E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_5/ev_plot$tally), use="complete.obs")  
cor((ev_plot$C17002_007E/ev_plot$C17002_001E), (ev_plot$HLB_ratio_6/ev_plot$tally), use="complete.obs")  

#map of economically vulnerable households by census tracts
ex6_fairfax_ev_prev_map <- ggplot() + 
  geom_sf(data=ev_plot, aes(fill=group_ev1), color="grey") + 
  scale_fill_manual(values=evpalette_orange, 
                    na.value="grey",
                    name="Prevalence of Household Incomes < Half the HLB",
                    drop=FALSE, 
                    labels=c("0         0.046","0.078","0.111","0.153","0.214","0.295","0.376"),
                    na.translate=FALSE,
                    guide=guide_legend(
                      direction="horizontal",
                      keyheight=unit(4, units="mm"),
                      keywidth=unit(20/length(labels), units="mm"),
                      title.position="top",
                      title.hjust=0.5,
                      label.hjust=1,
                      nrow=1,
                      byrow=TRUE,
                      reverse=FALSE,
                      label.position="bottom"
                    )) +
  labs(title="Prevalence of Economcially Vulnerable Households", 
       subtitle="Census Tracts in Fairfax County, Virginia",
       caption="Synthetic Population using IPUMS and\n ACS 2021 5-YR Tables B11016 and S1906") +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=1.0),
        plot.subtitle=element_text(size=12, hjust=0.5, vjust=0.5),
        plot.caption=element_text(size=9, hjust=0.5),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.position="bottom", 
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) + 
  theme_SDAD()

#ggsave("ex6_fairfax_ev_prev_map.pdf", width=10, height=11)
ex6_fairfax_ev_prev_map

#scatter plot HLB prevalence versus Census poverty
#C17002 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES
prev1.lm <- lm(ev_plot$pov_prev ~ ev_plot$HLB_prev, ev_plot)
prev2.lm <- lm(ev_plot$pov_prev_plus ~ ev_plot$HLB_prev, ev_plot)

ex5_fairfax_hlb_vs_poverty_plot <-
ggplot(data=ev_plot, aes(x=HLB_prev, y=pov_prev)) +
  geom_segment(aes(x=0, xend=0.7, y=0, yend=0.7), color="black", lty=2) +
  geom_segment(aes(x=0, xend=0.7, y=unname(prev1.lm$coefficients[1]), yend=unname(prev1.lm$coefficients[2])*0.7), color=cbPalette[2]) +
  geom_segment(aes(x=0, xend=0.7, y=unname(prev2.lm$coefficients[1]), yend=unname(prev2.lm$coefficients[2])*0.7), color=cbPalette[3]) +
  geom_point(aes(x=HLB_prev, y=pov_prev), color=cbPalette[2], pch=19 , cex=1) +
  geom_point(aes(x=HLB_prev, y=pov_prev_plus), color=cbPalette[3], pch=19 , cex=1) +
  scale_y_continuous(breaks=seq(0, 0.70, by=0.10),
                     labels=seq(0, 0.70, by=0.10),
                     limits=c(-0.07, 0.70)) +
  scale_x_continuous(breaks=seq(0, 0.70, by=0.10),
                     labels=seq(0, 0.70, by=0.10),
                     limits=c(0, 0.70)) +
  ylab("Ratio of Family Income to Poverty Threshold") +
  xlab("Ratio of Household Income to Household Living Budget") +
  labs(title="Prevalence of Economically Vulnerable Households vs Families in Poverty\nwithin Fairfax County, Virginia Census Tracts", 
       subtitle="Orange = Regression line for Economically Vulnerable Ratio (<0.99] vs Poverty Ratio (<0.99]\nBlue = Regression line for Economically Vulnerable Ratio (<0.99] vs Poverty Ratio (<1.99]",
       caption="\nACS 1-YR 2021 Table C17002 Poverty Status in the Past 12 Months of Families") +
  theme_SDAD() +
  theme(plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=-4),
        plot.subtitle=element_text(size=10, hjust=0.5, vjust=-5),
        plot.caption=element_text(size=10, hjust=0),
        axis.title=element_text(size=10)) +
  annotate("text", x=0.3, y=0.6, adj=0, size=3.5, label="Reference Line: Intercept = 0, Slope = 1") +
  annotate("text", x=0.5, y=0.3, adj=0, size=3.5, label="Correlation = 0.87") + 
  annotate("text", x=0.5, y=0.1, adj=0, size=3.5, label="Correlation = 0.70")  
  
#ggsave("ex5_fairfax_hlb_vs_poverty_plot.pdf", width=12, height=10)
ex5_fairfax_hlb_vs_poverty_plot

