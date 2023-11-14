#boxplots of economically vulnerable households with 1 adult and 3 children
#income deficit by household combination identified by puma

library(ggplot2)
library(stringi)
options(scipen=999)

#read in synthetic data
#read in synthetic data with HLB estimates
fx <- read_csv(archive_read("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/derived_variables/fairfax_va_hlb_2021.csv.zip"))     
#change hh_comb and serialno to factors and check using table
#add the leading 0 to the hh_comb with only 5-digits 
levels(fx$hh_comb)[levels(fx$hh_comb)=="10000"] <- "010000"
levels(fx$hh_comb)[levels(fx$hh_comb)=="11000"] <- "011000"
levels(fx$hh_comb)[levels(fx$hh_comb)=="20000"] <- "020000"
 fx$hh_comb <- as.factor(fx$hh_comb); View(data.frame(table(fx$hh_comb, useNA="always")))
fx$serialno <- as.factor(fx$serialno); View(data.frame(table(fx$serialno, useNA="always"))) 
         fx <- fx[, -c(1,2)]

#keep only economically vulnerable households
temp1 <- fx[fx$eco_vul==1,]
dim(temp1); View(temp1) 

#create a variable for household size
temp1$hh_size <- rep(0, dim(temp1)[1])
    digitsum1 <- function(x) sum(as.numeric(unlist(strsplit(as.character(x), split = ""))))
for(i in 1:dim(temp1)[1]){
  temp1$hh_size[i] <- digitsum1(as.character(temp1$hh_comb[i]))
}
dim(temp1); View(temp1)

#create a variable for the number of children
temp3 <- as.numeric(stri_sub(temp1$hh_comb, -5, -1))
temp1$hh_children <- rep(0, dim(temp1)[1])
digitsum1 <- function(x) sum(as.numeric(unlist(strsplit(as.character(x), split = ""))))
for(i in 1:dim(temp1)[1]){
  temp1$hh_children[i] <- digitsum1(as.character(temp3[i]))
}
dim(temp1); View(temp1) 

#create a variable for the 2021 official poverty measure (ACS 5YR 2021)
#https://aspe.hhs.gov/2021-poverty-guidelines
temp1$OPM <- ifelse(temp1$hh_size==1, 12880,
             ifelse(temp1$hh_size==2, 17420,
             ifelse(temp1$hh_size==3, 21960,     
             ifelse(temp1$hh_size==4, 26500,      
             ifelse(temp1$hh_size==5, 31040,
             ifelse(temp1$hh_size==6, 35580,     
             ifelse(temp1$hh_size==7, 40120,      
             ifelse(temp1$hh_size==8, 44660, 44660+(temp1$hh_size-8)*4540))))))))   
summary(temp1$OPM)

#create a variable for the 2021 supplemental poverty measure (ACS 5YR 2021)
#https://aspe.hhs.gov/2021-poverty-guidelines



#look at all only 4 person household combinations
#1 adult and 3 children
  temp1$hh_comb2 <- paste0(temp1$hh_size,":",temp1$hh_children)
     household_4 <- temp1[temp1$hh_comb2=="4:3",]
dim(household_4); View(household_4)    
#subtract the poverty threshold from HLB = income deficit
household_4$dif <- household_4$OPM-household_4$hlb_year; summary(household_4$dif)
summary(household_4$dif)

household_4$hh_comb <- factor(household_4$hh_comb)
   household_4$puma <- factor(household_4$puma)
       palette_puma <- c("59301"="#51C3CC", "59302"="#99F9FF",
                         "59303"="#B2FCFF", "59304"="#CCFEFF", 
                         "59305"="#E5FFFF",
                         "59306"="#FFE5CC", "59307"="#FFCA99",
                         "59308"="#FFAD65", "59309"="#FF8E32")       

ex9_fairfax_income_deficit_hh4_boxplots <-       
ggplot(household_4, aes(x=hh_comb, y=dif, fill=puma)) +
 geom_boxplot(aes(x=hh_comb, y=dif), outlier.shape=NA, 
               fill="white", colour="black") + 
  geom_jitter(aes(x=hh_comb, y=dif, fill=puma), 
              width=0.40, pch=21, colour="black", cex=1.25) +
  xlab("\nHoushold Combination: Adult/Teenager/Schooler/Preschooler/Toddler/Infant") + 
  ylab("\n2021 Official Poverty Threshold minus Yearly HLB ($)") +
  scale_fill_manual(values=palette_puma) +  
  scale_y_continuous(limits=c(-129000, -59000),
                     breaks=seq(from=-129000, to=-59000, by=10000)) + 
  theme_minimal() + 
  labs(title="        Economically Vulnerable Households in Fairfax County, VA\nPoverty Threshold  minus Yearly Household Living Budget\nHousehold Combinations of 4:3 (1 Adult/3 Children)", 
       subtitle="Filled circles identify the PUMA the household is located in.", 
       caption="") +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.8, vjust=3.0),
        plot.subtitle=element_text(size=12, hjust=0.8, vjust=-2.5),
        panel.grid.major.y=element_line(color=cbPalette[1], size=0.5), 
        axis.text.x=element_text(size=9, vjust=5.0),
        axis.text.y=element_text(size=9, hjust=1.5), 
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11, vjust=4),
        legend.position="bottom")

#ggsave("ex9_fairfax_income_deficit_hh4_boxplots.pdf", width=11, height=9)
ex9_fairfax_income_deficit_hh4_boxplots
