#this program calculates the HLB components for taxes and healthcare 
#for every household in the synthetic population
options(scipen=999)
library(archive)
library(readr)
library(dplyr)
library(knitr)
library(usincometaxes)

#read in hlb6
syn <- read_csv(archive_read("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/data_tables/fairfax_va_pums_synpop_2021.csv.zip"))     
syn <- syn[,-1]
syn <- syn[with(syn, order(puma, tract, hh_comb, serialno)), ]
dim(syn);  str(syn)
syn$hh_comb <- as.factor(syn$hh_comb); View(data.frame(table(syn$hh_comb, useNA="always")))
#insert the leading 0 in the hh_comb with only 5-digits
  levels(syn$hh_comb)[levels(syn$hh_comb)=="10000"] <- "010000"
  levels(syn$hh_comb)[levels(syn$hh_comb)=="11000"] <- "011000"
  levels(syn$hh_comb)[levels(syn$hh_comb)=="20000"] <- "020000"
  View(data.frame(table(syn$hh_comb, useNA="always"))) 
syn$serialno <- as.factor(syn$serialno); View(data.frame(table(syn$serialno, useNA="always"))) 


#calculate the healthcare component threshold for the HLB based on NASEM (2023)
#assume healthcare insurance is through an employer
#employer pays 76% of premium (National Compensation Survey (2022) northeast mid-Atlantic) 
#second-lowest-cost Silver Plan from ACA Marketplace for VA market rating area 10
#assume all adults are 40
 adult <- 450.70
#infant, toddler, preschool, schoolers 0-11 
 child <- 269.79
#teenager 12-18
  teen <- 321.98
#MOOP_individual
   ind <- 4000.00 
#MOOP_family
family <- 8000.00 

syn$hh_size <- (syn$no_adult + syn$no_infant + syn$no_toddler +
                syn$no_schooler + syn$no_schooler + syn$no_teenager) 
    premium <- (syn$no_adult*adult + 
                (syn$no_infant + syn$no_toddler + syn$no_schooler)*child +
                (syn$no_schooler + syn$no_teenager)*teen) 
       MOOP <- ifelse(syn$hh_size==1, ind, family)

#total monthly healthcare cost
syn$healthcare_cost_month <- premium*0.24 + MOOP/12
summary(syn$healthcare_cost_month, include.na=TRUE)

temp <- syn[,c(2,1,5,3,16)]; dim(temp); names(temp)
temp <- temp[with(temp, order(puma, tract, hh_comb, serialno)), ]; str(temp)

#calculate the tax component threshold for the HLB using TAXSIM
#https://taxsim.nber.org/taxsim35/  
#hlb6 <- read_csv(archive_read("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/derived_variables/fairfax_va_hlb6_2021.csv.zip"))     
hlb6 <- read.xlsx("~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/fairfax_va_hlb6_2021.xlsx", 
                  colNames=TRUE); str(hlb6)
hlb6 <- hlb6[,-1]
dim(hlb6);  str(hlb6)
 hlb6$hh_comb <- as.factor(hlb6$hh_comb)
#insert the leading 0 in the hh_comb with only 5-digits
  levels(hlb6$hh_comb)[levels(hlb6$hh_comb)=="10000"] <- "010000"
  levels(hlb6$hh_comb)[levels(hlb6$hh_comb)=="11000"] <- "011000"
  levels(hlb6$hh_comb)[levels(hlb6$hh_comb)=="20000"] <- "020000"
View(data.frame(table(hlb6$hh_comb, useNA="always"))) 
hlb6$serialno <- as.factor(hlb6$serialno); View(data.frame(table(hlb6$serialno, useNA="always"))) 
         hlb6 <- hlb6[with(hlb6, order(puma, tract, hh_comb, serialno)), ]

#ck<-data.frame(puma1=hlb6$puma,puma2=temp$puma,tract1=hlb6$tract,tract2=temp$tract,
#               hh_comb1=hlb6$serialno,hh_comb2=temp$serialno,
#               serialno1=hlb6$serialno,serialno2=temp$serialno)
#identical(ck$puma1, ck$puma2)
#identical(ck$tract1, ck$tract2)
#identical(ck$serialno1, ck$serialno2)

hlb8 <- data.frame(hlb6, healthcare_cost_month=temp$healthcare_cost_month)
str(hlb8); names(hlb8); dim(hlb8)

#create the HLB budget for each household in the synthetic population
hlb8$hlb_no_tax_yr <- (hlb8$healthcare_cost_month+hlb8$broadband_cost_month+
                       hlb8$food_cost_month+hlb8$childcare_cost_month+
                       hlb8$housing_cost_month+hlb8$transp_cost_month+
                       hlb8$other_costs_month)*12

#need to create a marriage status vector
#assume if there are two adults in the household they are married single otherwise
mstat <- ifelse(hlb8$no_adult==2, "married, jointly", "single")

#create a column for the age of the spouse and three columns for the age of the youngest three children
#assume the head of the household is 40
  AGE <- hlb8[, c(7:11)]
      AGE$no_teenager[AGE$no_teenager>=1] <- 13 
      AGE$no_schooler[AGE$no_schooler>=1] <- 7 
      AGE$no_preschooler[AGE$no_preschooler>=1] <- 4 
      AGE$no_toddler[AGE$no_toddler>=1] <- 2 
      AGE$no_infant[AGE$no_infant>=1] <- 1 
      
 age1 <- rep(NA, dim(AGE)[1])
 age2 <- rep(NA, dim(AGE)[1])
 age3 <- rep(NA, dim(AGE)[1])
 for(i in 1:dim(AGE)[1]){
      temp<-sort(unlist(AGE[i,]))
   age1[i]<-temp[1]
   age2[i]<-temp[2]
   age3[i]<-temp[3]
 }   
 
age1[is.na(age1)] <-0; summary(age1, includeNA=TRUE) 
age1[is.na(age2)] <-0; summary(age2, includeNA=TRUE)  
age1[is.na(age3)] <-0; summary(age3, includeNA=TRUE) 

Fairfax_TAXES<-data.frame(taxsimid=hlb8$serial, 
                          state=rep("VA", dim(hlb8)[1]),
                          year=rep(2022, dim(hlb8)[1]), 
                          mstat=mstat,
                          pwages=hlb8$hlb_no_tax_yr,
                          page=rep(40, dim(hlb8)[1]), 
                          age1=age1,
                          age2=age2,
                          age3=age3)
View(Fairfax_TAXES)

                     Fairfax_taxes <- taxsim_calculate_taxes(Fairfax_TAXES)
         Fairfax_taxes$total_taxes <- Fairfax_taxes$fiitax+Fairfax_taxes$siitax+Fairfax_taxes$tfica
        colnames(Fairfax_taxes)[1] <- "serial" 
View(Fairfax_taxes); dim(Fairfax_taxes)

               Fairfax_combined1 <- merge(hlb8, Fairfax_taxes[,c(1,9)], by="serial")
View(Fairfax_combined1); dim(Fairfax_combined1)
     Fairfax_combined1$hlb_year <- round((Fairfax_combined1$hlb_no_tax_yr +
                                          Fairfax_combined1$total_taxes), 2)
      Fairfax_combined1$eco_vul <- ifelse(Fairfax_combined1$hh_income<Fairfax_combined1$hlb_year, 1, 0)
dim(Fairfax_combined1); str(Fairfax_combined1)

#write.csv(Fairfax_combined1, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/fairfax_va_hlb_2021.csv")



#code used to test the TAXSIM software
family_income <- data.frame(
  taxsimid = c(1, 2, 3),
     state = c("DC", "MD", "VA"),
      year = c(2022, 2022, 2022),
     mstat = c("single", "single", "single"),
    pwages = c(10000, 10000, 10000), # primary wages
      page = c(0, 0, 0)) # primary age

family_taxes <- taxsim_calculate_taxes(COL1)

 
