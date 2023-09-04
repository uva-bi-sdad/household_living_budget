#this code creates a puma data set for Fairfax, VA from a dmv puma data set
#variables that are added include: 
#income categories to match ACS Table S1901 (nine categories)
#household size (5 categories: 1, 2, 3, 4, >=5)
#index to match the 45 income categories by household size combinations
#HLB tax component
#HLB components
 
library(dplyr)
library(tidyverse)
library(archive)

puma <- read_csv(archive_read("https://github.com/uva-bi-sdad/household_living_budget/raw/main/data/demographic/acs_puma/dmv_acs_puma_household_2020.csv.zip"))     
#dim(puma); names(puma); View(puma)

#keep the pumas in Fairfax only
fx_puma <- c(59301, 59302, 59303, 59304, 59305, 59306, 59307, 59308, 59309)
     fx <- subset(puma, puma$PUMA %in% fx_puma); dim(fx) 

#create a new variable for household size 
       fx$hh_size <- fx$adult + fx$infant + fx$preschooler + fx$teenager + fx$toddler + fx$schooler
summary(fx$hh_size)
#create a variable for the number of children
   fx$no_children <- fx$infant + fx$preschooler + fx$teenager + fx$toddler + fx$schooler
summary(fx$no_children)
#create a variable that recodes household size into 5 categories
fx$hh_size_recode <- ifelse(fx$hh_size >= 5, 5, fx$hh_size)
summary(fx$hh_size_recode)

#create a variable for income category that agrees with 
#ACS Table S1901 | INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS) 2021 5-YR Estimates 
#first adjust income for inflation from 2020 (puma) to 2021 acs 5-year estimates
fx$hh_income_2021 <- round(fx$hhincome*(399.0/381.2), 0)
summary(fx$hh_income_2021)
#create 9 income category groups
 fx$hh_income_cat <- cut(fx$hh_income_2021,
                         breaks=c(0, 14999, 24999, 34999, 49999, 74999, 99999, 149999, 199999, 1700000),
                         labels=c("Less than $15,000",
                                  "$15,000 to $24,999",              
                                  "$25,000 to $34,999",
                                  "$35,000 to $49,999",
                                  "$50,000 to $74,999",
                                  "$75,000 to $99,999",
                                  "$100,000 to $149,999",
                                  "$150,000 to $199,999",
                                  "$200,000 or more"))
table(fx$hh_income_cat)

#create an index variable that identifies the cells in the 2-way table income category (9) x household size (5) table
#the index is used when creating the synthetic population
fx$index <- ifelse(fx$hh_income_cat=="Less than $15,000" & fx$hh_size_recode==1, 1,
            ifelse(fx$hh_income_cat=="Less than $15,000" & fx$hh_size_recode==2, 2,       
            ifelse(fx$hh_income_cat=="Less than $15,000" & fx$hh_size_recode==3, 3,
            ifelse(fx$hh_income_cat=="Less than $15,000" & fx$hh_size_recode==4, 4,       
            ifelse(fx$hh_income_cat=="Less than $15,000" & fx$hh_size_recode==5, 5,
            ifelse(fx$hh_income_cat=="$15,000 to $24,999" & fx$hh_size_recode==1, 6,       
            ifelse(fx$hh_income_cat=="$15,000 to $24,999" & fx$hh_size_recode==2, 7,
            ifelse(fx$hh_income_cat=="$15,000 to $24,999" & fx$hh_size_recode==3, 8,          
            ifelse(fx$hh_income_cat=="$15,000 to $24,999" & fx$hh_size_recode==4, 9,
            ifelse(fx$hh_income_cat=="$15,000 to $24,999" & fx$hh_size_recode==5, 10,                 
            ifelse(fx$hh_income_cat=="$25,000 to $34,999" & fx$hh_size_recode==1, 11,
            ifelse(fx$hh_income_cat=="$25,000 to $34,999" & fx$hh_size_recode==2, 12,       
            ifelse(fx$hh_income_cat=="$25,000 to $34,999" & fx$hh_size_recode==3, 13,
            ifelse(fx$hh_income_cat=="$25,000 to $34,999" & fx$hh_size_recode==4, 14,       
            ifelse(fx$hh_income_cat=="$25,000 to $34,999" & fx$hh_size_recode==5, 15,
            ifelse(fx$hh_income_cat=="$35,000 to $49,999" & fx$hh_size_recode==1, 16,       
            ifelse(fx$hh_income_cat=="$35,000 to $49,999" & fx$hh_size_recode==2, 17,
            ifelse(fx$hh_income_cat=="$35,000 to $49,999" & fx$hh_size_recode==3, 18,          
            ifelse(fx$hh_income_cat=="$35,000 to $49,999" & fx$hh_size_recode==4, 19,
            ifelse(fx$hh_income_cat=="$35,000 to $49,999" & fx$hh_size_recode==5, 20,                
            ifelse(fx$hh_income_cat=="$50,000 to $74,999" & fx$hh_size_recode==1, 21,
            ifelse(fx$hh_income_cat=="$50,000 to $74,999" & fx$hh_size_recode==2, 22,       
            ifelse(fx$hh_income_cat=="$50,000 to $74,999" & fx$hh_size_recode==3, 23,
            ifelse(fx$hh_income_cat=="$50,000 to $74,999" & fx$hh_size_recode==4, 24,       
            ifelse(fx$hh_income_cat=="$50,000 to $74,999" & fx$hh_size_recode==5, 25,
            ifelse(fx$hh_income_cat=="$75,000 to $99,999" & fx$hh_size_recode==1, 26,       
            ifelse(fx$hh_income_cat=="$75,000 to $99,999" & fx$hh_size_recode==2, 27,
            ifelse(fx$hh_income_cat=="$75,000 to $99,999" & fx$hh_size_recode==3, 28,          
            ifelse(fx$hh_income_cat=="$75,000 to $99,999" & fx$hh_size_recode==4, 29,
            ifelse(fx$hh_income_cat=="$75,000 to $99,999" & fx$hh_size_recode==5, 30,                 
            ifelse(fx$hh_income_cat=="$100,000 to $149,999" & fx$hh_size_recode==1, 31,
            ifelse(fx$hh_income_cat=="$100,000 to $149,999" & fx$hh_size_recode==2, 32,       
            ifelse(fx$hh_income_cat=="$100,000 to $149,999" & fx$hh_size_recode==3, 33,
            ifelse(fx$hh_income_cat=="$100,000 to $149,999" & fx$hh_size_recode==4, 34,       
            ifelse(fx$hh_income_cat=="$100,000 to $149,999" & fx$hh_size_recode==5, 35,
            ifelse(fx$hh_income_cat=="$150,000 to $199,999" & fx$hh_size_recode==1, 36,       
            ifelse(fx$hh_income_cat=="$150,000 to $199,999" & fx$hh_size_recode==2, 37,
            ifelse(fx$hh_income_cat=="$150,000 to $199,999" & fx$hh_size_recode==3, 38,          
            ifelse(fx$hh_income_cat=="$150,000 to $199,999" & fx$hh_size_recode==4, 39,
            ifelse(fx$hh_income_cat=="$150,000 to $199,999" & fx$hh_size_recode==5, 40,   
            ifelse(fx$hh_income_cat=="$200,000 or more" & fx$hh_size_recode==1, 41,       
            ifelse(fx$hh_income_cat=="$200,000 or more" & fx$hh_size_recode==2, 42,
            ifelse(fx$hh_income_cat=="$200,000 or more" & fx$hh_size_recode==3, 43,          
            ifelse(fx$hh_income_cat=="$200,000 or more" & fx$hh_size_recode==4, 44, 45
            ))))))))))))))))))))))))))))))))))))))))))))    
table(fx$index)
sum(table(fx$index))

names(fx)[1] <- "puma"
table(fx$puma)

#write.csv(fx, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/fairfax_puma_va_new_variables_2020.csv")

  
  
  