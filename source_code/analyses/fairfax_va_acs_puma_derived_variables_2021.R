#Introduction into Census Microdata
#https://walker-data.com/census-r/introduction-to-census-microdata.html

#derived variables are added to the Farifax acs pums data: 
#income categories to match ACS Table S1901 (nine categories)
#household size (5 categories: 1, 2, 3, 4, >=5)
#index to match the 45 income categories by household size combinations
#household combinations

library(tidycensus)
library(tidyr)

View(pums_variables)

fx_pums <- c("59301", "59302", "59303", "59304", "59305", "59306", "59307", "59308", "59309")
fx_vars <- c("WGTP",     #household weight
             "AGEP",     #householders age
             "NOC",      #number of children in household
             "NP",       #number of persons in household
             "MAR",      #marital status:1=married/2=widowed/3=divorced/4=separated/5=never married or <15
             "HINCP")    #household income

fx_pums <- 
 get_pums(
  variables = fx_vars,
  state = "VA",
  puma = fx_pums,
  year = 2021,
  survey = "acs5",
  return_vacant = FALSE,
  recode = TRUE
)

dim(fx_pums); length(unique(fx_pums$SERIALNO)); dim(fx_pums[fx_pums$SPORDER==1,]) #24434 households 
fx_pums <- fx_pums[with(fx_pums, order(PUMA, SERIALNO, SPORDER)),]; dim(fx_pums)
View(fx_pums)

#create a variable for the six age categories used to construct the household combination variable
age_cat <-
  ifelse(fx_pums$AGEP>=19, "adult", 
        ifelse((fx_pums$AGEP==0 & fx_pums$NOC>0), "infant",
              ifelse((fx_pums$AGEP>=1 & fx_pums$AGEP<=3), "toddler", 
                    ifelse((fx_pums$AGEP>=4 & fx_pums$AGEP<=5), "preschooler", 
                          ifelse((fx_pums$AGEP>=6 & fx_pums$AGEP<=11), "schooler", "teenager"))))) 
length(age_cat) 
temp <- data.frame(SERIALNO=fx_pums$SERIALNO, age_cat=age_cat, NOP=c(rep(1, length(age_cat)))) 
dim(temp); summary(temp)
#create a column for each of the 6 age categories
temp2 <- pivot_wider(temp,
                     names_from = "age_cat", 
                     values_from = "NOP", 
                     values_fill = 0,
                     values_fn = sum)  
dim(temp2); names(temp2)
#combine the number of age categories to create the household combination variable
hh_size <- temp2$adult+temp2$teenager+temp2$schooler+temp2$preschooler+temp2$toddler+temp2$infant
hh_comb <- paste0(hh_size,":",temp2$adult,temp2$teenager,temp2$schooler,temp2$preschooler,temp2$toddler,temp2$infant)
length(hh_comb); rm(temp, temp2)

#create 9 income category groups
#change the negative income values to 0
                       temp <- fx_pums[!duplicated(fx_pums$SERIALNO),]; dim(temp)
   temp$HINCP[temp$HINCP<0] <- 0
              hh_income_cat <- cut(temp$HINCP,
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
table(hh_income_cat); length(hh_income_cat)  

#recode the household size variable
hh_size_recode <- ifelse(hh_size>5, 5, hh_size) 
length(hh_size_recode); table(hh_size_recode)
 
#create an index to match the income categories (9) x household size (5) table 
#create an index variable that identifies the cells in the 2-way table income category (9) x household size (5) table
#the index is used when creating the synthetic population
index <- ifelse(hh_income_cat=="Less than $15,000" & hh_size_recode==1, 1,
         ifelse(hh_income_cat=="Less than $15,000" & hh_size_recode==2, 2,       
         ifelse(hh_income_cat=="Less than $15,000" & hh_size_recode==3, 3,
         ifelse(hh_income_cat=="Less than $15,000" & hh_size_recode==4, 4,       
         ifelse(hh_income_cat=="Less than $15,000" & hh_size_recode==5, 5,
         ifelse(hh_income_cat=="$15,000 to $24,999" & hh_size_recode==1, 6,       
         ifelse(hh_income_cat=="$15,000 to $24,999" & hh_size_recode==2, 7,
         ifelse(hh_income_cat=="$15,000 to $24,999" & hh_size_recode==3, 8,          
         ifelse(hh_income_cat=="$15,000 to $24,999" & hh_size_recode==4, 9,
         ifelse(hh_income_cat=="$15,000 to $24,999" & hh_size_recode==5, 10,                 
         ifelse(hh_income_cat=="$25,000 to $34,999" & hh_size_recode==1, 11,
         ifelse(hh_income_cat=="$25,000 to $34,999" & hh_size_recode==2, 12,       
         ifelse(hh_income_cat=="$25,000 to $34,999" & hh_size_recode==3, 13,
         ifelse(hh_income_cat=="$25,000 to $34,999" & hh_size_recode==4, 14,       
         ifelse(hh_income_cat=="$25,000 to $34,999" & hh_size_recode==5, 15,
         ifelse(hh_income_cat=="$35,000 to $49,999" & hh_size_recode==1, 16,       
         ifelse(hh_income_cat=="$35,000 to $49,999" & hh_size_recode==2, 17,
         ifelse(hh_income_cat=="$35,000 to $49,999" & hh_size_recode==3, 18,          
         ifelse(hh_income_cat=="$35,000 to $49,999" & hh_size_recode==4, 19,
         ifelse(hh_income_cat=="$35,000 to $49,999" & hh_size_recode==5, 20,                
         ifelse(hh_income_cat=="$50,000 to $74,999" & hh_size_recode==1, 21,
         ifelse(hh_income_cat=="$50,000 to $74,999" & hh_size_recode==2, 22,       
         ifelse(hh_income_cat=="$50,000 to $74,999" & hh_size_recode==3, 23,
         ifelse(hh_income_cat=="$50,000 to $74,999" & hh_size_recode==4, 24,       
         ifelse(hh_income_cat=="$50,000 to $74,999" & hh_size_recode==5, 25,
         ifelse(hh_income_cat=="$75,000 to $99,999" & hh_size_recode==1, 26,       
         ifelse(hh_income_cat=="$75,000 to $99,999" & hh_size_recode==2, 27,
         ifelse(hh_income_cat=="$75,000 to $99,999" & hh_size_recode==3, 28,          
         ifelse(hh_income_cat=="$75,000 to $99,999" & hh_size_recode==4, 29,
         ifelse(hh_income_cat=="$75,000 to $99,999" & hh_size_recode==5, 30,                 
         ifelse(hh_income_cat=="$100,000 to $149,999" & hh_size_recode==1, 31,
         ifelse(hh_income_cat=="$100,000 to $149,999" & hh_size_recode==2, 32,       
         ifelse(hh_income_cat=="$100,000 to $149,999" & hh_size_recode==3, 33,
         ifelse(hh_income_cat=="$100,000 to $149,999" & hh_size_recode==4, 34,       
         ifelse(hh_income_cat=="$100,000 to $149,999" & hh_size_recode==5, 35,
         ifelse(hh_income_cat=="$150,000 to $199,999" & hh_size_recode==1, 36,       
         ifelse(hh_income_cat=="$150,000 to $199,999" & hh_size_recode==2, 37,
         ifelse(hh_income_cat=="$150,000 to $199,999" & hh_size_recode==3, 38,          
         ifelse(hh_income_cat=="$150,000 to $199,999" & hh_size_recode==4, 39,
         ifelse(hh_income_cat=="$150,000 to $199,999" & hh_size_recode==5, 40,   
         ifelse(hh_income_cat=="$200,000 or more" & hh_size_recode==1, 41,       
         ifelse(hh_income_cat=="$200,000 or more" & hh_size_recode==2, 42,
         ifelse(hh_income_cat=="$200,000 or more" & hh_size_recode==3, 43,          
         ifelse(hh_income_cat=="$200,000 or more" & hh_size_recode==4, 44, 45
         ))))))))))))))))))))))))))))))))))))))))))))    
table(index); length(index)

fairfax_va_acs_puma_derived_variables_2021 <- 
     data.frame(puma = temp$PUMA,
                serialno = temp$SERIALNO,
                hh_comb = hh_comb,
                index = index,
                hh_size_recode = hh_size_recode,
                hh_income_cat = hh_income_cat,
                hh_income = temp$HINCP)
dim(fairfax_va_acs_puma_derived_variables_2021)
View(fairfax_va_acs_puma_derived_variables_2021)  