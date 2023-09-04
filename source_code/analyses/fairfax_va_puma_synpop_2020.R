#program to generate the synthetic data for Fairfax County, VA  
options(scipen=999)
library(tidyverse)
library(dplyr)

 ipf <- read.csv("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/derived_variables/fairfax_va_acs_ipf_estimates_2022.csv")
puma <- read.csv("https://github.com/uva-bi-sdad/household_living_budget/raw/main/documents/products/derived_variables/fairfax_va_puma_new_variables_2020.csv")
puma <- puma[, -1]
  cw <- read.csv("https://github.com/uva-bi-sdad/household_living_budget/raw/main/data/demographic/crosswalks/fairfax_crosswalk_puma_ct_2020.csv")
#dim(ipf); names(ipf); View(ipf)
#dim(puma); names(puma); View(puma)
#dim(cw); names(cw); View(cw) 

#number of observations in each puma  
#  59301 59302 59303 59304 59305 59306 59307 59308 59309 
#   2718  2600  3335  1888  2549  2058  3424  2420  1930   
  
#data wrangling
#merge the crosswalk data with the ipf estimates to get the tracts within a puma
            temp <- merge(cw, ipf, by="geoid")
        ipf_long <- temp[with(temp, order(puma, tract, income_recode, size_recode)), ]
dim(ipf_long); rm(temp)
        ipf_long <- ipf_long[ipf_long$tract<980300,]
dim(ipf_long); names(ipf_long); View(ipf_long); length(unique(ipf_long$tract)) 

#check number of tracts and pumas
length(unique(ipf_long$geoid)); length(unique(ipf_long$puma))

############################################################################
#synthetic data for 59301 (contains 2718 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59301
      ipf_long_59301 <- ipf_long[ipf_long$puma==59301,] 
#dim(ipf_long_59301); names(ipf_long_59301); View(ipf_long_59301); length(unique(ipf_long_59301$tract)) 
#create tract column names
   ipf_long_59301$CT <- paste0("CT", ipf_long_59301$tract) 
                temp <- ipf_long_59301[,c(4,5,6,7)]; dim(temp)
      ipf_wide_59301 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp) 
#create a variable to use as the index
ipf_wide_59301$index <- c(1:45)  
#dim(ipf_wide_59301); names(ipf_wide_59301); View(ipf_wide_59301)
    
#pull out 59301 from the puma data 
   puma59301 <- puma[puma$puma==59301, ] 
#dim(puma59301); names(puma59301); View(puma59301)
    syn59301 <- cbind(tract=NA, puma59301[1,])
#get the number of tracts within puma 59301   
   no_tracts <- length(unique(ipf_long_59301$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59301$index)) 
missing_cell <- setdiff(1:45, cell); missing_cell
   
#synthetic populations for census tracts in puma 59301
sum(ipf_wide_59301[,c(3:no_tracts)]) #number of households in puma 59301
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59301$index))){
    temp1 <- puma59301[(puma59301$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59301[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59301)[j], unname(unlist(ipf_wide_59301[cell[i],j]))), temp1[index,])
 syn59301 <- rbind(syn59301, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59301 <- syn59301[-1,] 
syn59301$tract <- substring(syn59301$tract, 3, 8) 
dim(syn59301)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59301[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59301$index))[,2]-test

rm(no_tracts, cell, missing_cell)


############################################################################
#synthetic data for 59302 (contains 2600 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59302
     ipf_long_59302 <- ipf_long[ipf_long$puma==59302,] 
#dim(ipf_long_59302); names(ipf_long_59302); View(ipf_long_59302); length(unique(ipf_long_59302$tract)) 
#create tract column names
  ipf_long_59302$CT <- paste0("CT", ipf_long_59302$tract) 
               temp <- ipf_long_59302[,c(4,5,6,7)]
     ipf_wide_59302 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp)
#create a variable to use as the index
ipf_wide_59302$index <- c(1:45)  
#dim(ipf_wide_59302); names(ipf_wide_59302); View(ipf_wide_59302)

#pull out 59302 from the puma data 
puma59302 <- puma[puma$puma==59302, ]  
#dim(puma59302); names(puma59302); View(puma59302)
 syn59302 <- cbind(tract=NA, puma59302[1,])
#get the number of tracts within puma 59302   
no_tracts <- length(unique(ipf_long_59302$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59302$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59302               
sum(ipf_wide_59302[,c(3:no_tracts)]) #number of households in puma 59302
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59302$index))){
    temp1 <- puma59302[(puma59302$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59302[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59302)[j], unname(unlist(ipf_wide_59302[cell[i],j]))), temp1[index,])
 syn59302 <- rbind(syn59302, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59302 <- syn59302[-1,] 
syn59302$tract <- substring(syn59302$tract, 3, 8) 
dim(syn59302)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59302[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59302$index))[,2]-test

rm(no_tracts, cell, missing_cell) 
 

############################################################################
#synthetic data for 59303 (contains 3335 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59303
      ipf_long_59303 <- ipf_long[ipf_long$puma==59303,] 
#dim(ipf_long_59303); names(ipf_long_59303); View(ipf_long_59303); length(unique(ipf_long_59303$tract)) 
#create tract column names
   ipf_long_59303$CT <- paste0("CT", ipf_long_59303$tract) 
                temp <- ipf_long_59303[,c(4,5,6,7)]
      ipf_wide_59303 <- temp %>% pivot_wider(names_from=CT, values_from=ipf) 
#create a variable to use as the index
ipf_wide_59303$index <- c(1:45)  
#dim(ipf_wide_59303); names(ipf_wide_59303); View(ipf_wide_59303)

#pull out 59303 from the puma data 
   puma59303 <- puma[puma$puma==59303, ]  
#dim(puma59303); names(puma59303); View(puma59303)
    syn59303 <- cbind(tract=NA, puma59303[1,])
#get the number of tracts within puma 59303   
   no_tracts <- length(unique(ipf_long_59303$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59303$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59303               
sum(ipf_wide_59303[,c(3:no_tracts)]) #number of households in puma 59303
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59303$index))){
    temp1 <- puma59303[(puma59303$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59303[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59303)[j], unname(unlist(ipf_wide_59303[cell[i],j]))), temp1[index,])
 syn59303 <- rbind(syn59303, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59303 <- syn59303[-1,] 
syn59303$tract <- substring(syn59303$tract, 3, 8) 
dim(syn59303)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59303[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59303$index))[,2]-test

rm(no_tracts, cell, missing_cell) 


############################################################################
#synthetic data for 59304 (contains 1888 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59304
      ipf_long_59304 <- ipf_long[ipf_long$puma==59304,] 
#dim(ipf_long_59304); names(ipf_long_59304); View(ipf_long_59304); length(unique(ipf_long_59304$tract)) 
#create tract column names
   ipf_long_59304$CT <- paste0("CT", ipf_long_59304$tract) 
                temp <- ipf_long_59304[,c(4,5,6,7)]
      ipf_wide_59304 <- temp %>% pivot_wider(names_from=CT, values_from=ipf) 
#create a variable to use as the index
ipf_wide_59304$index <- c(1:45)  
#dim(ipf_wide_59304); names(ipf_wide_59304); View(ipf_wide_59304)

#pull out 59304 from the puma data 
   puma59304 <- puma[puma$puma==59304, ]  
#dim(puma59304); names(puma59304); View(puma59304)
    syn59304 <- cbind(tract=NA, puma59304[1,])
#get the number of tracts within puma 59304   
   no_tracts <- length(unique(ipf_long_59304$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59304$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59304               
sum(ipf_wide_59304[,c(3:no_tracts)]) #number of households in puma 59304
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59304$index))){
    temp1 <- puma59304[(puma59304$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59304[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59304)[j], unname(unlist(ipf_wide_59304[cell[i],j]))), temp1[index,])
 syn59304 <- rbind(syn59304, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59304 <- syn59304[-1,] 
syn59304$tract <- substring(syn59304$tract, 3, 8) 
dim(syn59304)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59304[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59304$index))[,2]-test

rm(no_tracts, cell, missing_cell) 


############################################################################
#synthetic data for 59305 (contains 2549 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59305
      ipf_long_59305 <- ipf_long[ipf_long$puma==59305,] 
#dim(ipf_long_59305); names(ipf_long_59305); View(ipf_long_59305); length(unique(ipf_long_59305$tract)) 
#create tract column names
   ipf_long_59305$CT <- paste0("CT", ipf_long_59305$tract) 
                temp <- ipf_long_59305[,c(4,5,6,7)]
      ipf_wide_59305 <- temp %>% pivot_wider(names_from=CT, values_from=ipf) 
#create a variable to use as the index
ipf_wide_59305$index <- c(1:45)  
#dim(ipf_wide_59305); names(ipf_wide_59305); View(ipf_wide_59305)

#pull out 59305 from the puma data 
   puma59305 <- puma[puma$puma==59305, ]  
#dim(puma59305); names(puma59305); View(puma59305)
    syn59305 <- cbind(tract=NA, puma59305[1,])
#get the number of tracts within puma 59305   
   no_tracts <- length(unique(ipf_long_59305$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59305$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59305               
sum(ipf_wide_59305[,c(3:no_tracts)]) #number of households in puma 59305
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59305$index))){
    temp1 <- puma59305[(puma59305$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59305[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59305)[j], unname(unlist(ipf_wide_59305[cell[i],j]))), temp1[index,])
 syn59305 <- rbind(syn59305, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59305 <- syn59305[-1,] 
syn59305$tract <- substring(syn59305$tract, 3, 8) 
dim(syn59305)

#check number of households per cell to see if they agree with the ipf estimates
if(length(missing_cell)==0) 
  {data.frame(table(syn59305$index))[,2]-apply(ipf_wide_59305[,3:no_tracts],1,sum)} else 
  {data.frame(table(syn59305$index))[,2]-apply(ipf_wide_59305[,3:no_tracts],1,sum)[-missing_cell]}

rm(no_tracts, cell, missing_cell) 


############################################################################
#synthetic data for 59306 (contains 2058 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59306
      ipf_long_59306 <- ipf_long[ipf_long$puma==59306,] 
#dim(ipf_long_59306); names(ipf_long_59306); View(ipf_long_59306); length(unique(ipf_long_59306$tract)) 
#create tract column names
   ipf_long_59306$CT <- paste0("CT", ipf_long_59306$tract) 
                temp <- ipf_long_59306[,c(4,5,6,7)]
      ipf_wide_59306 <- temp %>% pivot_wider(names_from=CT, values_from=ipf) 
#create a variable to use as the index
ipf_wide_59306$index <- c(1:45)  
#dim(ipf_wide_59306); names(ipf_wide_59306); View(ipf_wide_59306)

#pull out 59306 from the puma data 
   puma59306 <- puma[puma$puma==59306, ]  
#dim(puma59306); names(puma59306); View(puma59306)
    syn59306 <- cbind(tract=NA, puma59306[1,])
#get the number of tracts within puma 59306   
   no_tracts <- length(unique(ipf_long_59306$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59306$index))
missing_cell <- setdiff(1:45, cell); missing_cell
 
#synthetic populations for census tracts in puma 59306               
sum(ipf_wide_59306[,c(3:no_tracts)]) #number of households in puma 59306
   j_stop <- no_tracts
for(j in 3:j_stop){
  for(i in 1:length(unique(puma59306$index))){
    temp1 <- puma59306[(puma59306$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59306[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59306)[j], unname(unlist(ipf_wide_59306[cell[i],j]))), temp1[index,])
 syn59306 <- rbind(syn59306, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59306 <- syn59306[-1,] 
syn59306$tract <- substring(syn59306$tract, 3, 8) 
dim(syn59306)

#check number of households per cell to see if they agree with the ipf estimates
if(length(missing_cell)==0) 
  {data.frame(table(syn59306$index))[,2]-apply(ipf_wide_59306[,3:no_tracts],1,sum)} else 
  {data.frame(table(syn59306$index))[,2]-apply(ipf_wide_59306[,3:no_tracts],1,sum)[-missing_cell]}

rm(no_tracts, cell, missing_cell) 
 

############################################################################
#synthetic data for 59307 (contains 3424 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59307
      ipf_long_59307 <- ipf_long[ipf_long$puma==59307,] 
#dim(ipf_long_59307); names(ipf_long_59307); View(ipf_long_59307); length(unique(ipf_long_59307$tract)) 
#create tract column names
   ipf_long_59307$CT <- paste0("CT", ipf_long_59307$tract) 
                temp <- ipf_long_59307[,c(4,5,6,7)]
      ipf_wide_59307 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp) 
#create a variable to use as the index
ipf_wide_59307$index <- c(1:45)  
#dim(ipf_wide_59307); names(ipf_wide_59307); View(ipf_wide_59307)

#pull out 59307 from the puma data 
   puma59307 <- puma[puma$puma==59307, ] 
#dim(puma59307); names(puma59307); View(puma59307)
    syn59307 <- cbind(tract=NA, puma59307[1,])
#get the number of tracts within puma 59307   
   no_tracts <- length(unique(ipf_long_59307$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59307$index))
missing_cell <- setdiff(1:45, cell); missing_cell
 
#synthetic populations for census tracts in puma 59307
sum(ipf_wide_59307[,c(3:no_tracts)]) #number of households in puma 59307 
   j_stop <- no_tracts
for(j in 3:j_stop){
  for(i in 1:length(unique(puma59307$index))){
    temp1 <- puma59307[(puma59307$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59307[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59307)[j], unname(unlist(ipf_wide_59307[cell[i],j]))), temp1[index,])
 syn59307 <- rbind(syn59307, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59307 <- syn59307[-1,] 
syn59307$tract <- substring(syn59307$tract, 3, 8) 
dim(syn59307)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59307[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59307$index))[,2]-test

rm(no_tracts, cell, missing_cell)


############################################################################
#synthetic data for 59308 (contains 2420 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59308
      ipf_long_59308 <- ipf_long[ipf_long$puma==59308,] 
#dim(ipf_long_59308); names(ipf_long_59308); View(ipf_long_59308); length(unique(ipf_long_59308$tract)) 
#create tract column names
   ipf_long_59308$CT <- paste0("CT", ipf_long_59308$tract) 
                temp <- ipf_long_59308[,c(4,5,6,7)]
      ipf_wide_59308 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp) 
#create a variable to use as the index
ipf_wide_59308$index <- c(1:45)  
#dim(ipf_wide_59308); names(ipf_wide_59308); View(ipf_wide_59308)

#pull out 59308 from the puma data 
   puma59308 <- puma[puma$puma==59308, ]
#dim(puma59308); names(puma59308); View(puma59308)
    syn59308 <- cbind(tract=NA, puma59308[1,])
#get the number of tracts within puma 59308   
   no_tracts <- length(unique(ipf_long_59308$tract)) 
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59308$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59308
sum(ipf_wide_59308[,c(3:no_tracts)]) #number of households in puma 59308 
   j_stop <- no_tracts
for(j in 3:j_stop){
  for(i in 1:length(unique(puma59308$index))){
    temp1 <- puma59308[(puma59308$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59308[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59308)[j], unname(unlist(ipf_wide_59308[cell[i],j]))), temp1[index,])
 syn59308 <- rbind(syn59308, temp2)
rm(index, temp1, temp2)
}}

#cleanup
      syn59308 <- syn59308[-1,] 
syn59308$tract <- substring(syn59308$tract, 3, 8) 
dim(syn59308)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59308[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59308$index))[,2]-test

rm(no_tracts, cell, missing_cell)


############################################################################
#synthetic data for 59309 (contains 1930 observations)
#put ipf_long into a short format to use in generating the synpop keep only puma 59309
      ipf_long_59309 <- ipf_long[ipf_long$puma==59309,] 
#dim(ipf_long_59309); names(ipf_long_59309); View(ipf_long_59309); length(unique(ipf_long_59309$tract)) 
#create tract column names
   ipf_long_59309$CT <- paste0("CT", ipf_long_59309$tract) 
                temp <- ipf_long_59309[,c(4,5,6,7)]
      ipf_wide_59309 <- temp %>% pivot_wider(names_from=CT, values_from=ipf); rm(temp) 
#create a variable to use as the index
ipf_wide_59309$index <- c(1:45)  
#dim(ipf_wide_59309); names(ipf_wide_59309); View(ipf_wide_59309)

#pull out 59309 from the puma data 
   puma59309 <- puma[puma$puma==59309, ] 
#dim(puma59309); names(puma59309); View(puma59309)
    syn59309 <- cbind(tract=NA, puma59309[1,])
#get the number of tracts within puma 59309   
   no_tracts <- length(unique(ipf_long_59309$tract))+2
#check if the puma has a household in each cell and record the missing cells
        cell <- sort(unique(puma59309$index))
missing_cell <- setdiff(1:45, cell); missing_cell

#synthetic populations for census tracts in puma 59309
sum(ipf_wide_59309[,c(3:no_tracts)]) #number of households in puma 59309 
for(j in 3:no_tracts){
  for(i in 1:length(unique(puma59309$index))){
    temp1 <- puma59309[(puma59309$index==cell[i]),  ]
    index <- sample(c(1:dim(temp1)[1]), size=unname(unlist(ipf_wide_59309[cell[i],j])), replace=TRUE)
    temp2 <- cbind(tract=rep(names(ipf_wide_59309)[j], unname(unlist(ipf_wide_59309[cell[i],j]))), temp1[index,])
 syn59309 <- rbind(syn59309, temp2)
rm(index, temp1, temp2)
}}

#cleanup
syn59309 <- syn59309[-1,] 
syn59309$tract <- substring(syn59309$tract, 3, 8) 
dim(syn59309)

#check number of households per cell to see if they agree with the ipf estimates
test <- apply(ipf_wide_59309[,3:no_tracts],1,sum); test <- test[test>0]
data.frame(table(syn59309$index))[,2]-test

rm(no_tracts, cell, missing_cell)


#combine the data from the nine pumas
fx.syn <- data.frame(rbind(syn59301,syn59302,syn59303,syn59304,syn59305,
                           syn59306,syn59307,syn59308,syn59309))
dim(fx.syn)

write.csv(fx.syn, "~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/fairfax_va_puma_synpop_2020.csv")


