#HLB range for household size <= 5
#comparison with the poverty threshold

library(ggplot2)
library(stringi)
options(scipen=999)

#read in synthetic data
fx <- read.csv("~/Documents/Documents/Food Insecurity/Data/COL_Fairfax/FINAL/fx_syn.csv")

#create the HLB ratio
fx$HLB_ratio <- round(fx$hhincome/(12*fx$HLBnew), 2)
dim(fx); summary(fx$HLB_ratio)

#keep only economically vulnerable households
temp1 <- fx[fx$HLB_ratio<1,]
dim(temp1); View(temp1) 

#count the length of the hh_comb variable
temp2 <- stri_length(as.character(temp1$hh_comb))
table(temp2)
#only keep households where the length of hh_comb <=6
#this excludes households where the number of adults is in the double digits
temp1 <- temp1[temp2==6,]
dim(temp1) 

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

       temp4 <- data.frame(table(temp1$hh_size, temp1$hh_children))
names(temp4) <- c("hh_size","hh_children","total")
       temp4 <- temp4[temp4$total>0,]
dim(temp4); View(temp4)

pt <- temp1[temp1$hh_size<6,]; 
dim(pt); View(pt)
rm(temp1, temp2, temp3)

#plot the HLB range versus the poverty threshold
     pt$hh_comb2 <- paste0(pt$hh_size,":",pt$hh_children)
       pt$HLB_yr <- pt$HLBnew*12
colnames(pt)[26] <- "HLB_month"
temp1 <- aggregate(pt$HLB_month, by=list(pt$hh_size, pt$hh_children, pt$hh_comb2), FUN=min) 
temp1 <- temp1[temp1$Group.1<=7, c(3,4)]; names(temp1) <- c("hh_comb", "min")
temp2 <- aggregate(pt$HLB_month, by=list(pt$hh_size, pt$hh_children, pt$hh_comb2), FUN=median) 
temp2 <- temp2[temp2$Group.1<=7, c(3,4)]; names(temp2) <- c("hh_comb", "median")
temp3 <- aggregate(pt$HLB_month, by=list(pt$hh_size, pt$hh_children, pt$hh_comb2), FUN=max) 
temp3 <- temp3[temp3$Group.1<=7, c(3,4)]; names(temp3) <- c("hh_comb", "max")
          threshold_plot <- data.frame(hh_comb=temp1$hh_comb, min=round(temp1$min,2), 
                                       median=round(temp2$median,2), max=round(temp3$max,2))
          threshold_plot <- threshold_plot[ordered(threshold_plot$hh_comb),]
threshold_plot$threshold <- round(c(15225,19597,20172,22892,23556,23578,30186,
                                    30679,29678,29782,36402,36932,35801,34926,34391)/12, 2)
  threshold_plot$no_comb <- c(1,1,5,1,5,11,1,5,15,12,1,5,13,17,9)

ggplot(threshold_plot) +
  geom_hline(yintercept=seq(from=1000, to=13500, by=1000), color="#D2D2D2") +  
  geom_vline(xintercept=c("1:0","2:0","3:0","4:0","5:0"), color="#D2D2D2") +  
  geom_linerange(aes(x=hh_comb, ymin=min, ymax=max), color=cbPalette[2], linewidth=3, alpha=0.5) +
  geom_point(aes(x=hh_comb, y=threshold), color="black", size=2.00) +
  geom_point(aes(x=hh_comb, y=threshold), color=cbPalette[1], size=1.75) +
  geom_point(aes(x=hh_comb, y=median), color="black", size=2.00) +
  geom_point(aes(x=hh_comb, y=median), color=cbPalette[3], size=1.75) +
  scale_y_continuous(breaks=c(seq(from=1000, to=13500, by=1000)),
                     labels=c("1000","2000","3000","4000","5000","6000",
                              "7000","8000","9000","10000","11000","12000",
                              "13000")) +
  ylab("Monthly Household Living Budget ($)") +
  xlab("Household Size : Number of Children") +
  labs(title="Economically Vulnerable Households in Fairfax County, Virginia", 
       subtitle="Grey Circle = Poverty Threshold; Blue Circle = HLB Median;\nYellow Rectangle = HLB Range; # = Number of Household Combinations\n",  
       caption="\n2022 Census Poverty Thresholds by Family Size and Number of Children") +
  coord_flip() +
  annotate("text", y=5061.14, x="1:0", label="1", adj=0, size=4) + 
  annotate("text", y=6456.43, x="2:0", label="1", adj=0, size=4) + 
  annotate("text", y=6412.69, x="2:1", label="5", adj=0, size=4) +
  annotate("text", y=8230.05, x="3:0", label="1", adj=0, size=4) + 
  annotate("text", y=8304.17, x="3:1", label="5", adj=0, size=4) + 
  annotate("text", y=8240.86, x="3:2", label="11", adj=0, size=4) +
  annotate("text", y=10345.30, x="4:0", label="1", adj=0, size=4) + 
  annotate("text", y=10991.33, x="4:1", label="5", adj=0, size=4) + 
  annotate("text", y=10576.02, x="4:2", label="15", adj=0, size=4) +
  annotate("text", y=10676.61, x="4:3", label="12", adj=0, size=4) + 
  annotate("text", y=13101.81, x="5:0", label="1", adj=0, size=4) + 
  annotate("text", y=12841.94, x="5:1", label="5", adj=0, size=4) +
  annotate("text", y=13134.62, x="5:2", label="13", adj=0, size=4) + 
  annotate("text", y=13049.98, x="5:3", label="17", adj=0, size=4) + 
  annotate("text", y=13339.13, x="5:4", label="9", adj=0, size=4) +
  theme(panel.background=element_rect(fill="transparent"),
        text=element_text(color="#22211d"),
        plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=-1.0),
        plot.subtitle=element_text(size=11, hjust=0.5, vjust=-0.5),
        plot.caption=element_text(size=9, hjust=0))

rm(temp1, temp2, temp3, temp4)


