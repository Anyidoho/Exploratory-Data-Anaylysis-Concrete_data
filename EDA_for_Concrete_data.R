## DESCRIPTIVE STATISTICS FOR CONCRETE DATA##
##set up working directory to Advanced Data Analysis Project 1##
setwd("C:/Users/panyidoh/Desktop/Advanced Data Analysis_Project 1")
## confirming working directory##
getwd()

## importing Concrete data from Data as a csv file ##
datac=read.csv(file.choose(),header=T)
View(datac)
names(datac)
attach(datac)

## CEMENT ##

## Summary for data##
summary(datac[,2:11])
summary(Cement)

## Histogram for concrete variable##
install.packages('ggplot2')
library(ggplot2)

breaks = c(137,167,197,227,257,287,317,347,374)

ggplot(datac, aes(Cement)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Cement") +
  labs(x="Cement (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Cement ##
ggplot(datac, aes(sample = Cement))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Cement") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))

## SLAG## 

breaks = c(0,20,40,60,80,100,120,140,160,180,193)

ggplot(datac, aes(Slag)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Slag") +
  labs(x="Slag (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = Slag))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Slag") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))

##FLY ASH##
breaks = c(0,20,100,120,140,160,180,200,220,240,260)

ggplot(datac, aes(Fly.ash)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Fly Ash") +
  labs(x="Fly Ash (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = Fly.ash))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Fly Ash") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))




## WATER ##
breaks = c(160,170,180,190,200,210,220,230,240)

ggplot(datac, aes(Water)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Water") +
  labs(x="Water (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = Water))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Water") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))



## SP ##
breaks = c(4.4,6.4,8.4,10.4,12.4,14.4,16.4,18.4,19)
names(datac)
ggplot(datac, aes(SP)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Superplasticizer") +
  labs(x="Superplasticizer (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = SP))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Superplasticizer") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))



## CA ##
breaks = c(703,743,783,823,863,903,943,983,1023,1049.9)
names(datac)
ggplot(datac, aes(Coarse.Aggr.)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Coarse Aggregate") +
  labs(x="Coarse Aggregate (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = Coarse.Aggr.))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Coarse Aggregate") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))




## FA ##
breaks = c(640.6,660.6,680.6,700.6,720.6,740.6,760.6,780.6,800.6,820.6,840.6,860.6,902)
names(datac)
ggplot(datac, aes(Fine.Aggr.)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Fine Aggregate") +
  labs(x="Fine Aggregate (Kg)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = Fine.Aggr.))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Fine Aggregate") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))


## Slump ##
breaks = c(0,4,8,12,16,20,24,29)
names(datac)
ggplot(datac, aes(SLUMP.cm.)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Slump") +
  labs(x="Slump (cm)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for Slag ##
ggplot(datac, aes(sample = SLUMP.cm.))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Slump") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))



## flow ##
breaks = c(20,30,40,50,60,70,78)
names(datac)
ggplot(datac, aes(FLOW.cm.)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Flow") +
  labs(x="Flow (cm)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for flow ##
ggplot(datac, aes(sample = FLOW.cm.))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Flow") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))


## Compressive ##
breaks = c(17.19,27.19,37.19,47.19,58.53)
names(datac)
ggplot(datac, aes(Compressive.Strength..28.day..Mpa.)) +
  geom_histogram(color="black", fill="light blue", breaks=breaks) +theme_bw()+
  scale_x_continuous(breaks=breaks)+labs(title="Histogram for Concrete compressive strength ") +
  labs(x="Concrete compressive strength (MPa)", y="Frequency")+ 
  theme(plot.title=element_text(hjust=0.5))

## QQ plot for flow ##
ggplot(datac, aes(sample = Compressive.Strength..28.day..Mpa.))+ stat_qq(col="blue") + stat_qq_line(col="black",size=0.8)+theme_bw()+
  labs(title="QQ Plot of Sample Data and Standard Normal Data for Concrete compressive strength ") +
  labs(x="Standard Normal Quantiles", y="Sample Quantiles")+ 
  theme(plot.title=element_text(hjust=0.5))

## Box and Whisker plots
Cement=Cement
Slag=Slag
Fly.ash=Fly.ash
Water=Water
SP=SP
Coarse.aggr=Coarse.Aggr.
Fine.aggr=Fine.Aggr.
Slump=SLUMP.cm.
Flow=FLOW.cm.
Compressive.Strength=Compressive.Strength..28.day..Mpa.

boxplot(Cement,Slag , Fly.ash, Water,Coarse.aggr,SP,Fine.aggr, Flow, Slump,Compressive.Strength,
        at = c(1,2,3,4,5,6,7,8,9,10),
  
        names = c("Cement", "Slag", "Fly.ash", "Water","Coarse.","SP","Fine.","Flow","Slump","Strength"),
        
        las = 2,
        
        col = c("light blue","light green"),
        
        border = "brown",
        
        horizontal = TRUE,
        
        notch = TRUE
        
)+title(main="Box plot for all Variables",cex.main=1.2)+theme_bw()+scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700,800,900,1000))


install.packages("psych")








