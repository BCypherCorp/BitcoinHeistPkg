library(BitcoinHeistShard1)
library(BitcoinHeistShard2)
library(BitcoinHeistShard3)
library(BitcoinHeistShard4)
library(BitcoinHeistShard5)
library(BitcoinHeistShard6)
library(BitcoinHeistShard7)
library(janitor)
library(tidyverse)
library(plyr)
library(gridExtra)
library(compareDF)
library(plotly)
library(viridis)
library(ggplot2)
#R data package process
#Merge the entire dataset into object 'BitcoinHeist'
shards1and2 <- rbind(BitcoinHeistShard1, BitcoinHeistShard2)
shards2and3 <- rbind(BitcoinHeistShard2, BitcoinHeistShard3)
shards4and5 <- rbind(BitcoinHeistShard4, BitcoinHeistShard5)
shards6and7 <- rbind(BitcoinHeistShard6, BitcoinHeistShard7)
firstHalf <- rbind(shards1and2, shards2and3)
secondHalf <- rbind(shards4and5, shards6and7)
fullSet <- rbind(firstHalf,secondHalf)
BitcoinHeist <- unique(fullSet)
nrow(BitcoinHeist)

#Create white data and virus data objects
whitedata<-BitcoinHeist[BitcoinHeist$label=="white",]
virusdata<-BitcoinHeist[BitcoinHeist$label!="white",]
show(virusdata)
show(whitedata)
allfeatures<-c("length","weight","neighbors","count","looped","income")
features= allfeatures[c(1,2,3,4,5,6)]
uniqueWhiteAddresses <- unique(whitedata)
uniqueVirusAddresses <- unique(virusdata)
show(uniqueWhiteAddresses)
show(uniqueVirusAddresses)
#####################################################
#Ransomware Functions
#####################################################

#' Function to list the ransomware families are in this dataset
#'
#' @param None
#' @return Ransomware families in dataset.
#' @examples
#' getRansomwareFamilies()
getRansomwareFamilies <- function(){
  uniqueVirusLabels <- unique(virusdata$label)
  return(uniqueVirusLabels)
}


#What is the most active ransomware family?
mostActiveFamily <- function(){
  mostActive <- tail(names(sort(table(virusdata$label))), 1)
  return(mostActive)
}
#Example usage: mostActiveFamily()

#How many unique addresses belong to each ransomware family?
uniqueAddressesOfRansomewareFamilies <- function(){
  uniqueAddresses <- count(uniqueVirusAddresses, 'label')
  return(uniqueAddresses)
}
#Example usage: uniqueAddressesOfRansomewareFamilies()

#Get the addresses of a ransomware family
getAddressesBelongingTo <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)
  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }
  return(addressesBelongingTo)
}
#Example usage: getAddressesBelongingTo("montrealAPT")

#Sums the income across all unique wallets belonging to a ransomware family
getTotalIncomeOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$length <- NULL
  addressesBelongingTo$weight <- NULL
  addressesBelongingTo$count <- NULL
  addressesBelongingTo$looped <- NULL
  addressesBelongingTo$neighbors <- NULL
  addressesBelongingTo$label <- NULL
  satoshiTotal <- colSums(addressesBelongingTo)
  BTCtotal <- satoshiTotal / 100000000

  income_data <- tibble(
    Total_Satoshi = c(satoshiTotal),
    Total_BTC = c(BTCtotal)
  )

  return(income_data)
}
#Example usage: getTotalIncomeOf("montrealAPT")

#Sums count of all unique addresses belonging to a ransomware family
getTotalCountOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$length <- NULL
  addressesBelongingTo$weight <- NULL
  addressesBelongingTo$looped <- NULL
  addressesBelongingTo$income <- NULL
  addressesBelongingTo$neighbors <- NULL
  show(addressesBelongingTo)
  addressesBelongingTo$label <- NULL

  countTotal = colSums(addressesBelongingTo)

  return(countTotal)
}
#Example usage: getTotalCountOf("montrealAPT")

#Sums length of all unique addresses belonging to a ransomware family
getTotalLengthOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$weight <- NULL
  addressesBelongingTo$looped <- NULL
  addressesBelongingTo$count <- NULL
  addressesBelongingTo$income <- NULL
  addressesBelongingTo$neighbors <- NULL
  show(addressesBelongingTo)
  addressesBelongingTo$label <- NULL

  lengthTotal = colSums(addressesBelongingTo)

  return(lengthTotal)
}
#Example usage: getTotalLengthOf("montrealAPT")

#Sums weight of all unique addresses belonging to a ransomware family
getTotalWeightOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$length <- NULL
  addressesBelongingTo$looped <- NULL
  addressesBelongingTo$income <- NULL
  addressesBelongingTo$neighbors <- NULL
  addressesBelongingTo$count <- NULL
  show(addressesBelongingTo)
  addressesBelongingTo$label <- NULL

  weightTotal = colSums(addressesBelongingTo)

  return(weightTotal)
}
#Example usage: getTotalWeightOf("montrealAPT")

#Sums looped attribute of all unique addresses belonging to a ransomware family
getTotalLoopedOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$length <- NULL
  addressesBelongingTo$income <- NULL
  addressesBelongingTo$neighbors <- NULL
  addressesBelongingTo$count <- NULL
  addressesBelongingTo$weight <- NULL
  show(addressesBelongingTo)
  addressesBelongingTo$label <- NULL

  loopedTotal = colSums(addressesBelongingTo)

  return(loopedTotal)
}
#Example usage: getTotalLoopedOf("montrealAPT")

#Sums neighbors attribute of all unique addresses belonging to a ransomware family
getTotalNeighborsOf <- function(x){
  addressesBelongingTo <- uniqueVirusAddresses %>% filter(label == x)

  if(nrow(addressesBelongingTo) == 0){
    show("Plese enter valid ransomware label. These are the ransomware families in the study:")
    return(getRansomwareFamilies())
  }

  addressesBelongingTo$X1 <- NULL
  addressesBelongingTo$address <- NULL
  addressesBelongingTo$year <- NULL
  addressesBelongingTo$day <- NULL
  addressesBelongingTo$length <- NULL
  addressesBelongingTo$income <- NULL
  addressesBelongingTo$count <- NULL
  addressesBelongingTo$weight <- NULL
  addressesBelongingTo$looped <- NULL
  show(addressesBelongingTo)
  addressesBelongingTo$label <- NULL

  neighborsTotal = colSums(addressesBelongingTo)

  return(neighborsTotal)
}
#Example usage: getTotalNeighborsOf("montrealAPT")

#RESULTS S1 - Averages of all features (weight, length, mean, income, neighbors, count, looped, neighbors)
#Average results for virus addresses
mean_features_virus <- ddply(virusdata,.(label),summarize,weight=mean(weight),length=mean(length),mean=mean(looped),
                             income=mean(income),neighbors=mean(neighbors),count=mean(count),N=length(address),U=length(unique(address)))

#RESULTS S2 - White address Histogram summaries.
p11<-ggplot(BitcoinHeist=whitedata)+geom_bar(aes(whitedata$length), width=2,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Length",y="Address")+scale_x_continuous(breaks=c(1,10,50,100,144)) +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p21<-ggplot(BitcoinHeist=whitedata)+geom_histogram(aes(whitedata$weight),binwidth=0.1,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Weight",y="Address") + xlim(0, 25) + scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p31<-ggplot(BitcoinHeist=whitedata[whitedata$count>1,])+geom_histogram(aes(whitedata$count),binwidth = 250,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Count",y="Address")+scale_x_continuous( )  +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p41<-ggplot(BitcoinHeist=whitedata[whitedata$looped>1,])+geom_histogram(aes(whitedata$looped),binwidth=200,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Looped",y="Address")  +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p51<-ggplot(BitcoinHeist=whitedata[whitedata$neighbors<10,])+geom_histogram(aes(whitedata$neighbors),binwidth=1,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Neighbors",y="Address") + xlim(0,10) + scale_y_log10() +
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p61<-ggplot(data=whitedata[whitedata$income<2*10^9,])+geom_histogram(aes(income),binwidth=50000000,color="black", fill = "skyblue1")+
  theme_classic()+labs(x="Income",y="Address")+scale_x_continuous( ) +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

#RESULTS S3 - Virus address Histogram summaries
p1<-ggplot(BitcoinHeist=virusdata)+geom_bar(aes(virusdata$length), width=2,color="black", fill = "red")+
  theme_classic()+labs(x="Length",y="Address")+scale_x_continuous(breaks=c(1,10,50,100,144)) +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p2<-ggplot(data=virusdata[virusdata$weight<7.5,])+geom_histogram(aes(weight),binwidth=0.1,color="black", fill = "red")+
  theme_classic()+labs(x="Weight",y="Address")+scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7)) +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p3<-ggplot(data=virusdata[virusdata$count>1,])+geom_histogram(aes(count),binwidth = 250,color="black", fill = "red")+
  theme_classic()+labs(x="Count",y="Address")+scale_x_continuous( )  +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p4<-ggplot(BitcoinHeist=virusdata[virusdata$looped>1,])+geom_histogram(aes(virusdata$looped),binwidth=200,color="black", fill = "red")+
  theme_classic()+labs(x="Looped",y="Address")  +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p5<-ggplot(data=virusdata[virusdata$neighbors<10,])+geom_histogram(aes(neighbors),binwidth=1,color="black", fill = "red")+
  theme_classic()+labs(x="Neighbors",y="Address")+scale_x_continuous(breaks=c(1,3,5,7,9))  +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

p6<-ggplot(data=virusdata[virusdata$income<2*10^9,])+geom_histogram(aes(income),binwidth=50000000,color="black", fill = "red")+
  theme_classic()+labs(x="Income",y="Address")+scale_x_continuous( ) +scale_y_log10()+
  theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"));

#View all summary data side by side
grid.arrange(p11, p1)
grid.arrange(p21, p2)
grid.arrange(p31,p3)
grid.arrange(p41,p4)
grid.arrange(p51,p5)
grid.arrange(p61,p6)

#RESULTS S4
#Result S4 Address statistics
existing<-ddply(virusdata,.(address),summarize,N=length(weight))
q<-existing
p7<-ggplot(existing[existing$N<120,])+geom_histogram(aes(N),binwidth=3,color="black", fill = "red")+
  theme_bw()+ labs(x="Appearance (day)",y="Address count") +scale_y_log10()+scale_x_continuous(breaks=c(1,40,100))+
  theme(panel.border = element_blank(), axis.line = element_line(),axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_blank(),legend.text=element_text(size=14),legend.position = "top");p7

summary(existing$N)

#How similar are viruses? How many unique rows in them?
nrow(unique(virusdata[,features]))

#How similar are white addresses?
#How many unique rows in them?
nrow(unique(whitedata[,features]))

#Which families repeat the same pattern?


# RESULTS S5 - What is the probability of similarity searching and finding an exact virus with the same features?
