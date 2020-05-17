# BitcoinHeist Ransomware Dataset
> The dataset is used in Akcora, Cuneyt Gurcan and Li, Yitao and Gel, Yulia and 
Kantarcioglu, Murat . "BitcoinHeist: Topological Data Analysis for Ransomware Detection on the
Bitcoin Blockchain." IJCAI 2020.

We have downloaded and parsed the entire Bitcoin transaction graph from 2009 January to 2018 December. Using a time interval of 24 hours, we extracted daily transactions on the network and formed the Bitcoin graph. We filtered out the network edges that transfer less than B0.3, since ransom amounts are rarely below this threshold.

On the heterogeneous Bitcoin network, in each 24-hour snapshot we extract the following six features for an address: income, neighbors, weight, length, count, loop.

In 24 ransomware families, at least one address appears in more than one 24-hour time window. 
CryptoLocker has 13 addresses that appear more than 100 times each. 
The CryptoLocker address 1LXrSb67EaH1LGc6d6kWHq8rgv4ZBQAcpU appears for a maximum of 420 times. 
Four addresses have conflicting ransomware labels between Montreal and Padua datasets. 
APT (Montreal) and Jigsaw (Padua) ransomware families have two and one P2SH addresses (that start with '3'), respectively. 
All other addresses are ordinary addresses that start with ’1’.

## Usage
```
library(devtools)
library(usethis)
install_github("BlockchainDataAnalytics/BitcoinHeistPkg")
library(BitcoinHeistPkg)
library(BitcoinHeistShard1)
library(BitcoinHeistShard2)
library(BitcoinHeistShard3)
library(BitcoinHeistShard4)
library(BitcoinHeistShard5)
library(BitcoinHeistShard6)
library(BitcoinHeistShard7)

#Merge the entire dataset into object 'BitcoinHeist'
shards1and2 <- rbind(BitcoinHeistShard1, BitcoinHeistShard2)
shards2and3 <- rbind(BitcoinHeistShard2, BitcoinHeistShard3)
shards4and5 <- rbind(BitcoinHeistShard4, BitcoinHeistShard5)
shards6and7 <- rbind(BitcoinHeistShard6, BitcoinHeistShard7)
firstHalf <- rbind(shards1and2, shards2and3)
secondHalf <- rbind(shards4and5, shards6and7)
fullSet <- rbind(firstHalf,secondHalf)
BitcoinHeist <- unique(fullSet)

whitedata<-BitcoinHeist[BitcoinHeist$label=="white",]
virusdata<-BitcoinHeist[BitcoinHeist$label!="white",]

allfeatures<-c("length","weight","neighbors","count","looped","income")
features= allfeatures[c(1,2,3,4,5,6)]

uniqueWhiteAddresses <- unique(whitedata)
uniqueVirusAddresses <- unique(virusdata)
```

## Features

address: String. Bitcoin address.
year: Integer. Year.
day: Integer. Day of the year. 1 is the first day, 365 is the last day.
length: Integer.
weight: Float.
count: Integer.
looped: Integer.
neighbors: Integer.
income: Integer. Satoshi amount (1 bitcoin = 100 million satoshis).
label: Category String. Name of the ransomware family (e.g., Cryptxxx, cryptolocker etc) or 
white (i.e., not known to be ransomware).
Our graph features are designed to quantify specific transaction patterns. 
Loop is intended to count how many transaction i) split their coins; 
ii) move these coins in the network by using different paths and finally, and 
iii) merge them in a single address. Coins at this final address can then be sold and converted to fiat currency. 
Weight quantifies the merge behavior (i.e., the transaction has more input addresses than output addresses), 
where coins in multiple addresses are each passed through a succession of merging transactions and 
accumulated in a final address. 
Similar to weight, the count feature is designed to quantify the merging pattern. 
However, the count feature represents information on the number of transactions, 
whereas the weight feature represents information on the amount (what percent of these transactions’ output?) 
of transactions. Length is designed to quantify mixing rounds on Bitcoin, where transactions receive and 
distribute similar amounts of coins in multiple rounds with newly created addresses to hide the coin origin.

White Bitcoin addresses are capped at 1K per day (Bitcoin has 800K addresses daily).

Note that although we are certain about ransomware labels, we do not know if all white addresses are in fact not related to ransomware.

When compared to non-ransomware addresses, ransomware addresses exhibit more profound right skewness in distributions of feature values.


## Meta

Sudhanva Purushotham – sxp176930@utdallas.edu


