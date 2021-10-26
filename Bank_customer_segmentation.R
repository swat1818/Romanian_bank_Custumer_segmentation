getwd()
data1<-read.table("assignment1.txt",sep="\t",header=T)

# install these libraries first if not already installed
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

# corss_checking data before cleaning it
dim(data1)
# data has 1741041 rows & 9 columns of info 
# check unique customers in data
length(unique(data1$cust_id)) 
# 1) data has 25000 unique cust_IDs
# check how many satisfaction surveys were conducted
sum(table(data1$satis_survey)) 
# at the begining, there are 29727 satisfaction survey responses.
# output is a table with percentages as continuous decimals


# PART A
# A.1: create a table of counts for satis_survey column
satis_survey_table=table(na.omit(data1$satis_survey))
satis_survey_table
# output shows 29727 records of information
# calculate % for the counts above
proptable=prop.table(satis_survey_table)
# convert decimals above to 2 decimal integers
satis_survey_table2=round(100*proptable,2)
# around 21%  customers are dis-satisfied and 25% are highly dis-satisfied. The firm is doing a very bad job in terms of customer satisfaction
cbind(satis_survey_table,satis_survey_table2)
# A.2: how many surveys were conducted by month?
# convert date from factor to date type
data1$new_date<-as.Date(data1$survey_date,"%m/%d/%Y")
# retrieve month from the new survey date column
data1$new_month<-format(data1$new_date,"%B")
data1$month<-format(data1$new_date,"%m")
# tabulate counts of surveys by month
survey_bymonth_table=table(na.omit(data1$new_month))
survey_bymonth_table
# outputs table of survey counts by month (Jan-Dec), 29727 surveys conducted

#----------------------------------------------------------------------------------

# Part B

# a. Remove -ve or missing values for total investments. Also delete any prior records for all such customers.
# subset to identify unique customers with bad data
sub_data<-subset(data1,(data1$total_investments<0) | (is.na(data1$total_investments)))
# calculate unique customers in data2
unique1<-unique(sub_data$cust_id)
partba_uniqueID=data.frame(unique1)
# 3) partba_uniqueID table has 12884 rows with bad data. Delete all data corresponding to these customers
data2<-data1[!(data1$cust_id %in% partba_uniqueID$unique1),]


# b. Remove Zero, negative or missing values for either cust_age, cust_tenure, or tottrans. Remove any prior data for all these customers.
# subset data3 to identify bad data as per part B(b)
sub_data2<-subset(data2,((data2$cust_age==0)|(data2$cust_age<0)|(is.na(data2$cust_age))|
                          (data2$cust_tenure==0)|(data2$cust_tenure<0)|(is.na(data2$cust_tenure))|
                           (data2$tottrans==0)|(data2$tottrans<0)|(is.na(data2$tottrans))))
unique2<-unique(sub_data2$cust_id)
partbb_uniqueID<-data.frame(unique2)
head(partbb_uniqueID)
# 7) 4343 unique customer IDs identified. Remove data corresponding to these customers
data3<-data2[!(data2$cust_id %in% partbb_uniqueID$unique2),]
# check how many customers remaning
dim(table(unique(data3$cust_id)))
# 7773 customers remaining in the data set. 1327605 customers deleted in part B.
#------------------------------------------------------------------------------------------

# Part C
# identify top 1% unique customers in total investments & tottrans column
quantile(data3$total_investments,c(0.99),na.rm = TRUE)
quantile(data3$tottrans,c(0.99),na.rm = TRUE)
# subset data3 using the quantile values above to identify unique customers
sub_data3<-subset(data3,((data3$total_investments>=424080)|(data3$tottrans>=201)))
uniquec=data.frame(unique(sub_data3$cust_id))
# 745 unique customers in subset above.Remove 445668 rows of data relating to these customers.
data4<-data3[!(data3$cust_id %in% uniquec$unique.sub_data3.cust_id.),]
dim(table(unique(data4$cust_id)))
# 368868 rows & 7028 customers remaining in the dataset

#--------------------------------------------------------------------------------------------
# Part D

# Scatter chart: Total investments by satisfaction levels
data5<-data4
data5$satis_survey[is.na(data5$satis_survey)]<-0
cust_table<-data.frame(data5%>%
                         filter(satis_survey>0)%>%
                         group_by(cust_id)%>%
                         summarize(satisfaction=max(satis_survey),year=max(format(new_date,"%Y"))))
cust_TI<-data.frame(data5%>%
                      group_by(cust_id)%>%
                      summarize(transactions=sum(tottrans),investment=sum(total_investments)))
sum(cust_TI$investment)
investment_by_satislevel <- merge(cust_TI,cust_table,by=c("cust_id"))
sum(investment_by_satislevel$investment)


graph3<- ggplot(investment_by_satislevel,aes(transactions,investment/1000,color=satisfaction)) + 
  geom_point() + 
  theme(legend.position=c(1,1), legend.justification=c(1,1))+
  xlab("Total transactions")+
  ylab("Total Investments in 000's")+
  xlim(0,5000)+
  ylim(0,15000)+
  scale_color_gradientn(colours = rainbow(4))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

graph3
# 35 rows were excluded to better zoom into the spread of data

# scatter plot 1: Total investments & transactions by age group
data4$agegroup<-cut(data4$cust_age,c(0,20,40,60,80,100))
levels(data4$agegroup)=c("0-20","20.01-40","40.01-60","60.01-80","80.01-100")
data4$agegroup<-as.factor(data4$agegroup)
investment_by_agegroup <- data.frame(data4%>%
                                       group_by(cust_id,agegroup)%>%
                                       summarize(transactions=sum(tottrans),investments=sum(total_investments))%>%
                                       ungroup())
head(investment_by_agegroup)
sum(investment_by_agegroup$investments)
head(data4)
graph2 <- ggplot(investment_by_agegroup,aes(transactions,investments/1000,color=agegroup)) + 
  geom_point() + 
  theme(legend.position=c(1,1), legend.justification=c(1,1))+
  xlim(0,10000)+
  ylim(0,15000)+
  xlab("Total transactions")+
  ylab("Total Investments in 000's")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
graph2
# 26 rows were excluded from graph to show the spread better.

getwd()
hw2<-read.table("assignment 1b.txt",sep="\t",header=TRUE)
library(dplyr)
library(tidyverse)

# PART E
#  identify non contiguous rows using lag command
remove(hw_e)
subset_e<-hw2%>%
  group_by(hw2$cust_id)%>%
  mutate(diff=months_since_survey-lag(months_since_survey))
unique_1<-subset(subset_e, subset_e$diff>1)
unique_parte<-data.frame(unique(unique_1$cust_id))
unique_parte=setNames(data.frame(table(unique_parte$unique.unique_1.cust_id.)),c("cust_id","counts"))
dim(unique_parte)
# 4219 customers with non contiguous data identified

# remove data corresponding to customers identified above
hw_e<-hw2[!c(hw2$cust_id %in% unique_parte$cust_id),]
dim(hw_e)
dim(table(hw_e$cust_id))
# 2809 customers remaining. 117140 rows remaining in the dataset. 

# PART F (11 lines)
hw_e$survey.results<-ifelse(is.na(hw_e$satis_survey),0,1)
subset_e<-data.frame(hw_e%>%
                group_by(cust_id)%>%
                summarise(survey.counts=sum(survey.results)))
dim(subset_e)
# create a counts table of K surveys per customer
counts.table=data.frame(table(subset_e$survey.counts))
counts.table=setNames(counts.table,c("No.of surveys","No.of customers"))
counts.table
# 326  (258+52+8+7+1) surveyed more than once. 7 customers not yet surveyed
# max times a customer was surveyed was 6. Only 1 customer was surveyed 6 times
# remove all customers who were surveyed more than once or not surveyed at all
delete1<-subset_e[c(subset_e$survey.counts>1 | subset_e$survey.counts==0),]
hw_f<-hw_e[!c(hw_e$cust_id %in% delete1$cust_id),]
dim(hw_f)
dim(table(unique(hw_f$cust_id)))
# 100046 rows remaining after removing customers who were surveyed more than once and not yet surveyed
# 2476 unique customers remaining in the data set.

# Part G
# a. find out min of months since survey of customers
remove(subset_g)
subset_g<-data.frame(hw_f%>%
                    filter(months_since_survey==0 | months_since_survey==-1)%>%
                    group_by(cust_id)%>%
                    summarise(Inv_1M_Bef=total_investments,min_months=months_since_survey))
# remove customers min=0 / those who were not present 1 month before survey
subset_g<-subset_g[!c(subset_g$min_months==0),]
# 2,158 customers who have atleast 1 month of data before surveying
# 2476-2158 = 314 customers were not present at least one month before survey

# b. identify customers max months
table1<-data.frame(hw_f%>%
                  filter(months_since_survey>=0 & months_since_survey<=3)%>%
                  group_by(cust_id)%>%
                  summarise(Inv_3M_Aft=total_investments,months_since_survey, max.month=max(months_since_survey)))
# add a calculated field to identify customers who left before 3 months 
table1$calcfield<-ifelse((table1$max.month<3),"left before 3","stayed")
nrow(table(subset(table1$cust_id,table1$calcfield=="left before 3")))
# 107 customers in the data set who left less than 3 months (0-2 months) after being surveyed. 
# remove duplicates for MSS<3 for customers who stayed beyond 3 months
table1$remove<-ifelse((table1$max.month==3 & table1$months_since_survey<3),"remove","keep")
table1<-table1[!c(table1$remove=="remove"),]
# set inv=0 for customers who left before 3 months.
table1$Inv_3M_Aft[table1$calcfield=="left before 3"]<-0
# remove duplicates to get 1 row per customer
merge_g2<-table1%>%
             group_by(cust_id)%>%
             summarise(Inv_3M_Aft=sum(Inv_3M_Aft))
# 2476 customers in this dataset

# c. extract cust id & first satis survey response of each customer
merge_g3<-data.frame(hw_f%>%
                      group_by(cust_id)%>%
                      filter(months_since_survey==0)%>%
                      summarise(satis_survey,months_since_survey))
merge_g3[,-3]
# create a frequency table of satis survey responses
frequency.table<-table(merge_g3$satis_survey)
frequency.table

# d. create final table with one row per customer
remove(final.cust.table)
final.cust.table<-Reduce(merge,list(merge_g1,merge_g2,merge_g3))
# 2158 customers in the final data set.

# calculate investment change
final.cust.table$Inv_Chg<-final.cust.table$Inv_3M_Aft-final.cust.table$Inv_1M_Bef

#calculate avg change in inv dollars for customers with satis rating 1&2
Avg_dissatisfied<-round(mean(subset(final.cust.table$Inv_Chg,satis_survey<=2)),2)
Avg_dissatisfied
# mean change in inv of dissatisfied customers is $627.15
# calculate avg change in inv dollars for customers who rate 4 & 5
Avg_satisfied<-round(mean(subset(final.cust.table$Inv_Chg,satis_survey>=4)),2)
Avg_satisfied
# Avg chg in inv of satisfied customers after survey is $877.54
# calculate difference in avg change in inv for satisfied and dis-satsified customers
Mean_diff<-Avg_satisfied-Avg_dissatisfied 
Mean_diff
# the difference between the mean values is $1504.69
# satisfied customers are investing $1504.69 more than dissatisfied customers. 
# It is a significant number, because, if all 860 dissatisfied customers invested $1504.69 more, it will bring $1.2m additional revenue

getwd()
data<-read.table("hw3_data.txt",sep="\t",header=TRUE)

# Part a
# standardize the variables

library(cluster)
library(NbClust)
library(factoextra)

# standardize numbers
x<- data[,c("Inv_1M_Bef","cust_age","cust_tenure","tottrans")]
x<-as.matrix(x)
x.scaled<-scale(x)

# part b
# Elbow plot
fviz_nbclust(x=x.scaled, FUNcluster = kmeans, nstart=1000, method="wss", k.max = 20) + 
  labs(title="Optimal Number of Clusters: Elbow Plot") + 
  coord_cartesian(ylim=c(0,8000)) + geom_line(size=2)
# optimum number of clusters is five as per the elbow plot

# part c
# 20 approaches to determine optimal clusters
nb <- NbClust(x.scaled, distance="euclidean", min.nc=2, max.nc=20, method="kmeans")
fviz_nbclust(nb)

# part d
# cluster analysis
set.seed(123)
results <- kmeans(x.scaled, centers=5, iter.max=1000, nstart=1000)
results 

# examine cluster centers
(round(results$centers,4))

# remap cluster numbers basde on cluster centers
Old.Clust.Num <- results$cluster
New.Clust.Num<- ifelse(Old.Clust.Num==1,2,ifelse(Old.Clust.Num==2,1,
                    ifelse(Old.Clust.Num==3,4,ifelse(Old.Clust.Num==4,5,3))))

# frequency counts of customers in each cluster based on New.Clust.Num 
table(New.Clust.Num)

# add new columns to data frame
data$client.cluster<-New.Clust.Num

# calculate averages based on new cluster assignment on the raw data
avg.Inv_1M_Bef<-round(aggregate(data$Inv_1M_Bef,by=list(New.Clust.Num),FUN=mean),2)
colnames(avg.Inv_1M_Bef)<-c("Cluster","Avg.Inv_1M_Bef")
avg.cust_age<-round(aggregate(data$cust_age,by=list(New.Clust.Num),FUN=mean),2)
avg.cust_tenure<-round(aggregate(data$cust_tenure,by=list(New.Clust.Num),FUN=mean),2)
avg.tottrans<-round(aggregate(data$tottrans,by=list(New.Clust.Num),FUN=mean),2)

# create a table of averges of 4 variables by cluster numbers
table1<-cbind(avg.Inv_1M_Bef,avg.cust_age[,-1],avg.cust_tenure[,-1],avg.tottrans[,-1])
colnames(table1)<-c("Cluster.number","Avg.Inv_1M_Bef","Avg.cust_age","Avg.cust_tenure","Avg.tottrans")
table1

# calulate avg change by inv category and cluster
avg.change<- aggregate(data$Inv_Chg,by=list(data$client.cluster,data$categ),FUN=mean)
colnames(avg.change)<-c("cluster#","category","Inv_change")
avg.change
good.exp<- round(avg.change[c(avg.change$categ=="good"),3],2)
bad.exp<-round(avg.change[c(avg.change$categ=="bad"),3],2)
good.exp-bad.exp

# create a table in the required format
library(tidyr)
table2<-spread(avg.change,category,Inv_change)
table2$Average_diff<-good.exp-bad.exp
colnames(table2)<-c("New cluster number","Average Inv_chg Bad","Average Inv_chg Good",
                    "Average Inv_chg Good-Average Inv_chg Bad")
table2

# import 12000 customers file 
data2<-read.table("12000 new customers.txt",sep="\t",header=T)

# standardize numbers
std2_Inv_1M_Bef = round((data2$Inv_1M_Bef - mean(data2$Inv_1M_Bef))/ sd(data2$Inv_1M_Bef),4)
std2_cust_age = round((data2$cust_age - mean(data2$cust_age))/ sd(data2$cust_age),4)
std2_cust_tenure = round((data2$cust_tenure - mean(data2$cust_tenure))/ sd(data2$cust_tenure),4)
std2_tottrans = round((data2$tottrans - mean(data2$tottrans))/ sd(data2$tottrans),4)
y.scaled<-cbind(std2_Inv_1M_Bef,std2_cust_age,std2_cust_tenure,std2_tottrans)

# remap cluster centers to new cluster numbers
remove(centers.table)
centers.table<-data.frame(round(results$centers,4))
centers.table$new.cluster.number<-ifelse(centers.table$Inv_1M_Bef==-0.1253,2,
                                         ifelse(centers.table$Inv_1M_Bef==-0.1149,1,
                                                ifelse(centers.table$Inv_1M_Bef==-0.1817,3,
                                                       ifelse(centers.table$Inv_1M_Bef==-0.2259,4,5))))
centers.table<-centers.table[order(centers.table[,5]),]
clusters.euc.dist<-as.matrix(centers.table[,-5])

# calculate euclidean distance from cluster centers
cluster1<-as.matrix(c(-0.1149,0.0644,1.7917,0.0647))
remove(euc.dist.1)
euc.dist.1<-NULL
for (i in 1:nrow(y.scaled)){
        temprow<-y.scaled[i,]
        x<-cluster1[1:4,]
        ED<-sqrt(sum(temprow-x)^2)
        euc.dist.1[i]<-ED
}
euc.dist.1

cluster2<-as.matrix(clusters.euc.dist[2,])
euc.dist.2<-NULL
for (i in 1:nrow(y.scaled)){
  temprow<-y.scaled[i,]
  x<-cluster2[1:4,]
  ED<-sqrt(sum(temprow-x)^2)
  euc.dist.2[i]<-ED
}
euc.dist.2

cluster3<-as.matrix(clusters.euc.dist[3,])
euc.dist.3<-NULL
for (i in 1:nrow(y.scaled)){
  temprow<-y.scaled[i,]
  x<-cluster3[1:4,]
  ED<-sqrt(sum(temprow-x)^2)
  euc.dist.3[i]<-ED
}
euc.dist.3

cluster4<-as.matrix(clusters.euc.dist[4,])
euc.dist.4<-NULL
for (i in 1:nrow(y.scaled)){
  temprow<-y.scaled[i,]
  x<-cluster4[1:4,]
  ED<-sqrt(sum(temprow-x)^2)
  euc.dist.4[i]<-ED
}
euc.dist.4

cluster5<-as.matrix(clusters.euc.dist[5,])
euc.dist.5<-NULL
for (i in 1:nrow(y.scaled)){
  temprow<-y.scaled[i,]
  x<-cluster5[1:4,]
  ED<-sqrt(sum(temprow-x)^2)
  euc.dist.5[i]<-ED
}
euc.dist.5

# create a matrix of euclidean distances from 5 cluster centers for all customers
remove(matrix1)
matrix1<-cbind(euc.dist.1,euc.dist.2,euc.dist.3,euc.dist.4,euc.dist.5)

# calculate min euclidean distance for each customer
library(matrixStats)
ed.min<-rowMins(matrix1)
ed.min[1:5]
matrix1[1:5,]

# identify the cluster based on the min educlidean distance
matrix1<-cbind(matrix1,ed.min)
cluster.new.no<-ifelse(ed.min-euc.dist.1==0,1,ifelse(ed.min-euc.dist.2==0,2,
                ifelse(ed.min-euc.dist.3==0,3,ifelse(ed.min-euc.dist.4==0,4,5))))
matrix1<-cbind(matrix1,cluster.new.no)
matrix1[1:5,]

# table showing number of customers in each cluster
table(cluster.new.no)

# flag customers
no.of_cutomers.at.risk=2753+4759
no.of_cutomers.at.risk
# Based on part F, clusters 2&3 lose more money due to bad cust service.Ie., clusters 2&3 are top two clusters
# 7512 customers are in the clusters 2&3 together

# % of customers at high risk of taking out money
percnt.at.risk<-no.of_cutomers.at.risk/12000
percnt.at.risk
# 62.6% of customers are at high risk of taking out money due to bad exp

# expected amount of dollars lost for these customers
# based on part f, amount of money the firm will lose from clusters 2&3 due to bad service is 4551 & 1883 respectively
# expected amount of money lost = no.of customers in the cluster * inv change good-bad
expected.loss<-(2753*4551)+(4759*1883)
expected.loss
# the firm might lose an estimated $21.49 millions due to bad service.




