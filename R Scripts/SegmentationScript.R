##############################################################################
# Segmentation script - efood 
# created by: Thiseas Papadopoulos
# date: April 2021
##############################################################################

# clear all variables in workspace
rm(list=ls())


#############################################
# specify local path where R libraries lie
# Thiseas
LibrariesPath = "C:/RLibrary"
.libPaths("C:/RLibrary" )
#############################################
#### load libraries ####
loadpackages = c("data.table","stringr","bigrquery","DBI","openxlsx")
lapply(loadpackages,FUN = require,character.only = TRUE,lib.loc = LibrariesPath)


#### set working directory
setwd("C:/Users/tpapadop1/Desktop/assignment")

#### list files
list.files()


## set up connection with Google cloud: BigQuery
projectid = 'bi-2019-test'
datasetid = 'ad_hoc'
bq_conn = dbConnect(bigquery(), 
                      project = projectid,
                      dataset = datasetid, 
                      use_legacy_sql = FALSE
)

# List all the tables in BigQuery data set
bigrquery::dbListTables(bq_conn)
# 3 tables¨"bi_dummy_orders"  "business_intelligence_analyst_2019_restaurants" "orders_jan2021"    
# we are interested in orders_jan2021    

# set up the sql query string 
# need to cast order_id as string in order to fetch it correctly
sql_query =  "select cast(order_id as STRING) as order_id, brand, submit_dt, user_id, shop_id, city, cuisine_parent, basket from `bi-2019-test.ad_hoc.orders_jan2021`;"

# submit query
orders21 = bq_project_query(projectid, sql_query)
#orders21b = query_exec(sql_query, projectid, use_legacy_sql = FALSE)
#orders21c = bq_perform_query(sql_query, projectid, use_legacy_sql = FALSE)


# store results into a DT
orders21DT = as.data.table(bq_table_download(orders21))


### check dataset - data quality inspection
str(orders21DT)
orders21DT[,.N] # 400k rows
orders21DT[is.na(order_id)] # no blank orders
orders21DT[is.na(user_id)] # no blank users
orders21DT[is.na(city)] # no blank city
orders21DT[is.na(basket)] # no blank basket
orders21DT[is.na(submit_dt)] # no blank date
orders21DT[is.na(cuisine_parent)] # no blank cuisine
##############  no blank values 

### additional checks
orders21DT[,max(basket)] # max basket: 151.85 euros
orders21DT[,min(basket)] # min basket: 0 euros
orders21DT[,mean(basket)] # avg basket: 8.72 euros
hist(orders21DT$basket) # as expected majority of orders lie within the 1-10 amount space
orders21DT[basket>50] # orders with more than 50 euros basket total
orders21DT[,.SD[which.max(basket)]] # order with max basket value, city: Irakleio, cuisine: Ethnic


### lets have a better look at orders with zero basket amount
orders21DT[basket==0] # just 9 orders  from 2 cities: Kavala, Veroia
orders21DT[basket==0, .N, by =.(user_id)] # user_id=38260132  appears twice in zero amount orders
orders21DT[basket==0, .N, by =.(shop_id)] # shop_id=387155373  appears 6 times in zero amount orders: i am guessing it's offer-related orders or the users had discount codes


#### identify most popular and highest spend by cuisine and city
orders21DT[, .(TotalAmount=sum(basket)), by=cuisine_parent] # meat cuisine the highest revenue
orders21DT[, .N, by=cuisine_parent] # breakfast comes 1st, meat 2nd

orders21DT[, .(TotalAmount=sum(basket)), by=city] # Irakleio comes 1st, Patra 2nd
orders21DT[, .N, by=city] # Patra comes 1st, Irakleio 2nd

### how many unique values?
length(unique(orders21DT$order_id)) #400k unique order ids
length(unique(orders21DT$user_id)) #162954 unique user ids
length(unique(orders21DT$city)) # 91 different cities
length(unique(orders21DT$cuisine_parent)) # 9 different cuisines


####################################################################################################
### calculate transactions and amount spent per customer. we need that for the customer segmentation
orders21DTperUserID = orders21DT[,.(TotalOrders=.N, TotalAmount=sum(basket)) ,by=.(user_id)]
hist(orders21DTperUserID$TotalAmount) 
#identify highest spender
orders21DTperUserID[,.SD[which.max(TotalAmount)]] # 449 euros with 4 orders
orders21DTperUserID[, head(.SD, 3), by=TotalAmount]

#identify most frequent spender
orders21DTperUserID[,.SD[which.max(TotalOrders)]] # 33 orders and spent 202 euros


orders21DT[,.N,by=.(user_id)][N>=2] # identify users with 2 or more orders in Jan - 
orders21DT[,.N,by=.(user_id)][N==1] # 77.3k users appeared once in Jan

### lets identify the unique combos of user id and cuisine
unique(orders21DT[,c('user_id','city','cuisine_parent')]) # 255k unique combos
unique(orders21DT[,c('user_id','city')]) # 165k (more than the 162.954 users) unique combos of user and city - this means that some of the users have ordered from different cities

### we need to answer the business question: which segment could be a valuable target for a marketing campaign about Coffee (Breakfast)
### create a flag per id, to identify the ones related to breakfast
orders21DT[,Breakfast:=0]
orders21DT[cuisine_parent=="Breakfast", Breakfast:=1]

### next step is to flag users involved or not in breakfast orders regardless of their other preferences
CustomersandBreakfastOrders = orders21DT[,.SD[which.max(Breakfast)], by=.(user_id), .SD = c("Breakfast")] 
CustomersandBreakfastOrders[Breakfast==1] # 70k users

#orders21DT[Breakfast==1,.N]


## "2021-01-01" to "2021-01-31"
#### dataset date range - convert to date
orders21DT[, submit_dt_updated:=as.Date(submit_dt)]
orders21DT[, range(submit_dt_updated)]



#############################################################
###################### 1st attempt for segmentation #########
#############################################################

### perform feature scaling although the two variables are on similar scale
ScaledDataset= as.data.table(scale(orders21DTperUserID[,-"user_id"]))

##### try k-means clustering using frequency and order value
# Using the elbow method to find the optimal number of clusters
set.seed(6) # for results reproduction
wcss = vector()
for (i in 1:6) wcss[i] = sum(kmeans(ScaledDataset, i)$withinss)
plot(1:6,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

### choose 4 clusters
ClusterResults_kmeans = kmeans(ScaledDataset,4)
y_kmeans = data.table(Cluster = as.character(ClusterResults_kmeans$cluster))

### summary of segments
ClusterSummary_kmeans = y_kmeans[,.(Count = .N), by = .(Cluster)][, Perc := 100*round(Count/sum(Count),2)][]

### bring segment allocation to users
FullDatasetWithCluster_kmeans = cbind(y_kmeans,orders21DTperUserID)
FullDatasetWithCluster_kmeans[, .(Count = .N , AvgOrders = mean(TotalOrders), AvgAmount = mean(TotalAmount)), by = .(Cluster)]
### most crowded segment is segment 3, containing infrequent users with 1-2 orders and low spenders 
### segment 1 is the less crowded, most frequent customers and higest spenders

#### export results 
write.xlsx(x = list(DatasetCustomerLevel_kmeans = FullDatasetWithCluster_kmeans),file = paste0(getwd(),"/DatasetCustomerLevel_kmeans.xlsx")) 
#### Power BI dataset
write.xlsx(x = list(Orders2021 = orders21DT),file = paste0(getwd(),"/Orders2021.xlsx")) 



save(list = ls(.GlobalEnv), file = "efoodv2.Rdata")

#############################################################
###################### 2nd attempt for segmentation #########
######## consider the business question we need to answer ###
###### incorporate the breakfast buyers dimension  ##########
#############################################################


# bring breakfast flag
DatasetforClusteringFinal_kproto = orders21DTperUserID[CustomersandBreakfastOrders , on ="user_id"]
DatasetforClusteringFinal_kproto[,  Breakfast:= as.factor(Breakfast)]

ScaledDataset_kproto= as.data.table(scale(DatasetforClusteringFinal_kproto[,-c("user_id","Breakfast")]))
Final_ScaledDataset_kproto = cbind(ScaledDataset_kproto,DatasetforClusteringFinal_kproto[,.(Breakfast)])
# we will use k-proto algorithm
# K-Prototypes is a k-means variation appropriate when working with mixed data types
# Using the elbow method to find the optimal number of clusters

library("caTools", lib.loc = LibrariesPath)
library("clustMixType", lib.loc = LibrariesPath) 


set.seed(6)
wcss = vector()
for (i in 1:6) wcss[i] = sum(kproto(Final_ScaledDataset_kproto, i)$withinss)
plot(1:6,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')



ClusterResults_kproto = kproto(x = Final_ScaledDataset_kproto, 4)

y_kproto = data.table(Cluster = as.character(ClusterResults_kproto$cluster))

### summary of segments
ClusterSummaryProto = y_kproto[,.(Count = .N), by = .(Cluster)][, Perc := 100*round(Count/sum(Count),2)][]

FullDatasetWithCluster_kproto = cbind(y_kproto,DatasetforClusteringFinal_kproto)
FullDatasetWithCluster_kproto[,.(Count = .N), by = .(Breakfast,Cluster)]
FullDatasetWithCluster_kproto[, .(Count = .N , AvgOrders = mean(TotalOrders), AvgAmount = mean(TotalAmount)), by = .(Cluster)]

#### export results - Power BI dataset
write.xlsx(x = list(DatasetCustomerLevel_kproto = FullDatasetWithCluster_kproto),file = paste0(getwd(),"/DatasetCustomerLevel_kproto.xlsx")) 

##################################################################################################################################################
