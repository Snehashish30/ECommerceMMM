
# E-COMMERCE CAPSTONE PROJECT

#--------------------- Exploratory Data Analysis ---------------------#

#Loading neccessary libraries
load.libraries <- c('lubridate',	'reshape2',	'zoo',	'ggplot2',	'MASS',	'car',	'cowplot',
                    'DataCombine',	'rpart',	'rpart.plot',	'DAAG',	'randomForest',	'dplyr')
install.libs <- load.libraries[!load.libraries %in% installed.packages()]

for (libs in install.libs) 
  install.packages(libs, dependencies = TRUE)

sapply(load.libraries, require, character = TRUE)

#---------------------------------------------------------------------#



rawdata <- read.csv("ConsumerElectronics.csv")
productlist <- read.csv("ProductList.csv")
investments <- read.csv("Investment.csv")
nps_Score <- read.csv("NPS.csv")

length(which(rawdata$product_analytic_vertical == "\\N"))

sapply(rawdata, function(x) length(which(x == "\\N")))
#Here \N is for NULL
# We have '\N' symbol for columns 
# deliverybdays 1312972,
# deliverycdays 1312971,
# product_analytic_vertical 5828
# since we have so many null in deliverybdays and deliverycdays, we will drop the columns

rawdata$product_analytic_vertical[which(rawdata$product_analytic_vertical=="\\N")] <- NA

sum(is.na(rawdata$product_analytic_vertical))

# Change the NA values from investments to 0
investments[is.na(investments)]<- 0

# renaming the column names for readability:
colnames(rawdata) <- c("FSN_ID","Order_Date","Year","Month","Order_ID","Order_Item_ID","GMV","Units","DeliverybDays",
                       "DeliverycDays","Payment_Type","SLA","Cust_ID","Pincode","Product_Super_Category","Product_Category",
                       "Product_Sub_Category","Product_Vertical","Product_MRP","Product_Procurement_SLA")
# Check for NA values
# Following columns have NA values
#   GMV       Cust_ID     Pincode
#   4904       4904        4904

#removing the rows with NA values since they are very few and would not impact the overall results
rawdata <- na.omit(rawdata)


head(rawdata)
str(rawdata)

summary(rawdata)

# Since the data to be used is from July 2015 to June 2016, removing the orders which is earlier than July 2015 and
# orders placed after June 2016
rawdata <- subset(rawdata,(Year==2015 & Month>=7) | (Year==2016 & Month<=6))
rawdata <- rawdata%>%filter(Product_Sub_Category %in% c("CameraAccessory","HomeAudio","GamingAccessory"))


#Creating a new Date Column for order date
rawdata$Date <- date(rawdata$Order_Date)
#finding the week of the year in which the Order was placed
rawdata$order_week <- week(rawdata$Date)

#rawdata$order_week <- paste(rawdata$Year,rawdata$order_week, sep="-")

levels(as.factor(rawdata$order_week))
week(date("2016-06-30"))
week(date("2015-07-01"))
week(date("2015-12-31"))
# Jan 2016 is coming as week 1. Ideally it should be week 54 as the last week in year 2015 is week 53
# Also both 30 jun 2016 and 1st july appear as week 26.

rawdata$order_week <- ifelse(rawdata$Year==2016 & rawdata$order_week<=26, rawdata$order_week+53, rawdata$order_week)
#If the  days to get item or order from warehouse for shipping(deliverybdays)+  
#days to get item or order from warehouse for shipping(deliverycdays) is more than the SLA, we will mark it as SLA Breach




#There are 1349 items where GMV is 0. 
#Since GMV is total revenue, such products may be considered as free products
length(which(rawdata$GMV == 0))

# Revenue(GMV) should ideally multiplication of Product units and MRP
# but it is coming less as well. meaning we have given discount on those products
# there are 38558 orders where GMV(revenue) is more that MRP*units

rawdata$gmv_cal <-ifelse(rawdata$GMV<=rawdata$Product_MRP*rawdata$Units, "Equal or Less", "More than gmv")
length(which(rawdata$gmv_cal=="More than gmv"))
length(which(rawdata$gmv_cal=="Equal or Less"))

#Considering only those records where GMV is equal or less than the product of units and MRP
rawdata <- subset(rawdata, gmv_cal=="Equal or Less")

#Delete the column after getting the subset
rawdata$gmv_cal <- NULL

#Creating new columns - List Price & Discount Offered based on the above calculation
rawdata$List_Price <- rawdata$GMV/rawdata$Units
rawdata$Discount_Percent <- round((rawdata$Product_MRP-rawdata$List_Price)*100/rawdata$Product_MRP, 0)

#replacing \\N from DeliverybDays and DeliverycDays with 0
rawdata$DeliverybDays[which(rawdata$DeliverybDays=="\\N")] <- 0
rawdata$DeliverycDays[which(rawdata$DeliverycDays=="\\N")] <- 0


rawdata$DeliverybDays <- as.numeric(as.character(rawdata$DeliverybDays))
rawdata$DeliverycDays <- as.numeric(as.character(rawdata$DeliverycDays))
rawdata$SLA_Breach <- ifelse((rawdata$DeliverybDays+rawdata$DeliverycDays) > rawdata$SLA, 1, 0)

#Removing NA values from Radio and other investments with 0
investments$Other[which(is.na(investments$Other))] <- 0
investments$Radio[which(is.na(investments$Radio))]<-0


#5.Holiday_column

holiday_list<-c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15",
                "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-10-11","2015-10-12","2015-11-13",
                "2015-11-14","2015-12-25","2015-12-26","2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02","2016-02-20","2016-02-21","2016-02-14",
                "2016-02-15","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27")

holiday_list <- as.Date(holiday_list)
order_week <- week(holiday_list)
holiday_year <- year(holiday_list)
order_week <- ifelse(holiday_year==2016 & order_week<=26,order_week+53,order_week)
holiday_df <- data.frame(order_week,holiday_year)


#holiday_df$order_week <- paste(holiday_df$holiday_year,holiday_df$order_week, sep="-")
holiday_df$holiday_freq<-1

# Payment type analysis
rawdata$prepaid_orders <- ifelse(rawdata$Payment_Type=="Prepaid",1,0)
rawdata$COD_orders <- ifelse(rawdata$Payment_Type=="COD",1,0)
########################################################################################################################################################################

############## Applying K- Means to cluster the products ##############
RFM_Data_analysis <- rawdata[,c("FSN_ID","Order_Date","Order_ID","Cust_ID","Product_MRP","Units","order_week","Product_Vertical")]
RFM_Data_analysis <- RFM_Data_analysis[order(RFM_Data_analysis$FSN_ID),]

## Making RFM data

monetary <- aggregate(Product_MRP~FSN_ID, RFM_Data_analysis, mean)

frequency <- RFM_Data_analysis[,c(2,1)]

k <- table(as.factor(frequency$FSN_ID))

k <- data.frame(k)


colnames(k)[1] <- c("FSN_ID")


recency <- RFM_Data_analysis[, c(2,1)]
recency$Order_Date <- strptime(x = as.character(recency$Order_Date), format = "%Y-%m-%d %H:%M")


maximum <- max(recency$Order_Date)

maximum <- maximum+1

#maximum$diff <- maximum-recency$Order_Date

recency$diff <- maximum-recency$Order_Date#maximum$diff

df<-aggregate(recency$diff, by=list(recency$FSN_ID), FUN="min")

colnames(df)[1] <- "FSN_ID"

colnames(df)[2] <- "Recency"

RFM <- merge(monetary, k, by = ("FSN_ID"))
RFM <- merge(RFM, df, by = ("FSN_ID"))
RFM$Recency <- as.numeric(RFM$Recency)
prod_vertical <- RFM_Data_analysis[,c("FSN_ID","Product_Vertical")]
prod_vertical <- unique(prod_vertical)
RFM <- merge(RFM, prod_vertical, by = ("FSN_ID"))

## Outlier treatment
unique(prod_vertical$Product_Vertical)
#loop and remove outliers for each product vertical levels
p <- unique(RFM$Product_Vertical)
p[which(p == "\\N")]
data <- RFM[0,]
data$ptag <- NULL
data$ptag_correct <- NULL
length(p)

for(i in 1:length(p)){
  dataset_grp <- RFM%>%filter(Product_Vertical == p[i])
  # we can remove outliers or check without removal to find the best cluster results 
  RFM_norm1 <- dataset_grp
  #if there are less than 3 SKUs then there is no question of mass and premium, and we should treat it as mass product only, as everyone will have to purchase it
  if(nrow(RFM_norm1)<=3 && nrow(RFM_norm1)>0){
    print(dataset_grp$Product_Vertical)
    RFM_norm1$ptag <- 3
  }
  else if (nrow(RFM_norm1)>0){
    RFM_norm1$Product_MRP <- scale(RFM_norm1$Product_MRP)
    RFM_norm1$Freq <- scale(RFM_norm1$Freq)
    RFM_norm1$Recency <- scale(RFM_norm1$Recency)
    ## Implementing K-Means algorithm
    
    clus3 <- kmeans(RFM_norm1[,c(-1,-5)], centers = 3, iter.max = 50, nstart = 50)
    
    dataset_grp$ptag <- clus3$cluster
    
    # manual inspection showed the insight
    #temp <- dataset_grp%>%group_by(ptag)%>%summarise(avg_monetary = mean(Product_MRP),avg_freq = mean(Freq),avg_rec = mean(Recency))
    # low frequent products are premium, very high freq products are mass, rest medium
    RFM_norm1$ptag <- as.factor(clus3$cluster)
    RFM_norm1 <- RFM_norm1[,c("FSN_ID","ptag")]
    
  }
  RFM_norm1 <- RFM_norm1[,c("FSN_ID","ptag")]
  RFM1 <- merge(RFM,RFM_norm1,by = ("FSN_ID"))
  RFM1$ptag_correct <- NA
  if(nrow(RFM1%>%filter(ptag == 1))>0){
    temp <- RFM1%>%filter(ptag == 1)
    ptag1_medFreq <- median(temp$Freq)
  }
  else{
    ptag1_medFreq<-0
  }
  if(nrow(RFM1%>%filter(ptag == 2))>0){
    temp <- RFM1%>%filter(ptag == 2)
    ptag2_medFreq <- median(temp$Freq)
  }
  else{
    ptag2_medFreq<-0
  }  
  if(nrow(RFM1%>%filter(ptag == 3))>0){
    temp <- RFM1%>%filter(ptag == 3)
    ptag3_medFreq <- median(temp$Freq)
  }
  else{
    ptag3_medFreq<-0
  }
  
  num<- unique(c(ptag1_medFreq,ptag2_medFreq,ptag3_medFreq))
  if(length(num) == 3){
    tags <- data.frame(c(1,2,3),c(ptag1_medFreq,ptag2_medFreq,ptag3_medFreq))
    colnames(tags) <- c("tags","tag_freq")
    tags_order <- tags%>%arrange(desc(tag_freq))
    RFM1[which(RFM1$ptag ==tags_order[1,1]),"ptag_correct"] <- "Mass"
    RFM1[which(RFM1$ptag ==tags_order[2,1]),"ptag_correct"] <- "Aspiring"
    RFM1[which(RFM1$ptag ==tags_order[3,1]),"ptag_correct"] <- "Premium"
  }
  if(length(num) == 2){
    tags <- data.frame(c(1,2,3),c(ptag1_medFreq,ptag2_medFreq,ptag3_medFreq))
    colnames(tags) <- c("tags","tag_freq")
    tags_order <- tags%>%arrange(desc(tag_freq))
    RFM1[which(RFM1$ptag ==tags_order[1,1]),"ptag_correct"] <- "Mass"
    RFM1[which(RFM1$ptag ==tags_order[2,1]),"ptag_correct"] <- "Premium"
  }
  if(length(num) == 1){
    tags <- data.frame(c(1,2,3),c(ptag1_medFreq,ptag2_medFreq,ptag3_medFreq))
    colnames(tags) <- c("tags","tag_freq")
    tags_order <- tags%>%arrange(desc(tag_freq))
    RFM1[which(RFM1$ptag ==tags_order[1,1]),"ptag_correct"] <- "Mass"
  }
  data <- rbind(data,RFM1)
}

# clustering complete 
t <- data%>%group_by(Product_Vertical,ptag_correct)%>%summarise(avg_price = median(Product_MRP),
                                                        avg_freq = median(Freq),
                                                        avg_rec = median(Recency),
                                                        cnt = length(Product_Vertical))%>%arrange(Product_Vertical,avg_freq)

View(t)


# box <- boxplot.stats(RFM$Product_MRP)
# out <- box$out
# 
# RFM1 <- RFM[ !RFM$Product_MRP %in% out, ]
# 
# RFM <- RFM1
# 
# box <- boxplot.stats(RFM$Freq)
# out <- box$out
# 
# RFM1 <- RFM[ !RFM$Freq %in% out, ]
# 
# RFM <- RFM1
# 
# box <- boxplot.stats(RFM$Recency)
# out <- box$out
# 
# RFM1 <- RFM[ !RFM$Recency %in% out, ]
# 
# RFM <- RFM1
# 
# ## Standardisation of data
# 
# RFM_norm1 <- RFM[,-1]
# 
# RFM_norm1$Product_MRP <- scale(RFM_norm1$Product_MRP)
# RFM_norm1$Freq <- scale(RFM_norm1$Freq)
# RFM_norm1$Recency <- scale(RFM_norm1$Recency)
# 
# ## Implementing K-Means algorithm
# 
# clus3 <- kmeans(RFM_norm1, centers = 3, iter.max = 50, nstart = 50)
# 
# RFM <- cbind(RFM, clus3$cluster)
# 
# colnames(RFM)[5] <- "ClusterID"
# 
# RFM <- RFM[, c("FSN_ID","ClusterID")]
# 

#Merge the clusters with original data

rawdata<- merge(rawdata, RFM, by="FSN_ID", all.x=TRUE)
master_rawdata <- rawdata
rawdata <- rawdata%>%left_join(data[,c(1,7)], by = c("FSN_ID"))

colnames(rawdata)[28] <- "ProductSellCategory"

#Delete the temporarily used data frames
rm(RFM1)
rm(RFM_norm1)
rm(RFM_Data_analysis)
rm(k)
rm(recency)
rm(frequency)
rm(monetary)
rm(box)
rm(df)
rm(clus3)
rm(temp)
rm(tags)
rm(tags_order)
#The out liers removed while creating RFM data, when merged into original data, the corresponding ClusterID would be NA. Assign those records with the clusterID as 4 which corresponds to NA Data

# rawdata$ProductSellCategory[which(is.na(rawdata$ProductSellCategory))]<-4
# 
# #change the clusterID~productSellCategory to factor type
# rawdata$ProductSellCategory <- as.factor(rawdata$ProductSellCategory)
# 
# 
# rawdata$ProductSellCategory_1 <- ifelse(rawdata$ProductSellCategory ==1 ,1,0)
# rawdata$ProductSellCategory_2 <- ifelse(rawdata$ProductSellCategory ==2 ,1,0)
# rawdata$ProductSellCategory_3 <- ifelse(rawdata$ProductSellCategory ==3 ,1,0)
# rawdata$ProductSellCategory_4 <- ifelse(rawdata$ProductSellCategory ==4 ,1,0)
# 
# #delelte the main column after creating the dummy columns out of it
# rawdata$ProductSellCategory <- NULL
########################################################################################################################################################################

# Merging data from different files

rawdata$holiday_flg <- ifelse(is.na(match(rawdata$order_week,holiday_df$order_week)),0,1)
orders<- merge(rawdata,nps_Score, by.x=c("Year","Month"))
orders <- merge(orders,productlist, by.x = c("Product_Vertical"), by.y = c("ï..Product"))

#Outlier treatment

#GMV
quantile(orders$GMV, seq(0,1,0.01))
#We can see there is major difference between 99% (30750) and 100% (226947)
orders$GMV[orders$GMV>30750] <- median(orders$GMV)

#Product_MRP
quantile(orders$Product_MRP, seq(0,1,0.01))
#We can see there is major difference between 99% (46950) and 100% ( 299999)
orders$Product_MRP[orders$Product_MRP>46950] <- median(orders$Product_MRP)

#SLA
quantile(orders$SLA, seq(0,1,0.01))
#We can see there is major difference between 99% (13) and 100% ( 1006)
orders$SLA[orders$SLA>13] <- median(orders$SLA)

#Product_Procurement_SLA
quantile(orders$Product_Procurement_SLA, seq(0,1,0.01))
#We can see there is major difference between 99% (13) and 100% ( 1000)
orders$Product_Procurement_SLA[orders$Product_Procurement_SLA>13] <- median(orders$Product_Procurement_SLA)


#List_Price
quantile(orders$List_Price, seq(0,1,0.01))
#We can see there is major difference between 99% (29990.00) and 100% (226947)
orders$List_Price[orders$List_Price>29990] <- median(orders$List_Price)

#Units
quantile(orders$Units, seq(0,1,0.01))
#We can see there is major difference between 99% (2) and 100% (50)
orders$Units[orders$Units>2] <- median(orders$Units)

summary(orders)

CameraAccessory_df <- subset(orders, Product_Sub_Category=="CameraAccessory")
HomeAudio_df <- subset(orders, Product_Sub_Category=="HomeAudio")
GamingAccessory_df <- subset(orders, Product_Sub_Category=="GamingAccessory")

# identify Primary KPIs 
# Start of Aggregation
holiday_aggr <- aggregate(holiday_freq~order_week,holiday_df,FUN = sum)

kpis<-function(df){
  
  Prod_Aggr<- aggregate(cbind(List_Price,Product_MRP,SLA,Discount_Percent,Product_Procurement_SLA,NPS)~ order_week,df,FUN = mean)
  
  gmv_aggr<- aggregate(cbind(GMV,Units)~order_week,df,FUN = sum)
  
  #prodType_aggr<- aggregate(cbind(ProductSellCategory_1,ProductSellCategory_2,ProductSellCategory_3,ProductSellCategory_4, SLA_Breach)~order_week,df,FUN=sum)
  
  payType_aggr <- aggregate(cbind(prepaid_orders,COD_orders)~order_week,df,FUN=sum)
  
  #merging the various aggregates
  final_df<- merge(Prod_Aggr,gmv_aggr)
  #final_df<- merge(final_df,prodType_aggr)
  final_df<-merge(final_df,payType_aggr)
  final_df<- merge(final_df,holiday_aggr,all.x = TRUE)
  final_df$holiday_freq[is.na(final_df$holiday_freq)] <-0
  final_df<-merge(final_df,adstock, by.x="order_week",by.y="week_year")
  
  
  
  return(final_df)
  
}
adstock <- read.csv("adstockFile.csv", stringsAsFactors = F)
#We need 3 models for each product tag
CameraAccessory_df1 <- CameraAccessory_df%>%filter(ProductSellCategory == "Mass")
CameraAccessory_Mass_final <- kpis(CameraAccessory_df1)
CameraAccessory_df1 <- CameraAccessory_df%>%filter(ProductSellCategory == "Premium")
CameraAccessory_Premium_final <- kpis(CameraAccessory_df1)
CameraAccessory_df1 <- CameraAccessory_df%>%filter(ProductSellCategory == "Aspiring")
CameraAccessory_Aspiring_final <- kpis(CameraAccessory_df1)

#Gaming Accessories
GamingAccessory_df1 <- GamingAccessory_df%>%filter(ProductSellCategory == "Mass")
GamingAccessory_Mass_final <- kpis(GamingAccessory_df1)
GamingAccessory_df1 <- GamingAccessory_df%>%filter(ProductSellCategory == "Premium")
GamingAccessory_Premium_final <- kpis(GamingAccessory_df1)
GamingAccessory_df1 <- GamingAccessory_df%>%filter(ProductSellCategory == "Aspiring")
GamingAccessory_Aspiring_final <- kpis(GamingAccessory_df1)

#HomeAudio
HomeAudio_df1 <- HomeAudio_df%>%filter(ProductSellCategory == "Mass")
HomeAudio_Mass_final <- kpis(HomeAudio_df1)
HomeAudio_df1 <- HomeAudio_df%>%filter(ProductSellCategory == "Premium")
HomeAudio_Premium_final <- kpis(HomeAudio_df1)
HomeAudio_df1 <- HomeAudio_df%>%filter(ProductSellCategory == "Aspiring")
HomeAudio_Aspiring_final <- kpis(HomeAudio_df1)

#######################################################################################################################################################
#Plotting the effect of advertisment on Total sales
# CAmera Accessories
options(scipen=999)
plot_grid(
  ggplot(CameraAccessory_Mass_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(Online_marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)

plot_grid(
  ggplot(CameraAccessory_Premium_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(Online_marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(CameraAccessory_Premium_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)

# Home Audios
plot_grid(
  ggplot(HomeAudio_Mass_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(Online_marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)


# Gaming Accessory
plot_grid(
  ggplot(GamingAccessory_Mass_final,aes(TV,GMV))+geom_point()+ geom_smooth()+ labs(x = "TV Advertisment", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(Digital,GMV))+geom_point()+ geom_smooth()+ labs(x = "Digital Advertisment", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(Sponsorship,GMV))+geom_point()+ geom_smooth()+ labs(x = "Sponsorship", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(Content.Marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Content Marketing", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(Online_marketing,GMV))+geom_point()+ geom_smooth()+ labs(x = "Online Marketing", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(Affiliates,GMV))+geom_point()+ geom_smooth()+ labs(x = "Affiliates", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(SEM,GMV))+geom_point()+ geom_smooth()+ labs(x = "SEM", y = "GMV")
)

plot_grid(
  ggplot(CameraAccessory_Mass_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(HomeAudio_Mass_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(GamingAccessory_Mass_final,aes(order_week,GMV, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "GMV"),
  ggplot(CameraAccessory_Mass_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent", title="Camera Accessory"),
  ggplot(HomeAudio_Mass_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent",title="Home Audio"),
  ggplot(GamingAccessory_Mass_final,aes(order_week,Discount_Percent, fill = as.factor(ifelse(holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Is Holiday:", x = "Week", y = "Discount Percent", title="Game Accessory")
  
)
#######################################################################################################################################################
lagvar <- function(df){
  
  lag_df <- slide(data=df,Var = "List_Price", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "List_Price", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "List_Price", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "Units", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "Units", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "Units", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "Discount_Percent", slideBy = -3)
  
  
  lag_df <- slide(data=lag_df,Var = "SLA", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "SLA", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "SLA", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "Product_MRP", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "Product_MRP", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "Product_MRP", slideBy = -3)
  
  
  lag_df <- slide(data=lag_df,Var = "Product_Procurement_SLA", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "Product_Procurement_SLA", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "Product_Procurement_SLA", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "GMV", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "GMV", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "GMV", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "holiday_freq", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "holiday_freq", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "holiday_freq", slideBy = -3)
  
  
  lag_df <- slide(data=lag_df,Var = "prepaid_orders", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "prepaid_orders", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "prepaid_orders", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "COD_orders", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "COD_orders", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "COD_orders", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_1", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_1", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_1", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_2", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_2", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_2", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_3", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_3", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_3", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_4", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_4", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "ProductSellCategory_4", slideBy = -3)
  
  lag_df <- slide(data=lag_df,Var = "SLA_Breach", slideBy = -1)
  lag_df <- slide(data=lag_df,Var = "SLA_Breach", slideBy = -2)
  lag_df <- slide(data=lag_df,Var = "SLA_Breach", slideBy = -3)
  
  #lag_df<-lag_df[,-c(30,31,32,36,37,38)]
  
  
  lag_df<- na.omit(lag_df)
  
  return(lag_df)
}

MA_AVG <- function(df){
  
  df$ListPrice_MA1 <- rollmean(df$List_Price, k=2,fill=NA, align = "right")  
  df$ListPrice_MA2 <- rollmean(df$List_Price, k=3,fill=NA, align = "right")  
  df$ListPrice_MA3 <- rollmean(df$List_Price, k=4,fill=NA, align = "right")
  
  
  df$Units_MA1 <- rollmean(df$Units, k=2,fill=NA, align = "right")  
  df$Units_MA2 <- rollmean(df$Units, k=3,fill=NA, align = "right")  
  df$Units_MA3 <- rollmean(df$Units, k=4,fill=NA, align = "right")
  
  df$DiscountPercent_MA1 <- rollmean(df$Discount_Percent, k=2,fill=NA, align = "right")  
  df$DiscountPercent_MA2 <- rollmean(df$Discount_Percent, k=3,fill=NA, align = "right")
  df$DiscountPercent_MA3 <- rollmean(df$Discount_Percent, k=4,fill=NA, align = "right")
  
  df$Product_MRP_MA1 <- rollmean(df$Product_MRP, k=2,fill=NA, align = "right")  
  df$Product_MRP_MA2 <- rollmean(df$Product_MRP, k=3,fill=NA, align = "right")
  df$Product_MRP_MA3 <- rollmean(df$Product_MRP, k=4,fill=NA, align = "right")
  
  df$SLA_MA1 <- rollmean(df$SLA, k=2,fill=NA, align = "right")  
  df$SLA_MA2 <- rollmean(df$SLA, k=3,fill=NA, align = "right")
  df$SLA_MA3 <- rollmean(df$SLA, k=4,fill=NA, align = "right")
  
  df$Product_Procurement_SLA_MA1 <- rollmean(df$Product_Procurement_SLA, k=2,fill=NA, align = "right")  
  df$Product_Procurement_SLA_MA2 <- rollmean(df$Product_Procurement_SLA, k=3,fill=NA, align = "right")
  df$Product_Procurement_SLA_MA3 <- rollmean(df$Product_Procurement_SLA, k=4,fill=NA, align = "right")
  
  df$ProductSellCategory_1_MA1 <- rollmean(df$ProductSellCategory_1, k=2,fill=NA, align = "right")  
  df$ProductSellCategory_1_MA2 <- rollmean(df$ProductSellCategory_1, k=3,fill=NA, align = "right")
  df$ProductSellCategory_1_MA3 <- rollmean(df$ProductSellCategory_1, k=4,fill=NA, align = "right")
  
  df$ProductSellCategory_2_MA1 <- rollmean(df$ProductSellCategory_2, k=2,fill=NA, align = "right")  
  df$ProductSellCategory_2_MA2 <- rollmean(df$ProductSellCategory_2, k=3,fill=NA, align = "right")
  df$ProductSellCategory_2_MA3 <- rollmean(df$ProductSellCategory_2, k=4,fill=NA, align = "right")
  
  df$ProductSellCategory_3_MA1 <- rollmean(df$ProductSellCategory_3, k=2,fill=NA, align = "right")  
  df$ProductSellCategory_3_MA2 <- rollmean(df$ProductSellCategory_3, k=3,fill=NA, align = "right")
  df$ProductSellCategory_3_MA3 <- rollmean(df$ProductSellCategory_3, k=4,fill=NA, align = "right")
  
  df$ProductSellCategory_4_MA1 <- rollmean(df$ProductSellCategory_4, k=2,fill=NA, align = "right")  
  df$ProductSellCategory_4_MA2 <- rollmean(df$ProductSellCategory_4, k=3,fill=NA, align = "right")
  df$ProductSellCategory_4_MA3 <- rollmean(df$ProductSellCategory_4, k=4,fill=NA, align = "right")
  
  df$prepaid_orders_MA1 <- rollmean(df$prepaid_orders, k=2,fill=NA, align = "right")  
  df$prepaid_orders_MA2 <- rollmean(df$prepaid_orders, k=3,fill=NA, align = "right")
  df$prepaid_orders_MA3 <- rollmean(df$prepaid_orders, k=4,fill=NA, align = "right")
  
  df$COD_orders_MA1 <- rollmean(df$COD_orders, k=2,fill=NA, align = "right")  
  df$COD_orders_MA2 <- rollmean(df$COD_orders, k=3,fill=NA, align = "right")
  df$COD_orders_MA3 <- rollmean(df$COD_orders, k=4,fill=NA, align = "right")
  
  df$holiday_freq_MA1 <- rollmean(df$holiday_freq, k=2,fill=NA, align = "right")  
  df$holiday_freq_MA2 <- rollmean(df$holiday_freq, k=3,fill=NA, align = "right")
  df$holiday_freq_MA3 <- rollmean(df$holiday_freq, k=4,fill=NA, align = "right")
  
  df$SLA_Breach_MA1 <- rollmean(df$SLA_Breach, k=2,fill=NA, align = "right")  
  df$SLA_Breach_MA2 <- rollmean(df$SLA_Breach, k=3,fill=NA, align = "right")
  df$SLA_Breach_MA3 <- rollmean(df$SLA_Breach, k=4,fill=NA, align = "right")
  
  df <- na.omit(df)
  
}

########################################################################################################################################################################################
#AdStock is created in the excel itself with the decay factor of 60%, 36%, 22%, 8% and 5% is subsequent weeks
########################################################################################################################################################################################


#List of cols to be discarded for modelling lag variables

cols_to_be_removed <- c ("Units","List_Price","Product_MRP","SLA","prepaid_orders","SLA_Breach","COD_orders","holiday_freq","Product_Procurement_SLA","Discount_Percent","NPS.Score","TV","Digital","Sponsorship","Content.Marketing","Online.marketing","Affiliates","SEM","Radio","Other","ProductSellCategory_1","ProductSellCategory_2","ProductSellCategory_3","ProductSellCategory_4")

CameraAccessory_lag <- lagvar(CameraAccessory_final)
CameraAccessory_lag <- CameraAccessory_lag[,!(names(CameraAccessory_lag) %in% cols_to_be_removed)]


GamingAccessory_lag <- lagvar(GamingAccessory_final)
GamingAccessory_lag <- GamingAccessory_lag[,!(names(GamingAccessory_lag) %in% cols_to_be_removed)]

HomeAudio_lag <- lagvar(HomeAudio_final)
HomeAudio_lag <- HomeAudio_lag[,!(names(HomeAudio_lag) %in% cols_to_be_removed)]

CameraAccessory_MA <- MA_AVG(CameraAccessory_final)
CameraAccessory_MA <- CameraAccessory_MA[,!(names(CameraAccessory_MA) %in% cols_to_be_removed)]
GamingAccessory_MA <- MA_AVG(GamingAccessory_final)
HomeAudio_MA<-MA_AVG(HomeAudio_final)






#set.seed(100)
# randomly generate row indices for train dataset
#trainindices= sample(1:nrow(CameraAccessory_lag_model), 0.9*nrow(CameraAccessory_lag_model))

# generate the train data set
#train = CameraAccessory_lag_model[trainindices,]
#Similarly store the rest of the observations into an object "test".
#test = CameraAccessory_lag_model[-trainindices,]


########################################################################################################################################################################################

#Modelling of the distributed lag for Camera Accessory Segment

CameraAccessory_lag_model <- data.frame(scale(CameraAccessory_lag[,-c(1)]))

train = CameraAccessory_lag_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_my, direction="both")

step

final_df
model_my <- lm(formula = GMV ~ List_Price + Discount_Percent + SLA_Breach +
                 holiday_freq + NPS + Digital + Sponsorship + Content.Marketing + Online_marketing + 
                 Affiliates + COD_orders, data = final_df)

summary(model_my)

model_opt <- lm(formula = GMV ~ List_Price + Discount_Percent + SLA_Breach + 
                  NPS + Digital + Sponsorship + Content.Marketing + Online_marketing + 
                  Affiliates + COD_orders, data = final_df)

model_opt <- lm(formula = GMV ~ List_Price + SLA_Breach + 
                  NPS + Sponsorship + 
                  Affiliates + COD_orders, data = final_df)


summary(model_opt)


model_2 <- lm(formula = GMV ~ ♦ + List_Price.2 + List_Price.3 + 
                Units.1 + Units.2 + Discount_Percent.2 + Discount_Percent.3 + 
                SLA.2 + Product_MRP.1 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                GMV.1 + GMV.2 + GMV.3 + holiday_freq.1 + holiday_freq.2 + 
                holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
                ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
                ProductSellCategory_3.3 + SLA_Breach.1 + SLA_Breach.3, data = train)



summary(model_2)	
#Adjusted R-squared:  0.835 	


#eliminating following variables due to high P values
ProductSellCategory_3.3, GMV.2, SLA.2, List_Price.3


model_3 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 +  
                Units.1 + Units.2 + Discount_Percent.2 + Discount_Percent.3 + 
                Product_MRP.1 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                GMV.1 + GMV.3 + holiday_freq.1 + holiday_freq.2 + 
                holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
                ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)



summary(model_3)	
#Adjusted R-squared:  0.795 	


#eliminating following variables due to high P values
SLA_Breach.3, holiday_freq.1, GMV.1 , Discount_Percent.2

Product_Procurement_SLA.1

model_4 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 +  
                Units.1 + Units.2 +  Discount_Percent.3 + 
                Product_MRP.1 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                GMV.3 + holiday_freq.2 + 
                holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
                ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
                SLA_Breach.1 , data = train)



summary(model_4)	
#Adjusted R-squared:  0.755 	


#eliminating following variables due to high P values
Product_Procurement_SLA.1, Discount_Percent.3 , List_Price.1

model_5 <- lm(formula = GMV ~ List_Price.2 +  
                Units.1 + Units.2 +   
                Product_MRP.1 + Product_MRP.2 + Product_MRP.3 +  
                GMV.3 + holiday_freq.2 + 
                holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
                ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
                SLA_Breach.1 , data = train)



summary(model_5)	
#Adjusted R-squared:  0.741 	


#eliminating following variables due to high P values
Product_MRP.1 


model_6 <- lm(formula = GMV ~ List_Price.2 +  
                Units.1 + Units.2 +   
                Product_MRP.2 + Product_MRP.3 +  
                GMV.3 + holiday_freq.2 + 
                holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
                ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
                SLA_Breach.1 , data = train)



summary(model_6)	
#Adjusted R-squared:  0.741
sort(vif(model_6),decreasing = T)


#Further removing of variables is resulting in drastic decrease in the Adjusted R-squared value

out <- CVlm(data = train, form.lm = GMV ~ List_Price.2 +  
              Units.1 + Units.2 +   
              Product_MRP.2 + Product_MRP.3 +  
              GMV.3 + holiday_freq.2 + 
              holiday_freq.3 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
              COD_orders.1 + COD_orders.2 + COD_orders.3 + ProductSellCategory_1.1 + 
              ProductSellCategory_2.1 + ProductSellCategory_3.1 + ProductSellCategory_3.2 + 
              SLA_Breach.1, m =3, plotit = c("Observed","Residual"), seed = 29)

#fold 1
#Sum of squares = 12.2    Mean square = 0.76    n = 16 
sqrt(mean((out$cvpred - out$GMV)^2))
#1.03   -  mean sum of  squares

########################################################################################################################################################################################

#Modelling of the distributed lag for Gaming Accessory Segment

GamingAccessory_lag_model <- data.frame(scale(GamingAccessory_lag[,-c(1)]))

train = GamingAccessory_lag_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.1 + Units.2 + Units.3 + Discount_Percent.2 + Discount_Percent.3 + 
                SLA.2 + SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + GMV.3 + holiday_freq.1 + 
                holiday_freq.2 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 + ProductSellCategory_1.2 + 
                ProductSellCategory_1.3 + ProductSellCategory_2.3 + ProductSellCategory_3.1 + 
                ProductSellCategory_3.2 + SLA_Breach.1 + SLA_Breach.3, data = train)



summary(model_2)	
#Adjusted R-squared:  0.784 	


#eliminating following variables due to high P values
ProductSellCategory_1.2, SLA.2


model_3 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.1 + Units.2 + Units.3 + Discount_Percent.2 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + GMV.3 + holiday_freq.1 + 
                holiday_freq.2 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_2.3 + ProductSellCategory_3.1 + 
                ProductSellCategory_3.2 + SLA_Breach.1 + SLA_Breach.3, data = train)


summary(model_3)	
#Adjusted R-squared:  0.771	


#eliminating following variables due to high P values
Discount_Percent.2, ProductSellCategory_2.3	


model_4 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.1 + Units.2 + Units.3 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + GMV.3 + holiday_freq.1 + 
                holiday_freq.2 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                ProductSellCategory_3.2 + SLA_Breach.1 + SLA_Breach.3, data = train)

summary(model_4)	
#Adjusted R-squared:   0.762	


#eliminating following variables due to high P values
ProductSellCategory_3.2, Units.1


model_5 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.2 + Units.3 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + GMV.3 + holiday_freq.1 + 
                holiday_freq.2 + prepaid_orders.1 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)

summary(model_5)	
#Adjusted R-squared:   0.765	


#eliminating following variables due to high P values	
prepaid_orders.1, holiday_freq.1

model_6 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.2 + Units.3 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + GMV.3 + 
                holiday_freq.2 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)

summary(model_6)	
#Adjusted R-squared:   0.759

#eliminating following variables due to high P values	
GMV.3

model_7 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.2 + Units.3 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + 
                holiday_freq.2 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)	

summary(model_7)	
#Adjusted R-squared:   0.753

#eliminating following variables due to high P values		
Units.3 	


model_8 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.2 + Discount_Percent.3 + 
                SLA.3 + Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + 
                holiday_freq.2 + prepaid_orders.2 + prepaid_orders.3 + 
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)	

summary(model_8)	
#Adjusted R-squared:   0.76

#eliminating following variables due to high P values		
prepaid_orders.3, SLA.3

model_9 <- lm(formula = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
                Units.2 + Discount_Percent.3 + 
                Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.2 + 
                holiday_freq.2 + prepaid_orders.2 +  
                COD_orders.2 + ProductSellCategory_1.1 +  
                ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
                SLA_Breach.1 + SLA_Breach.3, data = train)	

summary(model_9)	
#Adjusted R-squared:   0.744


#Further removing of variables is resulting in drastic decrease in the Adjusted R-squared value


out <- CVlm(data = train, form.lm = GMV ~ List_Price.1 + List_Price.2 + List_Price.3 + 
              Units.2 + Discount_Percent.3 + 
              Product_MRP.1 + Product_MRP.3 + Product_Procurement_SLA.1 + 
              Product_Procurement_SLA.2 + GMV.2 + 
              holiday_freq.2 + prepaid_orders.2 +  
              COD_orders.2 + ProductSellCategory_1.1 +  
              ProductSellCategory_1.3 + ProductSellCategory_3.1 + 
              SLA_Breach.1 + SLA_Breach.3, m =3, plotit = c("Observed","Residual"), seed = 29)


#fold 2 
#Sum of squares = 13.5    Mean square = 0.79    n = 17 
sqrt(mean((out$cvpred - out$GMV)^2))
# 0.989

#elasticity
model_9$coefficients["List_Price.2"] * mean(train$List_Price.2)/mean(train$GMV)

########################################################################################################################################################################################

#Modelling of the distributed lag  for HomeAudio_lag segment

HomeAudio_lag_model <- data.frame(scale(HomeAudio_lag[,-c(1)]))

train = HomeAudio_lag_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ List_Price.1 + List_Price.3 + Units.1 + Units.2 + 
                Units.3 + Discount_Percent.1 + Discount_Percent.2 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + Product_Procurement_SLA.3 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 + holiday_freq.3 + prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.1 + ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + ProductSellCategory_3.2 + SLA_Breach.2 + 
                SLA_Breach.3, data = train)



summary(model_2)	
#Adjusted R-squared:  0.68 	


#eliminating following variables due to high P values
#ProductSellCategory_3.2, ProductSellCategory_2.1, Discount_Percent.2, List_Price.3, List_Price.1


model_3 <- lm(formula = GMV ~  Units.1 + Units.2 + 
                Units.3 + Discount_Percent.1 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + Product_Procurement_SLA.3 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 + holiday_freq.3 + prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + SLA_Breach.2 + 
                SLA_Breach.3, data = train)


summary(model_3)	
#Adjusted R-squared:  0.652	


#eliminating following variables due to high P values
#holiday_freq.3, Product_Procurement_SLA.3


model_4 <- lm(formula = GMV ~  Units.1 + Units.2 + 
                Units.3 + Discount_Percent.1 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 +  prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + SLA_Breach.2 + 
                SLA_Breach.3, data = train)

summary(model_4)	
#Adjusted R-squared:   0.665	


#eliminating following variables due to high P values
#Units.3


model_5 <- lm(formula = GMV ~  Units.1 + Units.2 + 
                Discount_Percent.1 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_MRP.3 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 +  prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + SLA_Breach.2 + 
                SLA_Breach.3, data = train)

summary(model_5)	
#Adjusted R-squared:   0.652	


#eliminating following variables due to high P values	
#Product_MRP.3

model_6 <- lm(formula = GMV ~  Units.1 + Units.2 + 
                Discount_Percent.1 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 +  prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + SLA_Breach.2 + 
                SLA_Breach.3, data = train)

summary(model_6)	
#Adjusted R-squared:   0.64

#eliminating following variables due to high P values	
#SLA_Breach.3

model_7 <- lm(formula = GMV ~  Units.1 + Units.2 + 
                Discount_Percent.1 + Discount_Percent.3 + 
                SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_Procurement_SLA.1 + 
                Product_Procurement_SLA.2 + GMV.1 + 
                GMV.2 + GMV.3 + holiday_freq.2 +  prepaid_orders.1 + 
                prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
                COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
                ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
                ProductSellCategory_3.1 + SLA_Breach.2, data = train)	

summary(model_7)	
#Adjusted R-squared:   0.633

#Further removing of variables is resulting in drastic decrease in the Adjusted R-squared value


out <- CVlm(data = train, form.lm = GMV ~  Units.1 + Units.2 + 
              Discount_Percent.1 + Discount_Percent.3 + 
              SLA.1 + SLA.2 + SLA.3 + Product_MRP.2 + Product_Procurement_SLA.1 + 
              Product_Procurement_SLA.2 + GMV.1 + 
              GMV.2 + GMV.3 + holiday_freq.2 +  prepaid_orders.1 + 
              prepaid_orders.2 + prepaid_orders.3 + COD_orders.1 + COD_orders.2 + 
              COD_orders.3 + ProductSellCategory_1.1 + ProductSellCategory_1.3 + 
              ProductSellCategory_2.2 + ProductSellCategory_2.3 + 
              ProductSellCategory_3.1 + SLA_Breach.2, m =3, plotit = c("Observed","Residual"), seed = 29)


#fold 1
#Sum of squares = 39.4    Mean square = 2.63    n = 15 
sqrt(mean((out$cvpred - out$GMV)^2))
#1.03

########################################################################################################################################################################################

#Modelling basic variables for CameraAccessory segment
str(CameraAccessory_final)


CameraAccessory_basic_model <- data.frame(scale(CameraAccessory_final[,-c(1)]))

train = CameraAccessory_basic_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
                Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + 
                TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                Affiliates + COD_orders, data = train)

summary(model_2)	
#Adjusted R-squared:  0.991 	


#eliminating following variables due to high P values
#TV


model_3 <- lm(formula = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
                Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + 
                Digital + Sponsorship + Content.Marketing + Online.marketing + 
                Affiliates + COD_orders, data = train)

summary(model_3)	
#Adjusted R-squared:  0.991 	


#eliminating following variables due to high P values
#Content.Marketing, Digital	

model_4 <- lm(formula = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
                Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + 
                Sponsorship + Online.marketing + 
                Affiliates + COD_orders, data = train)

summary(model_4)	
#Adjusted R-squared:  0.99 

#eliminating following variables due to high P values
#Sponsorship	

model_5 <- lm(formula = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
                Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + Online.marketing + 
                Affiliates + COD_orders, data = train)

summary(model_5)	
#Adjusted R-squared:  0.99 

#eliminating following variables due to high P values
#Online.marketing, Affiliates	


model_6 <- lm(formula = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
                Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + COD_orders, data = train)

summary(model_6)	
#Adjusted R-squared:  0.987

#All the existing variables seems to be prominent with significantly lower p values

out <- CVlm(data = train, form.lm = GMV ~ List_Price + Discount_Percent + Product_Procurement_SLA + 
              Units + ProductSellCategory_4 + SLA_Breach + holiday_freq + COD_orders, m =3, plotit = c("Observed","Residual"), seed = 29)	

#fold 2
#Sum of squares = 0.22    Mean square = 0.01    n = 18 	
sqrt(mean((out$cvpred - out$GMV)^2))
# 0.266

#elasticity

list_price_elasticity <- model_6$coefficients["List_Price"] * mean(train$List_Price)/mean(train$GMV)
Discount_Percent_elascticity <- model_6$coefficients["Discount_Percent"] * mean(train$Discount_Percent)/mean(train$GMV)
Product_Procurement_SLA_elasticity <- model_6$coefficients["Product_Procurement_SLA"] * mean(train$Product_Procurement_SLA)/mean(train$GMV)
Units_elasticity <- model_6$coefficients["Units"] * mean(train$Units)/mean(train$GMV)
ProductSellCategory_4_elasticity <- model_6$coefficients["ProductSellCategory_4"] * mean(train$ProductSellCategory_4)/mean(train$GMV)
SLA_Breach_elasticity <- model_6$coefficients["SLA_Breach"] * mean(train$SLA_Breach)/mean(train$GMV)
holiday_freq_elasticity <- model_6$coefficients["holiday_freq"] * mean(train$holiday_freq)/mean(train$GMV)
COD_orders$elasticity <- model_6$coefficients["COD_orders"] * mean(train$COD_orders)/mean(train$GMV)
########################################################################################################################################################################################

#Modelling basic variables for GamingAccessory segment
str(GamingAccessory_final)

GamingAccessory_basic_model <- data.frame(scale(GamingAccessory_final[,-c(1)]))


train = GamingAccessory_basic_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step

model_2 <- lm(formula = GMV ~ List_Price + Product_MRP + SLA + Product_Procurement_SLA + 
                NPS.Score + Units + ProductSellCategory_2 + SLA_Breach + 
                TV + Digital + Content.Marketing + Affiliates + Radio + Other, 
              data = train)

summary(model_2)	
#Adjusted R-squared:  0.989

#eliminating following variables due to high P values
#SLA, Content.Marketing


model_3 <- lm(formula = GMV ~ List_Price + Product_MRP + Product_Procurement_SLA + 
                NPS.Score + Units + ProductSellCategory_2 + SLA_Breach + 
                TV + Digital + Affiliates + Radio + Other, 
              data = train)

summary(model_3)	
#Adjusted R-squared:  0.988

#eliminating following variables due to high vif values
#Other, Radio

model_4 <- lm(formula = GMV ~ List_Price + Product_MRP + Product_Procurement_SLA + 
                NPS.Score + Units + ProductSellCategory_2 + SLA_Breach + 
                TV + Digital + Affiliates, data = train)

summary(model_4)	
#Adjusted R-squared:  0.977 

#eliminating following variables due to high vif values
#SLA_Breach, NPS.Score, Affiliates, TV	

model_5 <- lm(formula = GMV ~ List_Price + Product_MRP + Product_Procurement_SLA + 
                Units + ProductSellCategory_2 + Digital , data = train)	

summary(model_5)	
#Adjusted R-squared:  0.977 	


#All the existing variables seems to be prominent with significantly lower p values

out <- CVlm(data = train, form.lm = GMV ~ List_Price + Product_MRP + Product_Procurement_SLA + 
              Units + ProductSellCategory_2 + Digital ,  m =3, plotit = c("Observed","Residual"), seed = 29)	


#fold 2
#Sum of squares = 0.37    Mean square = 0.02    n = 18 	
sqrt(mean((out$cvpred - out$GMV)^2))
# 0.193	

########################################################################################################################################################################################

#Modelling basic variables for Home Audio segment
str(HomeAudio_final)


HomeAudio_basic_model <- data.frame(scale(HomeAudio_final[,-c(1)]))

train = HomeAudio_basic_model

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step

model_2 <- lm(formula = GMV ~ List_Price + SLA + Product_Procurement_SLA + 
                NPS.Score + ProductSellCategory_2 + ProductSellCategory_4 + 
                SLA_Breach + TV + Digital + Sponsorship + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other, data = train)

summary(model_2)	
#Adjusted R-squared:  0.997

#eliminating following variables due to high P values
#Product_Procurement_SLA, Digital

model_3 <- lm(formula = GMV ~ List_Price + SLA + 
                NPS.Score + ProductSellCategory_2 + ProductSellCategory_4 + 
                SLA_Breach + TV + Sponsorship + Content.Marketing + 
                Online.marketing + Affiliates + SEM + Radio + Other, data = train)

summary(model_3)	
#Adjusted R-squared:  0.997

#eliminating following variables due to high P values
#Affiliates, ProductSellCategory_2, SLA	

model_4 <- lm(formula = GMV ~ List_Price +  
                NPS.Score + ProductSellCategory_4 + 
                SLA_Breach + TV + Sponsorship + Content.Marketing + 
                Online.marketing + SEM + Radio + Other, data = train)

summary(model_4)	
#Adjusted R-squared:  0.996

#eliminating following variables due to high P values
#Online.marketing, Content.Marketing, TV, NPS.Score


model_5 <- lm(formula = GMV ~ List_Price +  
                ProductSellCategory_4 + 
                SLA_Breach + Sponsorship + SEM + Radio + Other, data = train)

summary(model_5)	
#Adjusted R-squared:  0.996	

#All the existing variables seems to be prominent with significantly lower p values

out <- CVlm(data = train, form.lm = GMV ~ List_Price +  
              ProductSellCategory_4 + SLA_Breach + Sponsorship + SEM + Radio + Other,  m =3, plotit = c("Observed","Residual"), seed = 29)

#fold 1 
#Sum of squares = 0.15    Mean square = 0.01    n = 17 	
sqrt(mean((out$cvpred - out$GMV)^2))
#0.073

########################################################################################################################################################################################

########################################################################################################################################################################################
#Adstock Modelling

# since the adstock is calculated as cumulative value of the current investment values and of the past weeks with some decay factor, 
# we are assuming lag model wont be necessary, since the adstock itself would accomodate the effects of lag.

########################################################################################################################################################################################
#Modelling of Adstock KPIs for Camera Accessory Segment

str(CameraAccessory_final)

cols_to_be_removed <- c ("Units","List_Price","Product_MRP","SLA","prepaid_orders","SLA_Breach","COD_orders","holiday_freq","Product_Procurement_SLA","Discount_Percent","ProductSellCategory_1","ProductSellCategory_2","ProductSellCategory_3","ProductSellCategory_4")

CameraAccessory_adstock_model <- CameraAccessory_final[,!(names(CameraAccessory_final) %in% cols_to_be_removed)]

train <- data.frame(scale(CameraAccessory_adstock_model[,-c(1)]))


model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ NPS.Score + TV + Digital + Sponsorship + SEM + 
                Radio + Other, data = train)

out <- CVlm(data = train, form.lm = GMV ~ NPS.Score + TV + Digital + Sponsorship + SEM + 
              Radio + Other,  m =3, plotit = c("Observed","Residual"), seed = 29)	

#fold 1
#Sum of squares = 9.47    Mean square = 0.56    n = 17 	
sqrt(mean((out$cvpred - out$GMV)^2))
0.989

########################################################################################################################################################################################

#Modelling of Adstock KPIs for Gaming Accessory Segment

str(GamingAccessory_final)

GamingAccessory_adstock_model <- GamingAccessory_final[,!(names(GamingAccessory_final) %in% cols_to_be_removed)]

train <- data.frame(scale(GamingAccessory_adstock_model[,-c(1)]))

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ NPS.Score + TV + Digital + Sponsorship + SEM + 
                Radio + Other, data = train)

out <- CVlm(data = train, form.lm = GMV ~ NPS.Score + TV + Digital + Sponsorship + SEM + 
              Radio + Other,  m =3, plotit = c("Observed","Residual"), seed = 29)	

#fold 1
#Sum of squares = 8.56    Mean square = 0.5    n = 17 
sqrt(mean((out$cvpred - out$GMV)^2))
#0.935	

########################################################################################################################################################################################

#Modelling of Adstock KPIs for Home Audio Accessory Segment

str(HomeAudio_final)

HomeAudio_adstock_model <- HomeAudio_final[,!(names(HomeAudio_final) %in% cols_to_be_removed)]

train <- data.frame(scale(HomeAudio_adstock_model[,-c(1)]))

model_1 <-lm(GMV~.,data=train)

summary(model_1)

step <- stepAIC(model_1, direction="both")

step


model_2 <- lm(formula = GMV ~ TV + Digital + Content.Marketing + Radio + 
                Other, data = train)

out <- CVlm(data = train, form.lm = GMV ~ TV + Digital + Content.Marketing + Radio + 
              Other,  m =3, plotit = c("Observed","Residual"), seed = 29)	

#fold 3
#Sum of squares = 5.96    Mean square = 0.35    n = 17 
sqrt(mean((out$cvpred - out$GMV)^2))
#0.934

########################################################################################################################################################################################