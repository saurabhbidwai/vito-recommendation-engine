setwd("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/")

OfferCategorization=read.csv("OfferCategorization.csv")
OfferLocation=read.csv("OfferLocation.csv")
Shop=read.csv("Shop.csv")
Seller=read.csv("Seller.csv")
Payment=read.csv("Payment.csv")
OfferMaster=read.csv("OfferMaster.csv")
BankMaster=read.csv("BankMaster.csv")
CategoryMaster=read.csv("CategoryMaster.csv")
MerchantMaster=read.csv("MerchantMaster.csv")
Events_Data=read.csv("Events_Data.csv")
CustomerMaster=read.csv("CustomerMaster.csv")

#install.packages("dplyr")
library(dplyr)

colnames(BankMaster) = c("BankID", "Bank_Name")
colnames(MerchantMaster)[1]="MerchantID"
Events_Data$X=NULL

# Merging OfferLocation & Shop
off_Shop=merge(x = OfferLocation,y = Shop,by = "ShopID",all.x=TRUE)
colnames(off_Shop)
off_Shop=off_Shop[,c(1,2,7)]

# Merging  Offercategorizaton & CategoryMaster
offC_Cate=merge(x = OfferCategorization,y = CategoryMaster, by = "CategoryID",all.x = TRUE)
colnames(offC_Cate)
offC_Cate=offC_Cate[,-4]

# Merging Off_Shop & OfferMaster
off_Shop_Master=merge(x = OfferMaster,y = off_Shop,by = "OfferID",all.x=TRUE)
colnames(off_Shop_Master)
off_Shop_Master=off_Shop_Master[,c(1,3,16,17,19,22,24,25)]

#Merging offC_Cate & Offer Master 
offC_Cate_Master=merge(x = OfferMaster,y = offC_Cate,by = "OfferID",all.x=TRUE)
colnames(offC_Cate_Master)
offC_Cate_Master=offC_Cate_Master[,c(1,3,16,17,19,22,24,25)]
View(offC_Cate_Master)

#Merging off_Shop_Master & offC_Cate_Master
Off_Shop_Cate=merge(x = off_Shop_Master,y = offC_Cate_Master,by = "OfferID")
colnames(Off_Shop_Cate)
nrow(Off_Shop_Cate)

#Selecting OfferID, ShopID, CategoryID , CategoryName
Shop_Cate=Off_Shop_Cate[,c(1,7,8,14,15)]

# Merging Event Data & CutomerID
Cust_Offer = merge(x = Events_Data,y = CustomerMaster,by = "CustomerID")
View(Cust_Offer)
colnames(Cust_Offer)
Cust_Offer=Cust_Offer[,c(1,2)]

#Merging Cust_Offer & Shop_Cate
final_data=merge(x = Cust_Offer,y = Shop_Cate,by="OfferID")
View(final_data)

# Selecting Only OfferID, CustomerID,Category Name,Category ID
colnames(final_data)
final_data=final_data[,c(1,2,5,6)]
View(final_data)

#install.packages("jsonlite")
library(jsonlite)
Events_Data_replace=fromJSON("Events_Data_replace.json",simplifyDataFrame = FALSE)

json_file <- lapply(Events_Data_replace, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

E_Customer_Offer <- do.call("rbind", json_file)
E_Customer_Offer <- as.data.frame(E_Customer_Offer)
#View(E_Customer_Offer)
E_CustOffer <-  select(E_Customer_Offer,event_timestamp,arrival_timestamp,attributes.Merchants,attributes.Locations)
E_CustOfferResult <- filter(E_CustOffer, nchar( as.character( attributes.Merchants)) == 5 &  nchar( as.character( attributes.Locations)) <= 3 )
E_CustOfferResult <- rename(E_CustOfferResult, OfferID = attributes.Merchants , CustomerID = attributes.Locations )
# View(E_CustOfferResult)
# View(H2)

str(E_CustOfferResult)
# str(H2)
# E_CustOfferResult[,1:4]=lapply(E_CustOfferResult[,1:4],as.integer)
E_CustOfferResult$event_timestamp=as.numeric(as.character(E_CustOfferResult$event_timestamp))
E_CustOfferResult$arrival_timestamp=as.numeric(as.character(E_CustOfferResult$arrival_timestamp))
# Final=inner_join(E_CustOfferResult,H2)

str(E_CustOfferResult)
# str(H2)
# E_CustOfferResult[,1:4]=lapply(E_CustOfferResult[,1:4],as.integer)
E_CustOfferResult$event_timestamp=as.numeric(as.character(E_CustOfferResult$event_timestamp))
E_CustOfferResult$arrival_timestamp=as.numeric(as.character(E_CustOfferResult$arrival_timestamp))
# Final=inner_join(E_CustOfferResult,H2)
# View(Final)

E_CustOfferResult$spended_time=(E_CustOfferResult$event_timestamp - E_CustOfferResult$arrival_timestamp)*(-60000) 
E_CustOfferResult$interest=ifelse(E_CustOfferResult$spended_time>mean(E_CustOfferResult$spended_time),"YES","NO")

#install.packages("recommenderlab")
library(recommenderlab)

r <- as(E_CustOfferResult[,3:4], "realRatingMatrix")
mydata=(as(r,'matrix'))
View(mydata)

mydata_NA=which(is.na(mydata)==T)
mydata[mydata_NA]=0
#View(mydata)

a1=c()
a=c()
b=c()
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    if(mydata[i,j]!=0){
      a1=c(a1,mydata[i,j])
      a=c(a,names(mydata[i,])[j])
      b=c(b,names(mydata[,j])[i])
    }
  }
}

rate=cbind(b,a,a1)
colnames(rate)=c("OfferID","CustomerID","value")
#View(rate)
rate=as.data.frame(rate)
my=merge(x = rate,y = E_CustOfferResult,by = "CustomerID",all.x = TRUE,all.y = TRUE)

s11=merge(x = rate,y = E_CustOfferResult,by = "CustomerID",all.x = TRUE,all.y = TRUE)
s12=subset(s11,s11$interest=="YES")
sNo=subset(s11,s11$interest=="NO")
#View(s12)
s12$OfferID.x=as.numeric(as.character(s12$OfferID.x))
s12$OfferID.y=as.numeric(as.character(s12$OfferID.y))
off1=subset(s12,s12$OfferID.x==s12$OfferID.y)

sNo$OfferID.x=as.numeric(as.character(sNo$OfferID.x))
sNo$OfferID.y=as.numeric(as.character(sNo$OfferID.y))
sNo_off=subset(sNo,sNo$OfferID.x==sNo$OfferID.y)
#View(sNo_off)
#View(off1)

library(doBy)
final_data_Yes<-(off1[firstobs(off1[,1]),])
final_data_No<-(sNo_off[firstobs(sNo_off[,1]),])
final_recom<-rbind(final_data_Yes,final_data_No)
View(final_recom)

#
write.csv(final_recom,"Final1.csv")
final_recom$CustomerID=as.integer(as.character(final_recom$CustomerID))
final_recom=final_recom[,-6]
#finalR = inner_join(x = final_data,y = final_recom,by = "CustomerID")

finalR=left_join(x = final_recom,y = final_data,by = "CustomerID")
View(finalR)

write.csv(x = finalR,file = "RecommendationEngine.csv")
table(finalR$interest)

fRecm=finalR[finalR$OfferID.x==finalR$OfferID.y,]
View(fRecm)

Recomd1=(fRecm[firstobs(fRecm[,1]),])
View(Recomd1)
