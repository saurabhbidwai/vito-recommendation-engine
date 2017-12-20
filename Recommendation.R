setwd("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/")

OfferCategorization=read.csv("OfferCategorization.csv")#
OfferLocation=read.csv("OfferLocation.csv")#
Shop=read.csv("Shop.csv")#
Seller=read.csv("Seller.csv")#
Payment=read.csv("Payment.csv")#
OfferMaster=read.csv("OfferMaster.csv")#
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

A2=inner_join(OfferCategorization,CategoryMaster)

B2=inner_join(Shop,OfferLocation)

C2=inner_join(Payment,BankMaster)

D2=inner_join(C2,Seller)

E2=inner_join(D2,MerchantMaster)

F2=inner_join(E2,OfferMaster)

G2=inner_join(F2,A2)

H2=inner_join(G2,B2,by="OfferID")


#H2=inner_join(G2,Events_Data)

#I2=inner_join(H2,CustomerMaster)



#RecommenderRating#########################################

# install.packages("recommenderlab")
# library(recommenderlab)
# 
# r1 <- as(Events_Data, "realRatingMatrix")
# mydata1=(as(r,'matrix'))
# View(mydata1)
# mydata_NA=which(is.na(mydata)==T)
# mydata1[mydata_NA]=0

# mydata_offerID=as.integer(colnames(mydata))

# for(i in 1:length(mydata_offerID)){
#   for(j in 1:nrow(G2)){
#     if(mydata_offerID[i]==G2$OfferID[j]){
#       new_Event_Data[k]=G2[j]
#       k=k+1
#     }
#   }
# }

############################################################

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
# View(Final)

E_CustOfferResult$spended_time=(E_CustOfferResult$event_timestamp - E_CustOfferResult$arrival_timestamp)*(-60000) 
E_CustOfferResult$interest=ifelse(E_CustOfferResult$spended_time>mean(E_CustOfferResult$spended_time),"YES","NO")
#View(E_CustOfferResult)

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
my=inner_join(E_CustOfferResult,rate,by="CustomerID",copy=T)
#View(my)
#help("merge")

# merge(x = rate,y = E_CustOfferResult,by = "CustomerID",incomparables = NA)
# merge(x = rate,y = E_CustOfferResult,by = "CustomerID")
# merge(x = rate,y = E_CustOfferResult,by.x = "CustomerID",by.y = "CustomerID",all=FALSE)

#rate=as.data.frame(rate)
# E_CustOfferResult$OfferID=as.numeric(E_CustOfferResult$OfferID)
# E_CustOfferResult$CustomerID=as.numeric(E_CustOfferResult$CustomerID)
s11=merge(x = rate,y = E_CustOfferResult,by.x = "CustomerID",by.y = "CustomerID",all=TRUE)
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

write.csv(final_recom,"Final1.csv")

