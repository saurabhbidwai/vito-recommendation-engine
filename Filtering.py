# -*- coding: utf-8 -*-
"""
Created on Fri Mar 24 21:45:05 2017

@author: DELL
"""

import re
import pandas as pd

data=pd.read_csv("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/Events_Data_csv.csv")


CustomerID=[]
OfferID=[]

for i in range(len(data)):    
    CustomerID[i]=re.findall(r'CustomerID+\s\d{3}',data.values[i])
    OfferID[i]=re.findall(r'"OfferID"+\s+\d{1,5}',data.values[i])
    
    
###########################################################################################################################
data1=pd.read_csv("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/Second/Book1_csv.csv")
data11=""
for i in range(len(data1)):
    aa=""
    for j in range(31,39):
            aa=aa+str(data1.values[i,j])    
    CustomerID.append(re.findall(r'CustomerID+\s\d{3}',aa))
    OfferID.append(re.findall(r'"OfferID"+\s+\d{1,5}',aa))
    

#####################################################################################################################

my=open("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/Events_Data.txt","r")
c=[]
o=[]

for line1 in my:
    line1=line1.rstrip()
    a=re.findall(r'("?[O][f][f][e][r][I][D]*":*\D\d{1,6}")*,*("?[C][a][t][e][g][o][r][y]*":*\D\d{1,6}")*,*("?[C][u][s][t][o][m][e][r][I][D]*":*\D\d{1,6}")',line1)


import csv
myfile = open("E:/Python.csv", 'wb')
wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
wr.writerow(dat)

#

g=pd.read_csv("E:/Python.csv")

