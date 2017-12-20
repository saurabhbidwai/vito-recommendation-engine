import re
tmp=[]
with open('E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Analytics Assignment/Events_Data.txt', 'r') as f:
    for l1 in f:
        tmp.append(re.findall('("?[O][f][f][e][r][I][D]*":*\D\d{1,6}")*,*("?[C][a][t][e][g][o][r][y]*":*\D\d{1,6}")*,*("?[C][u][s][t][o][m][e][r][I][D]*":*\D\d{1,6}")',l1))

f_o1=[]
f_c2=[]

for i in range(len(tmp)):
    if(tmp[i]):
        if(tmp[i][0][0] and tmp[i][0][2]):
#            x1=re.findall('\d{5}',tmp[i][0][0])
#           x2=re.findall('\d{1,5}',tmp[i][0][2])
            f_o1.append(re.findall('\d{5}',tmp[i][0][0]))
            f_c2.append(re.findall('\d{1,5}',tmp[i][0][2]))
            
Off1=[]
Cust=[]            
for i in range(3480):
    Off1.append(int(f_o1[i][0]))
    Cust.append(int(f_c2[i][0]))

import pandas as pd    
fil = pd.DataFrame({'OfferID': Off1,'CustomerID': Cust})
fil.to_csv("E:/Aegis/R/Recommendation Project/Analytics Assignment (1)/Events_Data.csv")
