library("foreign")
library("plm")
mydata<-read.csv("mydata.csv",sep = ",",header = TRUE)
View(mydata)
Y<-cbind("GOI")
X<-cbind("CR","QR","RD","PD","ID","CCC","Size.Logarithm.of.assets")
pdim(mydata, index=c("ID.1","t"))
pooled.model <- lm(GOI ~ CR + QR + RD + PD + ID + CCC + Size.Logarithm.of.assets, data=mydata, na.action=na.omit)
summary(pooled.model)
lsdv.model <- lm( GOI ~ CR + QR + RD + PD + ID + CCC + Size.Logarithm.of.assets + as.factor(ID.1), data=mydata, na.action=na.omit)
summary(lsdv.model)
pmodel1 <- plm(GOI ~ CR + QR + RD + PD + ID + CCC + Size.Logarithm.of.assets, data=mydata, index=c("ID.1", "t"), na.action=na.omit, model="pooling")
summary(pmodel1)
pmodel2 <- plm(GOI ~ CR + QR + RD + PD + ID + CCC + Size.Logarithm.of.assets, data=mydata, index=c("ID.1", "t"), na.action=na.omit, model="within")
summary(pmodel2)
pFtest(pmodel2,pmodel1)
##random
pmodel3 <- plm(GOI ~ CR + QR + RD + PD  + ID + CCC + Size.Logarithm.of.assets, data=mydata, index=c("ID.1", "t"), na.action=na.omit, model="random")
summary(pmodel3)
##Fixed
pmodel4 <- plm(GOI ~ CR + QR + RD + PD  + ID + CCC + Size.Logarithm.of.assets, data=mydata, index=c("ID.1", "t"), na.action=na.omit, model="random")
summary(pmodel4)
## Hausman test

phtest(pmodel2, pmodel3)
## Breusch-Pagan test 

plmtest(pmodel3, effect="individual", type="bp")

Summary(Y)
rm(Mydata)
header
pdim(BESData, index=c("serialno", "year"))
save
save(Untitled2)
