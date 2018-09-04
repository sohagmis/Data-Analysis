install.packages("installr")
update("r")
library(installr)
updateR()
view(mydataabdullah.csv)
mydata<-read.csv("mydataabdullah.csv",header = T)
rm(Y)
View(mydataabdullah.csv)
library("foreign")
library("plm")
mydata<-read.csv("mydata.csv",sep = ",",header = TRUE)
rm(mydata)
View(mydata)
Y<-cbind("Market.Value")
X<-cbind("Equity.Multiplier","TD.TA.ratio","STD.TA.ratio","LTD.TA.ratio")
pdim(mydata, index=c("ID","t"))
## pooled model
pooled.model <- lm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio, data=mydata, na.action=na.omit)
summary(pooled.model)
## fixed model
lsdv.model <- lm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio + as.factor(ID), data=mydata, na.action=na.omit)
summary(lsdv.model)
## pooling model
pmodel1 <- plm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio, data=mydata, index=c("ID", "t"), na.action=na.omit, model="pooling")
summary(pmodel1)
## within
pmodel2 <- plm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio, data=mydata, index=c("ID", "t"), na.action=na.omit, model="within")
summary(pmodel2)
pFtest(pmodel2,pmodel1)
##random
pmodel3 <- plm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio, data=mydata, index=c("ID", "t"), na.action=na.omit, model="random")
summary(pmodel3)
##Fixed
pmodel4 <- plm(Market.Value ~ Equity.Multiplier + TD.TA.ratio + STD.TA.ratio + LTD.TA.ratio, data=mydata, index=c("ID", "t"), na.action=na.omit, model="random")
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