#################################################
#  Company    : AIMS
#  Purpose    : EDA for 

#  Comments   :
#################################################
rm(list=ls())
dev.off

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename,colClasses=c("EmployeeID"="character"))
dsn2<-na.omit(dsn)


idx<-sort(sample(nrow(dsn2),as.integer(.65*nrow(dsn2))))

training<-dsn2[idx,]
test<-dsn2[-idx,]




#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

drop_cols<-c(which( colnames(dsn2)=="EmployeeID" ))

CART_class<-rpart(Attrition ~.,data=training[,-drop_cols])
rpart.plot(CART_class)

Prediction <- predict(CART_class, test[,])
CART_predict_cat<-ifelse(Prediction[,2]>=.15,'Yes','No')
table(actual=test$Attrition ,CART_predict_cat)

wrong<- (test$Attrition!=CART_predict_cat )
error_rate<-sum(wrong)/length(wrong)
error_rate 
table(dsn2$Attrition)
att_rate<-sum(dsn2$Attrition=="Yes")/length(dsn2$Attrition)




