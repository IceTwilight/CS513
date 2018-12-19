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

#install.packages("C50", repos="http://R-Forge.R-project.org")
#install.packages("C50")
library('C50')
drop_cols<-c(which( colnames(dsn2)=="EmployeeID" ))
C50_class<-C5.0(Attrition ~.,data=training[,-6])
?C5.0
summary(C50_class )
dev.off()
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,2],C50=C50_predict)
wrong<- (test[,2]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,2])
c50_rate
C50_predict<-predict( C50_class ,test , type="prob" )




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




