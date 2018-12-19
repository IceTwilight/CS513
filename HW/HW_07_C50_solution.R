#################################################
#  Company    :  
#  Purpose    : EDA for 

#  Comments   :
#################################################
rm(list=ls())
dev.off

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename,colClasses=c("EmployeeID"="character"))
dsn2<-na.omit(dsn)
index <- seq(1,nrow(dsn2),by=5)


test<-dsn2[index,]
training <-dsn2[-index,]
drop_cols<-c(which( colnames(dsn2)=="EmployeeID" ))

# C50  classification 
library('C50')
C50_class <- C5.0(Attrition ~.,data=training[,-drop_cols] )

summary(C50_class )
dev.off()
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,2],C50=C50_predict)
wrong<- (test[,2]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,2])
c50_rate


