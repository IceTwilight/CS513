#########################################################
##  Purpose:  Final knn 
##  Developer: KD         
##
#########################################################

rm(list=ls())

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename)

Survived2<-ifelse(dsn$Survived=='Yes',1,0)
Gender<-ifelse(dsn$Sex=='Female',1,0)
Child<-ifelse(dsn$Age=='Child',1,0)

First_class<-ifelse(dsn$Class=='1st',1,0)
Second_class<-ifelse(dsn$Class=='2nd',1,0)
Third_class<-ifelse(dsn$Class=='3rd',1,0)
Crew_class<-ifelse(dsn$Class=='Crew',1,0)



df_titanic2<-data.frame(Survived2,Gender,Child,
                        First_class,Second_class,
                        Third_class,Crew_class)
index <- seq (1,nrow(df_titanic2),by=4)
test<- df_titanic2[index,]
training<-df_titanic2[-index,]

library("neuralnet")
 net_titanic  <- neuralnet(Survived2~Gender+Child+
                      First_class+Second_class+
                      Third_class+Crew_class
                      ,training, hidden=6, threshold=0.01)
 net_results_test <-compute(net_titanic , test[,c(-1)])
 plot(net_titanic)
 
 ANN=as.numeric( net_results_test$net.result)


 ANN_cat<-ifelse(ANN<.4,0,1)
 table(Prediction=ANN_cat,Actual=test$Survived2)
 
