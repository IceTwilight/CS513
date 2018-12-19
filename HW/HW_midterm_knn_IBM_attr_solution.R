#################################################
#  Company    : Stevens 
#  Purpose    : EDA

#  Comments   :

 
rm(list=ls())

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename)

summary(dsn)
dsn2<-na.omit(dsn)


##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

mmnorm(5,0,10)
x<-0:10
mmnorm(x,min(x),max(x))

#Create indicators for Marital status.
#MaritalStatus_ind1<-ifelse(dsn2$MaritalStatus=="Divorced",1,0)
#MaritalStatus_ind2<-ifelse(dsn2$MaritalStatus=="Married",1,0)

# normalize the data 

dsn2_normalized<-as.data.frame (         
  cbind( Age=mmnorm(dsn2[,1],min(dsn2[,1]),max(dsn2[,1]))
         ,JobSatisfaction=mmnorm(dsn2[,2],min(dsn2[,2]),max(dsn2[,2]))
         ,MaritalStatus_ind1=ifelse(dsn2$MaritalStatus=="Divorced",1,0)
         ,MaritalStatus_ind2=ifelse(dsn2$MaritalStatus=="Married",1,0)
         ,MonthlyIncome=mmnorm(dsn2[,4],min(dsn2[,4]),max(dsn2[,4]))
         ,YearsAtCompany=mmnorm(dsn2[,5],min(dsn2[,5]),max(dsn2[,5]))
         ,Attrition=factor(dsn2[,6])
         
  )
) 




#Use knn(k=3) to  predict "attrition rate" 
#for a random sample(30%) of the data (test dataset)

index<-sort(sample(nrow(dsn2_normalized),round(.30*nrow(dsn2_normalized))))
training<-dsn2_normalized[-index,]
test<-dsn2_normalized[index,]
library(kknn)
predict_k3 <- kknn(formula=factor(Attrition)~., training, test, k=3,kernel ="rectangular"  )

fit <- fitted(predict_k3)
table(Acutal=test$Attrition,Prediction=fit)



