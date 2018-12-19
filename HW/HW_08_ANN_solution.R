

#  First Name      : Khasha
#  Last Name       : Dehnad
#  Id              : 12345
#  purpose         : running ANN on IBM attrition dataset

remove(list=ls())

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename)
summary(dsn)
##Age	Attrition	BusinessTravel	DistanceFromHome	Education	EmployeeID
# EnvironmentSatisfaction	Gender	MaritalStatus	MonthlyIncome
# NumCompaniesWorked	OverTime	TotalWorkingYears

dsn2<-as.data.frame (         
  cbind(  Age=dsn$Age
         ,Attrition=dsn$Attrition
         ,BusinessTravel=ifelse(dsn$BusinessTravel=="Non-Travel",0
                            ,ifelse(dsn$BusinessTravel=="Travel_Rarely",1,2) )
         ,DistanceFromHome=dsn$DistanceFromHome
         ,Education=dsn$Education
         ,EnvironmentSatisfaction=dsn$EnvironmentSatisfaction
         ,Gender=ifelse(dsn$Gender=="Male",0,1)
         ,MaritalStatus_ind1=ifelse(dsn$MaritalStatus=="Divorced",1,0)
         ,MaritalStatus_ind2=ifelse(dsn$MaritalStatus=="Married",1,0)
         ,MonthlyIncome=dsn$MonthlyIncome
         ,NumCompaniesWorked=dsn$NumCompaniesWorked
         ,OverTime=ifelse(dsn$OverTime =="No",0,1)
         ,TotalWorkingYears=dsn$TotalWorkingYears
  )
)
           
         




dsn3<-na.omit(dsn2)
index <- seq(1,nrow(dsn3),by=5)


test<-dsn3[index,]
training <-dsn3[-index,]


library("neuralnet")
?neuralnet()



net_IBM  <- neuralnet(Attrition ~Age+	BusinessTravel+DistanceFromHome+Education+
                        EnvironmentSatisfaction+Gender+MaritalStatus_ind1+MaritalStatus_ind2+
                        MonthlyIncome+NumCompaniesWorked+OverTime+TotalWorkingYears
                        ,data=training
                      , hidden=7, threshold=0.01)




#Plot the neural network
plot(net_IBM)

net_IBM_results <-compute(net_IBM, test[,-2])
ANN=as.numeric(net_IBM_results$net.result)


ANN_round<-round(ANN)
ANN_cat<-ifelse(ANN<1.5,1,2)




table(Actual=test$Class,ANN_cat)

wrong<- (test$Class!=ANN_cat)
rate<-sum(wrong)/length(wrong)


