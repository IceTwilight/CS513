#########################################################
##  Purpose: Create pretty classification tree
##  Developer: KD         
##
#########################################################

#########################################################
##  Step 0: Clear the environment
##           
##
#########################################################
rm(list=ls())

filename<-file.choose()  
dsn<-  read.csv(filename)


index <- seq (1,nrow(dsn),by=4)
test<- dsn[index,]
training<-dsn[-index,]

#########################################################
##  Step 1: Load the relavent packages
##           
##
#########################################################
#installed.packages()

#install.packages("rpart")  # CART standard package
?install.packages()

#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
  
#########################################################
 
##  Step 2:  example
##           
##
#########################################################
 

dev.off()
CART_class<-rpart( Survived~.,data=training)
rpart.plot(CART_class)


CART_predict<-predict(CART_class,test)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Yes','No')
table(Actual=test[,4],CART=CART_predict_cat)
CART_wrong<-sum(test[,4]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,4])
CART_error_rate

