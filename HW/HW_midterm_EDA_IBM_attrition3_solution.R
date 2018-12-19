#################################################
#  Company    : Stevens 
#  Purpose    : EDA for 

#  Comments   :
#################################################
rm(list=ls())

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename)

#I.	Summarizing each column (e.g. min, max, mean )
#II.	Identifying missing values
summary(dsn)
dsn2<-dsn[is.na(dsn$MonthlyIncome), ]
  
#III.	Displaying the frequency table of "Attrition" vs. "MaritalStatus" 
table(dsn$Attrition,dsn$MaritalStatus)

#IV.	Displaying the scatter plot of "Age", "MonthlyIncome" and "YearsAtCompany", one pair at a time
pairs(dsn[,c(1,3,5) ] )

#V.	Displaying the boxplot plot of "Age", "MonthlyIncome" and "YearsAtCompany", one pair at a time

boxplot(dsn[c(1,3,5)])

#VI.	Replacing the missing values of
#"MonthlyIncome" with the "mean" of "MonthlyIncome

mean(dsn$MonthlyIncome,na.rm=TRUE)

dsn[is.na(dsn$MonthlyIncome ),"MonthlyIncome"]<-mean(dsn$MonthlyIncome,na.rm=TRUE) 
