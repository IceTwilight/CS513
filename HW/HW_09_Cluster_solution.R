#################################################
#  Company    : Stevens 
#  Purpose    :  

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

# TotalWorkingYears", "Gender" and "Education
dsn2_normalized<-as.data.frame (         
  cbind( TotalWorkingYears=mmnorm(dsn2[,13],min(dsn2[,13]),max(dsn2[,13]))
         ,Education=mmnorm(dsn2[,5],min(dsn2[,5]),max(dsn2[,5]))
         ,Gender=ifelse(dsn2$Gender=="Male",0,1)
         ,Attrition=factor(dsn2[,2])
         
  )
)

dsn2_normalized_dist<-dist(dsn2_normalized[,c(-4)])
hclust_resutls<-hclust(dsn2_normalized_dist,method ="average" )
hclust_2<-cutree(hclust_resutls,2)
table(Hclust=hclust_2,Actual=dsn2_normalized[,4])

?kmeans

kmeans_2<- kmeans(dsn2_normalized[,c(-4)],2,nstart = 10)
str(kmeans_2)
kmeans_2$cluster
table(kmeans_2$cluster,dsn2_normalized[,4])



