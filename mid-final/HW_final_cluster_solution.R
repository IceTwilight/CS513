
#Cluster NY zip codes into 4 clusters using the NY_ZIP.csv 
#	Hierarchical clustering (centroid)
#	K-means, population   (centroid) 

filename<-file.choose() ## reading IBM dataset
dsn<-  read.csv(filename)

NY_zip<-dsn[,4:9]
?dist
# Perform Hclust analysis 
NY_dist<-dist(NY_zip )
hclust_resutls<-hclust(NY_dist )
hclust_4<-cutree(hclust_resutls,4)



# Perform kmeans analysis 
?kmeans

kmeans_4<- kmeans(NY_zip ,4,nstart = 10)
kmeans_4$cluster
