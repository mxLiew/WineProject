
###Clustering
#redwine
redwine = na.omit(redwine)

#standardise the data
redwineScaled = scale(redwine[1:11])

#agglomerative hierarchical clustering
d_red = dist(redwineScaled, method = "euclidean")
hcl_red = hclust(d_red, method = "ward.D2") #using ward.D2 for more even cluster
plot(hcl_red, cex = 0.6, hang=-1, main = "Red Wine Dendrogram")

#find the best number of clusters
fviz_nbclust(redwineScaled, FUN = hcut, method="wss")
###set k, very important just change K and run all the code below to cut and compute clusters
k=7
#cut tree into into 7 groups
groups = cutree(hcl_red, k=k)
table(groups)
plot(hcl_red, cex=0.6)
rect.hclust(hcl_red, k=k, border=2.5)
#show cluster of each row of data #difficult to see minute 
#difference of the wine, cannot taste the difference, 
#that's why the quality is between 3-8.
redwineWithGrp<-cbind(redwine, groups)
View(redwineWithGrp)

#####
for(i in 1:k){
  print(i)
  if(i==1){
    redClusters <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), c("fixed acidity",
                                                                       "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                                                                       "free sulfur dioxide", "total sulfur dioxide", "density", "pH", 
                                                                       "sulphates", "alcohol", "quality","N"))
  }
  grp <- subset(redwineWithGrp,groups==i)
  quality_mean <-mean(grp$`quality`)
  fixed_A_mean <-mean(grp$`fixed acidity`)
  volatile_A_mean <-mean(grp$`volatile acidity`)
  citric_A_Mean <-mean(grp$`citric acid`)
  residual_sugar_mean <-mean(grp$`residual sugar`)
  chloride_mean <-mean(grp$`chlorides`)
  free_s_d_mean<-mean(grp$`free sulfur dioxide`)
  total_s_d_mean<-mean(grp$`total sulfur dioxide`)
  density_mean<-mean(grp$`density`)
  pH_mean<-mean(grp$`pH`)
  sulphates_mean<-mean(grp$`sulphates`)
  alcohol_mean<-mean(grp$`alcohol`)
  N<-nrow(grp)
  cluster_mean<-c(fixed_A_mean,volatile_A_mean,citric_A_Mean,residual_sugar_mean,chloride_mean,free_s_d_mean,total_s_d_mean,density_mean,pH_mean,sulphates_mean,alcohol_mean,quality_mean,N)
  redClusters <- rbind(redClusters,cluster_mean)
}
colnames(redClusters)<-c("fixed acidity",
                         "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                         "free sulfur dioxide", "total sulfur dioxide", "density", "pH", 
                         "sulphates", "alcohol", "quality","N")
View(redClusters)

###############################################################
#white wine
whitewine<-read_delim("~/Desktop/BT2101 project/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
colnames(whitewine) <- c("fixed acidity",
                         "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                         "free sulfur dioxide", "total sulfur dioxide", "density", "pH", 
                         "sulphates", "alcohol", "quality")
#clustering for white wine
whitewine = na.omit(whitewine)

#standardise the data
whitewineScaled = scale(whitewine[1:11])

#agglomerative hierarchical clustering
d_white = dist(whitewineScaled, method = "euclidean")
hcl_white = hclust(d_white, method = "ward.D2")
plot(hcl_white, cex = 0.6, hang=-1, main = "White Wine Dendrogram")

#find the best number of clusters
fviz_nbclust(whitewineScaled, FUN = hcut, method="wss")
#cut tree into into k groups
k=5 
groups = cutree(hcl_white, k=k)
table(groups)
plot(hcl_white, cex=0.6)
rect.hclust(hcl_white, k=k, border=2.5)
#show cluster of each row of data
whitewineWithGrp<-cbind(whitewine, groups)
View(whitewineWithGrp)

for(i in 1:k){
  print(i)
  if(i==1){
    whiteClusters <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), c("fixed acidity",
                                                                         "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                                                                         "free sulfur dioxide", "total sulfur dioxide", "density", "pH", 
                                                                         "sulphates", "alcohol", "quality","N"))
  }
  grp <- subset(whitewineWithGrp,groups==i)
  quality_mean <-mean(grp$`quality`)
  fixed_A_mean <-mean(grp$`fixed acidity`)
  volatile_A_mean <-mean(grp$`volatile acidity`)
  citric_A_Mean <-mean(grp$`citric acid`)
  residual_sugar_mean <-mean(grp$`residual sugar`)
  chloride_mean <-mean(grp$`chlorides`)
  free_s_d_mean<-mean(grp$`free sulfur dioxide`)
  total_s_d_mean<-mean(grp$`total sulfur dioxide`)
  density_mean<-mean(grp$`density`)
  pH_mean<-mean(grp$`pH`)
  sulphates_mean<-mean(grp$`sulphates`)
  alcohol_mean<-mean(grp$`alcohol`)
  N<-nrow(grp)
  cluster_mean<-c(fixed_A_mean,volatile_A_mean,citric_A_Mean,residual_sugar_mean,chloride_mean,free_s_d_mean,total_s_d_mean,density_mean,pH_mean,sulphates_mean,alcohol_mean,quality_mean,N)
  whiteClusters <- rbind(whiteClusters,cluster_mean)
}
colnames(whiteClusters)<-c("fixed acidity",
                           "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                           "free sulfur dioxide", "total sulfur dioxide", "density", "pH", 
                           "sulphates", "alcohol", "quality","N")
View(whiteClusters)


