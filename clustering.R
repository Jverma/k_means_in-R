############################
# Author -  Janu Verma
# j.verma5@gmail.com
##############################

# Cluster Analysis of wine dataset 
# This datast contains 13 chemical measurements on 178 Italian wine samples.
# The first column contains the type. We will use k-means as a predictiive model to infer the types. 

#####################################################################
# Compute optimal number of clusters by looking at within clusters sum of squares for different number of clusters 

wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}
                     
###################################################################
require(rattle)
# Rattle is a free graphical interface for data mining with R.
# We use rattle to access data from the UCI Mchine Learning Repository 
# ((http://www.ics.uci.edu/~mlearn/MLRepository.html) 

#Load Data
data(wine)  

# remove the first column from the data and then scale the data
input <- scale(wine[-1])

# print optimal number of clusters
pdf("Number of Clusters.pdf")
wssplot(input)
dev.off()  


####################################################
set.seed(1234)
# Clusters 
fit <- kmeans(input, 3, nstart=25)

require(graphics)

# Plot the Clusters
pdf("clusters.pdf")
plot(input, col=fit$cluster)
points(fit$centers, col=1:3, pch = 8, cex = 2)
dev.off()

# ggplot visual
df <- data.frame(input)
df$cluster <- factor(fit$cluster)
centers <- as.data.frame(fit$centers)
require(ggplot2)
pdf("Clustering of Wine data.pdf")
ggplot(data=df, aes(x=Alcohol,y=Malic,color=cluster)) + geom_point()
dev.off() 

# Size of the Clusters
size <- fit$size
#size

# Means of the columns for the Clusters
mean_coulmns <- aggregate(input, by=list(fit$cluster), FUN=mean)
#mean_columns

###################################################
# Measuring How Good is the Clustering

# Cross Tabulation : A table comparing type assigned by clustering and original values

cross <- table(wine$Type, fit$cluster)
cross
# This shows that the clustering gives a pretty good prediction. 

# Fit cluster centers to each observation
fit_centers <- fitted(fit)

# Residue
residue <- input - fitted(fit)

######################################################
# Assign Cluster to each observation

 mydata <- data.frame(input, fit$cluster)
 write.table(mydata, file="clustered_observations.csv", sep=",", row.names=F, col.names=T, quote=F)





                 
