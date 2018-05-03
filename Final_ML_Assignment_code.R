# install.packages("gtools", dependencies = T)
# library(gtools) # if problems calling library, install.packages("gtools", dependencies = T)
# library(qdap)   # qualitative data analysis package (it masks %>%)
# library(tm)     # framework for text mining; it loads NLP package
# library(Rgraphviz)  # depict the terms within the tm package framework
# library(SnowballC); library(RWeka); library(rJava); library(RWekajars)  # wordStem is masked from SnowballC
# library(Rstem) # stemming terms as a link from R to Snowball C stemmer
# require(stats)
# install.packages("ggplot2")
# library(ggplot2)
# #==================NbClust package 
# install.packages("NbClust",dependencies = TRUE)
# install.packages("mclust")
# library(mclust)
# library(NbClust)
# #install.packages("dplyr")
# #install.packages("tidyr")
# require(dplyr)
# require(tidyr)
# #install.packages("stringr", dependencies = TRUE)
# library(stringr)
# #install.packages("RPostgreSQL")
require("RPostgreSQL")
library(DBI)


library(DBI)
pw <- {  "noor7168" }
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "mydb",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = pw)
rm(pw) # removes the password
dbExistsTable(con, "input_points_kmean_training_200")
df_point <- dbGetQuery(con, "SELECT * from input_points_kmean_training_200")
dbDisconnect(con)
#graphics.off()
nPoint<-df_point$point
#The point column is sperated into two columns (V1, V2)
a=read.table(text = nPoint, sep = ",", colClasses = "character")
#create a new column for Id 
a["id"]<-NA
a$id<-df_point$id
# clean both colunms from speacial character }{ and convert it to numeric 
z <- matrix(data = c(a$id,a$V1, a$V2), nrow = length(a$V1), ncol=3)
G<-apply(z, 2, function(x) as.numeric(gsub('[^0-9\\.\\-]', '', x)))
#Set the names of columns in pdset as id , X, Y 
pdset<-G # G[,2:3]
clname<-c("id","X","Y")
colnames(pdset)<-clname
#Standarization of  Data of points ( X, Y )
#Y_norma<- as.data.frame( scale(pdset[,3] ))
dfNormZ <- as.data.frame( scale(pdset[,2:3] ))
#dfNormZ <- as.data.frame(pdset[,2:3])
#dfNormZ <-data.frame(pdset[,2],Y_norma$V1)
columnames<-c("X_n","Y_n")
colnames(dfNormZ)<-columnames
#=========================================================
#===== using Elbow Method for  optimal number of clusters
#=========================================================
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max,function(k){kmeans(dfNormZ,k,
                                 nstart=50,
                                 iter.max = 15 )$tot.withinss})
#wss
plot.new()
frame()
plot(1:k.max, wss, type="b", pch = 19, frame = TRUE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#===================================================================
# Bayesian Inference Criterion for choosing K , 
#        Please select 1 to see the graph 
#===================================================================
d_clust <- Mclust(as.matrix(dfNormZ), G=1:15,
                  modelNames = mclust.options("emModelNames"))
plot.new()
frame()
plot(d_clust)

#====================================================================
#  Kmeans implementation  using k=5 Based on Elbow and NBCLUST methods 
#====================================================================
result<-kmeans(dfNormZ, 
               5,
               nstart=50,
               iter.max = 15 )
plot.new()
frame()
plot(dfNormZ[,1],dfNormZ[,2], 
     #type="b", 
     pch=19,
     xlab = "X-value",
     ylab = "Y-latency value",
     col=result$cluster)
#====================================================================
#  Saving Data in Text file 
#====================================================================
out <- cbind(id=pdset[,1], X=pdset[,2], Y=pdset[,3], X_n=dfNormZ$X_n, Y_n=dfNormZ$Y_n,clusterNum = result$cluster)
write.csv(out, file="cluster_file_normalized_final.csv")




