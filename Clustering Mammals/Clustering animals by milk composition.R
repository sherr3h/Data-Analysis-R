
rm(list=ls())
#Your task is 
#(1) determining a number of homogenous groups of animals on the basis of their milk composition using hierarchical clustering and 
#(2) characterising those groups.
#1. Discuss the convenience of standardising the data. (2 mark)
#2. Produce a dendrogram using an adequate distance measure and hierarchical clustering algorithm 
    #and select a sensible number of clusters according to the results. (4 mark)
#3. Use statistics and graphs to investigate the typical characteristics of each of the groups. (4 mark)

require(fpc)
load("MammalsMilk.RData")
summary(milk[,2:5])
apply(milk[,2:5],2,sd)

milk2<-scale(milk[,2:5])
summary(milk2)
apply(milk2,2,sd)
head(milk2)
cor(milk2)


(d <- dist(milk2, method = "euclidean"))
(hc <- hclust(d, method="ward.D"))

plot(hc,labels=milk$Name)
ward4 <-cutree(hc, k=4) #Extract a 4-cluster solution
rect.hclust(hc, k=4, border="blue")
plot(hc,labels=milk$Name) # Needs to be called again
(ward3 <- cutree(hc, k=3)) # Extract a 3-cluster solution
rect.hclust(hc, k=3, border="blue")
plot(hc,labels=milk$Name)
ward5 <-cutree(hc, k=5) #Extract a 5-cluster solution
rect.hclust(hc, k=5, border="blue")

(d0 <- dist(milk[2:5], method = "euclidean"))
(hc0 <- hclust(d0, method="ward.D"))
plot(hc0,labels=milk$Name) # Needs to be called again
(ward3 <- cutree(hc0, k=3)) # Extract a 3-cluster solution
rect.hclust(hc0, k=3, border="blue")



require(fpc)
ward3 <- cutree(hc, k=3)
clusval3 <- cluster.stats(d,ward3) # Validation stats for a 3-cluster solution
clusval3$ch #####[1] 39.6986
ward4 <- cutree(hc, k=4) # Extract a 4-cluster solution
clusval4 <- cluster.stats(d,ward4) # Validation stats for a 4-cluster solution
clusval4$ch # CH index for a 4-cluster solution [1] 52.84579
ward5 <- cutree(hc, k=5) # Extract a 5-cluster solution
clusval5 <- cluster.stats(d,ward5) # Validation stats for a 5-cluster solution
clusval5$ch # CH index for a 5-cluster solution [1] 55.81619
 
clusval3$avg.silwidth # Average silhouette width for a 3-cluster solution [1] 0.4499791
clusval4$avg.silwidth ### [1] 0.4895634
clusval5$avg.silwidth # [1] 0.4613777

#Going ahead with the 3-cluster solution, we now compute some basic statistics to help with the characterisation of the clusters.
# Cluster centroids (standarised data)

(st <- by(milk2,ward3,colMeans)) # Mean vector by cluster
(st <- matrix(unlist(st), nrow = 4))
colnames(st) <- c("Cluster 1","Cluster 2","Cluster 3")
st <- as.data.frame(st,row.names=c("Water","Protein","Fat","Lactose"))
print(st)

require(dendextend)
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
                    "median", "centroid", "ward.D2")
milk_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
    hc_milk <- hclust(d, method = hclust_methods[i])   
    milk_dendlist <- dendlist(milk_dendlist, as.dendrogram(hc_milk))
}
names(milk_dendlist) <- hclust_methods
#milk_dendlist
(milk_dendlist_cor <- cor.dendlist(milk_dendlist))
corrplot::corrplot(milk_dendlist_cor, "pie", "lower")

# Scatterplot matrix using the groups
dat <- cbind(ward3,milk2) # Append grouping information
(dat <- as.data.frame(dat))

require(lattice) # This package simplifies using group colours
a <- splom(~dat[c(2:5)], groups = ward3, data = dat,
           par.settings = simpleTheme(col = 1:3, pch = 1:3),
           auto.key = list(columns = 3, title = "Cluster"))
#b <- splom(~dat[c(5:6,11:12)], groups = hc3, data = dat,
#           par.settings = simpleTheme(col = 1:3, pch = 1:3),
#           auto.key = list(columns = 3, title = "Cluster"))
print(a, split=c(1,1,1,1), more=TRUE) # Side-by-side arrangement
#print(b, split=c(2,1,2,1))

d1 <- apply(t(milk2[ward3==1,]),2,function(x) dist(rbind(x,st[,1]))) # Cluster 1
d2 <- apply(t(milk2[ward3==2,]),2,function(x) dist(rbind(x,st[,2]))) # Cluster 2        
d3 <- apply(t(milk2[ward3==3,]),2,function(x) dist(rbind(x,st[,3]))) # Cluster 3
c1 <- rep(1:3,times=c(length(d1),length(d2),length(d3)))
c2 <- c(as.character(milk$Name[ward3==1]),as.character(milk$Name[ward3==2]),as.character(milk$Name[ward3==3]))
#c3 <- c(as.character(temp$Area[hc3==1]),as.character(temp$Area[hc3==2]),as.character(temp$Area[hc3==3]))
distances <- data.frame(Cluster=c1,Name=c2,Dist=c(d1,d2,d3))
# Plot distances to the cluster centroids
plot(distances$Cluster,distances$Dist,axes=F,ylab="Distance to cluster centroid",xlab="Cluster",col="white",xlim=c(0.75,3.25))
text(distances$Cluster,distances$Dist,labels=distances$Name)
axis(1,at=1:3,labels=c("Cluster 1","Cluster 2","Cluster 3")); axis(2)
#legend("topright",legend=levels(distances$Area),text.col=1:4,bty="n")

summary(dat[ward3==1,])


par(mfrow=c(2,2)) # Boxplots arrangement
par(mar=c(2,2,2,2))
for (i in 2:ncol(dat)){
    boxplot(dat[,i] ~ ward3,data=dat,main=names(dat)[i])
}
(require(reshape))
Milkdat.long <- melt(dat,id.vars=c("ward3")) 
Milk.mean <- aggregate( value ~  ward3 +variable, data=Milkdat.long, FUN=mean)
Milk.sd <- aggregate( value ~  ward3 +variable, data=Milkdat.long, FUN=sd)
Milk.mean[,4] <-Milk.sd[,3]
colnames(Milk.mean)[1] <- "Cluster"
colnames(Milk.mean)[3] <- "Mean"
colnames(Milk.mean)[4] <- "Sd"
Milk.mean <- Milk.mean[,c(2,1,3,4)]
(t(Milk.mean))

require(MASS)
(milk.lda <- lda(ward3 ~ .,data=dat)) # Using formula interface: Group on all the others
par(mfrow=c(1,1))
plot(milk.lda,dimen=2)
pred.gr <- c("Predict Cluster 1","Predict Cluster 2","Predict Cluster 3")
table(dat$ward3, factor(predict(milk.lda)$class,labels=pred.gr))




fmt <- function(x) {
    s <- format(x, digits=2)
    even <- ((1:length(s)) %% 2) == 0
    s[even] <- sprintf("(%s)", s[even])
    s
}

#Milkdat.long$variable   *
Milkdat.long$ward3<-as.character(Milkdat.long$ward3)
Milkdat.long$ward3 <- replace(Milkdat.long$ward3, Milkdat.long$ward3=="1", "Cluster1")
Milkdat.long$ward3 <- replace(Milkdat.long$ward3, Milkdat.long$ward3=="2", "Cluster2")
Milkdat.long$ward3 <- replace(Milkdat.long$ward3, Milkdat.long$ward3=="3", "Cluster3")

plot(dat[,2:5], col=dat$ward3)
plot(milk[,2:5], col=dat$ward3)




