library(tidyverse) 
library(MASS)
res <- read.csv("E:/r/RStudio/R/CustomerSegmentation.csv")
View(res)
#to drop null value 
library(tidyr)  #for remove null values
res1 <- drop_na(res)
str(res)
str(res1)
# to remove empty rows and empty coulms 
library(janitor)  #for empty rows and coulmn
res2 <- remove_empty(res1 , which = c("rows" , "cols") , quiet = FALSE)
#no empty rows and no empty coulmns
library(dplyr)  #for remove duplication
duplicated(res2)
#no data duplicated


# statistics 
#to get first 6 item 
head(res2)
#to get last 6 item 
tail(res2)
#to get main , median , mode , min and max
summary(res2)
#to get range of coulmn age
range(res2$Age)
range(res2$Annual.Income..k..)
range(res2$Spending.Score..1.100.)
#to get standard deviation of coulmn age
sd(res2$Age)
#to get varience of coulmn age
var(res2$Age)
#to make ploting of coulmn age
plot(as.factor(res2$Age))
# to know skweedness
plot(density(res2$Annual.Income..k..))

hist(res2$Annual.Income..k.. )
hist(iris$Sepal.Length )


#to removing oyt layer
(r <- mean(res2$Annual.Income..k.., trim=0.10) )
res2 <- subset(res2, res2$Annual.Income..k..  >= 50 & res2$Annual.Income..k.. < 60)
summary(res2)
quantile(res2$Annual.Income..k.., seq(from=0, to=1, length=11))

breaks <- c(18,25,45,68)
labels <- c( "teen_ager","youth","older")
wealth <- cut(res2$Age, breaks, labels)
res2 <- cbind(res2, wealth)
head(res2)

wt <- table(wealth)
percent <- wt/sum(wt)*100
wt <- rbind(wt, percent)
wt
plot(wt)  

nt <- table(wealth, res2$Annual.Income..k..)
print(nt)
plot(nt)  


plot(iris$Sepal.Length , iris$Petal.Length)

f_gender<-factor(res2$Gender)
print(f_gender)
levels(f_gender)<-c(2,1)
res2$Gender<-f_gender
res2$Gender<-as.numeric(res2$Gender)
view(res2)

# to compare between age and income
with(res2, cor(Annual.Income..k.., Spending.Score..1.100.))
with(res2, cor(log(Annual.Income..k..),Spending.Score..1.100. ) ) #This will give a better correlation
f = length(res2$Annual.Income..k..)
with(res2, cor(runif(f), Spending.Score..1.100.)) 


#removing out layers
(res3 <- mean(res$Age, trim=0.10) )
#to get only female on first 5 rows
subset(res2,Gender=="Female")[1:5,]
#to get only age between 50 and 60

subset(res2,Age>=50 &Age<60)
res5 <- iris[, 1:4]
dim(res5)

with(res2, {
  hist(Annual.Income..k.., main="Distribution of Household Income",   freq=FALSE)
  lines(density(Annual.Income..k..), lty=2, lwd=2)
  xvals = seq(from=min(Annual.Income..k..), to=max(Annual.Income..k..),length=100)
  param = fitdistr(Annual.Income..k.., "lognormal")
  lines(xvals, dlnorm(xvals, meanlog=param$estimate[1],
                      sdlog=param$estimate[2]), col="blue")
} )

logAnnual.Income..k..  = log10(res2$Annual.Income..k..)
hist(logAnnual.Income..k.. , main="Distribution of Household Income", freq=FALSE)
lines(density(logAnnual.Income..k.. ), lty=2, lwd=2)  # line type (lty) 2 is dashed
xvals = seq(from=min(logAnnual.Income..k.. ), to=max(logAnnual.Income..k.. ), length=100)
param = fitdistr(logAnnual.Income..k.. , "normal")
lines(xvals, dnorm(xvals, param$estimate[1],  param$estimate[2]), 
      lwd=2, col="blue")


boxplot(Age ~ as.factor(Annual.Income..k..), data=res2, range=0, outline=F, log="y",
        xlab="# age", ylab="Income")

boxplot(Age ~ wealth, data = res2, main="age by Wealth", Xlab="Category",
        ylab="# age")


# visualization 

library(ggplot2)

# histogram 

View(res)
ggplot(res,aes(x=Age))+
  geom_histogram(aes(y=..density..),binwidth = 4,fill='black',color='white',alpha=0.4,lwd=1.5)+ggtitle("Age of Customers")+
  theme_classic()+xlab("Age")+ylab("Numbers of Customers")

#boxplot
boxplot(res$Age,
        main="Age BoxPlot",
        col="Green",
        border="Black",
        horizontal=TRUE)


##piechart
gender<-res$Gender

gender<-as.factor(gender)

table(gender)

gen<-table(gender) 

percent<-round(gen/400*100)

label<-paste(names(gen), percent , "%" ,sep=" ")

pie(gen,main="Gender Ratio",col = c(7,1),labels= label)


#scatterplot
plot(x=res$Annual.Income..k..,
     y=res$Age,
     main="Scatter Plot",
     xlab="Annual Income",
     ylab="Age",
     xlim=c(15,137),
     ylim = c(18,70))



#algorithms 

# kmeans
library(cluster)
library(rpart)
library(rpart.plot)
library(factoextra)
#Apply kmeans to res2
km <- kmeans(res2 , centers = 3)
print(km)
km$cluster
km$center
#Compare the Gender label with the clustering result
table(res2$Gender, km$cluster)
#Plot the clusters and their centres.
plot(res2$CustomerID,res2$Spending.Score..1.100.,col=km$cluster)
#position of centers
points(km$centers, col = 1:3, pch = 8)
#Appropriate number of clusters
w <- numeric(15)#vector of zeros   
for(i in 1:15) w[i]=sum(kmeans(res2,i)$withinss)
plot( 1:15, w , type='b', xlab="number of cluster", ylab="within grp sum square")

#2-hierarchical clustering 
#data
res.label<-res$Gender
table(res.label)
res_data<-res[c(1,3,4,5)]
View(res_data)
#scale
res_scale=scale(res_data)
#Distance 
r_dist<-dist(res_scale)
#hierarchical clustering
h_cust<-hclust(r_dist,method = "complete")
print(h_cust)
#dendogram
plot(h_cust)
rect.hclust(h_cust,k=3,border = 2:5)
#clusters
res.clusters<-cutree( h_cust , k=3 )
#visualize the cluster
rownames(res_scale)<-paste(res$Gender,1:dim(res),sep = "_")
fviz_cluster(list(data=res_scale,cluster=res.clusters ))
table(res.clusters,res$Gender)

# dbscan 
library(dbscan)
colnames(res2)=c('CustomerID','Gender','Age','AnnualIncome','SpendingScore')
res7=res2[,-1]
res2=as.data.frame(scale(res7))
res2_db=dbscan::dbscan(res2,eps = 2,minPts = 5)
prev= res2_db$cluster
prev
unique(prev)
table(prev)
plot(res2[1:4],col=prev)

