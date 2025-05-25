gym<- gym_members_exercise_tracking
str(gym)
summary(gym)
#data preperation
gym$Gender<- as.factor(gym$Gender)
gym$Workout_Type<- as.factor(gym$Workout_Type)
gym$Experience_Level<- as.factor(gym$Experience_Level)
summary(gym)
sum(duplicated(gym))
anyNA(gym)
# descriptives
library(psych)
describe(gym)
library(dplyr)
gym.quan <- gym[,-c(2,10,14)]  #excluding the categorical variables 
View(gym.quan)  
mean.quan<- colMeans(gym.quan)
mean.quan
#varriance covariance mattrix 
var.quan<- var(gym.quan)
var.quan
#correlation matrix
cor.quan<- cor(gym.quan)
cor.quan
cor.qual<- cor(as.numeric(gym$Workout_Type),as.numeric(gym$Experience_Level),method="spearman")
cor.qual   #spearman corr for categorical variables
# outliers 
boxplot(gym.quan)
gym.o<- gym.quan
mahalanobis(gym.o,mean.quan,var.quan)
gym.o$mahalanobis<- mahalanobis(gym.o,mean.quan,var.quan)
gym.o$pvalue <- pchisq(gym.o$mahalanobis,df= ncol(gym.o)-1,lower.tail = FALSE)
View(gym.o)
#BACON
library(wbacon)
w<-wBACON(gym.quan)
which(is_outlier(w))
summary(w)
plot(w)
#nadas part#######

#graphs
library(GGally)
ggpairs(gym.quan)
library(ggcorrplot)
ggcorrplot(cor.quan)
#1)descremenent 


#2)cluster
gym.c <- scale(gym.quan)
View(gym.c)
d<- dist(gym.c, method = "euclidean")
fit<- hclust(d , method = "ward.D")
plot(fit)   #complex ; large sample size
rect.hclust(fit, k=3 ,border = "red")
groups<- cutree(fit, k=3)
table(groups)
gym.c <- data.frame(gym.c) 
gym.c$gr <- groups
gym.c %>%
  group_by(gr) %>%
  summarise_at(vars(1:12), mean, na.rm = TRUE)

#1 and 2 are overlapping
#elbow 
wss <- function(data, hc, max_k) {
  sapply(1:max_k, function(k) {
    clusters <- cutree (hc, k)
    sum(sapply(split (data, clusters), function(cluster) {
      cluster <- matrix (unlist (cluster), ncol = ncol (data), byrow = FALSE)
      sum(rowSums ((cluster - colMeans (cluster))^2))
    }))
  })
  }

wss_values_fit <- wss (gym.c, hc= fit, max_k = 12)
plot(1:12,wss_values_fit, type = "b" , pch= 19, frame = FALSE)

#diana
library(cluster)
gym.d<- scale(gym.quan)
result<- diana(gym.d)
result.h <- as.hclust(result)
plot(result.h)
rect.hclust(result.h, k=3 , border = "red")


groups.d<- cutree(result.h, k=3)
gym.d <- data.frame(gym.d) 
gym.d$gr <- groups.d
gym.d %>%
  group_by(gr) %>%
  summarise_at(vars(1:12), mean, na.rm = TRUE)
ggplot(gym.d, aes(x= Age , y= Weight..kg. ,col=factor(gr))) + geom_point()

#elbow 
wss <- function(data, hc, max_k) {
  sapply(1:max_k, function(k) {
    clusters <- cutree (hc, k)
    sum(sapply(split (data, clusters), function(cluster) {
      cluster <- matrix (unlist (cluster), ncol = ncol (data), byrow = FALSE)
      sum(rowSums ((cluster - colMeans (cluster))^2))
    }))
  })
}

wss_values_fit.d <- wss (gym.d, hc= result.h, max_k = 12)
plot(1:12,wss_values_fit.d, type = "b" , pch= 19, frame = FALSE)

