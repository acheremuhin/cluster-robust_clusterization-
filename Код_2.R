library(ggplot2)
# Создаем базу данных
set.seed(739)
n <- 500 # numer of points
y <- abs(c(rnorm(n,5,2)))
x <- c(rnorm(n,5,1))/sqrt(y)
class<-rep(1,n)
dat1 <- as.data.frame(cbind(x,y,class))
y <- c(rnorm(n,8,0.4))
x <- rlnorm(n, meanlog = log(7), sdlog = 0.2)
class<-rep(2,n)
dat2 <- as.data.frame(cbind(x,y,class))
y <- c(runif(n, min = 2, max = 7))
x <- c(rnorm(n,8,0.4))
class<-rep(3,n)
dat3 <- as.data.frame(cbind(x,y,class))
y <- c(runif(n/10, min = 0, max = 10))
x <- c(runif(n/10, min = 0, max = 10))
class<-rep(0,n/10)
dat4 <- as.data.frame(cbind(x,y,class))
dat <- rbind(dat1,dat2,dat3,dat4)
colnames(dat) <- c("x","y", "class")
dat$class <- as.factor(dat$class)
dat_test <- as.data.frame(dat)
p_base <- ggplot(dat,aes(x=x,y=y,color=class)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

# Кластеризация
library(otrimle)
clus <- otrimleg(dat[,c(1,2)], G=2:5, monitor=1)
clus$solution
clus$ibic
clus$iloglik

clus_2 <-otrimle(dat[,c(1,2)], G = 5, npr.max = 0.01, erc = 20, monitor = TRUE)
clus_2$code
clus_2$flag
clus_2$iloglik
clus_2$mean # центра кластеров
head(clus_2$tau) # вероятности принадлежности к кластерам
head(clus_2$cluster) # принадлежность к кластерам
dat_2<-cbind(dat[,1:2],clus_2$cluster)
colnames(dat_2) <- c("x","y", "class")
dat_2$class <- as.factor(dat_2$class)
p_base <- ggplot(dat_2,aes(x=x,y=y,color=class)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

clus_3 <-otrimle(dat[,c(1,2)], G = 3, npr.max = 0.01, erc = 20, monitor = TRUE)
clus_3$code
clus_3$flag
dat_3<-cbind(dat[,1:2],clus_3$cluster)
colnames(dat_3) <- c("x","y", "class")
dat_3$class <- as.factor(dat_3$class)
p_base <- ggplot(dat_3,aes(x=x,y=y,color=class)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

clus_4 <-otrimle(dat[,c(1,2)], G = 4, npr.max = 0.01, erc = 20, monitor = TRUE)
clus_4$code
clus_4$flag
dat_4<-cbind(dat[,1:2],clus_4$cluster)
colnames(dat_4) <- c("x","y", "class")
dat_4$class <- as.factor(dat_4$class)
p_base <- ggplot(dat_4,aes(x=x,y=y,color=class)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

clus_5 <-otrimle(dat[,c(1,2)], G = 2, npr.max = 0.01, erc = 20, monitor = TRUE)
clus_5$code
clus_5$flag
dat_5<-cbind(dat[,1:2],clus_5$cluster)
colnames(dat_5) <- c("x","y", "class")
dat_5$class <- as.factor(dat_5$class)
p_base <- ggplot(dat_5,aes(x=x,y=y,color=class)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

library(ClusterR)
external_validation(true_clust, clus_5$cluster,summary_stats = TRUE)