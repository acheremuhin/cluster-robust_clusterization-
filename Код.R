library(tibble)
library(otrimle)
library(readr)
library(ggplot2)
# Создаем базу данных
set.seed(739)
n <- 7500 # numer of points
nr_other_vars <- 4
mat <- matrix(rnorm(nr_other_vars*n),n,nr_other_vars)
me<-4 # mean
x <- c(rnorm(n/3,me/2,1),rnorm(2*n/3,-me/2,1))
y <- c(rnorm(n/3,0,1),rnorm(n/3,me,1),rnorm(n/3,-me,1))
true_clust <- c(rep(1,n/3),rep(2,n/3),rep(3,n/3)) # true clusters
dat <- cbind(mat,x,y)
dat<- as.data.frame(scale(dat)) # scaling
summary(dat)
dat4plot <- dat
dat4plot$true_clust_fct <- factor(true_clust)
p_base <- ggplot(dat4plot,aes(x=x,y=y,color=true_clust_fct)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)

# Кластеризация
clus1 <- InitClust(dat4plot[,c(5,6)], G = 3 , k = 10 , knnd.trim = 0.01 , modelName='E')

clus2 <-otrimle(dat4plot[,c(5,6)], G = 3, npr.max = 0.01, erc = 20, monitor = TRUE)
clus2$code
clus2$flag
clus2$iter # Количество итераций
clus2$logicd
clus2$iloglik
clus2$criterion
clus2$pi
clus2$mean
clus2$cov
clus2$tau
clus2$smd
clus2$cluster
clus2$size
clus2$exproportion
clus2$optimization

library(ClusterR)
external_validation(true_clust, clus2$cluster,summary_stats = TRUE)
plot(clus2, data=dat4plot[,c(5,6)], what="clustering")

clus3 <- otrimleg(dat4plot[,c(5,6)], G=2:4, monitor=1)
clus3$G
clus3$npar
clus3$denscrit
clus3$logicd
clus3$noiseprob
clus3$iloglik
clus3$criterion
clus3$ibic
clus3$ddpm
clus3$solution


