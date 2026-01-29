#' DESCRIPTION: going over key terminology
#' Script for introductory work
library(tidyverse)
# in-class ----------------------------------------------------------------

#nonparametric test
x<-c(3.2,5,10,100,50)
y<-c(1,2,3,2.1,1.2)

# nonparametric vs parametric ---------------------------------------------

df_xy <- tibble(group = c(rep("x", length(x)),
       rep("y", length(y))),
       value = c(x,y)
       )

df_xy%>%
  ggplot(aes(x=group,
             y=value))+
  geom_boxplot()

#parametric test
t.test(x,y)

#nonparametric test
wilcox.test(x,y)

#ANOVA is parametric

aov(weight~group,
    data=PlantGrowth)

#Kruskal wallis test is nonparametric version of anova

kruskal.test(weight~group,
             data=PlantGrowth)


# confidence interval -----------------------------------------------------


m<-lm(Petal.Length ~ Petal.Width,
      data=iris)

#gives range for 5% confidence interval, if interval overlaps 0, nonsignificant
confint(m)

# covariance vs correlation -----------------------------------------------

##Covariance and correlation both assess the direction of the linear
##relationship between variables (one variable is continuous)

##However, correlation also tells us about the strength of the relationship
##Covariance results range from negative infinity to positive infinity, aka
##nonstandardized results, while correlation results range from -1 to 1, so 
##standardized results

x<-rnorm(100,0,1)
y<-rnorm(100,0.8*x,1)

plot(x~y)

#gives correlation coefficient, also parametric
cor.test(x,y)

#nonparametric version called spearman
cor.test(x,y,method="spearman")

#gives covariance
cov(x,y)

#gives same value as cor.test
cov(x,y)/(sd(x)*sd(y))