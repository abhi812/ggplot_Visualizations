
#Homework 2, Abhishek kumar Gupta

library(tidyverse)
data(mpg)

#1.(a, 3.2.4 Exercises#4) Make a scatterplot of  hwy  vs  cyl
ggplot(data = mpg)+ geom_point(mapping = aes(x=cyl, y=hwy))

#1.(a, 3.2.4 #5) What happens if you make a scatterplot of  class  vs  drv ? 
#Why is the plot not useful?
ggplot(data = mpg)+ geom_point(mapping = aes( x=drv, y=class))

# The plot is not much useful because
#there is not any trend in the plot and nothing can be concluded out of the plot.


#1.(a, 3.3.1 Exercises#3) Map a continuous & categorical variable to color, size and shape

#continuous variable map to color
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, color=cty))

#continuous variable map to size
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, size=cty))

#continuous variable map to shape
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, shape=cty))

#categorical variable map to color
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

#categorical variable map to size
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, size=class))

#categorical variable map to shape
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))

#1.(a, 3.3.1 Exercises#4) What happens if you map the same variable to multiple aesthetics?
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, color=class, size=class, shape=class))
#We get a more refined plot where each feature is distinctily shown by differnt aesthetics

# 1.(a, 3.3.1 Exercises#6). What happens if you map an aesthetic to something other than a variable name,
#like aes(colour = displ < 5) ?
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, color=displ<5))

# here it shows different color for values which are true i.e <5 and different for >5

# 1.(a, 3.5.1 Exercises#4)What are the advantages to using faceting instead of the colour aesthetic?
#What are the disadvantages?
#How might the balance change if you had a larger dataset?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +    facet_wrap(~ class, nrow = 2)

#1(b)After reading this chapter, you should be ready to reproduce the plot in Figure 1 
#using the same mpg data from above. Please do so.

p<-ggplot(data = mpg) +  
  geom_point(mapping = aes(x=displ, y=hwy, alpha=cty), position = "jitter") +
  geom_smooth(mapping = aes(x = displ, y = hwy),method=lm,color="black", se=FALSE)+
  geom_smooth(mapping = aes(x = displ, y = hwy))+
  facet_wrap(~drv)
p+labs(x="Displacement", y="Highway Mpg")

#Problem 2(a.) I have used random normal distribution, random poisoon distribution, random binomial distribution
#and random chi square distribution. gather function used to join all the four variables.

df<- data.frame(a=rnorm(500),b=rpois(500, lambda=3),c=rbinom(500,20,0.5),d=rchisq(500, df=3))
head(df)
library(dplyr)
df2<-gather(df,key="groupVar", value="value")
head(df2)

#Problem 2(b.) Plot the densities of each distribution overlaid on each other on one plot
library(ggplot2)
ggplot(df2,mapping= aes(fill = groupVar, x=value))+ geom_density(alpha=0.4)

#problem-3 Housing proce data visualization

#read the CSV file form the working folder
myData<-read.csv("housingData.csv")

# scatter plot of sale price vs. lot area with housestyle as a color mapping
ggplot(data = myData)+
  geom_point(mapping = aes(x=LotArea, y=SalePrice, color=HouseStyle))

# bar grap plot of neoghborhood  with fill of MSZoning
ggplot(data = myData, mapping = aes(x=Neighborhood, fill=MSZoning))+
  geom_bar(alpha=0.5)

# bar grap plot of neoghborhood  with fill of lotshape and dodge aesthetic
ggplot(data = myData, mapping = aes(x=Neighborhood, fill=LotShape))+
  geom_bar(alpha=0.5, position = "dodge")

# box plot of condition1  with saleprice
ggplot(data = myData, mapping = aes(x=Condition1, y=SalePrice))+
  geom_boxplot()

# bar grap plot of overall quality  with fill of external qualtity along with plot in polar coordinates
bar<- ggplot(data=myData)+
  geom_bar(mapping=aes(x=OverallQual, fill=ExterQual))
bar
bar+coord_polar()



#problem 4: Missing Data
#(4a.)Explore the missingness of the data

#load the library Amelia and data freetrade
library(Amelia)
data(freetrade)

#load package mice for missingness analysis
install.packages("mice")
library(mice)

# gives the number of observation per variable pair
md.pairs(freetrade)

# display missing data pattern
md.pattern(freetrade)

#load package VIM for missingness analysis
library(VIM)

#used VIM's "aggr" function to also get overall information on missing
a<-aggr(freetrade)
summary(a)

# use VIM function "marginplot" to get a scatter plot that includes information on missing values
marginplot(freetrade[c("tariff","fiveop")], col=c("blue","red","orange"))

#looking at all of the plots with Missing Information
scattmatrixMiss(freetrade)


#(4b.) statistical test chi-square  used to determine if the missingness in the 
#tariff variable is independent with the country variable

chisq.test(freetrade$tariff,freetrade$country)

#removed Nepal from the data and again prformed Chisq test
freetrade=freetrade[which(freetrade$country!="Nepal"), ]
chisq.test(freetrade$tariff,freetrade$country)

#removed Philippines from the data and again prformed Chisq test
library(Amelia)
data(freetrade)
freetrade=freetrade[which(freetrade$country!="Philippines"), ]
chisq.test(freetrade$tariff,freetrade$country)

