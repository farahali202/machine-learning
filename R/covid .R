#set directory
setwd('C:/Users/Farah/Documents/R workin files')
library(Hmisc)
df=read.csv('COVID19_line_list_data.csv')
#describe data
describe(df)
summary(df)
View(df)

#data preprocessing

#death
df$death_new<-as.integer(df$death !=0)
death_rate=sum(df$death_new)/nrow(df)
death_rate
sum(is.na(df$death_new))
#age
dead=subset(df,death_new==1)
alive=subset(df,death_new==0)
mean(dead$age,na.rm=T)
mean(alive$age,na.rm=T)

median(dead$age,na.rm=T)
median(alive$age,na.rm=T)

diff(range(dead$age,na.rm=T))
diff(range(alive$age,na.rm=T))
#is this statistically significant?
#1. Comparing Means:
t.test(alive$age,dead$age,alternative ="two.sided",conf.level =0.95)
#result:
#1-p-value < 2.2e-16-->(less than the significance level(0.05) 
#reject the null hypothesis(The means of the two groups are equal.)

#2-Confidence Interval:
#Examine the confidence interval for the mean difference.
#If the confidence interval does not include zero, this is consistent with a significant difference.
#If the confidence interval does not include zero, it suggests that the difference is statistically significant.

#gender
male=subset(df,gender=="male")
female=subset(df,gender=="female")
mean(table(male$gender))
mean(table(female$gender))

median(table(male$gender))
median(table(female$gender))

#is this statistically significant?
#1. Comparing Means:
t.test(male$death_new,female$death_new
       ,alternative ="two.sided",conf.level =0.95)
#result:p-value = 0.002105<0.05-->reject the null hypothesis(The means of the two groups are equal.)
#2-Confidence Interval:95%confidence:male have from 1.7% to7,8% higher
#chance of dying

#outliers:
z_scores=scale(df$case_in_country)
outiliers=which(abs(z_scores)>2)
#This line identifies outliers by finding the indices of the data points where the absolute Z-score is greater than 2.
boxplot(df$case_in_country)

#standard deviation
#Magnitude of Dispersion:
#A larger standard deviation indicates a greater degree of variability in the data.
#A smaller standard deviation suggests that the values are more closely packed around the mean.
df$gender=as.numeric(as.character(df$gender),na.rm = TRUE)
sd(df$gender)
var(df$gender)

quantile(df$age,na.rm=T)
median(df$age,na.rm=T)
range(df$age,na.rm=T)
max(df$age,na.rm=T)
min(df$age,na.rm=T)
sd(df$age,na.rm=T)
var(df$age,na.rm=T)
mad(df$age,na.rm=T)
IQR(df$age,na.rm=T)
#64-35
 
df$death_new <- factor(df$death_new, levels = c(0, 1))

#visualization data
library("ggplot2")


ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7, color = "white") 

ggplot(df, aes(x = country)) +
  geom_bar(fill = "red3", alpha = 0.7, color = "white")+coord_flip()

ggplot(df, aes(x = factor(gender))) +
  geom_bar(fill = "grey", alpha = 0.7, color = "white")

#density plot
d=df[!is.na(df$age),]

hist(d$age,col="green4",prob=TRUE)
lines(density(d$age,adjust=4,lwd=3,col="blue"))

#We noticed from the graphs above that the countries with the highest number of COVID-19 cases are China 
#and Japan, accounting for 35% of all cases across 38 countries. 
#Additionally, it is observed that males are the most affected category in terms of COVID-19-related deaths.
#Furthermore, the data suggests that individuals in the older age group (25-80) are more
#susceptible to the virus compared to younger individuals."


