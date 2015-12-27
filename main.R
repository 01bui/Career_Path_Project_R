# Read datasets into R
Bussiness<-read.csv("C:/Users/vbui/Google Drive/CUA Fall 2014/CSC530/Project1/Business.csv")
Science<-read.csv("C:/Users/vbui/Google Drive/CUA Fall 2014/CSC530/Project1/Science.csv")
Bussiness_Science<-read.csv("C:/Users/vbui/Google Drive/CUA Fall 2014/CSC530/Project1/Bussiness_Science.csv")
# Summary: mean, median, range and interquartiles range. How many missing values a variable has.
summary(Bussiness$Earnings)
summary(Science$Earnings)
# Calculate the mean, standard deviation and variance
Mean.Bussiness<-mean(Bussiness$Earnings)
Mean.Science<-mean(Science$Earnings)
Std.Bussiness<-sd(Bussiness$Earnings)
Std.Science<-sd(Science$Earnings)
Variance.Bussiness<-var(Bussiness$Earnings)
Variance.Science<-var(Science$Earnings)
# Covariance matrix for a whole dataset
cov(Bussiness$Earnings,Science$Earnings, use="complete")
# Correlation Coefficient
cor(Bussiness$Earnings,Science$Earnings, use="complete")
# Hypothesis Test of Correlation
# The null hypothesis for the test is that the population correleation is equal to 0, meaning that there is no correlation between the variables
# Perform a two-sided test of the correletion between Employment and Unemployment at 5% significance level
cor.test(Bussiness$Earnings,Science$Earnings, use="complete", conf.level=0.95)
# A hypothesis test that can help to determine whether a sample has been drawn from a normal distribution
# The null hypothesis for the test is that the sample is drawn from a normal distribution
shapiro.test(Bussiness$Earnings)
shapiro.test(Science$Earnings)
# Confidence Interval for a sample mean with Student's T-Tests
t.test(Bussiness$Earnings, conf.level = 0.99)
t.test(Bussiness$Earnings, conf.level = 0.99, alternative="greater")
t.test(Bussiness$Earnings, conf.level = 0.99, alternative="less")
t.test(Science$Earnings, conf.level = 0.99)
t.test(Science$Earnings, conf.level = 0.99, alternative="greater")
t.test(Science$Earnings, conf.level = 0.99, alternative="less")
# Prediction Interval
predict(lm(Bussiness$Earnings~1), interval="prediction", level=0.95)[1, ]
predict(lm(Science$Earnings~1), interval="prediction", level=0.95)[1, ]
# Frequency tables: summarize a categorical variable by displaying the number of observations belonging to each category
table(Bussiness$Occupation)
table(Science$Occupation)
table(Bussiness$Earnings<60000)
table(Science$Earnings)
summary(Bussiness)
summary(Science)
# Creating plots
#plot(Bussiness$Earnings, Science$Earnings)
#abline(coef(lm(Bussiness$Earnings, Science$Earnings)))
# Fit a normal distribution curve to the data
hist(Bussiness$Earnings,freq=F)
curve(dnorm(x, mean(Bussiness$Earnings), sd(Bussiness$Earnings)), add=T)
hist(Science$Earnings,freq=F)
curve(dnorm(x, mean(Science$Earnings), sd(Science$Earnings)), add=T)
# A normal probability plot to determine whether a sample is drawn from a normal distribution
qqnorm(Bussiness$Earnings)
qqline(Bussiness$Earnings)
qqnorm(Science$Earnings)
qqline(Science$Earnings)
#Stem-and-Leaf Plots
stem(Bussiness$Earnings)
stem(Science$Earning,)
# Boxplot
boxplot(Bussiness_Science$Earnings~Majors, Bussiness_Science)
# t-test
t.test(Bussiness$Earnings, mu=60000, alternative="less", conf.level=0.95)
t.test(Earnings~Majors, Bussiness_Science, Majors %in% c("Bussiness","Science"))
# Wilcoxon Rank-Sum test
wilcox.test(Earnings~Majors, Bussiness_Science, Majors %in% c("Bussiness","Science"))
wilcox.test(Bussiness$Earnings, mu=60000, alternative="less", conf.level=0.95)
# Analysis of Variance
EarningsANOVA<-aov(Earnings~Majors, Bussiness_Science)
anova(EarningsANOVA)
coef(EarningsANOVA)








