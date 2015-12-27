# Read datasets into R
Datasets<-read.csv("C:/Users/vbui/Google Drive/CUA Fall 2014/CSC530/Project1/ss10pmd1.csv")
# Extract the data into subset included People with College and Highschool degree
WCollege<-subset(Datasets, Datasets$SCHL>=21 & Datasets$WAGP > 0)
HighSchool<-subset(Datasets, Datasets$SCHL==16 & Datasets$WAGP > 0)

# Summary: mean, median, range and interquartiles range. How many missing values a variable has.
summary(WCollege$WAGP)
summary(HighSchool$WAGP)
# Calculate the mean, standard deviation and variance
Mean.WCollege<-mean(WCollege$WAGP)
Mean.WCollege
Mean.HighSchool<-mean(HighSchool$WAGP)
Mean.HighSchool
Std.WCollege<-sd(WCollege$WAGP)
Std.WCollege
Std.HighSchool<-sd(HighSchool$WAGP)
Std.HighSchool
Variance.WCollege<-var(WCollege$WAGP)
Variance.WCollege
Variance.HighSchool<-var(HighSchool$WAGP)
Variance.HighSchool

# A hypothesis test that can help to determine whether a sample has been drawn from a normal distribution
# The null hypothesis for the test is that the sample is drawn from a normal distribution
shapiro.test(Datasets$WAGP[3:5000])
shapiro.test(HighSchool$WAGP[3:5000])
# Conclude: Not follow normal distribution. 

# Confidence Interval for a sample mean with Student's T-Tests
t.test(WCollege$WAGP, conf.level = 0.95)
t.test(WCollege$WAGP, mu=40000, conf.level = 0.95, alternative="greater")
t.test(WCollege$WAGP, mu=40000, conf.level = 0.95, alternative="less")
t.test(HighSchool$WAGP, conf.level = 0.95)
t.test(HighSchool$WAGP, mu=30000, conf.level = 0.95, alternative="greater")
t.test(HighSchool$WAGP, conf.level = 0.95, alternative="less")

boxplot(WCollege$WAGP~SCHL, WCollege,main="Income of people with different degree", xlab="Degree",ylab="Earnings")
# Analysis of Variance
EarningsANOVA<-aov(WAGP~SCHL, WCollege)
anova(EarningsANOVA)
coef(EarningsANOVA)
TukeyHSD(EarningsANOVA)
pairwise.t.test(WCollege$WAGP, WCollege$SCHL, p.adj="bonferroni")
bartlett.test(WAGP~SCHL, WCollege)
var.test(WAGP~SCHL, WCollege)
# Prediction Interval
predict(lm(WCollege$WAGP~1), interval="prediction", level=0.90)[1, ]
predict(lm(HighSchool$WAGP~1), interval="prediction", level=0.95)[1, ]
# ?? Meaning of Prediction Interval??

# Bootstrap goes here
library(boot)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=WCollege, statistic=rsq, R=1000, formula=WCollege$WAGP~SCHL+FOD1P)
# view results
results
plot(results)
# get 95% confidence interval
boot.ci(results, conf = 0.95, type=c("basic"))

# Frequency tables: summarize a categorical variable by displaying the number of observations belonging to each category
# The below command shows how many people get a specified degree (>=21 for bachelors and higher) vs. their income
table(Datasets$WAGP, useNA="ifany")

# Creating plots
# This plot tells Education Attainment vs. Income
plot(WCollege$WAGP[1:500], pch=1, col=2, main="Figure 1. Education Attainment vs. Earnings", xlab="Sample Size", ylab="Earnings")
par(new=T)
plot(HighSchool$WAGP[1:500], pch=2, col=4, axes=F,xlab="",ylab="")
par(new=T)
legend("topright", legend=c("College degree", "Highschool"), pch=c(1,2), col=c(2,4), cex=0.8)

# Find records of people with maximum income
Max_College_HS<-subset(Datasets, (Datasets$SCHL>=21 | Datasets$SCHL==16)& Datasets$WAGP==415000)
Max_College_HS<-subset(Datasets, Datasets$SCHL==16 & Datasets$WAGP==415000 & Datasets$AGEP>50)
Max_College_HS<-subset(Datasets, Datasets$SCHL==16 & Datasets$WAGP==415000 & Datasets$AGEP<50)
HighSchool_minus2<-subset(HighSchool, !HighSchool$row.names == 27632)
Max_College_HS<-subset(HighSchool_minus2, HighSchool_minus2$WAGP==415000 & HighSchool_minus2$AGEP<50)
# \n 0-15: Less than High School diploma\n16: Regular High School Diploma\n17-20: With High School diploma but no college degree\n21-24: bachelor's degree and beyond
# This plot tells income of Bachelors, Masters, Professional degree beyond bachelor and Docterate
plot(WCollege)
plot(WCollege$SCHL, WCollege$WAGP, pch=20, main="Fig 1. Education Attainment vs. Earnings", xlab="Education Attainment", ylab="Earnings")
# Earnings of All majors Bachelors
Bachelor<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0)
plot(Bachelor$WAGP, pch=20, main="Fig 2. Earnings of Bachelors, All majors", xlab="Total number of respondents", ylab="Earnings")
# Earnings of Computer Science Bachelors
par(mfrow=c(1,4))
BachelorCS<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
plot(BachelorCS$WAGP, pch=20, main="Fig 3a. Earnings of Bachelors, Computer Science", xlab="Total number of respondents", ylab="Earnings")
BachelorPsycho<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==5200)
plot(BachelorPsycho$WAGP, pch=20, main="Fig 3b. Earnings of Bachelors, Psychology", xlab="Total number of respondents", ylab="Earnings")
BachelorHistory<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==6402)
plot(BachelorHistory$WAGP, pch=20, main="Fig 3c. Earnings of Bachelors, History", xlab="Total number of respondents", ylab="Earnings")
BachelorBussiness<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==6200)
plot(BachelorBussiness$WAGP, pch=20, main="Fig 3d. Earnings of Bachelors, Bussiness", xlab="Total number of respondents", ylab="Earnings")

par(mfrow=c(1,2))
BachelorCS<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
plot(BachelorCS$WAGP, pch=20, ylim=c(1,500000), main="Fig 2a. Earnings of Bachelors-Computer Science", xlab="Total number of respondents", ylab="Earnings")
BachelorEdu<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==2300)
plot(BachelorEdu$WAGP, pch=20, ylim=c(1,500000), main="Fig 2b. Earnings of Bachelors-General Education", xlab="Total number of respondents", ylab="Earnings")
summary(BachelorCS$WAGP)
summary(BachelorEdu$WAGP)
BachelorCS_EDU<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & (Datasets$FOD1P==2102 | Datasets$FOD1P==2300))
boxplot(BachelorCS_EDU$WAGP~FOD1P, BachelorCS_EDU, main = "Boxplots for Barchelors Comp.Sci & Education", xaxt = "n", xlab = "", ylab = "Earnings")
axis(1, at=1:2, labels=c("CS", "EDU"))

#Plot for differnt degree of the same majors
#CS<-subset(Datasets, (Datasets$SCHL==21 | Datasets$SCHL==22 | Datasets$SCHL==23) & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
CS.Bachelors<-subset(Datasets, Datasets$SCHL==21 & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
CS.Masters<-subset(Datasets, Datasets$SCHL==22 & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
CS.PhDs<-subset(Datasets, Datasets$SCHL==24 & Datasets$WAGP > 0 & Datasets$FOD1P==2102)
plot(CS.Bachelors$WAGP, col=2, xlim=c(1,400),xlab="Samples Size",ylab="Earnings",main="Earnings of CS, All degrees")
par(new=T)
plot(CS.Masters$WAGP, col=4, axes=F,xlab="",ylab="")
par(new=T)
plot(CS.PhDs$WAGP, pch=10, axes=F,xlab="",ylab="")
par(new=T)
legend(locator(1), legend=c("Bachelor", "Master", "PhD"), col=par("red", "blue", "black"), pch=c(1,1,10))

# Histogram 
hist(Datasets$WAGP)
hist(WCollege$WAGP, main = "Histogram of Earnings of people have College degree", xlab = "Earnings", ylab = "Frequency")
hist(HighSchool$WAGP, main = "Histogram of Earnings of people have High School Diploma", xlab = "Earnings", ylab = "Frequency")

# Fit a normal distribution curve to the data
hist(Datasets$WAGP,freq=F)
curve(dnorm(x, mean(Datasets$WAGP), sd(Datasets$WAGP)), add=T)

par(mfrow=c(2,2))
hist(WCollege$WAGP,freq=F, main = "Histogram of Earnings of people have College degree", xlab = "Earnings", ylab = "Frequency")
curve(dnorm(x, mean(WCollege$WAGP), sd(WCollege$WAGP)), add=T)
qqnorm(WCollege$WAGP)
qqline(WCollege$WAGP)
# Conclusion: Data points curve from above the line to below the line and then back to above the line. Data has positive skew/ right-skewed
hist(HighSchool$WAGP,freq=F, main = "Histogram of Earnings of people have High School Diploma", xlab = "Earnings", ylab = "Frequency")
curve(dnorm(x, mean(HighSchool$WAGP), sd(HighSchool$WAGP)), add=T)
qqnorm(HighSchool$WAGP)
qqline(HighSchool$WAGP)

# A normal probability plot to determine whether a sample is drawn from a normal distribution
qqnorm(Datasets$WAGP)
qqline(Datasets$WAGP)

# Conclusion: A sample is not drawn from a normal distribution

# Stem-and-Leaf Plots
stem(Datasets$WAGP)
stem(WCollege$WAGP)
stem(HighSchool$WAGP)

# Bar Charts
barplot(table(Datasets$WAGP, Datasets$SCHL, useNA="ifany"))
barplot(table(WCollege$WAGP, WCollege$SCHL, useNA="ifany"))

# Boxplot
boxplot(Datasets$WAGP~SCHL, Datasets)
boxplot(WCollege$WAGP~SCHL, WCollege)
boxplot(HighSchool$WAGP~SCHL, HighSchool)

#CI & Hypothesis
# Kruskal-Wallis Test
kruskal.test(WCollege$WAGP~SCHL, WCollege)
pairwise.wilcox.test(WCollege$WAGP, WCollege$SCHL)

# Wilcoxon Rank-Sum test
wilcox.test(Bussiness$Earnings, mu=60000, alternative="less", conf.level=0.95)

# Build a model to predict a person's income from their degree and their majors
#model<-lm(Datasets$WAGP~SCHL+FOD1P, Datasets)
model<-lm(WCollege$WAGP~SCHL+FOD1P, WCollege)
formula(model)
summary(model)
coef(model)
confint(model, level=0.95)
residuals(model)
WCollege$resids<-rstudent(model)
WCollege$fittedvals<-fitted(model)
# Create residual plots for a model object

plot(resids~WAGP, WCollege)
plot(resids~SCHL, WCollege)

# Leverage
hatvalues(model)
# Plot of the residuals against the leverage
#Cook's Distance
cooks.distance(model)
par(mfrow=c(3,2))
hist(WCollege$resids)
plot(model, which=2)
plot(model, which=1)
plot(resids~WAGP, WCollege)
plot(resids~SCHL, WCollege)

plot(model, which=4)
plot(model, which=5)
plot(model, which=6)

newdata<-data.frame(SCHL=c(21, 21, 24, 24), FOD1P=c(2300, 2102, 2300, 2102))
newdata$predictions<-predict(model, newdata, interval="confidence", level=0.95)
newdata


