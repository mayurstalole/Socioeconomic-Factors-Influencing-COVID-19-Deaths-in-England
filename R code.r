install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
library(corrplot)
library(psych)
library(rcompanion)
library(corrgram)


# set working directory
setwd(dirname(file.choose()))
getwd()

# Loading Final.csv having the data into the variable ds.
ds <- read.csv("Final1.csv")


# Load the required libraries
library(corrplot)

# Check for missing values in the dataset
summary(is.na(ds))
head(ds)
# Remove rows with any missing values
ds <- na.omit(ds)

# Check data types of each column
str(ds)

# Use summary statistics, histograms, boxplots, etc., to explore and understand the data
summary(ds)

# Save the cleaned dataset to a new CSV file
write.csv(ds, "CleanedDataset.csv", row.names = FALSE)

#Find mean, standard Deviation, Max And Min of the Dependent variable.
mean(ds$TotalDeathsPerThousand)
sd (ds$TotalDeathsPerThousand)
max(ds$TotalDeathsPerThousand)
min(ds$TotalDeathsPerThousand)


# boxplot of Dependent variable
boxplot(ds$TotalDeathsPerThousand, xlab="Total Deaths by Local Population Per Thousand", ylab="Count", main = "Boxplot of Total Deaths")
qqnorm(ds$TotalDeathsPerThousand, xlab = "TotalDeaths", ylab = "Count")
qqline(ds$TotalDeathsPerThousand, col=2) ## red color
ks.test(ds$TotalDeathsPerThousand, pnorm, mean(ds$TotalDeathsPerThousand), sd(ds$TotalDeathsPerThousand))
shapiro.test(ds$TotalDeathsPerThousand)

plotNormalHistogram(ds$TotalDeathsPerThousand, main = "Histogram", xlab = "Total Deaths by local population per thousand")


#------------ correlation matrix--------------------

# select variables by excluding dependent variables and others not required

ds1 <- ds[, -c(1,2)]
cor(ds1,method ="pearson")

#Multivariate analysis on indepndent variables.
multvaranalysis <- ds[, -c(1,2,3)]
pairs(multvaranalysis, main = "Pairs Plot")
cor_matrix <- cor(multvaranalysis, use = "complete.obs")

pairs.panels(ds1, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")

library(corrgram)


# Draw the corrgram which works best with pearson correlation.

corrgram(ds1, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="variables")



# test correlation of dependent variable with all independent variables
cor.test(ds$TotalDeathsPerThousand, ds$Age0To19, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$Age20To29, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$age30to59, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$age60andabove, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$UnsharedBungalow, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$SharedDwelling, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$GoodHealth, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$FairHealth, method = "spearman")
cor.test(ds$TotalDeathsPerThousand, ds$PoorHealth, method = "spearman")


# looking at internal correlations between independent variables
cor.test(ds$Age0To19, ds$GoodHealth, method = "spearman")
cor.test(ds$Age20To29, ds$FairHealth, method = "spearman")
cor.test(ds$Age30To59, ds$PoorHealth, method = "spearman")
cor.test(ds$Age60AndAbove, ds$SharedDwelling, method = "spearman")
cor.test(ds$FairHealth, ds$UnsharedBungalow, method = "spearman")
cor.test(ds$PoorHealth, ds$UnsharedFlat, method = "spearman")
cor.test(ds$Age20To29, ds$UnsharedBungalow, method = "spearman")
cor.test(ds$Age30To59, ds$SharedDwelling, method = "spearman")
cor.test(ds$Age0To19, ds$UnsharedFlat, method = "spearman")
cor.test(ds$Age60AndAbove, ds$FairHealth, method = "spearman")
cor.test(ds$GoodHealth, ds$Age30To59, method = "spearman")
cor.test(ds$PoorHealth, ds$Age20To29, method = "spearman")
cor.test(ds$UnsharedBungalow, ds$Age30To59, method = "spearman")
cor.test(ds$SharedDwelling, ds$Age20To29, method = "spearman")
cor.test(ds$UnsharedFlat, ds$GoodHealth, method = "spearman")
cor.test(ds$Age0To19, ds$FairHealth, method = "spearman")
cor.test(ds$Age30To59, ds$UnsharedBungalow, method = "spearman")
cor.test(ds$Age20To29, ds$SharedDwelling, method = "spearman")
cor.test(ds$Age60AndAbove, ds$PoorHealth, method = "spearman")
cor.test(ds$GoodHealth, ds$UnsharedFlat, method = "spearman")
cor.test(ds$FairHealth, ds$Age0To19, method = "spearman")
cor.test(ds$PoorHealth, ds$Age60AndAbove, method = "spearman")
cor.test(ds$UnsharedBungalow, ds$GoodHealth, method = "spearman")
cor.test(ds$SharedDwelling, ds$PoorHealth, method = "spearman")
cor.test(ds$UnsharedFlat, ds$Age60AndAbove, method = "spearman")
cor.test(ds$Age0To19, ds$UnsharedBungalow, method = "spearman")
cor.test(ds$Age30To59, ds$FairHealth, method = "spearman")



#partial correlation

library(ppcor)

#calculate partial correlation using Pearson and then Spearman
pcor.test(ds$TotalDeathsPerThousand, ds$Age30To59, ds$UnsharedBungalow)
pcor.test(ds$TotalDeathsPerThousand, ds$FairHealth, ds$Age0To19)
pcor.test(ds$TotalDeathsPerThousand, ds$UnsharedBungalow, ds$Age30To59, method="spearman")
pcor.test(ds$TotalDeathsPerThousand, ds$Age0To19, ds$FairHealth, method="spearman")



cor0 <- cor(ds1, method = "pearson")
round(cor0, 2)
library(corrplot)
corrplot(cor0, type = "upper", tl.col = "black", tl.srt = 45)

correl1 <- ds1[, -c(2,7,9)]
cor1 <- cor(correl1, method = "pearson")
round(correl1, 2)
library(corrplot)


# Check the class of corr1
class(cor1)

# If it's a list, try to convert it to a numeric matrix
cor1 <- as.matrix(cor1)

# Check the class again
class(cor1)

# Plot the corr1 matrix
corrplot(cor1, type = "upper", tl.col = "black", tl.srt = 45)


#-----Section 04-------------------------------------------
library(nFactors)
library(psych)
#cor.test(x, y,
#	alternative = c("two.sided", "less", "greater"),
#	method = c("pearson", "kendall", "pearson"),
#	exact = NULL, conf.level = 0.95, continuity = FALSE, ...)

varforKMO <- correl1[, -c(1)]

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(varforKMO))

# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(varforKMO))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")


#-----Section 08-------------------------------------------

# Varimax Rotated Principal Components
# retaining 'nFactors' components

library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(ds1, nfactors=4, rotate="varimax")
fit

# create four variables to represent the rotated components
fit$scores
fit.data <- data.frame(fit$scores)

#------------- Multiple Regression------------------------

# model with all variables
attach(ds)
model1 <- lm(TotalDeathsPerThousand ~ UnsharedBungalow + UnsharedFlat + 
               SharedDwelling + Age0To19 + Age20To29 + Age30To59 +
               Age60AndAbove + GoodHealth + FairHealth + PoorHealth )

summary(model1)
# calculate variance inflation factor
library(car)
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high


# use a stepwise approach to search for a best model
library(RcmdrMisc)

# forward stepwise selection
model2 <- stepwise(model1, direction = "forward")
summary(model2)
hist(model2$residuals)
rug(model2$residuals)
plot(model2$residuals ~ model2$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model2$residuals, "pnorm", mean(model2$residuals), sd(model2$residuals))
sqrt(vif(model2)) > 2
calc.relimp(model2, type = c("lmg"), rela = TRUE)


model3 <- lm(TotalDeathsPerThousand ~  UnsharedFlat + Age20To29 + 
               Age60AndAbove  + FairHealth + PoorHealth )
summary(model3)
sqrt(vif(model3)) > 2

# use a stepwise approach to search for a best model
library(RcmdrMisc)

# forward stepwise selection
model4 <- stepwise(model3, direction = "forward")
summary(model4)
hist(model4$residuals)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
sqrt(vif(model4)) > 2
library(relaimpo)
calc.relimp(model4, type = c("lmg"), rela = TRUE)

anova(model1, model2, test = "F")
anova(model3, model4, test = "F")
