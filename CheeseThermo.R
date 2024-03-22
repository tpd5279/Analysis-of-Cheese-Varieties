#######################################################################################################################
## Cheese Thermophysical data set 
## Author: Tina Dhekial-Phukan
## Primary objective of analysis: Identify the texture of the cheese using the thermophysical characteristics for new 
##                                cheese products
## File includes code for: 
## (1) Data pre-processing and exploratory data analysis.
## (2) Linear Discriminant Analysis.
## (3) Quadratic Discriminant Analysis.
#######################################################################################################################

options(digits=12)

## Install libraries
install.packages(c('tibble', 'dplyr', 'readr'))
library(tibble)
library(dplyr)
library(readr)
install.packages("ggplot2")
install.packages("lattice")
library(caret)
library(MASS)
library(dplyr)
install.packages("car")
library(car)
install.packages("glmtoolbox")
library(glmtoolbox)

## Read in file
cheese <- read.csv(file="cheeseThermophysical.csv")

View(cheese)
dim(cheese)
str(cheese)

################################################################################
## Exploratory data analysis
################################################################################

## Checking for columns with missing values
NA.sum <- colSums(is.na(cheese))
NA.sum

## Descriptive summary statistics
summary(cheese)
apply(cheese[4:9], 2, mean)
apply(cheese[4:9], 2, sd)

## Converting texture as a factor variable
cheese$texture <- as.factor(cheese$texture)

## Checking for zero values in the FD column
sum(cheese$G80 == 0)
sum(cheese$vLTmax == 0)
sum(cheese$vCO == 0)
sum(cheese$Fmax == 0)
sum(cheese$FD == 0)
sum(cheese$FO == 0)

## Box plots of the numeric variables grouped by texture
library(patchwork)
bxplt1 <- ggplot(cheese, aes(x=texture, y=G80,color=texture))+ geom_boxplot()
bxplt2 <- ggplot(cheese, aes(x=texture, y=vLTmax,color=texture))+ geom_boxplot()
bxplt3 <- ggplot(cheese, aes(x=texture, y=vCO,color=texture))+ geom_boxplot()
bxplt4 <- ggplot(cheese, aes(x=texture, y=Fmax,color=texture))+ geom_boxplot()
bxplt5 <- ggplot(cheese, aes(x=texture, y=FD,color=texture))+ geom_boxplot()
bxplt6 <- ggplot(cheese, aes(x=texture, y=FO,color=texture))+ geom_boxplot()
(bxplt1/bxplt2/bxplt3|bxplt4/bxplt5/bxplt6)

## Plotting correlation matrix of the numeric variables to check multicollinearity
install.packages("corrplot")
library(corrplot)
corr_mat <- cor(cheese[c(-1:-3)], method = "pearson", use = "pairwise.complete.obs")
par(mfrow=c(1,1))
corrplot(corr_mat, method = 'number', tl.cex=0.7, number.cex = 0.6, type = "lower", diag = FALSE)
corrplot(corr_mat, method = 'color', tl.cex=0.6, type = "lower", diag = FALSE,
         mar=c(0,0,1,0), cex.main = 0.9)

## Plot univariate histograms of the numeric variables
vnames <- names(cheese[c(-1:-3)])

## Histograms of the thermophysical variables
par(mfrow=c(2,3))
for (v in vnames) 
{
  hist(cheese[, v], main="", ylab="Freq", xlab = paste(v, sep=""))
}

hist(log(cheese$G80), ylab="Freq", xlab="G80_log")

## Frequency of varieties of cheese texture
table(cheese$texture)

## Creating a new column for logarithm of G80
cheese$G80_log <- log(cheese$G80)

################################################################################
## Create training and test data sets using stratified sampling
################################################################################

cheese[4:9] <- scale(cheese[4:9])
apply(cheese[4:9], 2, mean)
apply(cheese[4:9], 2, sd)

## Tabulate frequencies for the two levels of the categorical variables
set.seed(1)
train <- createDataPartition(paste(cheese$texture, sep = ""), p = 0.7, list = FALSE)
train.data <- cheese[train, ]
dim(train.data)
test.data <- cheese[-train, ]
dim(test.data)
cheese.train <- cheese$texture[train]
length(cheese.train)
cheese.test <- cheese$texture[-train]
length(cheese.test)

################################################################################
## Linear Discriminant Analysis
################################################################################

## Run Linear Discriminant Analysis
lda.fit <- lda(texture ~ G80 + vLTmax + vCO + Fmax + FD + FO, data = train.data)
lda.fit
lda.pred <- predict(lda.fit, test.data)
lda.class <- lda.pred$class
table(lda.class, cheese.test)
mean(lda.class == cheese.test)

## Run Linear Discriminant Analysis
lda.fit1 <- lda(texture ~ G80_log + vLTmax + vCO + Fmax + FD + FO, data = train.data)
lda.fit1
lda.pred1 <- predict(lda.fit1, test.data)
lda.class1 <- lda.pred1$class
table(lda.class1, cheese.test)
mean(lda.class1 == cheese.test)

#define data to plot
lda_plot <- cbind(train.data, predict(lda.fit)$x)

#create plot
ggplot(lda_plot, aes(LD1, LD2)) + geom_point(aes(color = texture)) 
ggplot(lda_plot, aes(LD2, LD3)) + geom_point(aes(color = texture))
ggplot(lda_plot, aes(LD1, LD3)) + geom_point(aes(color = texture))

install.packages("ggord")
library(ggord)
ggord(lda.fit, train.data$texture, ylim = c(-10, 10))

################################################################################
## Quadratic Discriminant Analysis
################################################################################

## Run Quadratic Discriminant Analysis
qda.fit <- qda(texture ~ G80 + vLTmax + vCO + Fmax + FD + FO, data = train.data)
qda.fit
qda.pred <- predict(qda.fit, test.data)
qda.class <- qda.pred$class
table(qda.class, cheese.test)
mean(qda.class == cheese.test)
