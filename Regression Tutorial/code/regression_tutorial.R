##Beginners Guide to Regression Analysis and Plot Interpretations
#"The road to machine learning starts with Regression. Are you ready?"
#Source: https://www.hackerearth.com/pt-br/practice/machine-learning/machine-learning-algorithms/beginners-guide-regression-analysis-plot-interpretations/tutorial/
#Author: Manish Saraswat

#set data working directory
path <- paste0(getwd(),"/data")
download.file("http://blog.hackerearth.com/wp-content/uploads/2016/12/airfoil_self_noise.csv", destfile = paste0(path, "/airfoil_self_noise.csv"))
#setwd(path)

#load data and check data
# Originally, the data set is available in .txt file on UCI: https://archive.ics.uci.edu/ml/datasets/Airfoil+Self-Noise
mydata <- read.csv("http://blog.hackerearth.com/wp-content/uploads/2016/12/airfoil_self_noise.csv")
str(mydata)
#’data.frame':	1503 obs. of  6 variables:
#$ Frquency.Hz.        : int  800 1000 1250 1600 2000 2500 3150 4000 5000 6300 ...
# $ Angle_of_Attack     : num  0 0 0 0 0 0 0 0 0 0 ...
#$ Chord_Length        : num  0.305 0.305 0.305 0.305 0.305 ...
# $ Free_stream_velocity: num  71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 ...
#$ Displacement        : num  0.00266 0.00266 0.00266 0.00266 0.00266 ...
#$ Sound_pressure_level: num  126 125 126 128 127 ...

#This data has 5 independent variables and Sound_pressure_level as the dependent variable (to be predicted). 
#In predictive modeling, we should always check missing values in data. 
#If any data is missing, we can use methods like mean, median, and predictive modeling imputation to make up for missing data.
#check missing values
colSums(is.na(mydata))
#        Frquency.Hz.      Angle_of_Attack         Chord_Length 
#                   0                    0                    0 
#Free_stream_velocity         Displacement Sound_pressure_level 
#                   0                    0                    0 

#This data set has no missing values. Good for us! 
#Now, to avoid multicollinearity, let's check correlation matrix.
cor(mydata)
#Angle_of_Attack and Displacement show 75% correlation.
#Usually, correlation above 80% (subjective) is considered higher. 
#We can forego this combination and won't remove any variable.
#In R, the base function lm is used for regression. 
#We can run regression on this data by:
regmodel <- lm(Sound_pressure_level ~ ., data = mydata)
summary(regmodel)

#~ . tells lm to use all the independent variables. 
#Let's understand the regression output in detail:
# Intercept - This is the βo value. 
# It's the prediction made by model when all the independent variables are set to zero.
# Estimate - This represents regression coefficients for respective variables. It's the value of slope. 
# Let's interpret Estimate for Chord_Length. When Chord_Length is increased by 1 unit, holding other variables constant, Sound_pressure_level decreases by a value of -35.69.
# Std. Error - This determines the level of variability associated with the estimates. 
# Smaller the standard error of an estimate is, more accurate will be the predictions.
# t value - t statistic is generally used to determine variable significance, i.e. if a variable is significantly adding information to the model. 
# t value > 2 suggests the variable is significant. I used it as an optional value as the same information can be extracted from the p value.
# p value - It's the probability value of respective variables determining their significance in the model.
# p value < 0.05 is always desirable.

# The adjusted R² implies that our model explains ~51% total variance in the data. 
# And, the overall p value of the model is significant. 

# Can we still improve this model ? Let's try to do it. 
# Now, we'll check the residual plots, understand the pattern and derive actionable insights (if any): 

#set graphic output
png(filename=paste0(path,"/graphics/residuals.png"))

par(mfrow=c(2,2))

#create residual plots
plot (regmodel)
dev.off()

# Residual vs. Fitted value catches attention. I see signs of heteroskedasticity in this data. 
# A collection of random variables is heteroscedastic if there are sub-populations that have different variabilities from others. 
# "Variability" could be quantified by the variance or any other measure of statistical dispersion.
#To overcome this situation, we'll build another model with log(y).
#The existence of heteroscedasticity is a major concern in the application of regression analysis, 
#including the analysis of variance, as it can invalidate statistical tests of significance that 
#assume that the modelling errors are uncorrelated and uniform—hence that their variances do not 
#vary with the effects being modeled.
regmodel <- update(regmodel, log(Sound_pressure_level)~.)
summary(regmodel)
#The improvement isn't significant, we've increased our adjusted R² to 52.19%. 

#Let's divide the data set into train and test to check our final evaluation metric. We'll keep 70% data in train and 30% in test file. 
#The reason being that we should keep enough data in train so that the model identifies obvious emerging patterns.
#sample
set.seed(1)
d <- sample ( x = nrow(mydata), size = nrow(mydata)*0.7)
train <- mydata[d,] #1052 rows 
test <- mydata[-d,] #451 rows

#train model
regmodel <- lm (log(Sound_pressure_level)~.,data = train)
summary(regmodel)

#test model
regpred <- predict(regmodel, test)

#convert back to original value
regpred <- exp(regpred)

#install.packages("Metrics", dependencies = T)
library(Metrics)
rmse(actual = test$Sound_pressure_level,predicted = regpred)
#[1] 5.034019

#Interpretation of RMSE error will be more helpful while comparing it with other models. 
#5.03 error is the optimal value we could expect. 
#You should spend more time and try to obtain a lower error rate than 5.03. 
#For example, you should next check for outlier values using a box plot:
#save the output of boxplot
png(filename=paste0(path,"/graphics/boxplot.png"))
d <- boxplot(train$Displacement,varwidth = T,outline = T,border = T,plot = T)
dev.off()
d$out #enlist outlier observations

#Summary
#My motive in writing this article is to get you started at solving regression
#problems, with a greater focus on the theoretical aspects. Running an algorithm 
#isn't  rocket science, but knowing how it works will surely give you more 
#control over what you do.