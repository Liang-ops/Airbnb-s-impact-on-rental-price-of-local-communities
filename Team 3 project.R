#-------- library---------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(GGally)

#------reading data------------------------------------
view(kc.house.data)
kc.house.data <- read.csv("kc_house_data.csv")

#------------------------------------------------------

#---------data Structure-------------------------------

#size

length(kc.house.data$id) #21613 rows
length(kc.house.data) #21 columns

#missing values
kc.house.data[!complete.cases(kc.house.data),]
sum(is.na(kc.house.data))
##both methods confirm there are no missing values in orginal data

#datatypes
str(kc.house.data) ##Date is only character data type all others are numeric

#------------------------------------------------------

#-----chaning date to date data type-------------------

as.Date(kc.house.data$date,"YYYYMMDDT000000", "%Y%m%dT000000")

kc.house.data.chardate<- gsub("T000000", "", as.character(kc.house.data$date))
kc.house.data.chardate

kc.house.Dates<- as.Date(kc.house.data.chardate, "%Y%m%d")
kc.house.Dates

str(kc.house.Dates) #Now date data type

#-----Removing old date column--------

kc.house.data$date <- NULL

#---------- Adding column DATE------------------

kc.house.data$DATE <-kc.house.Dates
str(kc.house.data)

#-------------------------------------------------------

##-------------yr_renovated column -----

#------------- Makeing yr_renovated a catigorical varible-----

kc.house.data.V2 <- kc.house.data %>%
  mutate(Renovate = case_when(yr_renovated == 0 ~ 0,
                              yr_renovated != 0 ~ 1,
                              TRUE ~ 0))

kc.house.data.V2$Renovate

#-----------Removing yr_renovated----------

kc.house.data.V2$yr_renovated <- NULL

str(kc.house.data.V2) #21 rows still

#-------------------------------------------------

##------------Zipcode column----------------------

length(unique(kc.house.data.V2$zipcode)) #70

#zipcode is a categorical variable, this specifically has 70 unique values 
#thus will add 69 variables to a model, zip code will be dropped from the data

kc.house.data.V2$zipcode <- NULL
str(kc.house.data.V2) #20 columns

#-------------------------------------------------

#-----------Waterfront----------------------------

plot(kc.house.data.V2$waterfront)

totalwaterfronts <- sum(kc.house.data.V2$waterfront)
totalrowswf <- length(kc.house.data.V2$waterfront)
percent.waterfront <- (totalwaterfronts/totalrowswf)*100

percent.waterfront #less than 1 percent of the data has a water front veiw
#water front cannot be an accurate predictor will be removed

kc.house.data.V2$waterfront <- NULL

#--------------------------------------------------------

##View Varible

plot(kc.house.data.V2$view) #catigorical 4 varibles
hist(kc.house.data.V2$view) #Mostly 0

viewcounts.table <- data.frame(table(kc.house.data.V2$view)) ##19489 0s
viewpcts <- c((viewcounts.table$Freq/21613)*100)

viewpcts
##The view variable is being cut because over 90% of data does not have a view
#additionally there is no way to validate the differences in these view rankings
#Which would make it difficult to explain to a stakeholder


kc.house.data.V2$view <- NULL

#----------------------------------------------------------------------

#-------------Condition-----------------------------------------------

hist(kc.house.data.V2$condition)

conditioncounts.table <- data.frame(table(kc.house.data.V2$condition)) 
conditioncountspct <- c((conditioncounts.table$Freq/21613)*100)

conditioncountspct #less than 1 percent of data is categorized as 1 or 2
## condition will be recoded so that values 1-3 are recoded as 1, values 4 will become 2, and 5 will become 3.

##------Recoding condition to reduce complexitiy

kc.house.data.V3 <- kc.house.data.V2 %>%
  mutate(sale.condition = case_when(condition <= 3 ~ 1,
                                    condition == 4 ~ 2,
                                    condition == 5 ~3,
                                    TRUE ~ 0))

str(kc.house.data.V3) ##Use kc.house.data.V3 as data from now on

kc.house.data.V3$condition <- NULL ###removing condition

#--------------------------------------------------------------------

#------------Grade---------------------------------------------------

hist(kc.house.data.V3$grade) ##fairly semetric
boxplot(kc.house.data.V3$grade)  ##Shows many outliers but needs to be explored

outliers.grade <- sort(unique(boxplot(kc.house.data.V3$grade)$out))
Gradecounts.table <- data.frame(table(kc.house.data.V3$grade)) 
Gradecountspct <- c((Gradecounts.table$Freq/21613)*100)

Gradecounts.table
Gradecountspct ##1,3,4,5, 10,11, 12, 13 will be removed as outliers
#This is roughly a 8 percent deduction in data

##removing outliers

kc.house.data.V4. <- kc.house.data.V3 %>%
  filter(grade != 1) %>%
  filter(grade != 3) %>%
  filter(grade != 4) %>%
  filter(grade != 5) %>%
  filter(grade != 10) %>%
  filter(grade != 11) %>%
  filter(grade != 12) %>%
  filter(grade != 13) 

unique(kc.house.data.V4.$grade) #outliers removed

length(kc.house.data.V4.$grade) ## 19702 rows

#--------------------------------------------------------------------------------------

#----------- yr_built---------------------------------------------------------------

unique(kc.house.data.V4.$yr_built) ##over 100 values
##Since sale date is moving these two variables will be combined into a single 


#------ Creating "Age.at.Sale" varible --------------------------------------------

head(kc.house.data.V4.$DATE) #Formate "%Y-%m-%d

d <- as.POSIXct(kc.house.data.V4.$DATE, format("%Y-%m-%d"))
d1 <- format(d, format("%Y"))
#str(d1)

d1.n <- as.numeric(d1)
#str(d1.n) #numeric vector of all sale date years in order

d2 <- kc.house.data.V4.$yr_built
#str(d2) Numeric vector of yr_built

Age.at.Sale <- (d1.n - d2)

hist(Age.at.Sale) #Slightly right skewed

kc.house.data.V4.$Age.at.Sale <- Age.at.Sale

str(kc.house.data.V4.)

#---------------------------------------------------------------------------------------

#-------------Notes-----------------------------------------------------------------------

#If disered yr_built and Date can both be dropped

##Use this code if want to drop those vaibles

##kc.house.data.V4.$Date <- NULL
##kc.house.data.V4.$yr_built <- NULL

#------------------------------------------------

#------------------Countinous Varibles-----------

#--------Bedrooms----------------------------------

boxplot(kc.house.data.V4.$bedrooms) #Noticealbe extreme outlier

sort(unique(kc.house.data.V4.$bedrooms))

kc.house.data.V5 <- kc.house.data.V4. %>%  #Removes outliers 4 sd away
  group_by(bedrooms) %>%
  filter(!(abs(bedrooms - median(bedrooms))
           > 4*sd(bedrooms)))

#------------------------------------------------------------------------

#------------Bathrooms-------------------------------------------------

boxplot(kc.house.data.V5$bathrooms) ##Some upper tail outliers

kc.house.data.V6 <- kc.house.data.V5 %>%
  group_by(bathrooms) %>%
  filter(!(abs(bathrooms - median(bathrooms))# Removes outliers 4 SD away
           > 4*sd(bathrooms)))

boxplot(kc.house.data.V6$bathrooms) #outliers outside of 4 standard deviations away
##From median have been deleted, they rest will remain included.

#--------------------------------------------------------------------------

#--------Floors------------------------------------------------------------

hist(kc.house.data.V6$floors) #right skew

boxplot(kc.house.data.V6$floors) #No outliers

#No need to modify, while it is right skewed, nomrizing the data removes context

#---------------------------------------------------------------------------

#---------------------Notes-------------------------------------------

# The columns: sqft_living,	sqft_lot,	sqft_above,	sqft_basement

# Sqft living is the sum of sqft_above and	sqft_basement

#Plan to change sqft_basement to catigorical varible Basement and remove sqft_above varible

#----------------------------------------------------------------------

#-----------Chaning sqft_basement to Basement-------------------------------------------------

kc.house.data.V7 <- kc.house.data.V6 %>%
  mutate(Basement = case_when(sqft_basement == 0 ~ 0,
                              sqft_basement > 0 ~ 1,#if there is squarefootage in basement it is recoded to 1
                              TRUE ~ 0))

kc.house.data.V7$sqft_basement <- NULL 
kc.house.data.V7$sqft_above <- NULL

hist(kc.house.data.V7$Basement)

#----------------------------------------------------------------------

#---------------- sqft_living and sqft_living15--------------------------

hist(kc.house.data.V7$sqft_living15)
hist(kc.house.data.V7$sqft_living) #is right skewed

boxplot(kc.house.data.V7$sqft_living15)
boxplot(kc.house.data.V7$sqft_living)

#------- Notes-----------------------------------------------------------------------

#sqft_living and sqft_living15 are similar in shape

#sqft_living15 is not an explainable variable and will be removed

kc.house.data.V7$sqft_living15 <- NULL

#-------------------------------------------------------------------------------------------

#--------------- Trasforming sqft_living------------------------------------------------------ 

#Attempt at normal transformation

normfunction <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

trans.sqft_living <- normfunction(kc.house.data.V7$sqft_living)

hist(trans.sqft_living)

#Was not effective still right skewed

#Log method

logtrans.living <- log(kc.house.data.V7$sqft_living)

hist(logtrans.living) #Sysmetric

boxplot(logtrans.living) #might have outliers but they do not appear extreme and will be kept in.

kc.house.data.V7$Logsqft_Living <- logtrans.living

#--------------------------------------------------------------------------------------------

#--------------------sqft_lot & sqft_lot15------------------------------------------------------

#I do not know what the sqft_lot15 varibleis and thus will not use it since it cannot be explained in business

kc.house.data.V7$sqft_lot15 <- NULL

boxplot(kc.house.data.V7$sqft_lot) ## Extreme outliers

boxplot(kc.house.data.V7$sqft_lot, outline = FALSE) #Identifiying Exterme outliers

outlierslot <- sort(unique(boxplot
                           (kc.house.data.V7$sqft_lot)$out))

removalpct<- (length(outlierslot)/
                length(kc.house.data.V7$sqft_lot))*100

removalpct ##roughly 8%

kc.house.data.V8 <- kc.house.data.V7 %>%
  group_by(sqft_lot) %>%
  filter((sqft_lot < min(outlierslot)))

boxplot(kc.house.data.V8$sqft_lot) #No more extreme outliers

hist(kc.house.data.V8$sqft_lot) #slightly right skewed, dont believe transfomration is needed and can keep all remaining values


# -------------------------Lat and Long-------------------------------------------

#-----------------------------Notes-----------------------------------------------

#These varibles are not explainable in business sense, while they are continous 

#they will be broken into quadrents (East, West) (North, and South)

#This will be eaiser to explain and give context

#---------------------------------------------------------------------------------------

#------------------Lat to North and South----------------------------------------------

#using median as break point

cuttofflat <- median(kc.house.data.V8$lat)

kc.house.data.V9 <- kc.house.data.V8 %>%
  mutate(North.South = case_when(lat > cuttofflat ~ 1,
                                 lat <= cuttofflat ~ 0,
                                 TRUE ~ 0)) # 1 = N, 0 = S

hist(kc.house.data.V9$North.South)

kc.house.data.V9$lat <- NULL

#------------------Long East and west---------------------------------------------------------

#median as cutt off

cuttoflong <- median(kc.house.data.V9$long)

kc.house.data.V10 <- kc.house.data.V9 %>%
  mutate(East.West = case_when(long > cuttoflong ~ 1,
                               long <= cuttoflong ~ 0,
                               TRUE ~ 0)) # 1 = E, 0 = W

hist(kc.house.data.V10$East.West)

kc.house.data.V10$long <- NULL

#---------------------------------------------------------------------------------

#---------------------Data cleaning notes------------------------------------------

str(kc.house.data.V10) ## 17,694 rows X 17 cloumns

#colnames(kc.house.data.V10) column names

#Id will not be used for modeling, used as identifier

#Price target varible

#DATE and yr_build have the option to be removed

#sqft_living has the option to be removed

#This leaves 11 predictor variables for modeling.

Clean.kc.housing.data <- kc.house.data.V10
colnames(Clean.kc.housing.data)

#Use Clean.kc.housing.data for future analysis
#------------------------------------------------------------------------------------------------------
# ----------------------------------Data exploration and data visualization----------------------------
#Because id and date cannot prove the relationship with our target variable price, 
#so we decide to exclude them.
Clean.kc.housing.data$id <- NULL

# We have transfrom the data in sqft living from big scale to a small scale,
#so we will exclude the sqft living and only keep the log sqft living
Clean.kc.housing.data$sqft_living <- NULL

# We have created a new column called age at sale which gives us the information 
# about ages of the houses then we can drop the DATE and yr_built
Clean.kc.housing.data$DATE <- NULL
Clean.kc.housing.data$yr_built <- NULL

#First we want to see the distribution of price
price_plot_theme = theme(axis.title = element_text(size = 22.5),
                         axis.text = element_text(size = 20))
price_plot = ggplot(data = Clean.kc.housing.data, aes(x = price)) + 
  geom_histogram(col = 'black') +
  # makes a boxplot
  geom_vline(xintercept = mean(Clean.kc.housing.data$price),
             linetype = 2, col = 'red', lwd = 2) +
  # add theme to the plot
  theme(panel.background = element_rect(fill = 'dark grey', colour = 'red'))+
  # add line denoting mean house price
  xlab('Price (US$)') + # change x axis label
  ylab('Frequency') + # change y axis label
  price_plot_theme + # change size of axis titles and text
  scale_x_continuous(trans = 'log',
                     breaks = c(100000, 250000, 500000, 1000000, 
                                2500000, 5000000),
                     labels = c('100,000', '250,000', '500,000', 
                                '1,000,000', '2,500,000', '5,000,000')) +
  scale_y_continuous(breaks = c(seq(0, 3000, 500)),
                     labels = c(seq(0, 3000, 500)),
                     limits = c(0, 3000))
price_plot
cor(Clean.kc.housing.data)
#The dashed red line denotes the mean house price is $476762.6
#You can see that not many houses are priced over 1 million dollars, the majority are below that and are roughly centered around the mean price.

# Then we want to see the relationship between target variable and predictors
# Checking Relationship between price, bedrooms, bathrooms, sqft lot, floors
plot1<-ggpairs(data=Clean.kc.housing.data, columns=c(1:5),axisLabels="show")
plot1

# Checking Relationship between price,grade, renovate, condition and age at sale
plot2<-ggpairs(data=Clean.kc.housing.data, columns=c(1,6:10),axisLabels="show")
plot2

# Checking Relationship between price, basement, logsqft_living, North.South and East.West
plot3=ggpairs(data=Clean.kc.housing.data, columns=c(1,11:14),axisLabels="show")
plot3

# from above plots we can know the correlation between each variables and we focus on the correaltion
# between target variable and predictors:
# correlation between price vs bedrooms: 0.273
# correlation between price vs bathrooms: 0.388
# correlation between price vs sqft_lot: -0.007
# correlation between price vs floors: 0.213
# correlation between price vs Renovate: 0.152
# correlation between price vs sale.condition: 0.092
# correlation between price vs Age.at.Sale: 0.074
# correlation between price vs grade: 0.550
# correlation between price vs Logsqft_living: 0.539
# correlation between price vs basement: 0.219
# correlation between price vs North.South: 0.441
# correlation between price vs East.West: -0.017

# According to the correlations listed above, we may need to drop some weak correlations and we will not to 
# explore the corraltion which is too tiny to help our prediction like sqft_lot
# such as East.West, becasue they are too weak to impact target variable

# then we will show some correlations separately.

# --------------------------------house quality exploration--------------------------------------------
# log Price vs. Grade ->> Nice correlation, grade increases, price increases as well
boxplot(log(price)~ grade, data = Clean.kc.housing.data, 
        col=(c("grey","yellow","brown","gold")),
        main = "log(price) vs. Grade", xlab = "Grade", ylab = "log(price)")

# price vs. Condition ->> The median price for houses in each condition are close and they are close to the mean house price, 
# so most of prices in each condition are same. Though there is an increase in median house price as condition improves, 
# we don't believe it's a good predictor.
boxplot(log(price) ~ sale.condition, data = Clean.kc.housing.data, 
        col=(c("grey","yellow","brown")),
        main = "log(price) vs. Sale.condition", xlab = "Sale.condition", ylab = "log(price)")
abline(h = log(mean(Clean.kc.housing.data$price)), col = 'red')

# log Price vs. Age.at.sale ->> this obviously is not a liner relationship with our target variable, at the agr between
# 40 to 60 the house price is lower than the older one or newer one, so the age cannot have big impact on the price
price_Age<- aggregate(log(price) ~ Age.at.Sale, FUN = mean, data = Clean.kc.housing.data)
plot(price_Age,main = "log(Price) vs. house age", xlab = "Age", ylab = "log(price)")

# log Price vs. renovate ->> As the mean price line and the boxplot, it's obviously that whether the house renovate will have
# big affect on the sale price, the median price and max price have big different between these two situations.
boxplot(log(price) ~ Renovate, data = Clean.kc.housing.data, 
        col=(c("grey","yellow","brown","gold")),
        main = "log(price) vs. Renovate", xlab = "Renovate", ylab = "log(price)")
abline(h = log(mean(Clean.kc.housing.data$price)), col = 'red')

# --------------------------------------house location exploration-----------------------------------------
# log Price vs. North.South ->> We can figure out the median has large different whether the location
# is higher than the median of latitude, so it's a good predictor.
boxplot(log(price) ~ North.South, data = Clean.kc.housing.data, 
        col=(c("grey","yellow")),
        main = "log(price) vs. North.South", xlab = "North.South", ylab = "log(price)")

# log Price vs. East.West ->> We can figure out the median has no different whether the location
# is higher than the median of longitude, so it's not a good predictor.
boxplot(log(price) ~ East.West, data = Clean.kc.housing.data, 
        col=(c("grey","yellow")),
        main = "log(price) vs. East.West", xlab = "East.West", ylab = "log(price)")

# ----------------------------------------house size exploration----------------------------------------------
# log Price vs. floors ->> the number of floors appears to have little effect on house prices. 
# If anything, the median house price for 3 and 3.5 floors is slightly less than or roughly equal to 2 and 2.5 floors. 
# we would therefore not expect 'floors' to be a strong predictor of house price in the predictive models.
boxplot(log(price) ~ floors, data = Clean.kc.housing.data, 
        col=(c("grey","yellow")),
        main = "log(price) vs. Floors", xlab = "Floors", ylab = "log(price)")

# log Price vs. Logsqft_living ->> Since the boxplot is too crowded we will plot aggregated vectors to see the relationship between 2 variables. 
# as the scatterplot shows as Logsqft_living increases, price increases as well.
price_sqftliving <- aggregate(log(price)~Logsqft_Living, FUN=mean, data=Clean.kc.housing.data)
plot(price_sqftliving,main = "log(price) vs. Logsqft_Living", xlab = "Logsqft_Living", ylab = "log(price)")
#price vs. bathrooms --> as the plot shows the whole tread is when bathrooms increases then price increases as well.
#except some special situation when bathrooms equal or greater than five.
price_bathrooms <- aggregate(log(price)~bathrooms, FUN=mean, data=Clean.kc.housing.data)
plot(price_bathrooms,main = "log(price) vs. bathrooms", xlab = "Bathrooms", ylab = "log(price)")

#log price vs. bedrooms --> as the boxplot shows the whole tread is when bathrooms increases then price increases as well.
#except some special situation when bathrooms equal or greater than 10.
boxplot(log(price) ~ bedrooms, data = Clean.kc.housing.data, 
        col=(c("grey","yellow")),
        main = "log(price) vs. Bedrooms", xlab = "Bedrooms", ylab = "log(price)")

# after the exploration, according to the plot and correlation, we will suggest to use bedrooms, bathrooms, Logsqft_living,
#North.South, renovate, grade
#--------------------------------------------Libraries---------------------------------------------------------------------------------

library(ggplot2)
library(readr)
library(lattice)
library(caret)
library(Matrix)
library(glmnet)

#view(Clean.kc.housing.data)
#str(Clean.kc.housing.data)

#------------------------------------Developing Test Data for regression analysis---------------------------------------------------------------------------------------------------------------------------

set.seed(123)
IndexMatrix <- createDataPartition(Clean.kc.housing.data$price, p=.8, list = FALSE, times = 1)
#Parting data once for index matrix, using an 80%/20% split

HTrainingDF <-Clean.kc.housing.data[IndexMatrix,]
HTestDF <- Clean.kc.housing.data[-IndexMatrix,]
#Applying data to training df and test df

ctrlspecs <- trainControl(method = "cv", number=10, savePredictions = "all")
#Structuring training approach (method and 10-fold)

lambda_vector <- 10^seq(5, -5, length=500)
#creating a sequence of values for lambda values

#------------------------------------Training Lasso regression model, (k-fold cross validation)---------------------------------------------------------------------------------------------------------------------------

set.seed(123)
model1 <- train(price ~ bedrooms + bathrooms + Logsqft_Living + North.South + Renovate + grade,
                data = HTrainingDF, 
                preProcess= c("center", "scale"), 
                method="glmnet", 
                tuneGrid=expand.grid(alpha=1, lambda=lambda_vector),
                trControl=ctrlspecs, 
                na.action=na.omit)
set.seed(123)
model2 <- train(price ~ bedrooms + bathrooms + Logsqft_Living + North.South + Renovate + grade,
                data = HTrainingDF, 
                preProcess=c("center", "scale"),
                method = "lm", 
                trControl=ctrlspecs,
                na.action=na.omit)
#creating training model1 the lasso regression, and model 2 OLS multiple linear regression model

#------------------------------------Exploring models---------------------------------------------------------------------------------------------------------------------------

#print(model1)
#summary(model1)
#setting seed for reproducible data, and setting Lasso regression model
#model1$bestTune$lambda
#best lambda value of 359.013
#round(coef(model1$finalModel, model1$bestTune$lambda), digits = 2)
#plot(log(model1$results$lambda), model1$results$RMSE, xlab="log(lambda)", ylab="RSME",xlim=c(-5,0))
#log(model1$bestTune$lambda)
#varImp(model1)
ggplot(varImp(model1))
#the GGplot demonstrates most important features

#------------------------------------Evaluating model and final model, (k-fold cross validation)---------------------------------------------------------------------------------------------------------------------------

perdictions1 <- predict(model1, newdata=HTestDF)

predictions2 <- predict(model2, newdata=HTestDF)

#view(mod1perf)
#print(model2)
#summary(model2)
#model2$finalModel$coefficients

model_list <- list(model1, model2)
  RSample <- resamples(model_list)
    summary(RSample)

compare_models(model1, model2, metric="RMSE")
#not statistically significant

compare_models(model1, model2, metric="Rsquared")
#not statistically significant



#------------------------------------Developing Predictive Data---------------------------------------------------------------------------------------------------------------------------

mod1perf <- data.frame(RMSE= RMSE(perdictions1, HTestDF$price),
                       rsquared=R2(perdictions1, HTestDF$price))

mod2perf <- data.frame(RMSE= RMSE(predictions2, HTestDF$price),
                       rsquared=R2(predictions2, HTestDF$price))
#view(mod1perf)
#print(model2)
#summary(model2)
#model2$finalModel$coefficients
print(mod2perf)
#------------------------------------Determining best predictive model---------------------------------------------------------------------------------------------------------------------------

comp <- matrix(c(mod1perf$RMSE, mod1perf$rsquared,
                 mod2perf$RMSE, mod2perf$rsquared),
               ncol=2, byrow=TRUE)
colnames(comp) <- c("RMSE", "R-Squared")
rownames(comp) <- c("Lasso Reg", "OLS MLR")

comp <- as.table(comp)
round(comp, digits = 6)

#The Lasso Regression given new data, outperformed the OLS Regression. 
#Scoring a lower RMSE value, and fractionally a higher R-squared value.
#We will choose the Lasso, model1, for our perdictive model of housing prices

