#Ques.1. Read the file in Zip format and get it into R.

unzip("D:/acadgild/datasets/AirQualityUCI/AirQualityUCI.zip")
AirQualityUCI <- read.csv("D:/acadgild/datasets/AirQualityUCI/AirQualityUCI.csv", sep=";")
View(AirQualityUCI)


#QUes.2. Create Univariate for all the columns.

sapply(AirQualityUCI$Time,mean)

tapply(AirQualityUCI$PT08.S1.CO.,AirQualityUCI$Date, mean)

tapply(AirQualityUCI$PT08.S3.NOx.,AirQualityUCI$Time, mean)

tapply(AirQualityUCI$PT08.S4.NO2.,AirQualityUCI$Date, mean)

round(tapply(AirQualityUCI$NO2.GT., AirQualityUCI$Time,mean),3)


#Ques.3. Check for missing values in all columns.

summary(AirQualityUCI)

sapply(AirQualityUCI, function(x) sum(is.na (x)))


#to count all the NA 
sum(is.na(AirQualityUCI))

#Ques.4. Impute the missing values using appropriate methods.

library(Hmisc)
AirQualityUCI$PT08.S1.CO.<-impute(AirQualityUCI$PT08.S1.CO.,mean)
AirQualityUCI$NMHC.GT.<-impute(AirQualityUCI$NMHC.GT.,mean)
AirQualityUCI$PT08.S2.NMHC.<-impute(AirQualityUCI$PT08.S2.NMHC.,mean)
AirQualityUCI$NOx.GT.<-impute(AirQualityUCI$NOx.GT.,mean)
AirQualityUCI$PT08.S3.NOx.<-impute(AirQualityUCI$PT08.S3.NOx.,mean)
AirQualityUCI$NO2.GT.<-impute(AirQualityUCI$NO2.GT.,mean)
AirQualityUCI$PT08.S4.NO2.<-impute(AirQualityUCI$PT08.S4.NO2.,mean)
AirQualityUCI$PT08.S5.O3.<-impute(AirQualityUCI$PT08.S5.O3.,mean)

summary(AirQualityUCI)
str(AirQualityUCI)

#Ques.5. Create bi-variate analysis for all relationships.

library(psych)
pairs.panels( AirQualityUCI1[,c(1,2,3,4,5,6)],
              method = "pearson", # correlation method
              hist.col = "red",
              density = TRUE,  # show density plots
              ellipses = TRUE, # show correlation ellipses
              lm=TRUE,
              main ="Bi-variate Scatterplots with Pearson Correlation & Histogram"
)


#Ques.6. Test relevant hypothesis for valid relations.
# Ho: Mean of first variable - Mean of second variable is equal to 0
# Ha: Mean of first variable - Mean of second variable is not equal to 0


AirQualityUCI1$T<-as.numeric(AirQualityUCI1$T)
t.test(AirQualityUCI1$T, AirQualityUCI1$Time ,alternative = "two.sided",mu=0 ,paired = TRUE)

AirQualityUCI1$AH<-as.numeric(AirQualityUCI1$AH)
t.test(AirQualityUCI1$AH,AirQualityUCI1$Time,alternative = "two.sided",mu = 0,paired = TRUE)

#(mean difference for both the variables are not equal to zero)


#Ques.7. Create cross tabulations with derived variables.



#Ques.8. Check for trends and patterns in time series.

ts (AirQualityUCI1, frequency = 4)

summary(AirQualityUCI1)

str(AirQualityUCI)

#Ques.9. Find out the most polluted time of the day and the name of the chemical compound.
library(ggplot2)
ggplot




install.packages("prodlim")
install.packages("Publish")
library(prodlim)
library(publish)
library(devtools)

univariateTable(~Date +Time +CO.GT.+PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+ PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+PT08.S5.O3.,data=AirQualityUCI)

tsData <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsData, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsData, s.window = "periodic")


library(dplyr)
AirQualityUCI1<-select(AirQualityUCI, -16:-17  )
View(AirQualityUCI1)
summary(AirQualityUCI1)
stlRes <- stl(AirQualityUCI1, s.window = "periodic")

boxplot(AirQualityUCI1)
scatter
