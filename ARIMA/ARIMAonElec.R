#Data Loading
powerData <- read.table('household_power_consumption.txt',sep = ';',header = TRUE, stringsAsFactors = FALSE)

#Data Transformation
powerData$Date<-as.Date(powerData$Date, format="%d/%m/%Y") #Date

# Time Transformation
install.packages("chron")
library(chron)
time_1 <- c(powerData$Time)
powerData$Time <- chron(times = time_1)

#numeric transformations #missing values marked as '?' get replaced by NA in R
for (i in 3:9)
  powerData[, i] <- as.numeric(powerData[, i]) 

#Initial Exploration
summary(powerData)

#Sampling
powerData_Sample <- subset(powerData,subset = (powerData$Date>='2007-02-08' & powerData$Date<='2007-02-09'))

# Adding new column Full Time stamp
fullTimestamp <- paste(powerData_Sample$Date,powerData_Sample$Time)
powerData_Sample$fullTimestamp <- as.POSIXct(fullTimestamp) # Full Time stamp

summary(powerData_Sample)

#representativeness
par(mfrow = c(2,2))
boxplot(powerData$Global_active_power, powerData_Sample$Global_active_power, horizontal = TRUE, outline = FALSE, xlab='Global Active Power', ylab='Sample    Population ', main='Representativeness (without outliers)', col=(c("gold","darkgreen")))
boxplot(powerData$Global_reactive_power, powerData_Sample$Global_reactive_power, horizontal = TRUE, outline = FALSE, xlab='Global Reactive Power', ylab='Sample    Population ',col=(c("gold","darkgreen")))
boxplot(powerData$Global_intensity, powerData_Sample$Global_intensity, horizontal = TRUE, outline = FALSE, xlab='Global Intensity', ylab='Sample    Population ',col=(c("gold","darkgreen")))
boxplot(powerData$Voltage, powerData_Sample$Voltage, horizontal = TRUE, outline = FALSE, xlab='Voltage', ylab='Sample    Population ',col=(c("gold","darkgreen")))

boxplot(powerData_Sample$Sub_metering_1, horizontal = TRUE)

#plotting wrt time
# electric attributes with respect to fullTimestamp
par(mfrow = c(2,1))
plot(powerData_Sample$fullTimestamp, powerData_Sample$Global_active_power, ylab = "Global Active Power",xlab = "", type = "l", col='#1D9867',col.axis='#1D9867',col.lab='#1D9867',font.lab='2',font.axis='2')

plot(powerData_Sample$Sub_metering_1 ~ powerData_Sample$fullTimestamp, ylab = "Energy sub metering", xlab = "", type = "l",col='#CA5A07',col.axis='#2757BE',col.lab='#2757BE', font.lab='2',font.axis='2')
lines(powerData_Sample$Sub_metering_2 ~ powerData_Sample$fullTimestamp, col = '#2757BE')
lines(powerData_Sample$Sub_metering_3 ~ powerData_Sample$fullTimestamp, col = '#1D9867')
legend("topleft", col = c("#CA5A07", "#2757BE", "#1D9867"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 2,cex =0.75 )

#### ARIMA ######
library('forecast')
#Data Load
trainData <- data.frame(Date=powerData[,1],GAP=powerData[,3])

#Data Cleaning: Replace missing values with previous value
library('zoo')
summary(trainData)
trainData <- transform(trainData, GAP = na.locf(GAP))
summary(trainData)

#aggregating for month
agg_Month <-
  aggregate(trainData,by=list(as.yearmon(trainData$Date,"%d
                                         %m%Y")), FUN = mean, na.rm=TRUE) 

#time series
tsmonth <- ts(agg_Month[,3],frequency = 12,start = c(2006,12))
plot(tsmonth, ylab='GAP',main='Global Active Power: Month Averaged',col='#A93226', lwd=2)
abline(h=mean(tsmonth),col='#A93226') 

#decompose
fm <- decompose(tsmonth)
plot(fm, col='#2757BE',lwd=2)


#plotting for time series analysis

plot(tsmonth, ylab='GAP',main='Global Active Power: Month Averaged',col='#A93226', lwd=2)
abline(h=mean(tsmonth),col='#A93226') 

plot(fm, col='#2757BE',lwd=2)


#predicting using auto.arima

library('forecast')
auto.arima(tsmonth) 
plot(forecast(tsmonth,h=12), ylab='Global Active Power: Month Averaged')
##testing
auto.arima(tsmonth, D=1) 
