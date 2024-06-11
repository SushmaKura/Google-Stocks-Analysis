library(ggplot2)
setwd("C:/Users/Shaik/OneDrive/Desktop/PROBABILITY AND INTRO TO STATISTICS")

#Import Data-set
library(readr)
Google_Stock <- read.csv("Google_StockPrice.csv", TRUE, ",")

#Data Frame for Analysis
names(Google_Stock)[1] <- "Date"
names(Google_Stock)[2] <- "Open"
names(Google_Stock)[3] <- "High"
names(Google_Stock)[4] <- "Low"
names(Google_Stock)[5] <- "Close"
names(Google_Stock)[6] <- "Volume"

#Variable to appropriate datatypes
Google_Stock$Open <- as.numeric(Google_Stock$Open)
Google_Stock$High <- as.numeric(Google_Stock$High)
Google_Stock$Low <- as.numeric(Google_Stock$Low)
Google_Stock$Close <- as.numeric(Google_Stock$Close)
Google_Stock$Volume <- as.numeric(Google_Stock$Volume)

#renaming the variable
colnames(Google_Stock) <- c("Date", "Open", "High", "Low", "Close", "Volume")


#Drop unnecessary variable
Google_Stock <- Google_Stock[,1:5]

#Cleaning the Date Variable
Google_Stock$Date <- as.Date(Google_Stock$Date, "%m-%d-%Y")

# checking data structure
str(Google_Stock)

# frequency table of the Open variable
table(Google_Stock$Open)
knitr::kable(Google_Stock$Open)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# exporting frequency table of the Close variable
write.csv(table(Google_Stock$Close), "close_frequency_table.csv")

# cross-tabulation of Open and Close variables
xtab <- table(Google_Stock$Open, Google_Stock$Close)
write.csv(xtab, "openclose_crosstab.csv")
xtab

# Table for the first and last 5 data
library(magrittr)
library(kableExtra)
Data_Head = head(Google_Stock,5)
Data_Tail = tail(Google_Stock,5)
StockPrice_Table= rbind(Data_Head,Data_Tail)
knitr::kable(StockPrice_Table)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# histogram of the Close variable
  ggplot(Google_Stock, aes(x = High)) + 
  geom_histogram(binwidth = 1,breaks = c(325,335,345,355,365,375,385,395), fill = "blue", color = "black") + 
  geom_density(aes(y = ..density.. * 100), color = "orange") +
  ggtitle(" Daily High Price of Google ") + 
  xlab("Price") + 
  ylab("Frequency (%)")

# histogram of the Close variable    
hist(Google_Stock$Close,
     main = "Distribution of Close Prices",
     xlab = "Close Prices",
     ylab = "Frequency", 
     col = "red")

# exporting histogram of the Open variable
hist(Google_Stock$Open,
     main = "Distribution of Open Prices",
     xlab = "Open Prices",
     ylab = "Frequency", 
     col = "green")

#Density graph
ggplot(data.frame(Google_Stock), aes(High)) + 
  geom_density() + 
  ggtitle("Google Daily Price High Graph")

# Plot the high values as a line graph
plot(Google_Stock$High, type = "l", xlab = "Date", ylab = "High Price", main = "Stock Prices over Time")


# Next, we convert the data frame to an ftable
F_Table <- ftable(Google_Stock[, c("Open", "High", "Low", "Close")], row.vars = 1)

