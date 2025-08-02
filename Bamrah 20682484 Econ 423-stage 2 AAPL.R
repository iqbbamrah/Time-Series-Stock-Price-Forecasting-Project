# Project Stock Data
AAPL <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Empirical Project/Stage 2 Project/Stocks/raw/AAPL.csv")
AMZN <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Empirical Project/Stage 2 Project/Stocks/raw/AMZN.csv")
FB <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Empirical Project/Stage 2 Project/Stocks/raw/FB.csv")
INTC <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Empirical Project/Stage 2 Project/Stocks/raw/INTC.csv")
MSFT <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Empirical Project/Stage 2 Project/Stocks/raw/MSFT.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
library(httr)
library(stringr)
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)
library(rtweet)

# Creating Models for the Twitter and Stock Price Data
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = "1378187192531611650-L625XzuNgaNHJ1Z2AUz3uIOKQxy0hu",
  access_secret = "mHWV82NFKkAUVzG6bi80K6MpHHaKGg6CueaKIGaz2kDGa")
NumberofTweets <- 750
tweetsAAPL1 <- search_tweets(q = "#apple", n = NumberofTweets, lang="en", since = "2021-03-28", until = "2021-03-29")
tweetsAAPL2 <- search_tweets(q = "#apple", n = NumberofTweets, lang="en", since = "2021-03-29", until = "2021-03-30")
tweetsAAPL3 <- search_tweets(q = "#apple", n = NumberofTweets, lang="en", since = "2021-03-30", until = "2021-03-31")
tweetsAAPL4 <- search_tweets(q = "#apple", n = NumberofTweets, lang="en", since = "2021-03-31", until = "2021-04-01")
tweetsAAPL5 <- search_tweets(q = "#apple", n = NumberofTweets, lang="en", since = "2021-04-01", until = "2021-04-02")

# AAPL
tweetsDFAAPL1 <- as.data.frame(tweetsAAPL1)
tweetsDFAAPL2 <- as.data.frame(tweetsAAPL2)
tweetsDFAAPL3 <- as.data.frame(tweetsAAPL3)
tweetsDFAAPL4 <- as.data.frame(tweetsAAPL4)
tweetsDFAAPL5 <- as.data.frame(tweetsAAPL5)

aapl <- subset(AAPL, select = c(Date, Adj.Close))
AAPL$Date <- as.Date(AAPL$Date)



A1 <- tweetsDFAAPL1
A1$text <- enc2native(A1$text)
A1$text <- gsub("^[[:space:]]*","",A1$text)
A1$text <- gsub("[[:space:]]*$","",A1$text)
A1$text <- gsub(" +"," ",A1$text)
A1$text <- gsub("'", "%%", A1$text)
A1$text <- iconv(A1$text, "latin1", "ASCII", sub="")
A1$text <- gsub("<(.*)>", "", A1$text)
A1$text <- gsub("\\ \\. ", " ", A1$text)
A1$text <- gsub("  ", " ", A1$text)
A1$text <- gsub("%%", "\'", A1$text)
A1$text <- gsub("https(.*)*$", "", A1$text)
A1$text <- gsub("\\n", "-", A1$text)
A1$text <- gsub("--", "-", A1$text)
A1$text <- gsub("&amp;", "&", A1$text)
A1$text[A1$text == " "] <- "<no text>"

CleanTweetsAAPL1 <- A1 %>% 
  select("text")

SentimentAAPL1 <- analyzeSentiment(CleanTweetsAAPL1)
Sentiment2AAPL1 <- SentimentAAPL1$SentimentQDAP
Sentiment3AAPL1 <- convertToDirection(Sentiment2AAPL1)

dateA1 <- A1$created
dateA1 <- str_extract(dateA1, "\\d{4}-\\d{2}-\\d{2}")
dateA1 <- as.Date(dateA1)
dateA1 <- as.Date(dateA1, format = "%m/%d/%y")

dfAAPL1 <- cbind(CleanTweetsAAPL1, Sentiment2AAPL1, Sentiment3AAPL1, dateA1)
dfAAPL1 <- dfAAPL1[complete.cases(dfAAPL1), ]

df2AAPL1 <- dfAAPL1 %>% 
  group_by(dateA1) %>%
  summarize(meanSentiment1 = mean(Sentiment2AAPL1, na.rm=TRUE))

DT::datatable(df2AAPL1, editable = TRUE)

freqAAPL1 <- dfAAPL1 %>% 
  group_by(dateA1,Sentiment3AAPL1) %>% 
  summarise(Freq1 = n())

freq2AAPL1 <- freqAAPL1 %>% 
  spread(key = Sentiment3AAPL1, value = Freq1)

DT::datatable(freq2AAPL1, editable = TRUE)

A2 <- tweetsDFAAPL2
A2$text <- enc2native(A2$text)
A2$text <- gsub("^[[:space:]]*","",A2$text)
A2$text <- gsub("[[:space:]]*$","",A2$text)
A2$text <- gsub(" +"," ",A2$text)
A2$text <- gsub("'", "%%", A2$text)
A2$text <- iconv(A2$text, "latin1", "ASCII", sub="")
A2$text <- gsub("<(.*)>", "", A2$text)
A2$text <- gsub("\\ \\. ", " ", A2$text)
A2$text <- gsub("  ", " ", A2$text)
A2$text <- gsub("%%", "\'", A2$text)
A2$text <- gsub("https(.*)*$", "", A2$text)
A2$text <- gsub("\\n", "-", A2$text)
A2$text <- gsub("--", "-", A2$text)
A2$text <- gsub("&amp;", "&", A2$text)
A2$text[A2$text == " "] <- "<no text>"

CleanTweetsAAPL2 <- A2 %>% 
  select("text")

SentimentAAPL2 <- analyzeSentiment(CleanTweetsAAPL2)
Sentiment2AAPL2 <- SentimentAAPL2$SentimentQDAP
Sentiment3AAPL2 <- convertToDirection(Sentiment2AAPL2)

dateA2 <- A2$created
dateA2 <- str_extract(dateA2, "\\d{4}-\\d{2}-\\d{2}")
dateA2 <- as.Date(dateA2)
dateA2 <- as.Date(dateA2, format = "%m/%d/%y")

dfAAPL2 <- cbind(CleanTweetsAAPL2, Sentiment2AAPL2, Sentiment3AAPL2, dateA2)
dfAAPL2 <- dfAAPL2[complete.cases(dfAAPL2), ]

df2AAPL2 <- dfAAPL2 %>% 
  group_by(dateA2) %>%
  summarize(meanSentiment2 = mean(Sentiment2AAPL2, na.rm=TRUE))

DT::datatable(df2AAPL2, editable = TRUE)

freqAAPL2 <- dfAAPL2 %>% 
  group_by(dateA2,Sentiment3AAPL2) %>% 
  summarise(Freq2 = n())

freq2AAPL2 <- freqAAPL2 %>% 
  spread(key = Sentiment3AAPL2, value = Freq2)

DT::datatable(freq2AAPL2, editable = TRUE)

A3 <- tweetsDFAAPL3
A3$text <- enc2native(A3$text)
A3$text <- gsub("^[[:space:]]*","",A3$text)
A3$text <- gsub("[[:space:]]*$","",A3$text)
A3$text <- gsub(" +"," ",A3$text)
A3$text <- gsub("'", "%%", A3$text)
A3$text <- iconv(A3$text, "latin1", "ASCII", sub="")
A3$text <- gsub("<(.*)>", "", A3$text)
A3$text <- gsub("\\ \\. ", " ", A3$text)
A3$text <- gsub("  ", " ", A3$text)
A3$text <- gsub("%%", "\'", A3$text)
A3$text <- gsub("https(.*)*$", "", A3$text)
A3$text <- gsub("\\n", "-", A3$text)
A3$text <- gsub("--", "-", A3$text)
A3$text <- gsub("&amp;", "&", A3$text)
A3$text[A3$text == " "] <- "<no text>"

CleanTweetsAAPL3 <- A3 %>% 
  select("text")

SentimentAAPL3 <- analyzeSentiment(CleanTweetsAAPL3)
Sentiment2AAPL3 <- SentimentAAPL3$SentimentQDAP
Sentiment3AAPL3 <- convertToDirection(Sentiment2AAPL3)

dateA3 <- A3$created
dateA3 <- str_extract(dateA3, "\\d{4}-\\d{2}-\\d{2}")
dateA3 <- as.Date(dateA3)
dateA3 <- as.Date(dateA3, format = "%m/%d/%y")

dfAAPL3 <- cbind(CleanTweetsAAPL3, Sentiment2AAPL3, Sentiment3AAPL3, dateA3)
dfAAPL3 <- dfAAPL3[complete.cases(dfAAPL3), ]

df2AAPL3 <- dfAAPL3 %>% 
  group_by(dateA3) %>%
  summarize(meanSentiment3 = mean(Sentiment2AAPL3, na.rm=TRUE))

DT::datatable(df2AAPL3, editable = TRUE)

freqAAPL3 <- dfAAPL3 %>% 
  group_by(dateA3,Sentiment3AAPL3) %>% 
  summarise(Freq3 = n())

freq2AAPL3 <- freqAAPL3 %>% 
  spread(key = Sentiment3AAPL3, value = Freq3)

DT::datatable(freq2AAPL3, editable = TRUE)

A4 <- tweetsDFAAPL4
A4$text <- enc2native(A4$text)
A4$text <- gsub("^[[:space:]]*","",A4$text)
A4$text <- gsub("[[:space:]]*$","",A4$text)
A4$text <- gsub(" +"," ",A4$text)
A4$text <- gsub("'", "%%", A4$text)
A4$text <- iconv(A4$text, "latin1", "ASCII", sub="")
A4$text <- gsub("<(.*)>", "", A4$text)
A4$text <- gsub("\\ \\. ", " ", A4$text)
A4$text <- gsub("  ", " ", A4$text)
A4$text <- gsub("%%", "\'", A4$text)
A4$text <- gsub("https(.*)*$", "", A4$text)
A4$text <- gsub("\\n", "-", A4$text)
A4$text <- gsub("--", "-", A4$text)
A4$text <- gsub("&amp;", "&", A4$text)
A4$text[A4$text == " "] <- "<no text>"

CleanTweetsAAPL4 <- A4 %>% 
  select("text")

SentimentAAPL4 <- analyzeSentiment(CleanTweetsAAPL4)
Sentiment2AAPL4 <- SentimentAAPL4$SentimentQDAP
Sentiment3AAPL4 <- convertToDirection(Sentiment2AAPL4)

dateA4 <- A4$created
dateA4 <- str_extract(dateA4, "\\d{4}-\\d{2}-\\d{2}")
dateA4 <- as.Date(dateA4)
dateA4 <- as.Date(dateA4, format = "%m/%d/%y")

dfAAPL4 <- cbind(CleanTweetsAAPL4, Sentiment2AAPL4, Sentiment3AAPL4, dateA4)
dfAAPL4 <- dfAAPL4[complete.cases(dfAAPL4), ]

df2AAPL4 <- dfAAPL4 %>% 
  group_by(dateA4) %>%
  summarize(meanSentiment4 = mean(Sentiment2AAPL4, na.rm=TRUE))

DT::datatable(df2AAPL4, editable = TRUE)

freqAAPL4 <- dfAAPL4 %>% 
  group_by(dateA4,Sentiment3AAPL4) %>% 
  summarise(Freq4 = n())

freq2AAPL4 <- freqAAPL4 %>% 
  spread(key = Sentiment3AAPL4, value = Freq4)

DT::datatable(freq2AAPL4, editable = TRUE)

A5 <- tweetsDFAAPL5
A5$text <- enc2native(A5$text)
A5$text <- gsub("^[[:space:]]*","",A5$text)
A5$text <- gsub("[[:space:]]*$","",A5$text)
A5$text <- gsub(" +"," ",A5$text)
A5$text <- gsub("'", "%%", A5$text)
A5$text <- iconv(A5$text, "latin1", "ASCII", sub="")
A5$text <- gsub("<(.*)>", "", A5$text)
A5$text <- gsub("\\ \\. ", " ", A5$text)
A5$text <- gsub("  ", " ", A5$text)
A5$text <- gsub("%%", "\'", A5$text)
A5$text <- gsub("https(.*)*$", "", A5$text)
A5$text <- gsub("\\n", "-", A5$text)
A5$text <- gsub("--", "-", A5$text)
A5$text <- gsub("&amp;", "&", A5$text)
A5$text[A5$text == " "] <- "<no text>"

CleanTweetsAAPL5 <- A5 %>% 
  select("text")

SentimentAAPL5 <- analyzeSentiment(CleanTweetsAAPL5)
Sentiment2AAPL5 <- SentimentAAPL5$SentimentQDAP
Sentiment3AAPL5 <- convertToDirection(Sentiment2AAPL5)

dateA5 <- A5$created
dateA5 <- str_extract(dateA5, "\\d{4}-\\d{2}-\\d{2}")
dateA5 <- as.Date(dateA5)
dateA5 <- as.Date(dateA5, format = "%m/%d/%y")

dfAAPL5 <- cbind(CleanTweetsAAPL5, Sentiment2AAPL5, Sentiment3AAPL5, dateA5)
dfAAPL5 <- dfAAPL5[complete.cases(dfAAPL5), ]

df2AAPL5 <- dfAAPL5 %>% 
  group_by(dateA5) %>%
  summarize(meanSentiment5 = mean(Sentiment2AAPL5, na.rm=TRUE))

DT::datatable(df2AAPL5, editable = TRUE)

freqAAPL5 <- dfAAPL5 %>% 
  group_by(dateA5,Sentiment3AAPL5) %>% 
  summarise(Freq5 = n())

freq2AAPL5 <- freqAAPL5 %>% 
  spread(key = Sentiment3AAPL5, value = Freq5)

DT::datatable(freq2AAPL5, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freqAAPL1$dateA1, y = freqAAPL1$Freq1, fill = freqAAPL1$Sentiment3AAPL1), stat = "identity") +
  geom_bar(mapping = aes(x = freqAAPL2$dateA2, y = freqAAPL2$Freq2, fill = freqAAPL2$Sentiment3AAPL2), stat = "identity") +
  geom_bar(mapping = aes(x = freqAAPL3$dateA3, y = freqAAPL3$Freq3, fill = freqAAPL3$Sentiment3AAPL3), stat = "identity") +
  geom_bar(mapping = aes(x = freqAAPL4$dateA4, y = freqAAPL4$Freq4, fill = freqAAPL4$Sentiment3AAPL4), stat = "identity") +
  geom_bar(mapping = aes(x = freqAAPL5$dateA5, y = freqAAPL5$Freq5, fill = freqAAPL5$Sentiment3AAPL5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

firstcolumn <- c("2021-03-28","2021-03-29","2021-03-30","2021-03-31","2021-04-01")
secondcolumn <- c(0.0673717713248756, 0.0550702648603633, 0.0824150828564159, 0.0502084731099398, 0.080831545296807)
MeanSentimentAAPL <- data.frame(firstcolumn, secondcolumn)
MeanSentimentAAPL$Date <- as.Date(MeanSentimentAAPL$firstcolumn)

(sAAPL <- ggplot(data = MeanSentimentAAPL, aes(x = firstcolumn, y = secondcolumn, group = 1)) +
  geom_line() + geom_point() +
  ylab("Mean Twitter Sentiment Score") +
  xlab("Date"))

AAPLReturns <- 100*(log(AAPL$Adj.Close[-1])-log(AAPL$Adj.Close)[-nrow(AAPL)])
AAPLReturns1 <- arima(AAPLReturns, order = c(1,0,0))
AAPL11 <- accuracy(AAPLReturns1)[1:4]
T <- data.frame(forecast(AAPLReturns1))
T$Date <- as.Date(MeanSentimentAAPL$firstcolumn)
T1 <- T[-c(6,7,8,9,10),]
  
(pAAPL <- ggplot(T1, aes(x = Date, y = Point.Forecast, group = 1)) +
  geom_line()+
  geom_point() +
  ylab("Adjusted Point Forecast Apple"))

plot(MeanSentimentAAPL$Date, MeanSentimentAAPL$secondcolumn, type = "l", col = "black",  xlab = 'Date', ylab = 'Mean Sentiment Score Apple')
par(new=TRUE)
plot(T1$Date,T1$Point.Forecast, type = "l", axes = F , xlab = NA, ylab = NA, col = "blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Adjusted Point Forecast Apple', col = "blue")

(summary(lm(T1$Point.Forecast~MeanSentimentAAPL$secondcolumn)))

