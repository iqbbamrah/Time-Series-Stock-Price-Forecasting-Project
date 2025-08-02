MSFT <- read.csv("/Users/iqbbamrah/Downloads/Stocks/price/raw/MSFT.csv")

# Creating Models for the Twitter and Stock Price Data
NumberofTweets <- 750
tweetsMSFT1 <- search_tweets(q = "#microsoft", n = NumberofTweets, lang="en", since = "2021-03-28", until = "2021-03-29")
tweetsMSFT2 <- search_tweets(q = "#microsoft", n = NumberofTweets, lang="en", since = "2021-03-29", until = "2021-03-30")
tweetsMSFT3 <- search_tweets(q = "#microsoft", n = NumberofTweets, lang="en", since = "2021-03-30", until = "2021-03-31")
tweetsMSFT4 <- search_tweets(q = "#microsoft", n = NumberofTweets, lang="en", since = "2021-03-31", until = "2021-04-01")
tweetsMSFT5 <- search_tweets(q = "#microsoft", n = NumberofTweets, lang="en", since = "2021-04-01", until = "2021-04-02")

# MSFT
tweetsDFMSFT1 <- as.data.frame(tweetsMSFT1)
tweetsDFMSFT2 <- as.data.frame(tweetsMSFT2)
tweetsDFMSFT3 <- as.data.frame(tweetsMSFT3)
tweetsDFMSFT4 <- as.data.frame(tweetsMSFT4)
tweetsDFMSFT5 <- as.data.frame(tweetsMSFT5)

A1 <- tweetsDFMSFT1
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

CleanTweetsMSFT1 <- A1 %>% 
  select("text")

SentimentMSFT1 <- analyzeSentiment(CleanTweetsMSFT1)
Sentiment2MSFT1 <- SentimentMSFT1$SentimentQDAP
Sentiment3MSFT1 <- convertToDirection(Sentiment2MSFT1)

dateA1 <- A1$created
dateA1 <- str_extract(dateA1, "\\d{4}-\\d{2}-\\d{2}")
dateA1 <- as.Date(dateA1)
dateA1 <- as.Date(dateA1, format = "%m/%d/%y")

dfMSFT1 <- cbind(CleanTweetsMSFT1, Sentiment2MSFT1, Sentiment3MSFT1, dateA1)
dfMSFT1 <- dfMSFT1[complete.cases(dfMSFT1), ]

df2MSFT1 <- dfMSFT1 %>% 
  group_by(dateA1) %>%
  summarize(meanSentiment1 = mean(Sentiment2MSFT1, na.rm=TRUE))

DT::datatable(df2MSFT1, editable = TRUE)

freqMSFT1 <- dfMSFT1 %>% 
  group_by(dateA1,Sentiment3MSFT1) %>% 
  summarise(Freq1 = n())

freq2MSFT1 <- freqMSFT1 %>% 
  spread(key = Sentiment3MSFT1, value = Freq1)

DT::datatable(freq2MSFT1, editable = TRUE)

A2 <- tweetsDFMSFT2
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

CleanTweetsMSFT2 <- A2 %>% 
  select("text")

SentimentMSFT2 <- analyzeSentiment(CleanTweetsMSFT2)
Sentiment2MSFT2 <- SentimentMSFT2$SentimentQDAP
Sentiment3MSFT2 <- convertToDirection(Sentiment2MSFT2)

dateA2 <- A2$created
dateA2 <- str_extract(dateA2, "\\d{4}-\\d{2}-\\d{2}")
dateA2 <- as.Date(dateA2)
dateA2 <- as.Date(dateA2, format = "%m/%d/%y")

dfMSFT2 <- cbind(CleanTweetsMSFT2, Sentiment2MSFT2, Sentiment3MSFT2, dateA2)
dfMSFT2 <- dfMSFT2[complete.cases(dfMSFT2), ]

df2MSFT2 <- dfMSFT2 %>% 
  group_by(dateA2) %>%
  summarize(meanSentiment2 = mean(Sentiment2MSFT2, na.rm=TRUE))

DT::datatable(df2MSFT2, editable = TRUE)

freqMSFT2 <- dfMSFT2 %>% 
  group_by(dateA2,Sentiment3MSFT2) %>% 
  summarise(Freq2 = n())

freq2MSFT2 <- freqMSFT2 %>% 
  spread(key = Sentiment3MSFT2, value = Freq2)

DT::datatable(freq2MSFT2, editable = TRUE)

A3 <- tweetsDFMSFT3
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

CleanTweetsMSFT3 <- A3 %>% 
  select("text")

SentimentMSFT3 <- analyzeSentiment(CleanTweetsMSFT3)
Sentiment2MSFT3 <- SentimentMSFT3$SentimentQDAP
Sentiment3MSFT3 <- convertToDirection(Sentiment2MSFT3)

dateA3 <- A3$created
dateA3 <- str_extract(dateA3, "\\d{4}-\\d{2}-\\d{2}")
dateA3 <- as.Date(dateA3)
dateA3 <- as.Date(dateA3, format = "%m/%d/%y")

dfMSFT3 <- cbind(CleanTweetsMSFT3, Sentiment2MSFT3, Sentiment3MSFT3, dateA3)
dfMSFT3 <- dfMSFT3[complete.cases(dfMSFT3), ]

df2MSFT3 <- dfMSFT3 %>% 
  group_by(dateA3) %>%
  summarize(meanSentiment3 = mean(Sentiment2MSFT3, na.rm=TRUE))

DT::datatable(df2MSFT3, editable = TRUE)

freqMSFT3 <- dfMSFT3 %>% 
  group_by(dateA3,Sentiment3MSFT3) %>% 
  summarise(Freq3 = n())

freq2MSFT3 <- freqMSFT3 %>% 
  spread(key = Sentiment3MSFT3, value = Freq3)

DT::datatable(freq2MSFT3, editable = TRUE)

A4 <- tweetsDFMSFT4
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

CleanTweetsMSFT4 <- A4 %>% 
  select("text")

SentimentMSFT4 <- analyzeSentiment(CleanTweetsMSFT4)
Sentiment2MSFT4 <- SentimentMSFT4$SentimentQDAP
Sentiment3MSFT4 <- convertToDirection(Sentiment2MSFT4)

dateA4 <- A4$created
dateA4 <- str_extract(dateA4, "\\d{4}-\\d{2}-\\d{2}")
dateA4 <- as.Date(dateA4)
dateA4 <- as.Date(dateA4, format = "%m/%d/%y")

dfMSFT4 <- cbind(CleanTweetsMSFT4, Sentiment2MSFT4, Sentiment3MSFT4, dateA4)
dfMSFT4 <- dfMSFT4[complete.cases(dfMSFT4), ]

df2MSFT4 <- dfMSFT4 %>% 
  group_by(dateA4) %>%
  summarize(meanSentiment4 = mean(Sentiment2MSFT4, na.rm=TRUE))

DT::datatable(df2MSFT4, editable = TRUE)

freqMSFT4 <- dfMSFT4 %>% 
  group_by(dateA4,Sentiment3MSFT4) %>% 
  summarise(Freq4 = n())

freq2MSFT4 <- freqMSFT4 %>% 
  spread(key = Sentiment3MSFT4, value = Freq4)

DT::datatable(freq2MSFT4, editable = TRUE)

A5 <- tweetsDFMSFT5
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

CleanTweetsMSFT5 <- A5 %>% 
  select("text")

SentimentMSFT5 <- analyzeSentiment(CleanTweetsMSFT5)
Sentiment2MSFT5 <- SentimentMSFT5$SentimentQDAP
Sentiment3MSFT5 <- convertToDirection(Sentiment2MSFT5)

dateA5 <- A5$created
dateA5 <- str_extract(dateA5, "\\d{4}-\\d{2}-\\d{2}")
dateA5 <- as.Date(dateA5)
dateA5 <- as.Date(dateA5, format = "%m/%d/%y")

dfMSFT5 <- cbind(CleanTweetsMSFT5, Sentiment2MSFT5, Sentiment3MSFT5, dateA5)
dfMSFT5 <- dfMSFT5[complete.cases(dfMSFT5), ]

df2MSFT5 <- dfMSFT5 %>% 
  group_by(dateA5) %>%
  summarize(meanSentiment5 = mean(Sentiment2MSFT5, na.rm=TRUE))

DT::datatable(df2MSFT5, editable = TRUE)

freqMSFT5 <- dfMSFT5 %>% 
  group_by(dateA5,Sentiment3MSFT5) %>% 
  summarise(Freq5 = n())

freq2MSFT5 <- freqMSFT5 %>% 
  spread(key = Sentiment3MSFT5, value = Freq5)

DT::datatable(freq2MSFT5, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freqMSFT1$dateA1, y = freqMSFT1$Freq1, fill = freqMSFT1$Sentiment3MSFT1), stat = "identity") +
  geom_bar(mapping = aes(x = freqMSFT2$dateA2, y = freqMSFT2$Freq2, fill = freqMSFT2$Sentiment3MSFT2), stat = "identity") +
  geom_bar(mapping = aes(x = freqMSFT3$dateA3, y = freqMSFT3$Freq3, fill = freqMSFT3$Sentiment3MSFT3), stat = "identity") +
  geom_bar(mapping = aes(x = freqMSFT4$dateA4, y = freqMSFT4$Freq4, fill = freqMSFT4$Sentiment3MSFT4), stat = "identity") +
  geom_bar(mapping = aes(x = freqMSFT5$dateA5, y = freqMSFT5$Freq5, fill = freqMSFT5$Sentiment3MSFT5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

firstcolumn <- c("2021-03-28","2021-03-29","2021-03-30","2021-03-31","2021-04-01")
secondcolumn <- c(mean(dfMSFT1$Sentiment2MSFT1), mean(dfMSFT2$Sentiment2MSFT2), mean(dfMSFT3$Sentiment2MSFT3), mean(dfMSFT4$Sentiment2MSFT4), mean(dfMSFT5$Sentiment2MSFT5))
MeanSentimentMSFT <- data.frame(firstcolumn, secondcolumn)
MeanSentimentMSFT$Date <- as.Date(MeanSentimentMSFT$firstcolumn)

(sMSFT <- ggplot(data = MeanSentimentMSFT, aes(x = firstcolumn, y = secondcolumn, group = 1)) +
    geom_line() + geom_point() +
    ylab("Mean Twitter Sentiment Score") +
    xlab("Date"))

MSFTReturns <- 100*(log(MSFT$Adj.Close[-1])-log(MSFT$Adj.Close)[-nrow(MSFT)])
MSFTReturns1 <- arima(MSFTReturns, order = c(1,0,0))
MSFT11 <- accuracy(MSFTReturns1)[1:4]
TMSFT <- data.frame(forecast(MSFTReturns1))
TMSFT$Date <- as.Date(MeanSentimentMSFT$firstcolumn)
TMSFT1 <- TMSFT[-c(6,7,8,9,10),]

(pMSFT <- ggplot(TMSFT1, aes(x = Date, y = Point.Forecast, group = 1)) +
    geom_line()+
    geom_point() +
    ylab("Adjusted Point Forecast Microsoft"))

plot(MeanSentimentMSFT$Date, MeanSentimentMSFT$secondcolumn, type = "l", col = "black",  xlab = 'Date', ylab = 'Mean Sentiment Score Microsoft')
par(new=TRUE)
plot(TMSFT1$Date,TMSFT1$Point.Forecast, type = "l", axes = F , xlab = NA, ylab = NA, col = "blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Adjusted Point Forecast Microsoft', col = "blue")

(summary(lm(TMSFT1$Point.Forecast~MeanSentimentMSFT$secondcolumn)))



