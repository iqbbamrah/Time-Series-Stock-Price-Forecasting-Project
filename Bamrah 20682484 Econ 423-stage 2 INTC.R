INTC <- read.csv("/Users/iqbbamrah/Downloads/Stocks/price/raw/INTC.csv")

# Creating Models for the Twitter and Stock Price Data
NumberofTweets <- 750
tweetsINTC1 <- search_tweets(q = "#intel", n = NumberofTweets, lang="en", since = "2021-03-28", until = "2021-03-29")
tweetsINTC2 <- search_tweets(q = "#intel", n = NumberofTweets, lang="en", since = "2021-03-29", until = "2021-03-30")
tweetsINTC3 <- search_tweets(q = "#intel", n = NumberofTweets, lang="en", since = "2021-03-30", until = "2021-03-31")
tweetsINTC4 <- search_tweets(q = "#intel", n = NumberofTweets, lang="en", since = "2021-03-31", until = "2021-04-01")
tweetsINTC5 <- search_tweets(q = "#intel", n = NumberofTweets, lang="en", since = "2021-04-01", until = "2021-04-02")

# INTC
tweetsDFINTC1 <- as.data.frame(tweetsINTC1)
tweetsDFINTC2 <- as.data.frame(tweetsINTC2)
tweetsDFINTC3 <- as.data.frame(tweetsINTC3)
tweetsDFINTC4 <- as.data.frame(tweetsINTC4)
tweetsDFINTC5 <- as.data.frame(tweetsINTC5)

A1 <- tweetsDFINTC1
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

CleanTweetsINTC1 <- A1 %>% 
  select("text")

SentimentINTC1 <- analyzeSentiment(CleanTweetsINTC1)
Sentiment2INTC1 <- SentimentINTC1$SentimentQDAP
Sentiment3INTC1 <- convertToDirection(Sentiment2INTC1)

dateA1 <- A1$created
dateA1 <- str_extract(dateA1, "\\d{4}-\\d{2}-\\d{2}")
dateA1 <- as.Date(dateA1)
dateA1 <- as.Date(dateA1, format = "%m/%d/%y")

dfINTC1 <- cbind(CleanTweetsINTC1, Sentiment2INTC1, Sentiment3INTC1, dateA1)
dfINTC1 <- dfINTC1[complete.cases(dfINTC1), ]

df2INTC1 <- dfINTC1 %>% 
  group_by(dateA1) %>%
  summarize(meanSentiment1 = mean(Sentiment2INTC1, na.rm=TRUE))

DT::datatable(df2INTC1, editable = TRUE)

freqINTC1 <- dfINTC1 %>% 
  group_by(dateA1,Sentiment3INTC1) %>% 
  summarise(Freq1 = n())

freq2INTC1 <- freqINTC1 %>% 
  spread(key = Sentiment3INTC1, value = Freq1)

DT::datatable(freq2INTC1, editable = TRUE)

A2 <- tweetsDFINTC2
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

CleanTweetsINTC2 <- A2 %>% 
  select("text")

SentimentINTC2 <- analyzeSentiment(CleanTweetsINTC2)
Sentiment2INTC2 <- SentimentINTC2$SentimentQDAP
Sentiment3INTC2 <- convertToDirection(Sentiment2INTC2)

dateA2 <- A2$created
dateA2 <- str_extract(dateA2, "\\d{4}-\\d{2}-\\d{2}")
dateA2 <- as.Date(dateA2)
dateA2 <- as.Date(dateA2, format = "%m/%d/%y")

dfINTC2 <- cbind(CleanTweetsINTC2, Sentiment2INTC2, Sentiment3INTC2, dateA2)
dfINTC2 <- dfINTC2[complete.cases(dfINTC2), ]

df2INTC2 <- dfINTC2 %>% 
  group_by(dateA2) %>%
  summarize(meanSentiment2 = mean(Sentiment2INTC2, na.rm=TRUE))

DT::datatable(df2INTC2, editable = TRUE)

freqINTC2 <- dfINTC2 %>% 
  group_by(dateA2,Sentiment3INTC2) %>% 
  summarise(Freq2 = n())

freq2INTC2 <- freqINTC2 %>% 
  spread(key = Sentiment3INTC2, value = Freq2)

DT::datatable(freq2INTC2, editable = TRUE)

A3 <- tweetsDFINTC3
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

CleanTweetsINTC3 <- A3 %>% 
  select("text")

SentimentINTC3 <- analyzeSentiment(CleanTweetsINTC3)
Sentiment2INTC3 <- SentimentINTC3$SentimentQDAP
Sentiment3INTC3 <- convertToDirection(Sentiment2INTC3)

dateA3 <- A3$created
dateA3 <- str_extract(dateA3, "\\d{4}-\\d{2}-\\d{2}")
dateA3 <- as.Date(dateA3)
dateA3 <- as.Date(dateA3, format = "%m/%d/%y")

dfINTC3 <- cbind(CleanTweetsINTC3, Sentiment2INTC3, Sentiment3INTC3, dateA3)
dfINTC3 <- dfINTC3[complete.cases(dfINTC3), ]

df2INTC3 <- dfINTC3 %>% 
  group_by(dateA3) %>%
  summarize(meanSentiment3 = mean(Sentiment2INTC3, na.rm=TRUE))

DT::datatable(df2INTC3, editable = TRUE)

freqINTC3 <- dfINTC3 %>% 
  group_by(dateA3,Sentiment3INTC3) %>% 
  summarise(Freq3 = n())

freq2INTC3 <- freqINTC3 %>% 
  spread(key = Sentiment3INTC3, value = Freq3)

DT::datatable(freq2INTC3, editable = TRUE)

A4 <- tweetsDFINTC4
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

CleanTweetsINTC4 <- A4 %>% 
  select("text")

SentimentINTC4 <- analyzeSentiment(CleanTweetsINTC4)
Sentiment2INTC4 <- SentimentINTC4$SentimentQDAP
Sentiment3INTC4 <- convertToDirection(Sentiment2INTC4)

dateA4 <- A4$created
dateA4 <- str_extract(dateA4, "\\d{4}-\\d{2}-\\d{2}")
dateA4 <- as.Date(dateA4)
dateA4 <- as.Date(dateA4, format = "%m/%d/%y")

dfINTC4 <- cbind(CleanTweetsINTC4, Sentiment2INTC4, Sentiment3INTC4, dateA4)
dfINTC4 <- dfINTC4[complete.cases(dfINTC4), ]

df2INTC4 <- dfINTC4 %>% 
  group_by(dateA4) %>%
  summarize(meanSentiment4 = mean(Sentiment2INTC4, na.rm=TRUE))

DT::datatable(df2INTC4, editable = TRUE)

freqINTC4 <- dfINTC4 %>% 
  group_by(dateA4,Sentiment3INTC4) %>% 
  summarise(Freq4 = n())

freq2INTC4 <- freqINTC4 %>% 
  spread(key = Sentiment3INTC4, value = Freq4)

DT::datatable(freq2INTC4, editable = TRUE)

A5 <- tweetsDFINTC5
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

CleanTweetsINTC5 <- A5 %>% 
  select("text")

SentimentINTC5 <- analyzeSentiment(CleanTweetsINTC5)
Sentiment2INTC5 <- SentimentINTC5$SentimentQDAP
Sentiment3INTC5 <- convertToDirection(Sentiment2INTC5)

dateA5 <- A5$created
dateA5 <- str_extract(dateA5, "\\d{4}-\\d{2}-\\d{2}")
dateA5 <- as.Date(dateA5)
dateA5 <- as.Date(dateA5, format = "%m/%d/%y")

dfINTC5 <- cbind(CleanTweetsINTC5, Sentiment2INTC5, Sentiment3INTC5, dateA5)
dfINTC5 <- dfINTC5[complete.cases(dfINTC5), ]

df2INTC5 <- dfINTC5 %>% 
  group_by(dateA5) %>%
  summarize(meanSentiment5 = mean(Sentiment2INTC5, na.rm=TRUE))

DT::datatable(df2INTC5, editable = TRUE)

freqINTC5 <- dfINTC5 %>% 
  group_by(dateA5,Sentiment3INTC5) %>% 
  summarise(Freq5 = n())

freq2INTC5 <- freqINTC5 %>% 
  spread(key = Sentiment3INTC5, value = Freq5)

DT::datatable(freq2INTC5, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freqINTC1$dateA1, y = freqINTC1$Freq1, fill = freqINTC1$Sentiment3INTC1), stat = "identity") +
  geom_bar(mapping = aes(x = freqINTC2$dateA2, y = freqINTC2$Freq2, fill = freqINTC2$Sentiment3INTC2), stat = "identity") +
  geom_bar(mapping = aes(x = freqINTC3$dateA3, y = freqINTC3$Freq3, fill = freqINTC3$Sentiment3INTC3), stat = "identity") +
  geom_bar(mapping = aes(x = freqINTC4$dateA4, y = freqINTC4$Freq4, fill = freqINTC4$Sentiment3INTC4), stat = "identity") +
  geom_bar(mapping = aes(x = freqINTC5$dateA5, y = freqINTC5$Freq5, fill = freqINTC5$Sentiment3INTC5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

firstcolumn <- c("2021-03-28","2021-03-29","2021-03-30","2021-03-31","2021-04-01")
secondcolumn <- c(mean(dfINTC1$Sentiment2INTC1), mean(dfINTC2$Sentiment2INTC2), mean(dfINTC3$Sentiment2INTC3), mean(dfINTC4$Sentiment2INTC4), mean(dfINTC5$Sentiment2INTC5))
MeanSentimentINTC <- data.frame(firstcolumn, secondcolumn)
MeanSentimentINTC$Date <- as.Date(MeanSentimentINTC$firstcolumn)

(sINTC <- ggplot(data = MeanSentimentINTC, aes(x = firstcolumn, y = secondcolumn, group = 1)) +
    geom_line() + geom_point() +
    ylab("Mean Twitter Sentiment Score") +
    xlab("Date"))

INTCReturns <- 100*(log(INTC$Adj.Close[-1])-log(INTC$Adj.Close)[-nrow(INTC)])
INTCReturns1 <- arima(INTCReturns, order = c(1,0,0))
INTC11 <- accuracy(INTCReturns1)[1:4]
TINTC <- data.frame(forecast(INTCReturns1))
TINTC$Date <- as.Date(MeanSentimentINTC$firstcolumn)
TINTC1 <- TINTC[-c(6,7,8,9,10),]

(pINTC <- ggplot(TINTC1, aes(x = Date, y = Point.Forecast, group = 1)) +
    geom_line()+
    geom_point() +
    ylab("Adjusted Point Forecast Intel"))

plot(MeanSentimentINTC$Date, MeanSentimentINTC$secondcolumn, type = "l", col = "black",  xlab = 'Date', ylab = 'Mean Sentiment Score Intel')
par(new=TRUE)
plot(TINTC1$Date,TINTC1$Point.Forecast, type = "l", axes = F , xlab = NA, ylab = NA, col = "blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Adjusted Point Forecast Intel', col = "blue")

(summary(lm(TINTC1$Point.Forecast~MeanSentimentINTC$secondcolumn)))




