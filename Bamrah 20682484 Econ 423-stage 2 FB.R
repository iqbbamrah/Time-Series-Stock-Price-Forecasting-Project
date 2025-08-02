FB <- read.csv("/Users/iqbbamrah/Downloads/Stocks/price/raw/FB.csv")

# Creating Models for the Twitter and Stock Price Data
NumberofTweets <- 750
tweetsFB1 <- search_tweets(q = "#facebook", n = NumberofTweets, lang="en", since = "2021-03-28", until = "2021-03-29")
tweetsFB2 <- search_tweets(q = "#facebook", n = NumberofTweets, lang="en", since = "2021-03-29", until = "2021-03-30")
tweetsFB3 <- search_tweets(q = "#facebook", n = NumberofTweets, lang="en", since = "2021-03-30", until = "2021-03-31")
tweetsFB4 <- search_tweets(q = "#facebook", n = NumberofTweets, lang="en", since = "2021-03-31", until = "2021-04-01")
tweetsFB5 <- search_tweets(q = "#facebook", n = NumberofTweets, lang="en", since = "2021-04-01", until = "2021-04-02")

# FB
tweetsDFFB1 <- as.data.frame(tweetsFB1)
tweetsDFFB2 <- as.data.frame(tweetsFB2)
tweetsDFFB3 <- as.data.frame(tweetsFB3)
tweetsDFFB4 <- as.data.frame(tweetsFB4)
tweetsDFFB5 <- as.data.frame(tweetsFB5)

A1 <- tweetsDFFB1
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

CleanTweetsFB1 <- A1 %>% 
  select("text")

SentimentFB1 <- analyzeSentiment(CleanTweetsFB1)
Sentiment2FB1 <- SentimentFB1$SentimentQDAP
Sentiment3FB1 <- convertToDirection(Sentiment2FB1)

dateA1 <- A1$created
dateA1 <- str_extract(dateA1, "\\d{4}-\\d{2}-\\d{2}")
dateA1 <- as.Date(dateA1)
dateA1 <- as.Date(dateA1, format = "%m/%d/%y")

dfFB1 <- cbind(CleanTweetsFB1, Sentiment2FB1, Sentiment3FB1, dateA1)
dfFB1 <- dfFB1[complete.cases(dfFB1), ]

df2FB1 <- dfFB1 %>% 
  group_by(dateA1) %>%
  summarize(meanSentiment1 = mean(Sentiment2FB1, na.rm=TRUE))

DT::datatable(df2FB1, editable = TRUE)

freqFB1 <- dfFB1 %>% 
  group_by(dateA1,Sentiment3FB1) %>% 
  summarise(Freq1 = n())

freq2FB1 <- freqFB1 %>% 
  spread(key = Sentiment3FB1, value = Freq1)

DT::datatable(freq2FB1, editable = TRUE)

A2 <- tweetsDFFB2
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

CleanTweetsFB2 <- A2 %>% 
  select("text")

SentimentFB2 <- analyzeSentiment(CleanTweetsFB2)
Sentiment2FB2 <- SentimentFB2$SentimentQDAP
Sentiment3FB2 <- convertToDirection(Sentiment2FB2)

dateA2 <- A2$created
dateA2 <- str_extract(dateA2, "\\d{4}-\\d{2}-\\d{2}")
dateA2 <- as.Date(dateA2)
dateA2 <- as.Date(dateA2, format = "%m/%d/%y")

dfFB2 <- cbind(CleanTweetsFB2, Sentiment2FB2, Sentiment3FB2, dateA2)
dfFB2 <- dfFB2[complete.cases(dfFB2), ]

df2FB2 <- dfFB2 %>% 
  group_by(dateA2) %>%
  summarize(meanSentiment2 = mean(Sentiment2FB2, na.rm=TRUE))

DT::datatable(df2FB2, editable = TRUE)

freqFB2 <- dfFB2 %>% 
  group_by(dateA2,Sentiment3FB2) %>% 
  summarise(Freq2 = n())

freq2FB2 <- freqFB2 %>% 
  spread(key = Sentiment3FB2, value = Freq2)

DT::datatable(freq2FB2, editable = TRUE)

A3 <- tweetsDFFB3
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

CleanTweetsFB3 <- A3 %>% 
  select("text")

SentimentFB3 <- analyzeSentiment(CleanTweetsFB3)
Sentiment2FB3 <- SentimentFB3$SentimentQDAP
Sentiment3FB3 <- convertToDirection(Sentiment2FB3)

dateA3 <- A3$created
dateA3 <- str_extract(dateA3, "\\d{4}-\\d{2}-\\d{2}")
dateA3 <- as.Date(dateA3)
dateA3 <- as.Date(dateA3, format = "%m/%d/%y")

dfFB3 <- cbind(CleanTweetsFB3, Sentiment2FB3, Sentiment3FB3, dateA3)
dfFB3 <- dfFB3[complete.cases(dfFB3), ]

df2FB3 <- dfFB3 %>% 
  group_by(dateA3) %>%
  summarize(meanSentiment3 = mean(Sentiment2FB3, na.rm=TRUE))

DT::datatable(df2FB3, editable = TRUE)

freqFB3 <- dfFB3 %>% 
  group_by(dateA3,Sentiment3FB3) %>% 
  summarise(Freq3 = n())

freq2FB3 <- freqFB3 %>% 
  spread(key = Sentiment3FB3, value = Freq3)

DT::datatable(freq2FB3, editable = TRUE)

A4 <- tweetsDFFB4
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

CleanTweetsFB4 <- A4 %>% 
  select("text")

SentimentFB4 <- analyzeSentiment(CleanTweetsFB4)
Sentiment2FB4 <- SentimentFB4$SentimentQDAP
Sentiment3FB4 <- convertToDirection(Sentiment2FB4)

dateA4 <- A4$created
dateA4 <- str_extract(dateA4, "\\d{4}-\\d{2}-\\d{2}")
dateA4 <- as.Date(dateA4)
dateA4 <- as.Date(dateA4, format = "%m/%d/%y")

dfFB4 <- cbind(CleanTweetsFB4, Sentiment2FB4, Sentiment3FB4, dateA4)
dfFB4 <- dfFB4[complete.cases(dfFB4), ]

df2FB4 <- dfFB4 %>% 
  group_by(dateA4) %>%
  summarize(meanSentiment4 = mean(Sentiment2FB4, na.rm=TRUE))

DT::datatable(df2FB4, editable = TRUE)

freqFB4 <- dfFB4 %>% 
  group_by(dateA4,Sentiment3FB4) %>% 
  summarise(Freq4 = n())

freq2FB4 <- freqFB4 %>% 
  spread(key = Sentiment3FB4, value = Freq4)

DT::datatable(freq2FB4, editable = TRUE)

A5 <- tweetsDFFB5
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

CleanTweetsFB5 <- A5 %>% 
  select("text")

SentimentFB5 <- analyzeSentiment(CleanTweetsFB5)
Sentiment2FB5 <- SentimentFB5$SentimentQDAP
Sentiment3FB5 <- convertToDirection(Sentiment2FB5)

dateA5 <- A5$created
dateA5 <- str_extract(dateA5, "\\d{4}-\\d{2}-\\d{2}")
dateA5 <- as.Date(dateA5)
dateA5 <- as.Date(dateA5, format = "%m/%d/%y")

dfFB5 <- cbind(CleanTweetsFB5, Sentiment2FB5, Sentiment3FB5, dateA5)
dfFB5 <- dfFB5[complete.cases(dfFB5), ]

df2FB5 <- dfFB5 %>% 
  group_by(dateA5) %>%
  summarize(meanSentiment5 = mean(Sentiment2FB5, na.rm=TRUE))

DT::datatable(df2FB5, editable = TRUE)

freqFB5 <- dfFB5 %>% 
  group_by(dateA5,Sentiment3FB5) %>% 
  summarise(Freq5 = n())

freq2FB5 <- freqFB5 %>% 
  spread(key = Sentiment3FB5, value = Freq5)

DT::datatable(freq2FB5, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freqFB1$dateA1, y = freqFB1$Freq1, fill = freqFB1$Sentiment3FB1), stat = "identity") +
  geom_bar(mapping = aes(x = freqFB2$dateA2, y = freqFB2$Freq2, fill = freqFB2$Sentiment3FB2), stat = "identity") +
  geom_bar(mapping = aes(x = freqFB3$dateA3, y = freqFB3$Freq3, fill = freqFB3$Sentiment3FB3), stat = "identity") +
  geom_bar(mapping = aes(x = freqFB4$dateA4, y = freqFB4$Freq4, fill = freqFB4$Sentiment3FB4), stat = "identity") +
  geom_bar(mapping = aes(x = freqFB5$dateA5, y = freqFB5$Freq5, fill = freqFB5$Sentiment3FB5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

firstcolumn <- c("2021-03-28","2021-03-29","2021-03-30","2021-03-31","2021-04-01")
secondcolumn <- c(mean(dfFB1$Sentiment2FB1), mean(dfFB2$Sentiment2FB2), mean(dfFB3$Sentiment2FB3), mean(dfFB4$Sentiment2FB4), mean(dfFB5$Sentiment2FB5))
MeanSentimentFB <- data.frame(firstcolumn, secondcolumn)
MeanSentimentFB$Date <- as.Date(MeanSentimentFB$firstcolumn)

(sFB <- ggplot(data = MeanSentimentFB, aes(x = firstcolumn, y = secondcolumn, group = 1)) +
    geom_line() + geom_point() +
    ylab("Mean Twitter Sentiment Score") +
    xlab("Date"))

FBReturns <- 100*(log(FB$Adj.Close[-1])-log(FB$Adj.Close)[-nrow(FB)])
FBReturns1 <- arima(FBReturns, order = c(1,0,0))
FB11 <- accuracy(FBReturns1)[1:4]
TFB <- data.frame(forecast(FBReturns1))
TFB$Date <- as.Date(MeanSentimentFB$firstcolumn)
TFB1 <- TFB[-c(6,7,8,9,10),]

(pFB <- ggplot(TFB1, aes(x = Date, y = Point.Forecast, group = 1)) +
    geom_line()+
    geom_point() +
    ylab("Adjusted Point Forecast Facebook"))

plot(MeanSentimentFB$Date, MeanSentimentFB$secondcolumn, type = "l", col = "black",  xlab = 'Date', ylab = 'Mean Sentiment Score Facebook')
par(new=TRUE)
plot(TFB1$Date,TFB1$Point.Forecast, type = "l", axes = F , xlab = NA, ylab = NA, col = "blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Adjusted Point Forecast Facebook', col = "blue")

(summary(lm(TFB1$Point.Forecast~MeanSentimentFB$secondcolumn)))




