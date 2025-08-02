AMZN <- read.csv("/Users/iqbbamrah/Downloads/Stocks/price/raw/AMZN.csv")

# Creating Models for the Twitter and Stock Price Data
NumberofTweets <- 750
tweetsAMZN1 <- search_tweets(q = "#amazon", n = NumberofTweets, lang="en", since = "2021-03-28", until = "2021-03-29")
tweetsAMZN2 <- search_tweets(q = "#amazon", n = NumberofTweets, lang="en", since = "2021-03-29", until = "2021-03-30")
tweetsAMZN3 <- search_tweets(q = "#amazon", n = NumberofTweets, lang="en", since = "2021-03-30", until = "2021-03-31")
tweetsAMZN4 <- search_tweets(q = "#amazon", n = NumberofTweets, lang="en", since = "2021-03-31", until = "2021-04-01")
tweetsAMZN5 <- search_tweets(q = "#amazon", n = NumberofTweets, lang="en", since = "2021-04-01", until = "2021-04-02")

# AMZN
tweetsDFAMZN1 <- as.data.frame(tweetsAMZN1)
tweetsDFAMZN2 <- as.data.frame(tweetsAMZN2)
tweetsDFAMZN3 <- as.data.frame(tweetsAMZN3)
tweetsDFAMZN4 <- as.data.frame(tweetsAMZN4)
tweetsDFAMZN5 <- as.data.frame(tweetsAMZN5)

A1 <- tweetsDFAMZN1
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

CleanTweetsAMZN1 <- A1 %>% 
  select("text")

SentimentAMZN1 <- analyzeSentiment(CleanTweetsAMZN1)
Sentiment2AMZN1 <- SentimentAMZN1$SentimentQDAP
Sentiment3AMZN1 <- convertToDirection(Sentiment2AMZN1)

dateA1 <- A1$created
dateA1 <- str_extract(dateA1, "\\d{4}-\\d{2}-\\d{2}")
dateA1 <- as.Date(dateA1)
dateA1 <- as.Date(dateA1, format = "%m/%d/%y")

dfAMZN1 <- cbind(CleanTweetsAMZN1, Sentiment2AMZN1, Sentiment3AMZN1, dateA1)
dfAMZN1 <- dfAMZN1[complete.cases(dfAMZN1), ]

df2AMZN1 <- dfAMZN1 %>% 
  group_by(dateA1) %>%
  summarize(meanSentiment1 = mean(Sentiment2AMZN1, na.rm=TRUE))

DT::datatable(df2AMZN1, editable = TRUE)

freqAMZN1 <- dfAMZN1 %>% 
  group_by(dateA1,Sentiment3AMZN1) %>% 
  summarise(Freq1 = n())

freq2AMZN1 <- freqAMZN1 %>% 
  spread(key = Sentiment3AMZN1, value = Freq1)

DT::datatable(freq2AMZN1, editable = TRUE)

A2 <- tweetsDFAMZN2
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

CleanTweetsAMZN2 <- A2 %>% 
  select("text")

SentimentAMZN2 <- analyzeSentiment(CleanTweetsAMZN2)
Sentiment2AMZN2 <- SentimentAMZN2$SentimentQDAP
Sentiment3AMZN2 <- convertToDirection(Sentiment2AMZN2)

dateA2 <- A2$created
dateA2 <- str_extract(dateA2, "\\d{4}-\\d{2}-\\d{2}")
dateA2 <- as.Date(dateA2)
dateA2 <- as.Date(dateA2, format = "%m/%d/%y")

dfAMZN2 <- cbind(CleanTweetsAMZN2, Sentiment2AMZN2, Sentiment3AMZN2, dateA2)
dfAMZN2 <- dfAMZN2[complete.cases(dfAMZN2), ]

df2AMZN2 <- dfAMZN2 %>% 
  group_by(dateA2) %>%
  summarize(meanSentiment2 = mean(Sentiment2AMZN2, na.rm=TRUE))

DT::datatable(df2AMZN2, editable = TRUE)

freqAMZN2 <- dfAMZN2 %>% 
  group_by(dateA2,Sentiment3AMZN2) %>% 
  summarise(Freq2 = n())

freq2AMZN2 <- freqAMZN2 %>% 
  spread(key = Sentiment3AMZN2, value = Freq2)

DT::datatable(freq2AMZN2, editable = TRUE)

A3 <- tweetsDFAMZN3
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

CleanTweetsAMZN3 <- A3 %>% 
  select("text")

SentimentAMZN3 <- analyzeSentiment(CleanTweetsAMZN3)
Sentiment2AMZN3 <- SentimentAMZN3$SentimentQDAP
Sentiment3AMZN3 <- convertToDirection(Sentiment2AMZN3)

dateA3 <- A3$created
dateA3 <- str_extract(dateA3, "\\d{4}-\\d{2}-\\d{2}")
dateA3 <- as.Date(dateA3)
dateA3 <- as.Date(dateA3, format = "%m/%d/%y")

dfAMZN3 <- cbind(CleanTweetsAMZN3, Sentiment2AMZN3, Sentiment3AMZN3, dateA3)
dfAMZN3 <- dfAMZN3[complete.cases(dfAMZN3), ]

df2AMZN3 <- dfAMZN3 %>% 
  group_by(dateA3) %>%
  summarize(meanSentiment3 = mean(Sentiment2AMZN3, na.rm=TRUE))

DT::datatable(df2AMZN3, editable = TRUE)

freqAMZN3 <- dfAMZN3 %>% 
  group_by(dateA3,Sentiment3AMZN3) %>% 
  summarise(Freq3 = n())

freq2AMZN3 <- freqAMZN3 %>% 
  spread(key = Sentiment3AMZN3, value = Freq3)

DT::datatable(freq2AMZN3, editable = TRUE)

A4 <- tweetsDFAMZN4
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

CleanTweetsAMZN4 <- A4 %>% 
  select("text")

SentimentAMZN4 <- analyzeSentiment(CleanTweetsAMZN4)
Sentiment2AMZN4 <- SentimentAMZN4$SentimentQDAP
Sentiment3AMZN4 <- convertToDirection(Sentiment2AMZN4)

dateA4 <- A4$created
dateA4 <- str_extract(dateA4, "\\d{4}-\\d{2}-\\d{2}")
dateA4 <- as.Date(dateA4)
dateA4 <- as.Date(dateA4, format = "%m/%d/%y")

dfAMZN4 <- cbind(CleanTweetsAMZN4, Sentiment2AMZN4, Sentiment3AMZN4, dateA4)
dfAMZN4 <- dfAMZN4[complete.cases(dfAMZN4), ]

df2AMZN4 <- dfAMZN4 %>% 
  group_by(dateA4) %>%
  summarize(meanSentiment4 = mean(Sentiment2AMZN4, na.rm=TRUE))

DT::datatable(df2AMZN4, editable = TRUE)

freqAMZN4 <- dfAMZN4 %>% 
  group_by(dateA4,Sentiment3AMZN4) %>% 
  summarise(Freq4 = n())

freq2AMZN4 <- freqAMZN4 %>% 
  spread(key = Sentiment3AMZN4, value = Freq4)

DT::datatable(freq2AMZN4, editable = TRUE)

A5 <- tweetsDFAMZN5
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

CleanTweetsAMZN5 <- A5 %>% 
  select("text")

SentimentAMZN5 <- analyzeSentiment(CleanTweetsAMZN5)
Sentiment2AMZN5 <- SentimentAMZN5$SentimentQDAP
Sentiment3AMZN5 <- convertToDirection(Sentiment2AMZN5)

dateA5 <- A5$created
dateA5 <- str_extract(dateA5, "\\d{4}-\\d{2}-\\d{2}")
dateA5 <- as.Date(dateA5)
dateA5 <- as.Date(dateA5, format = "%m/%d/%y")

dfAMZN5 <- cbind(CleanTweetsAMZN5, Sentiment2AMZN5, Sentiment3AMZN5, dateA5)
dfAMZN5 <- dfAMZN5[complete.cases(dfAMZN5), ]

df2AMZN5 <- dfAMZN5 %>% 
  group_by(dateA5) %>%
  summarize(meanSentiment5 = mean(Sentiment2AMZN5, na.rm=TRUE))

DT::datatable(df2AMZN5, editable = TRUE)

freqAMZN5 <- dfAMZN5 %>% 
  group_by(dateA5,Sentiment3AMZN5) %>% 
  summarise(Freq5 = n())

freq2AMZN5 <- freqAMZN5 %>% 
  spread(key = Sentiment3AMZN5, value = Freq5)

DT::datatable(freq2AMZN5, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freqAMZN1$dateA1, y = freqAMZN1$Freq1, fill = freqAMZN1$Sentiment3AMZN1), stat = "identity") +
  geom_bar(mapping = aes(x = freqAMZN2$dateA2, y = freqAMZN2$Freq2, fill = freqAMZN2$Sentiment3AMZN2), stat = "identity") +
  geom_bar(mapping = aes(x = freqAMZN3$dateA3, y = freqAMZN3$Freq3, fill = freqAMZN3$Sentiment3AMZN3), stat = "identity") +
  geom_bar(mapping = aes(x = freqAMZN4$dateA4, y = freqAMZN4$Freq4, fill = freqAMZN4$Sentiment3AMZN4), stat = "identity") +
  geom_bar(mapping = aes(x = freqAMZN5$dateA5, y = freqAMZN5$Freq5, fill = freqAMZN5$Sentiment3AMZN5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('Date')

firstcolumn <- c("2021-03-28","2021-03-29","2021-03-30","2021-03-31","2021-04-01")
secondcolumn <- c(mean(dfAMZN1$Sentiment2AMZN1), mean(dfAMZN2$Sentiment2AMZN2), mean(dfAMZN3$Sentiment2AMZN3), mean(dfAMZN4$Sentiment2AMZN4), mean(dfAMZN5$Sentiment2AMZN5))
MeanSentimentAMZN <- data.frame(firstcolumn, secondcolumn)
MeanSentimentAMZN$Date <- as.Date(MeanSentimentAMZN$firstcolumn)

(sAMZN <- ggplot(data = MeanSentimentAMZN, aes(x = firstcolumn, y = secondcolumn, group = 1)) +
  geom_line() + geom_point() +
  ylab("Mean Twitter Sentiment Score") +
  xlab("Date"))

AMZNReturns <- 100*(log(AMZN$Adj.Close[-1])-log(AMZN$Adj.Close)[-nrow(AMZN)])
AMZNReturns1 <- arima(AMZNReturns, order = c(1,0,0))
AMZN11 <- accuracy(AMZNReturns1)[1:4]
TAMZN <- data.frame(forecast(AMZNReturns1))
TAMZN$Date <- as.Date(MeanSentimentAMZN$firstcolumn)
TAMZN1 <- TAMZN[-c(6,7,8,9,10),]

(pAMZN <- ggplot(TAMZN1, aes(x = Date, y = Point.Forecast, group = 1)) +
  geom_line()+
  geom_point() +
  ylab("Adjusted Point Forecast Amazon"))

plot(MeanSentimentAMZN$Date, MeanSentimentAMZN$secondcolumn, type = "l", col = "black",  xlab = 'Date', ylab = 'Mean Sentiment Score Amazon')
par(new=TRUE)
plot(TAMZN1$Date,TAMZN1$Point.Forecast, type = "l", axes = F , xlab = NA, ylab = NA, col = "blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Adjusted Point Forecast Amazon', col = "blue")

(summary(lm(TAMZN1$Point.Forecast~MeanSentimentAMZN$secondcolumn)))




