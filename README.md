# Twitter Sentiment and Stock Market Movement

## Problem

Does what people say about a company on Twitter line up with where its stock is headed? This project tests that for five S&P 500 companies (**Apple, Amazon, Facebook, Intel and Microsoft**) by scoring the sentiment of tweets about each one and comparing it against a time-series forecast of its stock returns.

It builds on prior work linking overall Twitter mood to market movements (e.g. Bollen, Mao & Zeng, 2010, "Twitter mood predicts the stock market") and asks the same question at the level of individual companies. Completed for ECON 423 at the University of Waterloo; the analysis is in R.

## Data

- **Stock prices:** daily adjusted-close prices for AAPL, AMZN, FB, INTC and MSFT, 2021-01-01 to 2021-03-26. The price CSVs aren't included in this repo.
- **Tweets:** English-language tweets containing `#apple`, `#amazon`, `#facebook`, `#intel` and `#microsoft`, collected through the Twitter search API for five consecutive days, **2021-03-28 to 2021-04-01**, up to 750 tweets per day per company. The resulting mean daily sentiment scores are hard-coded in each script, since the search API only reached back about five days and those tweets can no longer be collected.

## Methodology

1. **Score the tweets.** Each tweet is cleaned and scored with the QDAP sentiment dictionary (`SentimentAnalysis::analyzeSentiment`), then classed as positive, neutral or negative. Scores are averaged by day to give one mean sentiment value per day.
2. **Forecast the stock.** Daily returns are computed as 100 × the log change in adjusted close. An AR(1) model (`arima(returns, order = c(1, 0, 0))`) is fit on the January–March returns and used to forecast the same five days the tweets cover.
3. **Compare the two.** The five daily forecasts are plotted against the five daily mean sentiment scores, and the forecast is regressed on mean sentiment (`lm(forecast ~ mean_sentiment)`) to test for a relationship.

Each company is analysed separately using the same steps. **Tools:** R (`rtweet`, `twitteR`, `SentimentAnalysis`, `forecast`, `dplyr`, `ggplot2`).

## Results

| Company | Hashtag | Regression of forecasted return on mean Twitter sentiment |
|---|---|---|
| Apple (AAPL) | `#apple` | No relationship (p = 0.9695) |
| Amazon (AMZN) | `#amazon` | No relationship (p = 0.479) |
| Facebook (FB) | `#facebook` | Not significant at the 5% level |
| Intel (INTC) | `#intel` | **Significant at the 5% level** |
| Microsoft (MSFT) | `#microsoft` | No relationship |

Intel was the only company with a significant result. The report attributes this to data coverage rather than to Intel being special: Intel is mentioned far less on Twitter than the other four, so the collection could capture close to all of its tweets for the period, whereas for the other four the tweet volume was too high for the collection to be complete.

## Key takeaways

- **The evidence is inconclusive.** It neither confirms nor rules out a relationship between company-level Twitter sentiment and stock movement.
- **Coverage drove the one significant result.** For four of the five companies only a sample of relevant tweets could be collected, so their sentiment scores may not reflect overall Twitter sentiment. More complete collection (e.g. Python-based) would be needed for a real test.
- **The sample is very small.** Each regression uses just five daily observations, so the p-values should be read with that in mind.
- **The stock side is a forecast.** The variable regressed on sentiment is the AR(1) forecast of returns, not realised returns for those days.

## How to run

1. Install the R packages: `dplyr`, `tidyr`, `ggplot2`, `httr`, `stringr`, `twitteR`, `magrittr`, `SentimentAnalysis`, `gridExtra`, `rtweet`, `forecast` and `DT`.
2. Supply daily price CSVs (`AAPL.csv`, `AMZN.csv`, `FB.csv`, `INTC.csv`, `MSFT.csv`, with `Date` and `Adj.Close` columns) and update the file paths in the AAPL script, which reads all five.
3. Run the AAPL script first, then the other four. Each script has the original mean daily sentiment scores hard-coded, so you can skip tweet collection; re-collecting would need your own Twitter/X API credentials and a recent date window.

## Repo structure

```
├── Bamrah 20682484 Econ 423-stage 2 AAPL.R   # run first: loads packages, reads all five price files, sets up Twitter access
├── Bamrah 20682484 Econ 423-stage 2 AMZN.R
├── Bamrah 20682484 Econ 423-stage 2 FB.R
├── Bamrah 20682484 Econ 423-stage 2 INTC.R
├── Bamrah 20682484 Econ 423-stage 2 MSFT.R
├── Bamrah 20682484 Econ 423-Final Project.pdf  # write-up
└── README.md
```
