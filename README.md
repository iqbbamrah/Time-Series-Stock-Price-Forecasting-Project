# Twitter Sentiment and Stock Market Movement

Does what people say about a company on Twitter line up with where its stock is headed? This project tests that for five S&P 500 companies — **Apple, Amazon, Facebook, Intel and Microsoft** — by scoring the sentiment of tweets about each one and comparing it against a time-series forecast of its stock returns.

Completed for ECON 423 at the University of Waterloo. The full write-up is in [`Bamrah 20682484 Econ 423-Final Project.pdf`](<Bamrah 20682484 Econ 423-Final Project.pdf>), and the analysis is in R.

## The question

Can tweets about a specific company be used to forecast the movement of its stock? The project builds on prior work linking Twitter mood to market movements (e.g. Bollen, Mao & Zeng, 2010, "Twitter mood predicts the stock market") and asks the same question at the level of individual companies.

## Data

- **Stock prices:** daily adjusted-close prices for AAPL, AMZN, FB, INTC and MSFT, 2021-01-01 to 2021-03-26.
- **Tweets:** English-language tweets containing `#apple`, `#amazon`, `#facebook`, `#intel` and `#microsoft`, collected through the Twitter search API for five consecutive days, **2021-03-28 to 2021-04-01**, up to 750 tweets per day per company.

## Method

1. **Score the tweets.** Each tweet is cleaned and scored with the QDAP sentiment dictionary (`SentimentAnalysis::analyzeSentiment`), then classed as positive, neutral or negative. Scores are averaged by day to give one mean sentiment value per day.
2. **Forecast the stock.** Daily returns are computed as 100 × the log change in adjusted close. An AR(1) model — `arima(returns, order = c(1, 0, 0))` — is fit on the January–March returns and used to forecast the same five days the tweets cover.
3. **Compare the two.** The five daily forecasts are plotted against the five daily mean sentiment scores, and the forecast is regressed on mean sentiment (`lm(forecast ~ mean_sentiment)`) to test for a relationship.

Each company is analysed separately, using the same steps.

## Results

| Company | Hashtag | Regression of forecasted return on mean Twitter sentiment |
|---|---|---|
| Apple (AAPL) | `#apple` | No relationship (p = 0.9695) |
| Amazon (AMZN) | `#amazon` | No relationship (p = 0.479) |
| Facebook (FB) | `#facebook` | Not significant at the 5% level |
| Intel (INTC) | `#intel` | **Significant at the 5% level** |
| Microsoft (MSFT) | `#microsoft` | No relationship |

Intel was the only company with a significant result. The report attributes this to data coverage rather than to Intel being special: Intel is mentioned far less on Twitter than the other four, so the collection could capture close to all of its tweets for the period, whereas for the other four the volume of tweets was too high for the collection to be complete.

## Limitations

- **Limited tweet coverage.** As above, for four of the five companies only a sample of the relevant tweets could be collected, so those sentiment scores may not reflect overall Twitter sentiment. The report's conclusion is that the evidence is inconclusive: it can't confirm or rule out a relationship, and it suggests Python-based collection as a way to gather more complete data.
- **Very small sample.** Each regression uses just five daily observations (one per day, 2021-03-28 to 2021-04-01), so the p-values should be read with that in mind.
- **The stock side is a forecast.** The variable regressed on sentiment is the AR(1) model's forecast of returns, not realised returns for those days.

## Running the code

The five scripts are `Bamrah 20682484 Econ 423-stage 2 {AAPL,AMZN,FB,INTC,MSFT}.R`. **Run the AAPL script first** — it loads the packages, reads the price data for all five stocks, and sets up the Twitter connection.

What you need to know before running:

- **The stock price CSVs are not in this repository.** The scripts read `AAPL.csv`, `AMZN.csv`, `FB.csv`, `INTC.csv` and `MSFT.csv` from absolute paths on the author's machine (e.g. `/Users/.../Stocks/raw/AAPL.csv`), and use their `Date` and `Adj.Close` columns. You'll need to supply daily price files in that format and update the paths.
- **Twitter access.** The tweet collection uses the Twitter API via `rtweet` and requires your own API credentials, set in the `create_token()` call in the AAPL script. The search API only reached back about five days, so the exact 2021-03-28 to 2021-04-01 tweets can no longer be collected. Twitter/X has also changed its API access since 2021. To re-run collection you would need current API access and to change the `since`/`until` dates in `search_tweets()` to a recent window (and the stock data to match).
- **You can skip the collection step.** Each script contains the mean daily sentiment scores from the original collection as hard-coded values (the `secondcolumn` vectors), so the forecasting, plots and regressions can be re-run from the price data alone.
- **R packages:** `dplyr`, `tidyr`, `ggplot2`, `httr`, `stringr`, `twitteR`, `magrittr`, `SentimentAnalysis`, `gridExtra`, `rtweet`, plus `forecast` (for `forecast()` and `accuracy()`) and `DT`.

## References

- Bollen, J., Mao, H., & Zeng, X.-J. (2010). Twitter mood predicts the stock market.
- Nisar, T. M., & Yeung, M. (2017). Twitter as a tool for forecasting stock market movements: A short-window event study.
- Pagolu, V. S., Challa, K. N., Panda, G., & Majhi, B. (2016). Sentiment Analysis of Twitter Data for Predicting Stock Market Movements.
- Xu, Y., & Cohen, S. B. (2018). Stock Movement Prediction from Tweets and Historical Prices. *Proceedings of ACL 2018*.
