COINT LONG & SJORT


selectPairs() -> pairs

train.start.date <- XX
train.end.date   <-- validation.start.date <- XX
validation.end.date <- XX

getPairsDataset(pairs, train.start.date, validation.end.date) -> prices

prices.train <- selectPrices(prices, train.start.date, train.end.date)
prices.test  <- selectPrices(prices, validations.start.date, validation.end.date)

checkCoint(pair.dataset) -> cointAnalysis

cointAnalysis:
  
  ticker.a
  ticker.b
  coint.flag
  period.size
  sd
  coef.ang
  coef.tem
  coef.lin
  df.value
  df.level
  

  
  
  

