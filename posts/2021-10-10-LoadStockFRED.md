---
title: Import, load, FRED, TDtrade in python, R
tags: python,R,stats,AI,prog
---

# import CSV data

```python
import pandas as pd
from pathlib import Path
mycsv = Path("/home/kali/pytorch-Deep-Learning/ISLR/dataset/Advertising.csv")
outDF=pd.read_csv(mycsv)
```

# import tdtrade

```python
import pandas as pd
apikey = "XXXXXXXXXXX"
ticker = "AAPL"
period = "day"
myurl = f"https://api.tdameritrade.com/v1/marketdata/{ticker}/pricehistory?apikey={apikey}&periodType={period}"
stkdata = pd.read_json(myurl)
#Series of json eg. [{open: ,close: ...}, {open: , close: ..}

stocks = stkdata["candles"].apply(pd.Series)
#Convert Series of json to Dataframe

stocks["datetime"] =  pd.to_datetime(stocks["datetime"],origin="unix", unit="ms")
```