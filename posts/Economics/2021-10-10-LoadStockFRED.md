---
title: Import, load, FRED, TDtrade in python, R
tags: python, R, musings, misc
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

```js
const bleh = async () => {
    const rawdata = await fetch("https://api.td...").then(response => response.text())
    const inter = window.atob(rawdata)
    const jsondata = JSON.parse(inter)["candles"]
    const tfdataframe = tf.data.array(jsondata)

}
```

# R

```R
library("fpp3")
library("fredr")
FREDAPI="XXXXXXXXXXXXXXXX"
fredr_set_key(FREDAPI)

retail <- fredr(
  series_id = "CEU4200000001",
  observation_start = as.Date("2010-01-01"),
  observation_end = as.Date("2021-12-01")
) %>%
    as_tsibble(index=date)

```

```R
retail %>% 
    head
```

```R
retail %>%
    mutate(natorder = row_number())%>% 
    update_tsibble(index = natorder, regular = TRUE) %>%
    ACF(value) %>%
    autoplot()
    
```

```R
retail %>%
    mutate(natorder = row_number())%>% 
    update_tsibble(index = natorder, regular = TRUE) %>%
    gg_lag(value, geom = "point")
```