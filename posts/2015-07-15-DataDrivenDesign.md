---
title: Data Driven Design Notes
tags: mathcs, appliedmath, AI, stats, notes
toc: y
---

# Loading big data


## Pandas 

* `series.astype("Sparse[str]")` Sparse series for tables with lots of missing data

```py
tot = 0
for i in pd.read_csv("test.csv",chunksize=100):
  tot += i["price"].sum()
```

Server is sqlalchemy, Client is pandas.
We want to get Server side cursors (streaming) to lazy fetch when the client pandas needs it.

```py
import pandas as pd
from sqlalchemy import create_engine

def process_sql_using_pandas():
    engine = create_engine(
        "postgresql://postgres:pass@localhost/example"
    )
    conn = engine.connect().execution_options(
        stream_results=True)

    for chunk_dataframe in pd.read_sql(
            "SELECT * FROM users", conn, chunksize=1000):
        print(f"Got dataframe w/{len(chunk_dataframe)} rows")
        # ... do something with dataframe ...

if __name__ == '__main__':
    process_sql_using_pandas()
```




# Chapter 2

## SQL relational drawback

* Requires a translational layer between your application code and database data
  * ORMs make this easier to translate DB data to object oriented data


## noSQL JSON Trees

## SQL vs NoSQL

* SQL better support for joins, many-one relation, many-many relation
* NoSQL schema flexibility, better performance due to locality, tree-like structure like JSON
