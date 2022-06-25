---
title: Import or Generate Sample Data
tags: python,R,stats,AI,prog
---

# import CSV data

```python
import pandas as pd
from pathlib import Path
mycsv = Path("/home/kali/pytorch-Deep-Learning/ISLR/dataset/Advertising.csv")
outDF=pd.read_csv(mycsv)
```

# Plotting functions

```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import torch

t = torch.linspace(0, 2*np.pi, 10)
t = np.sin(t)
plt.plot(t)
```

# Plotting distributions

```python
import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as stats
import math

mu = 0
variance = 1
sigma = math.sqrt(variance)
x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
plt.plot(x, stats.norm.pdf(x, mu, sigma))
plt.show()
```

# Probability distribution