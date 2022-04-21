---
title: Quick Pytorch
tags: AI, mathcs, prog, python
toc: y
---

https://towardsdatascience.com/pytorch-how-and-when-to-use-module-sequential-modulelist-and-moduledict-7a54597b5f17

ML Simplified:
$$Data Input \overset{linear}{\Rightarrow} s \overset{non-linear}{\Rightarrow} z$$

* Hidden layer represents activation function
* Edges between layers represent weights
* A 0-hidden layer is simply linear regression
  * Input layer ---weight--- Output layer
  * x ---multiplyWeight--- y


```text
  Input layer --weight-- Hidden layer(Activation) --weight-- Output layer

```

# Overview

1. Download `x::datasets`{.python}
2. Load data `DataLoader(x::datasets)`{.python} 
3. Define forward model `class myNNforward(nn.Module) 
4. Create the layers in forward model`nn.Sequential(nn.Linear(22,30),nn.ReLU())`{.python}
5. Define loss function and optimisation function
6. Create backprop train function using loss and optimisation function

# imports

```python
import torch
from torch import nn
from torch.utils.data import DataLoader
from torchvision import datasets
from torchvision.transforms import ToTensor, Lambda, Compose
import matplotlib.pyplot as plt
```
# Basics

# Example

```py
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
Boston = pd.read_csv("../dataset/Boston.csv")
x = Boston['lstat']
y = Boston['medv']

Convert_NP = lambda g: g.to_numpy().reshape(-1,1).astype('float32') #convert to one column ndarray
X = Convert_NP(x) #shape (560,1)
Y = Convert_NP(y) #shape (560,1)

X_NoBias = X #USED FOR PLOTTING PURPOSES


bias = np.ones((X.shape[0],1)).astype('float32') #column of 1's represent intercept coefficient
X_bias = np.append(X,bias,axis=1) #shape (560,2)
X = X_bias
print(f"X-dim: {X.shape}\nY-dim: {Y.shape}")
```

```py
Xtensor = torch.from_numpy(X)
Ytensor = torch.from_numpy(Y)
# print(X[:,[1]])
Xlayer = torch.from_numpy(X[:,[0]])
hidlayer = torch.from_numpy(X[:,[1]])
Ytensor = torch.from_numpy(Y)
```

```py
import random
import torch
from torch import nn, optim
import math
from IPython import display

device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
seed = 1
random.seed(seed)
torch.manual_seed(seed)

inputDim = 1
outputDim = 1
hiddenDim = 100

learning_rate = 1e-4
lambda_l2 = 1e-6

model = nn.Sequential(
    nn.Linear(inputDim, hiddenDim),
    nn.Linear(hiddenDim, outputDim)
)
model.to(device) # Convert to CUDA

# nn package also has different loss functions.
# we use MSE loss for our regression task
criterion = torch.nn.MSELoss()

# we use the optim package to apply
# stochastic gradient descent for our parameter updates
optimizer = torch.optim.SGD(model.parameters(), lr=learning_rate, weight_decay=lambda_l2) # built-in L2

# Training
for t in range(1000):
    
    # Feed forward to get the logits
    y_pred = model(Xlayer)
    
    # Compute the loss (MSE)
    loss = criterion(y_pred, Ytensor)
    print("[EPOCH]: %i, [LOSS or MSE]: %.6f" % (t, loss.item()))
    display.clear_output(wait=True)
    
    # zero the gradients before running
    # the backward pass.
    optimizer.zero_grad()
    
    # Backward pass to compute the gradient
    # of loss w.r.t our learnable params. 
    loss.backward()
    
    # Update params
    optimizer.step()
```

```py
plt.scatter(Xlayer.data.cpu().numpy(), Ytensor.data.cpu().numpy())
plt.plot(Xlayer.data.cpu().numpy(), y_pred.data.cpu().numpy(), 'r-', lw=5)
plt.axis('equal');
```


# Other
## download datasets

```python
# Download training data from open datasets.
training_data = datasets.FashionMNIST(
    root="data",
    train=True,
    download=True,
    transform=ToTensor(),
)

# Download test data from open datasets.
test_data = datasets.FashionMNIST(
    root="data",
    train=False,
    download=True,
    transform=ToTensor(),
)
```
## load data

```python
# Download training data from open datasets.
training_data = datasets.FashionMNIST(
    root="data",
    train=True,
    download=True,
    transform=ToTensor(),
)

# Download test data from open datasets.
test_data = datasets.FashionMNIST(
    root="data",
    train=False,
    download=True,
    transform=ToTensor(),
)
```

## forward model class

* The __init__ is where we define our weights and activation functions
  * The forward() is where we actually use these weights and activation functions.

```python
# Get cpu or gpu device for training.
device = "cuda" if torch.cuda.is_available() else "cpu"
print(f"Using {device} device")

# Define model
class NeuralNetwork(nn.Module):
    def __init__(self):
        super(NeuralNetwork, self).__init__()
        self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(28*28, 512),
            nn.ReLU(),
            nn.Linear(512, 512),
            nn.ReLU(),
            nn.Linear(512, 10)
        )

    def forward(self, x):
        x = self.flatten(x)
        logits = self.linear_relu_stack(x)
        return logits

model = NeuralNetwork().to(device)
print(model)
```
## loss and optimisation function

```python
loss_fn = nn.CrossEntropyLoss()
optimizer = torch.optim.SGD(model.parameters(), lr=1e-3)
```

## backprop training

```python
def train(dataloader, model, loss_fn, optimizer):
    size = len(dataloader.dataset)
    model.train()
    for batch, (X, y) in enumerate(dataloader):
        X, y = X.to(device), y.to(device)

        # Compute prediction error
        pred = model(X)
        loss = loss_fn(pred, y)

        # Backpropagation
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        if batch % 100 == 0:
            loss, current = loss.item(), batch * len(X)
            print(f"loss: {loss:>7f}  [{current:>5d}/{size:>5d}]")
```

# Displaying image

Either  
* display image from `datasets`{.python}
* display image through `iter(Dataloader(...))`{.python}