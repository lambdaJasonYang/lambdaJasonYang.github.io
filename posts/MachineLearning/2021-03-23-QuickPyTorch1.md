---
title: Quick Pytorch template 1
tags: AI, mathcs, prog, python
toc: y
---

# Summary

Build a neural network that binary classifies YES(1) or NO(0) whether we can create a target sum given a list of coins with certain values.  
eg.  
coins = {1,2,5} target=14   
OUTPUT: 1  
since 5+5+2+2=14


* We will Fix the target sum as 250 and the length of bag as 3 meaning we can only have coins of 3 different values.  
* We will Fix the bounded range of possible values for the coins as a number between 1 and 240.

# Imports

```python
## Standard libraries
import os
import math
import numpy as np
import time

## Imports for plotting
import matplotlib.pyplot as plt
%matplotlib inline

%config InlineBackend.figure_formats = ['pdf','svg']
from matplotlib.colors import to_rgba
import seaborn as sns
sns.set()

## Progress bar
from tqdm.notebook import tqdm

import torch
print("Using torch", torch.__version__)
device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")

```

# Coin Change problem

```python
def _get_change_making_matrix(set_of_coins, r: int):
    m = [[0 for _ in range(r + 1)] for _ in range(len(set_of_coins) + 1)]
    for i in range(1, r + 1):
        m[0][i] = float('inf')  # By default there is no way of making change
    return m

def change_making(coins, n: int):
    """This function assumes that all coins are available infinitely.
    n is the number to obtain with the fewest coins.
    coins is a list or tuple with the available denominations.
    """
    m = _get_change_making_matrix(coins, n)
    for c, coin in enumerate(coins, 1):
        for r in range(1, n + 1):
            # Just use the coin
            if coin == r:
                m[c][r] = 1
            # coin cannot be included.
            # Use the previous solution for making r,
            # excluding coin
            elif coin > r:
                m[c][r] = m[c - 1][r]
            # coin can be used.
            # Decide which one of the following solutions is the best:
            # 1. Using the previous solution for making r (without using coin).
            # 2. Using the previous solution for making r - coin (without
            #      using coin) plus this 1 extra coin.
            else:
                m[c][r] = min(m[c - 1][r], 1 + m[c][r - coin])
    return m[-1][-1]

```

```python
change_making([1,2,5],14) 
```
```{.txt filename=output}
4
```

## Torch variant coin change

```python
#coinChange that takes a coin::tensorArray and returns a tensor
def torchChangeMake(coin,n):
    coin.tolist()
    possible = 1 if change_making(coin.tolist(),n) != float('inf') else 0
    return torch.Tensor([possible])

torchChangeMake(torch.randint(1,7,(3,)),7)
```

```python
torch.randint(1,7,(3,))
```
```{.txt filename=output}
tensor([2, 5, 2])
```  

```python
torchChangeMake(torch.randint(1,7,(3,)),7)
```
```{.txt filename=output}
tensor([1.])
```

# Formatting Dataset for Dataloader

* Goal is to get a sense of how to build our dataset
* We wont be using the code here directly but we will use these concepts to build the dataloader

## Build our dataset INPUTS

* In our coin change problem, these are the bags of coins.
* `self.data` which we will implement in the future will follow this pattern

```python
torch.randint(low=1,high=5,size=(10,3))
```

```{.txt filename=output}
tensor([[2, 1, 3],
        [2, 1, 3],
        [1, 4, 1],
        [4, 1, 4],
        [3, 2, 2],
        [1, 4, 2],
        [3, 1, 3],
        [4, 1, 4],
        [2, 4, 2],
        [3, 2, 3]])
```

## Build out dataset OUTPUTS

* In our coin change problem, either 1 or 0 represent whether it is possible to make 40 from each of the bags from our previous section.
* `self.label` which we will implement in the future will follow this pattern

```python
acc = torch.empty((0,))
for i in torch.randint(low=1,high=30,size=(10,3)):
    acc = torch.cat((acc,torchChangeMake(i,40)),0)
    print(torchChangeMake(i,30))
```

```{.txt filename=output}
tensor([1.])
tensor([1.])
tensor([1.])
tensor([1.])
tensor([1.])
tensor([1.])
tensor([0.])
tensor([0.])
tensor([1.])
tensor([1.])
```

`acc` is the data output which merged the tensors in the loop above into 1 tensor array

```python
print(acc)
```

```{.txt filename=output}
tensor([1., 1., 1., 1., 1., 1., 0., 0., 1., 1.])
```

# Creating your Dataset class

* Create a class that extends `data.Dataset` to make it compatible with torch's dataloader.  
Must fill the 2 class parameters below
  * `self.data` 
    * In our coin change problem this is a list of lists AKA list of bags of coins
  * `self.label`
    * In our coin change problem this is a list of YES(1) or NO(0)

```python
import torch.utils.data as data
```

```{.python group=1 glabel="coinchange"}
class MainDataset(data.Dataset):
    def __init__(self,size):
        super().__init__()
        self.size = size #size is observation AKA Number of Inputs AKA rows of a database
        self.generateData()
    def generateData(self):
        self.buildinput()
        self.buildlabel() #label AKA the output AKA expected result
    def buildinput(self):
        FeaturesCount = 3
        data = torch.randint(low=1,high=240,size=(self.size,FeaturesCount))
        self.data = data
    def buildlabel(self):
        acc = torch.empty((0,))
        for i in self.data:
            minCoinsLabel = torchChangeMake(i,250)
            acc = torch.cat((acc,minCoinsLabel),0) #0 represents appended on 0-dim which is a typical array
        self.label = acc
    def __len__(self):
        # Number of data point we have. Alternatively self.data.shape[0], or self.label.shape[0]
        return self.size
    def __getitem__(self, idx):
        # Return the idx-th data point of the dataset
        # If we have multiple things to return (data point and label), we can return them as tuple
        data_point = self.data[idx]
        data_label = self.label[idx]
        return data_point, data_label
```

```{.python group=1 glabel="template"}
# ONLY THE TEMPLATE, DO NOT COPY AND USE THIS
class templateDataset(data.Dataset):
    def__init__(self,size):
        super().__init__()
        self.size = size
        self.generateData()
    def generateData():
        self.data = #... 
        self.label = #...
        pass
    def __len__(self):
        # Number of data point we have. Alternatively self.data.shape[0], or self.label.shape[0]
        return self.size
    def __getitem__(self, idx):
        # Return the idx-th data point of the dataset
        # If we have multiple things to return (data point and label), we can return them as tuple
        data_point = self.data[idx]
        data_label = self.label[idx]
        return data_point, data_label
```

```python
dataset = MainDataset(size=200)
print("Size of dataset:", len(dataset))
print("Data point 0:", dataset[0])
```

```{.txt filename=output}
print("Data point 0:", dataset[0])
Size of dataset: 200
Data point 0: (tensor([218,   6,  90]), tensor(0.))
```

# torch's Dataloader

* Since we built our dataset class based on torch's spec, loading it into torch's dataloader is trivial.

```python
data_loader = data.DataLoader(dataset, batch_size=8, shuffle=True)
```

```python
# next(iter(...)) catches the first batch of the data loader
# If shuffle is True, this will return a different batch every time we run this cell
# For iterating over the whole dataset, we can simple use "for batch in data_loader: ..."
data_inputs, data_labels = next(iter(data_loader))

# The shape of the outputs are [batch_size, d_1,...,d_N] where d_1,...,d_N are the
# dimensions of the data point returned from the dataset class
print("Data inputs", data_inputs.shape, "\n", data_inputs)
print("Data labels", data_labels.shape, "\n", data_labels)
```
```{.txt filename=output}
Data inputs torch.Size([8, 3]) 
 tensor([[177, 222,  63],
        [239, 187,  20],
        [224,  15,  76],
        [110, 177, 134],
        [ 67,  20, 173],
        [ 80,  58, 116],
        [ 35,  25, 212],
        [165,  74, 239]])
Data labels torch.Size([8]) 
 tensor([0., 0., 0., 0., 0., 0., 1., 0.])
```

# Building the Neuron

```python
import torch.nn as nn
import torch.nn.functional as F
```

* Simple Neuron
  * `__init__` is where we define the 
    * **FEATURE** dimensions `num_inputs` 
    * **OUTPUT** dimensions `num_outputs`
    * hidden or bias dimensions `num_hidden` 
    * layer transition functions (**linear** + **nonlinear activation**) 
  * `forward` is where we apply the functions 

Example: 
Coin change problem since we use a fixed bag of 3 coins, our `num_inputs=3`  
Our output is either yes or no which is 1 dimensions so `num_outputs=1`  
`num_hidden` can be arbitrarily reasonable, we pick 1  

```python
class ExampleNeuron(nn.Module):
    def __init__(self, num_inputs, num_hidden, num_outputs):
        super().__init__()
        self.linear1 = nn.Linear(num_inputs, num_hidden)
        self.act_fn = nn.Tanh()
        self.linear2 = nn.Linear(num_hidden, num_outputs)
    def forward(self,x):
        x = self.linear1(x)
        x = self.act_fn(x)
        x = self.linear2(x)
        return x
```

```python
model = ExampleNeuron(num_inputs=3, num_hidden=1, num_outputs=1)
```

# Loss function

```python
loss_module = nn.BCEWithLogitsLoss()
# loss_module = nn.L1Loss()
```

# Gradient Descent 

```python
optimizer = torch.optim.SGD(model.parameters(), lr=0.1)
```

# Training

```python
train_dataset = MainDataset(size=200)
train_data_loader = data.DataLoader(train_dataset, batch_size=12, shuffle=True)
```

```python
def train_model(model, optimizer, data_loader, loss_module, num_epochs=100):
    # Set model to train mode
    model.train()

    # Training loop
    for epoch in range(num_epochs):
        for data_inputs, data_labels in data_loader:

            ## Step 1: Move input data to device (only strictly necessary if we use GPU)
            data_inputs = data_inputs.to(device)
            data_labels = data_labels.to(device)

            ## Step 2: Run the model on the input data
            preds = model(data_inputs.float())
            preds = preds.squeeze(dim=1) # Output is [Batch size, 1], but we want [Batch size]

            ## Step 3: Calculate the loss
            loss = loss_module(preds, data_labels)

            ## Step 4: Perform backpropagation
            # Before calculating the gradients, we need to ensure that they are all zero.
            # The gradients would not be overwritten, but actually added to the existing ones.
            optimizer.zero_grad()
            # Perform backpropagation
            loss.backward()

            ## Step 5: Update the parameters
            optimizer.step()
```

```python
train_model(model.cuda(), optimizer, train_data_loader, loss_module)
```

# Evaluation

```python
test_dataset = MainDataset(size=500)
# drop_last -> Don't drop the last batch although it is smaller than 128
test_data_loader = data.DataLoader(test_dataset, batch_size=128, shuffle=False, drop_last=False)
```

```python
def eval_model(model, data_loader):
    model.eval() # Set model to eval mode
    true_preds, num_preds = 0., 0.

    with torch.no_grad(): # Deactivate gradients for the following code
        for data_inputs, data_labels in data_loader:

            # Determine prediction of model on dev set
            data_inputs, data_labels = data_inputs.to(device), data_labels.to(device)
            preds = model(data_inputs.float())
            preds = preds.squeeze(dim=1)
            preds = torch.sigmoid(preds) # Sigmoid to map predictions between 0 and 1
            pred_labels = (preds >= 0.5).long() # Binarize predictions to 0 and 1

            # Keep records of predictions for the accuracy metric (true_preds=TP+TN, num_preds=TP+TN+FP+FN)
            true_preds += (pred_labels == data_labels).sum()
            num_preds += data_labels.shape[0]

    acc = true_preds / num_preds
    print(f"Accuracy of the model: {100.0*acc:4.2f}%")
```


```python
eval_model(model, test_data_loader)
```

```{.txt filename=output}
Accuracy of the model: 79.20%
```

