---
title: Quick Matlab
tags: prog, QuickCode
---

# Basics

## Variables

Semicolon suppress output

```Matlab
a = 5;
b = 9;

% Saving variables to file
save datafile.mat %save all variable to file
save datafile.mat a  %save variable a to file

clear %clear variables

% Loading variable from file
load datafile.mat %load all variable from file
load datafile.mat a %load variable a from file
```

## Common math functions

```Matlab
%constants and functions
pi_ = pi
sin_ = sin(90)
eig_ = eig(ones(5))
abs_ = abs(-5)
imaginaryNum = 1i^2
```

## Matrices and Vectors

```Matlab
%row vector
x1 = [3, 5, 7]
x2 = [3 5 7] %we can use comma or space

%col vector
y = [3;5;7] 
z = [1 2 3;4 5 6] %2 by 3 matrix

%select element in matrix
z(2,3) % element in position 2,3
z(:,3) % allrow,third column
```

### Transpose
```Matlab
%Transpose using single  quote '
xT = [3 5 7]' 
```

## Matrix Operations

```Matlab
test1 = [3 5 7 12 10] %we can use comma or space

test1(1) %output 3, first element
test1(1) = 9 %change first element

test1(1:2) = [4 2] %change first 2 elements
test1([1 3]) %output 1st and 3rd element 

test1(end) %last element
test1(end-1) %next to last element 

test1 = [3 5 7 12 10] 
%logical indexing
test1 > 3 %outputs a mask
test1(test1 > 3)
test1(test1 == 3)
test1(test1 ~= 3)
test1(test1 > 3 & test1 < 7)
test1(test1 > 3 | test1 < 7)
test1(test1 ~= 3) = 99 %replace elem neq 3 with 99

```

## Plotting

```Matlab
xaxis = 1:12
yaxis = rand(1,12)
plot(xaxis,yaxis)
hold on %plots both graph on top instead of separate plots
plot(xaxis,yaxis+0.1,"r--o") %r for red, -- with o as marker
hold off
plot(yaxis) %x-axis auto ranges from 1 to size of yaxis
```



# Image label and training

Matlab
use Simulink image labeler
export will output: gTruth

```Matlab
trainingDataTable = objectDetectorTrainingData(gTruth)
acfDetector = trainACFObjectDetector(trainingDataTable,'NegativeSamplesFactor',2);
I = imread("C:\Users\User\Downloads\archive\shapes\squares\drawing(28).png");
bboxes = detect(acfDetector,I);
annotation = acfDetector.ModelName;
I = insertObjectAnnotation(I,'rectangle',bboxes,annotation);
figure
imshow(I)
```