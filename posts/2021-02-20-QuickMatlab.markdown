---
title: Quick Matlab
tags: prog, QuickCode
---

### Image label and training

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