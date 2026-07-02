# Ecosystem modelling of the Baltic Sea: Evaluating the application of Dynamic Bayesian Networks in evolving ecosystems
Introduction

The primary aim of this project was to evaluate the function of Dynamic Bayesian Network Models (DBNMs) in ecosystem modelling. The region of interest was the Baltic Sea.

1.	Hidden Markov Model

[Figure 1][https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/dcbedd9964b2fc58f3d83275bd216804f1419daa/Figure%201.png](https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/c4df2b1302182f9f603830b0d50adcce984d7c8f/Figure%201.jpg)

Figure 1 contains the R code written to create a Hidden Markov Model . Additional comments addressing what was written and why, has been added in the screenshot. 

[Figure 2]https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/c4df2b1302182f9f603830b0d50adcce984d7c8f/Figure%202.jpg
Figure 2 displays the graph produced by the code in Figure 1. This shows the shifts between learned ecosystem states in the Western Baltic. This was created using R package “ggplot2”. 


2.	Dynamic Bayesian Network (DBN)
[Figure 3]https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/c4df2b1302182f9f603830b0d50adcce984d7c8f/Figure%203.jpg
Figure 3 contains the R code written to create a Dynamic Bayesian Network. Additional comments addressing what was written and why, has been added in the screenshot.

[Figure 4]https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/c4df2b1302182f9f603830b0d50adcce984d7c8f/Figure%204.jpg
Figure 4: A prescribed DBN model of the Western Baltic created using the code from Figure 3. 

[Figure 5]https://github.com/AditiK-9/Baltic-sea-dbn-forecasting/blob/c4df2b1302182f9f603830b0d50adcce984d7c8f/Figure%205.jpg
Figure 5 shows the code written to predict Herring recruits in the current time slice (_t1) from the prescribed DBN model above. It also produces a line graph showing the actual vs predicted values and the evaluation metric, Root Mean Squared Error (RMSE). 
