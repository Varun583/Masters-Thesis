# Masters-Thesis
The repository contains the code used to obtain the results for my Master's thesis on Extended Bradley-Terry Models.

R packages required: dplyr and ggplot2

## Introduction
The Bradley-Terry model has been popular, facilitating pairwise comparisons, not only in sports, but also in various other fields. There have been many attempts to improve the initial Bradley-Terry model, proposed by Bradley and Terry, and some of these attempts are successful. Some of the extensions include:
- Modelling Ties
- Modelling Order Effects
- Modelling External Covariates

At the moment, we would like to explore the order effects, like the homeground advantage and many levels attached to it, in sporting scenarios. We would also like to incorporate neutral venue scenarios in our coding, such that, while measuring the order effect, one can also model games where there are no order effects.

## Models
Chapter 2 of the thesis gives a very clear picture on the mathematical formulation of the Bradley-Terry models we have built during the course of this thesis. The list of the models are given below:
- Vanilla Bradley-Terry Model (The original Bradley-Terry model with no order effects)
- Common Home-ground Advantage Model
- Common Hierarchical Home-ground Advantage Model
- Team-specific Home-ground Advantage Model
- Hierarchical Home-ground Advantage Model
- Pairwise Home-ground Advantage Model
For the hypothesis testing of these nested models, the Likelihood Ratio Test statistic has been used. 

## Implementation

The R files that have been attached to this repository contain all the codes that have been used to obtain the results for all the models.

- Functions.R contains all the models that have been built for the thesis.
- Implementation.R contains the main functions that have been used to obtain the results for the thesis.

## Results

The data of the fixtures of the sporting tournaments were obtained from reliable sources like basketballreference.com and espncricinfo.com
For the NBA, the Common Hierarchical Home-ground Advantage model was the highest of the order effects models that were statistically significant.




Simulation.R traces the steps taken to simulate the NBA playoffs of the 2022-23 season. However, this can be used to simulate any NBA playoffs with a regular home-and-away format, with a few changes.

SimulationNBA.R introduces the basic steps of the simulation of the NBA playoffs of the 2022-23 season, as a different approach is taken, as mentioned in the thesis. 
