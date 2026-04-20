# STAT 405 – Final Project

## Project Overview
This project applies Bayesian inference to a real-world medical prediction problem using clinical datasets. How do different Bayesian medical diagnostic models for coronary heart disease detection affect the calibration of probabilistic predictions across different inference methods when modeling medical risk factors.

Bayesian inference methods will be each used to compare the calibration of two Bayesian regression models and their reliability in predicting disease probabilities in medical angiographic diagnostics.

---

## Scientific Question

**How do different Bayesian medical diagnostic models for coronary heart disease detection affect the calibration of probabilistic predictions across different inference methods (Metropolis–Hastings Markov Chain Monte Carlo and Variational Inference)?**

---

## Team Members
1. Mohammed Fouzan Hashmi  
2. Giuliana Kim  
3. Diana Liang  

---

## Datasets

### Dataset
**Heart Failure Prediction Dataset**  
[https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction](https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction)

### Sources
**International application of a new probability algorithm for the diagnosis of coronary artery disease**  
[https://doi.org/10.1016/0002-9149(89)90524-9](https://doi.org/10.1016/0002-9149(89)90524-9)

**UC Irvine Machine Learning Repository for Heart Disease** 
[https://archive.ics.uci.edu/dataset/45/heart+disease](https://archive.ics.uci.edu/dataset/45/heart+disease)

---

## Planned Methodology
- Construct Bayesian logistic regression models for disease prediction
- Estimate posterior distributions using two inference approaches for both models each:
  - Metropolis–Hastings Markov Chain Monte Carlo (MH-MCMC)
  - Variational Inference (VI)
- Generate probabilistic predictions for patients
- Leverage cross validation and evaluate the **calibration of predicted probabilities** between models across the different method using tools such as:
  - calibration plots
  - Brier score
