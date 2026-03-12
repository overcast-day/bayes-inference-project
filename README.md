# STAT 405 – Final Project

## Project Overview
This project applies Bayesian inference to a real-world medical prediction problem using clinical datasets. The goal is to analyze how different Bayesian inference algorithms influence the **calibration of probabilistic predictions** when modeling medical risk factors.

Bayesian methods will be used to compare how different inference approaches affect the reliability of predicted disease probabilities in medical diagnostic prediction models.

---

## Scientific Question (Bayesian Perspective)

**How do different Bayesian inference methods (Metropolis–Hastings Markov Chain Monte Carlo and Variational Inference) influence the calibration of probabilistic predictions in medical diagnostic models?**

---

## Team Members
1. Mohammed Fouzan Hashmi  
2. Giuliana Kim  
3. Diana Liang  

---

## Datasets

### Primary Dataset
**Heart Failure Prediction Dataset**  
https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction

### Backup Dataset
**Hepatitis C Dataset**  
https://www.kaggle.com/datasets/fedesoriano/hepatitis-c-dataset

---

## Planned Methodology
- Construct Bayesian logistic regression models for disease prediction
- Estimate posterior distributions using two inference approaches:
  - Metropolis–Hastings Markov Chain Monte Carlo (MH-MCMC)
  - Variational Inference (VI)
- Generate probabilistic predictions for patients
- Evaluate the **calibration of predicted probabilities** using tools such as:
  - calibration plots
  - reliability diagrams
  - Brier score