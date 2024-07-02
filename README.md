# Multi Level Model - PISA and UKHLS Analysis

## Overview

This repository contains the code, data, and report for the multilevel project. The project investigates two datasets: the Programme for International Student Assessment (PISA) and the UK Household Longitudinal Study (UKHLS), focusing on reading performance of students and physical functioning of older adults, respectively.

## Table of Contents

- [PISA Analysis](#pisa-analysis)
  - [Introduction](#introduction)
  - [Descriptive Analysis](#descriptive-analysis)
  - [Model Selection Strategy](#model-selection-strategy)
  - [Interpretation of Final Model](#interpretation-of-final-model)
  - [Limitations](#limitations)
  - [Non-Technical Summary](#non-technical-summary)
- [UKHLS Analysis](#ukhls-analysis)
  - [Introduction](#introduction-1)
  - [Descriptive Analysis](#descriptive-analysis-1)
  - [Model Selection](#model-selection)
  - [Interpretation of Final Model](#interpretation-of-final-model-1)
  - [Limitations](#limitations-1)
  - [Conclusion and Non-Technical Summary](#conclusion-and-non-technical-summary)
- [Acknowledgements](#acknowledgements)

## PISA Analysis

### Introduction

The PISA dataset investigates factors associated with reading performance for 15-year-old students in the UK. It has a two-level hierarchical structure with students nested within schools. The dataset includes 7610 students across 334 schools. The analysis aims to address three key research questions:
1. How much do schools vary in their average reading performance?
2. Which student and school characteristics predict reading scores?
3. Do the effects of student-level variables differ between schools?

### Descriptive Analysis

The response variable is the reading score. Key student-level predictors include gender, immigration status, parental education, family wealth, cultural possessions, and learning minutes. School-level predictors include student-teacher ratio, school size, and school ownership type.

### Model Selection Strategy

We used multilevel models to account for the hierarchical structure of the data. The model selection process involved:
1. Adding student-level predictors.
2. Testing for interactions among student-level predictors.
3. Adding school-level predictors.
4. Testing for random slopes and cross-level interactions.

### Interpretation of Final Model

The final model includes both student and school-level predictors, their interactions, and random slopes for gender and learning minutes. Key findings include:
- Females score higher on average than males.
- Parental education, family wealth, and cultural possessions positively affect reading scores.
- School type and size significantly impact reading performance.

### Limitations

The analysis is limited by potential biases in self-reported data, the assumptions of linearity and normality, and the binary classification of gender. The model also does not account for all possible confounding variables.

### Non-Technical Summary

The study highlights the complex interplay of student and school-level factors in determining reading performance. It underscores the importance of considering both individual characteristics and school environment in educational assessments.

## UKHLS Analysis

### Introduction

The UKHLS dataset investigates physical functioning in older adults using the SF-12 questionnaire. The dataset includes 28,468 observations from 5,602 individuals. The analysis aims to address three key research questions:
1. What is the nature of change in physical functioning with age?
2. To what extent do individuals vary in their physical functioning trajectories?
3. What are the predictors of physical functioning?

### Descriptive Analysis

The dataset has a two-level structure with repeated measures over multiple waves. Key variables include age, gender, ethnicity, educational qualifications, co-resident partner status, housing tenure, and log income.

### Model Selection

The model selection process involved:
1. Adding fixed and random effects for age.
2. Testing for nonlinearity in age effects.
3. Adding time-invariant and time-varying covariates.
4. Comparing models using likelihood ratio tests and information criteria.

### Interpretation of Final Model

The final model includes both fixed and random effects for age, gender, ethnicity, educational qualifications, housing tenure, and log income. Key findings include:
- Physical functioning declines with age, with an accelerating rate of decline.
- Females and individuals of Asian and Black ethnicity report worse physical functioning.
- Higher educational attainment and homeownership are associated with better physical functioning.

### Limitations

The analysis assumes missing data are missing at random and does not account for all potential predictors of physical functioning. The use of self-reported measures may introduce biases.

### Conclusion and Non-Technical Summary

The study reveals significant individual variation in physical functioning trajectories and highlights the impact of socioeconomic factors on health in older adults.

## Usage

To run the project, run the R script.
