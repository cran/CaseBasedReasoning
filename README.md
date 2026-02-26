[![CRAN status](https://www.r-pkg.org/badges/version/CaseBasedReasoning)](https://CRAN.R-project.org/package=CaseBasedReasoning)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/CaseBasedReasoning)](https://CRAN.R-project.org/package=CaseBasedReasoning)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/CaseBasedReasoning)](https://CRAN.R-project.org/package=CaseBasedReasoning)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R ≥ 3.5.0](https://img.shields.io/badge/R-%E2%89%A5%203.5.0-276DC3.svg?logo=r)](https://cran.r-project.org/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

# Case Based Reasoning using Statistical Models

The R package case-based-reasoning provides an R interface case-based reasoning using machine learning methods.

## Introduction: What is Case Based Reasoning?

Case-Based Reasoning (CBR) is an artificial intelligence (AI) and problem-solving methodology that leverages the knowledge and experience gained from previously encountered situations, known as cases, to address new and complex problems. CBR relies on the principle that similar problems often have similar solutions, and it focuses on identifying, adapting, and reusing those solutions to solve new problems.

The CBR process consists of four main steps:

-   **Retrieve**: In this step, the system searches its case database to identify the most similar cases to the current problem. It uses similarity measures and pattern matching techniques to compare the features of the new problem with the existing cases.

-   **Reuse**: Once the relevant cases are retrieved, the system adapts the solutions from those cases to fit the new problem. This may involve combining multiple solutions, adjusting parameters, or modifying the solution to accommodate any differences between the old and new cases.

-   **Revise**: After the adapted solution has been applied to the new problem, the system evaluates the outcome to determine its effectiveness. If necessary, the solution is further revised and optimized to better suit the specific context of the problem.

-   **Retain**: Finally, the new problem and its corresponding solution are added to the case database for future reference. This step enhances the system's knowledge base and improves its ability to solve similar problems in the future.

CBR has been successfully applied in various domains, including medical diagnosis, legal reasoning, customer support, and design optimization. Its ability to learn from experience and adapt to new situations makes it a valuable approach in fields where expertise and problem-solving skills are crucial.

## How This Package Implements CBR

The central challenge in CBR is defining what "similar" means. This package solves the **Retrieve** step by learning a distance function from data rather than relying on ad-hoc similarity measures.

The workflow follows three stages:

1.  **Fit a statistical model** to the training data. The model captures which variables matter and how strongly they influence the outcome.
2.  **Derive a distance function** from the fitted model. The model's structure determines how distance between cases is measured.
3.  **Retrieve the k nearest cases** for each new query observation using the learned distance.

The package offers two families of models for this:

### Regression-based distance (Cox, linear, logistic)

A regression model (Cox proportional hazards, OLS, or logistic regression via the [rms](https://CRAN.R-project.org/package=rms) package) is fitted on the training data. The estimated regression coefficients are then used as **weights** in a weighted absolute distance function. Variables with larger coefficients contribute more to the distance, while variables the model deems irrelevant contribute little. This follows the approach of Dippon et al. (2002).

### Random Forest distance (proximity and depth)

A random forest (via the [ranger](https://github.com/imbs-hl/ranger) package) is fitted on the training data. Two distance measures can be extracted:

-   **Proximity distance**: Two observations are similar if they frequently land in the same terminal node across trees.
-   **Depth distance**: The distance between two observations is the sum of edges between their terminal nodes across all trees in the forest, following Englund and Verikas.

Both approaches produce an (n x m) distance matrix, where n is the number of training observations and m the number of query observations. This matrix can be used directly for clustering or visualization, or passed to the built-in k-nearest-neighbor search to retrieve the most similar cases.

## Installation

#### CRAN

    install.packages("CaseBasedReasoning")

#### GITHUB

    install.packages("devtools")
    devtools::install_github("sipemu/case-based-reasoning")

## Features

This R package provides two methods case-based reasoning by using an endpoint:

-   Linear, logistic, and CPH Regression

-   Proximity and Depth Measure extracted from a fitted random forest ([ranger](https://github.com/imbs-hl/ranger) package)

Besides the functionality of searching for similar cases, we added some additional features:

-   Automatic validation of the critical variables between the query and similar cases dataset

-   Checking proportional hazard assumption for the Cox Model

-   C++-functions for distance calculation

## Warning Message

"Warning: Cases with missing values in the dependent variable (Y) or predictor variables (X) have been dropped from the analysis. This may lead to a reduced dataset and potential loss of information. Please review your data and consider appropriate missing value imputation techniques to mitigate these issues."


## Example: Cox Beta Model

### Initialization

In the first example, we use the CPH model and the `ovarian` data set from the `survival` package. In the first step, we initialize the R6 data object.

    library(survival)
    library(CaseBasedReasoning)
    ovarian$resid.ds <- factor(ovarian$resid.ds)
    ovarian$rx <- factor(ovarian$rx)
    ovarian$ecog.ps <- factor(ovarian$ecog.ps)

    # initialize R6 object
    cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, data = ovarian)

### Similar Cases

After the initialization, we may want to get for each case in the query data the most similar case from the learning data.

```r
n <- nrow(ovarian)
trainID <- sample(1:n, floor(0.8 * n), FALSE)
testID <- (1:n)[-trainID]
cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, data = ovarian[trainID, ])

# fit model
cph_model$fit()

# get similar cases
matched_tbl <- cph_model$get_similar_cases(query = ovarian[testID, ], k = 3)

# or use the standard S3 predict interface
matched_tbl <- predict(cph_model, newdata = ovarian[testID, ], k = 3)
```

To analyze the results, you can extract the similar cases and training data and combine them:

-   **Note 1**: During the initialization step, all cases with missing values in the data and endpoint variables were removed. Be sure to conduct a missing value analysis beforehand.

-   **Note 2**: The data.frame returned from `cph_model$get_similar_cases()` contains four columns that help identify the query cases, their matches, and the distances between them:

    -   **caseId**: This column allows you to map the similar cases to cases in the data. For example, if you chose k = 3, the first three elements in the caseId column will be 1 (followed by three 2s, and so on). These three cases are the three most similar cases to case 0 in the verum data.

    -   **scDist**: The calculated distance between the cases.

    -   **scCaseId**: Grouping number of the query case with its matched data.

    -   **group**: Grouping indicator for matched or query data.

These columns help organize and interpret the results, ensuring a clear understanding of the most similar cases and their corresponding query cases.

### Distance Matrix

The distance matrix is a square matrix that represents the pairwise distances between a set of data points. In the context of Case-Based Reasoning (CBR), the distance matrix captures the dissimilarities between cases in the training and test (or query) datasets, based on the fitted model and the values of the predictor variables.

The distance matrix can be helpful in various situations:

-   **Identifying Similar Cases**: By examining the distance matrix, you can identify the most similar cases to a given query case. Smaller distances indicate higher similarity, enabling the retrieval of relevant cases for CBR.

-   **Clustering and Grouping**: The distance matrix can be used as input for clustering algorithms, such as hierarchical clustering or k-means clustering, to group cases with similar characteristics. This can provide insights into the structure and patterns within the data.

-   **Visualizing Relationships**: By creating a heatmap of the distance matrix, you can visualize the relationships between cases. This representation can help identify trends and anomalies in the data, guiding further analysis and decision-making.

-   **Model Validation**: The distance matrix can be used to assess the performance of the CPH regression model or other statistical models employed in the CBR process. Comparing the distance matrix for different models can help determine which model better captures the relationships between cases.

In summary, a distance matrix can provide valuable insights into the relationships between cases, facilitate the identification of similar cases for CBR, and aid in the validation of the chosen statistical models.

```r
distance_matrix <- cph_model$calc_distance_matrix()
```

`cph_model$calc_distance_matrix()` calculates the distance matrix between train and test data. When test data is omitted, the distances between observations in the training data are calculated. Rows are observations in train and columns observations of test. The distance matrix is saved internally in the `CoxModel` object: `cph_model$dist_matrix`.


## Contribution

### Responsible for Mathematical Model Development and Programming

-   [PD Dr. Jürgen Dippon](https://www.isa.uni-stuttgart.de/institut/team/Dippon/), Institut für Stochastik und Anwendungen, Universität Stuttgart

-   [Dr. Simon Müller](https://data-zoo.de/), DataZoo GmbH

### Medical Advisor

-   Dr. Peter Fritz

-   Professor Dr. Friedel

### Funding

The Robert Bosch Foundation funded this work. Special thanks go to Professor Dr. Friedel (Thoraxchirugie - Klinik Schillerhöhe).

## References

### Main

-   Dippon et al. [A statistical approach to case based reasoning, with application to breast cancer data](https://doi.org/10.1016/S0167-9473(02)00058-0) (2002),

-   Friedel et al. [Postoperative Survival of Lung Cancer Patients: Are There Predictors beyond TNM?](https://ar.iiarjournals.org/content/33/4/1609.short) (2012).

### Other {#other}

-   Englund and Verikas [A novel approach to estimate proximity in a random forest: An exploratory study](https://doi.org/10.1016/j.eswa.2012.01.157)

-   Stuart, E. et al. [Matching methods for causal inference: Designing observational studies](https://doi.org/10.4135/9781412995627.d14)

-   Defossez et al. [Temporal representation of care trajectories of cancer patients using data from a regional information system: an application in breast cancer](https://doi.org/10.1186/1472-6947-14-24)
