# Welcome to tRigon <img src='https://github.com/dlhoelscher/aRrow/blob/main/www/tRigon_logo.png' align="right" height="150"/>

## 1. Introduction

[![CRAN status](http://www.r-pkg.org/badges/version/tRigon)](https://cran.r-project.org/package=tRigon) 
[![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/tRigon)](https://cran.r-project.org/package=tRigon)


`tRigon - toolbox for integrative pathomics analysis` is a Shiny-based application for automated, simple and reproducible analysis of (path-) omics datasets. 

## 2. Installation

```
install.packages('tRigon')
```
## 3. Usage

1. **Run tRigon locally**

This option is particularly **recommended** when you have large datasets with many rows and columns.

By running the following script, the web application will open and you can then enjoy using tRigon.

```
tRigon::run_tRigon()
```

2. **Use the website**

Alternatively, you can access tRigon online via [https://labooratory.shinyapps.io/tRigon/](https://labooratory.shinyapps.io/tRigon/). 

> **Note** 
> tRigon is deployed for free at shinyapps.io. It only allows to use 1024 MB of memory. Therefore, large files can not be uploaded and/or analysed successfully. In this case, please run tRigon locally.

3. **Demo Data**

FLASH-generated pathomics demo files for five human and mouse datasets as well as three non-pathomics medical datasets have been provided. Please use this [link to access](https://git-ce.rwth-aachen.de/labooratory-ai/flash/-/blob/main/NGM_DataRepository.zip) the demo data.

## 4. Main Functions

**Table 1.** Overview of established tRigon functions

| Section      | Function                                                     | Explanation                                                                                                   |
|--------------|--------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------| 
| **Data** |                                                              |                                                                                                               |
|              | Processing data                                             | tRigon can process .csv files of pathomics data together with provided experiment / clinical data meta files. tRigon aggregates pathomics files and assigns them to the provided labels from the metadata. Users can choose between processing human or animal experiment data with calculations on specimen or single-structure level. |                                                                            
|              | Loading data                                                 | tRigon can also be used to load other data (e.g., other omics datasets) or already processed pathomics files. |                                                                  
| **Statistics** |                                                            |                                                                                                               |                                                                                                                                                  
|              | Descriptive statistics                                       | Based on a provided group label tRigon can calculate summary statistics (e.g., mean, median, standard deviation, interquartile range) for each chosen feature. |                                                                                                                                                                                                                                                                                       
|              | Statistical tests                                             | tRigon supports a range of non-parametric statistical tests such as:<br />(1) pairwise Wilcoxon-rank tests with Bonferroni correction for multiple testing<br />(2) Kruskal-Wallis tests<br />(3) differences in median with bootstrapped confidence intervals for each desired feature and provided group label. |                                                                                                 
|              | Correlation                                                    | Simple and multiple correlations based on the Pearson-correlation coefficient can be calculated and visualized as a scatter plot or correlation matrix for each chosen feature. Users can also specify a group and subgroup for specific correlation analysis. |
|              | Machine learning                                              | For calculation of feature importance tRigon supports random forests and recursive feature elimination (RFE) for classification and regression of chosen features based on a selected dependent variable. For RFE users can also specify the number of folds for cross-validation as well as repeats. |
|   **Plots** |                                                             |                                                                                                          |                                                                                                                                         
|                | Distribution plots                                          | Based on a provided group label tRigon plots selected feature distributions in a variety of plots:<br />(1) violin plots<br />(2) box plots<br />(3) ridgeline plots |      
|                | Clustering                                                  | tRigon supports k-means clustering for selected variables. Groups can also be plotted within a separate legend. | 
| **Data** |                                                              |                                                                                                               |
|                 | Markdown reports                                                  | For each function tRigon users can download a markdown report in .html format including all relevant inputs and outputs of the application. |                                                                      

## 5. How to cite

If you find tRigon useful, please consider citing the publication:

- tRigon: an R package and Shiny App for integrative pathomics analysis [David L. Hölscher, Michael Goedertier, Barbara M. Klinkhammer, Patrick Droste, Ivan G. Costa, Peter Boor, Roman D. Bülow]

## 6. Further information

Further information on how to use tRigon for analysis and data processing are provided in the **Help** section of the application.

Information regarding **FLASH (framework for large-scale histomorphometry)** can be found [here](https://www.nature.com/articles/s41467-023-36173-0)
