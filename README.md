# Welcome to tRigon <img src='https://github.com/dlhoelscher/aRrow/blob/main/www/tRigon_logo.png' align="right" height="150"/>

## 1. Introduction

[![CRAN status](http://www.r-pkg.org/badges/version/RawHummus)](https://cran.r-project.org/package=RawHummus) 
[![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/RawHummus)](https://cran.r-project.org/package=RawHummus)


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

Alternatively, you can access tRigon online via [https://bcdd.shinyapps.io/tRigon/](https://bcdd.shinyapps.io/tRigon/). 

> **Note** 
> tRigon is deployed for free at https://www.shinyapps.io. It only allows to use 1024 MB of memory. Therefore, large files can not be uploaded and/or analysed successfully. In this case, please run tRigon locally.

3. **Demo Data**

FLASH-generated pathomics demo files have been provided. Please use this [link to access](https://git-ce.rwth-aachen.de/labooratory-ai/flash/-/blob/main/NGM_DataRepository.zip) the demo data.

## 4. Main Functions

**Table 1.** Overview of established tRigon functions

| Section      | Function                                                     | Explanation                                                                                                   |
|--------------|--------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------| 
| **Data** |                                                              |                                                                                                               |
|              | Processing data                                             | tRigon can process FLASH-generated .csv files of pathomics data together with provided experiment / clinical meta data files. tRigon aggregates pathomics files and assigns them to the provided labels from the metadata |                                                                            
|              | Loading data                                                 | tRigon can be also used to load other data (e.g., other omics datasets) or already processed pathomics files    |                                                                  
| **Statistics** |                                                            |                                                                                                               |                                                                                                                                                  
|              | Descriptive statistics                                       | Based on a provided group label tRigon can calculate summary statistics (mean, median, standard deviation, interquartile range, etc.) for each feature |                                                                                                                                                                                                                                                                                       
|              | Statistical tests                                             | tRigon supports a range of non-parametric statistical tests such as:<br />(1) pairwise Wilcoxon-rank tests with Bonferroni correction for multiple testing<br />(2) Kruskal-Wallis tests<br />(3) differences in median with bootstrapped confidence intervalls |                                                                                                 
|              | Correlation                                                    | Simple and multiple correlations based on the Pearson-correlation coefficient can be calculated and visualised as a scatter plot or correlation matrix |
|              | Machine learning                                              | For calculation of feature importance tRigon supports random forests and recursive feature elimination for classification and regression of a selected dependent variable |
|   **Plots** |                                                             |                                                                                                          |                                                                                                                                         
|                | Distribution plots                                          | Based on a provided group label tRigon plots selected feature distributions in a variety of plots:<br />(1) violin plots<br />(2) box plots<br />(3) ridgeline plots |      
|                | Clustering                                                  | tRigon also supports k-means Clustering for selected variables |                                                                                                                                                                                                                
## 5. How to cite

If you find tRigon usful, please consider citing the publication:

- [Hölscher, D.L., Goedertier, M., Klinikhammer, B.M., Boor, P., Bülow, R.D.. 2023. tRigon: an R package and Shiny app for integrative pathomics analysis.]

## 6. Further information

Further information on how to use tRigon for analysis and data processing are provided in the **Help** section of the application.

Information regarding **FLASH (framework for large-scale histomorphometry)** can be found [here](https://www.nature.com/articles/s41467-023-36173-0)
