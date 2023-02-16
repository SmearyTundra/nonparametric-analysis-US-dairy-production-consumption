<!-- omit from toc -->
# Nonparametric Analysis of US Dairy Production and Consumption

This project was developed for the course of "Nonparametric Statistics" held at Politecnico di Milano in the academic year 2022-2023.

This report presents a nonparametric analysis of the evolution of dairy production and consumption in the United States from 1980 to 2021. It starts from the needs of a stakeholders which is about to enter the market and step-by-step presents our analysis and the findings to provide meaningful information to optimize production and pricing. We also provide an outlier analysis in terms of years and, in the last part, a spatial analysis and a clustering of the types of cheese.

<!-- omit from toc -->
# Table of contents

- [Installation](#installation)
  - [How to clone the repository](#how-to-clone-the-repository)
  - [How to install the packages](#how-to-install-the-packages)
- [Analysis and results](#analysis-and-results)
- [Authors](#authors)

# Installation

## How to clone the repository

```
git clone https://github.com/SmearyTundra/nonparametric-analysis-US-dairy-production-consumption
```

## How to install the packages

Install the required packages from CRAN

```
packages_list <-
    c(
        "tidyverse",
        "ggplot2",
        "mgcv",
        "rgl",
        "splines",
        "conformalInference",
        "pbapply",
        "parallel",
        "DepthProc",
        "progress",
        "dbscan",
        "beadplexr",
        "robustbase",
        "readxl",
        "tidyr",
        "car",
        "sp",
        "visreg",
        "mgcViz",
        "usmap",
        "raster",
        "sf",
        "maps",
        "ggspatial",
        "BNPTSclust",
        "roahd",
        "fda.usc",
        "npsp"
    )
install.packages(packages_list)
```

# Analysis and results

The repository contains different files to perform the analysis, here we report their explanation together with a hyperlink to the knitted PDF version:

- [`01-Conformal-Prediction.pdf`](./01-Conformal-Prediction.pdf) contains the implementation of the prediction intervals using a conformal approach.
- [`02-Permutation-Tests-for-GAM.pdf`](./02-Permutation-Tests-for-GAM.pdf) contains the permutation tests performed to reduce GAM and keep significant covariates only.
- [`03-GAM.pdf`](./03-GAM.pdf) provides the implementation for the GAM model and the reverse percentile bootstrap confindence intervals.
- [`04-Robustness.pdf`](./04-Robustness.pdf) contains the robust regression used to detect years outliers.
- [`05-Spatial-GAM.pdf`](./05-Spatial-GAM.pdf) empowers a GAM using spatial coordinates, while [`05-Spatial-Nonparametric.pdf`](./05-Spatial-Nonparametric.pdf) implements a nonparametric kriging.
- [`06-Functional-Depth.pdf`](./06-Functional-Depth.pdf) contains the Bayesian nonparametric clustering as well as an exploratory analysis of such clusters using depth measures.

The final presentations can be found here:

- [01 Midterm](./presentations/01%20Nonparametric%20Statistics%20Midterm%20Slides%20(Bucci%2C%20Cipriani%2C%20Corbo%2C%20Puricelli).pdf)
- [02 Endterm](./presentations/02%20Nonparametric%20Statistics%20Endterm%20Slides%20(Bucci%2C%20Cipriani%2C%20Corbo%2C%20Puricelli).pdf)
- [03 Final](./presentations/) TODO inserire

The final report can be found here:

- [`FILENAME.pdf`](./report/) TODO inserire

# Authors

- Teo Bucci ([@teobucci](https://www.github.com/teobucci))
- Filippo Cipriani ([@SmearyTundra](https://www.github.com/SmearyTundra))
- Gabriele Corbo ([@gabrielecorbo](https://www.github.com/gabrielecorbo))
- Andrea Puricelli ([@apuri99](https://www.github.com/apuri99))




