<!-- omit from toc -->
# Nonparametric Analysis of US Dairy Production and Consumption

This project was developed for the course of "Nonparametric Statistics" held at Politecnico di Milano in the academic year 2022-2023.

This report presents a nonparametric analysis of the evolution of dairy production and consumption in the United States from 1980 to 2021. It starts from the needs of a stakeholders which is about to enter the market and step-by-step presents our analysis and the findings to provide meaningful information to optimize production and pricing. We also provide an outlier analysis in terms of years and, in the last part, a spatial analysis and a clustering of the types of cheese.

<!-- omit from toc -->
# Table of contents

- [Installation](#installation)
  - [How to clone the repository](#how-to-clone-the-repository)
  - [How to install the packages](#how-to-install-the-packages)
- [Running the analysis](#running-the-analysis)
- [Final results](#final-results)
- [Authors](#authors)

# Installation

## How to clone the repository

```
git clone https://github.com/SmearyTundra/nonparametric-analysis-US-dairy-production-consumption
git submodule update --init
git submodule update --recursive
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
        "ISLR2",
        "car",
        "sp",
        "devtools",
        "visreg",
        "mgcViz",
        "usmap",
        "raster",
        "sf",
        "maps",
        "ggspatial",
        "BNPTSclust",
        "roahd",
        "fda.usc"
    )
install.packages(packages_list)
```

# Running the analysis

The repository contains different files to perform the analysis

- `FILENAME.R` explained
- `FILENAME.R` explained
- `FILENAME.R` explained

# Final results

The final presentations can be found here:

- [Presentation DATE](./path-to-file.pdf)

The final report can be found here:

- [`FILENAME.pdf`](./path-to-file.pdf)

# Authors

- Teo Bucci ([@teobucci](https://www.github.com/teobucci))
- Filippo Cipriani ([@SmearyTundra](https://www.github.com/SmearyTundra))
- Gabriele Corbo ([@gabrielecorbo](https://www.github.com/gabrielecorbo))
- Andrea Puricelli ([@apuri99](https://www.github.com/apuri99))




