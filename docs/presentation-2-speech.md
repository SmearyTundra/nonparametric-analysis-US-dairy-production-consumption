<!-- omit from toc -->
# Table of contents

- [SLIDE 1](#slide-1)
- [SLIDE 2](#slide-2)
- [SLIDE 3](#slide-3)
- [SLIDE 4](#slide-4)
- [SLIDE 5](#slide-5)
- [SLIDE 6](#slide-6)
- [SLIDE 7](#slide-7)
- [SLIDE 8](#slide-8)

# SLIDE 1

Hi everyone! My name is Andrea, this is Teo and there are Gabriele and Filippo online. :)
Our project is based on a Nonparametric Analysis of the US Dairy Production and Consumption on a dataset taken from tidytuesdey.

# SLIDE 2

Our dataset is made up of year by year measurements of the production and consumption of dairy products in the USA
Time-wise, the original dataset had data from 1980 to 2014 but we managed to update (from the USDA (United States Department of Agriculture) website) update the data up to 2021. 
We have many kinds of data regarding production like production costs, the milk price (which will be our target variable for the GAM model, but I’ll talk about it more later on) and how much milk is produced
And we have various consumption data for the main kinds of dairies

# SLIDE 3

To talk about the goals we first have to talk about our stakeholders
The main entities that would benefit the most from our analysis are cheese factories.
We thought a cheese factory may need:
- First, to improve the production chain. So we tried to see for which product the consumption has increased or decreased over the years
- Secondly, to create an optimal price strategy for a new competitor that wants to enter the market.

# SLIDE 4

For our data pipeline, the first thing we did was to adjust our data for inflation so that we could have a better understanding of how the prices actually changed over time
We also found very detailed data about the import and export. We want to use the import data as a regressor to see how much competition there is with the domestic production and export data as an indicator of the demand overseas

# SLIDE 5

These are the first results we got from our GAM model by using the average milk price as our target variable.
We used multiple Permutation Tests to decide wether we wanted to drop a covariate or not.

# SLIDE 6

We then performed conformal prediction, you can see the intervals in red for the GAM and in blue for the KNN distance
And with the robust regression, we managed to identify four years that were considered outliers such as 1983, 1984, 2002, and 2006.

# SLIDE 7

To see the trends in the consumption of certain kinds of dairies we performed some bayesian clustering using the model here above that uses a Dirichlet Process as a prior to produce 3 clusters:
- One with an ascending behavior
- One with a declining behavior
- One that looks like an oscillating behavior .
We verified that the clusters have no outliers

# SLIDE 8

This is in short what we have done so far, but for our future developments we wanted to:
- Try different Gam models and more distances for the conformal prediction
- ...

