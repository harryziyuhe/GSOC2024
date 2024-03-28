0: pre-req
https://github.com/harryziyuhe/gubernatorial_election_option_pricing/tree/main/code<br>
Codes accompanying a research paper documenting the effects of political uncertainty in the financial market using options data.

1: Easy
Installation went through without error

2: Medium
Identified bugs in Which-Factors.Rmd, Time-series-momentum.Rmd. Identified data issue with Improving-time-sries-momentum.Rmd

3: Hard - Commodities-long-run.Rmd
The descriptive statistics shows that for all periods under analysis, the returns on the equal-weighted portfolio is positive, and the interest rate adjusted carry returns account for more than the excess sport returns. The long-short backwardation-based portfolio, on the other hand, shows generally positive return but has an overall negative mean return between 1877 and 1945. The volatility of the long-short portfolio is also larger, with the volatiltiy of interest rate adjusted carry component significantly higher than that of a equal-weighted portfolio.

Delving into the regression analyses, the positive and statistically significant interpcepts point to consistently positive alphas for both portfolio. For the equal-weighted portfolio, the business cycle and inflation state are significantly correlated with the overall returns. Specifically, recession is negatively correlated with return whereas inflation is positive correlated with returns. Moving to a longer (one year and five years) time horizon, these correlations lose statistical significance. Overall, the relationships between business cycle, inflation state, and backwardation state and returns of the long-short portfolio are weak, both over the short and long term time horizon.

These results are surprising as they are inconsistent with the findings of Levine et al (2018). In their original paper, Levine et al (2018) finds statistically significant correlation between inflation and the returns of both equal-weighted and equal-risk portfolios over the three time horizons. The business cycle variable also remains significant with the one-year horizon and the backwardation state has statistically significant correlation with return in the short-term. 

Given these discrepancies, it is worthwhile to delve further to understand the sources of divergence. The authors' descriptive statistics show noticeable different levels of return under different macro circumstances. It might be helpful to replicate those summary statistics as well and better understand what factors are driving those results.

Additionally, the replication report's construction of a new long-short portfolio allows for more direct examination of how backwardation/contango states affect portfolio returns. Levine et al (2018) show that mean returns are higher during backwardation but it is driven by positive and high interest rate adjusted carry returns. If the correlations do exist, the long-short portfolio should perform better than the long-only portfolios because it allows greater flexibility and can take advantages of the effects of backwardation states on the overall returns.

For the investment styles multivariate regressions, we see that for the equal-weighted portfolio, momentum is positively correlated with return in the short-term, and the other two variables, value and aggregate carry, have no significant correlation with return. For long-short portfolio, momentum has positive correlation in the short-term and negative correlation in the medium-term (1 yar time horizon). Aggregate carry only shows statistically significant and negative correlation in the five year horizon model. The results for the equal-weighted portfolio again is not entirely consistent with Levine et al's (2018) findings, where carry has strong correlation with returns in all three time horizon models. 

Overall, the replication report present results that are inconsistent with Levine et al's (2018) findings despite following the same empirical strategies. While the alphas on the models are quite similar and consistent for all regression models, the reported correlations between macro variables such as business cycles, inflation, and backwardation/contago states and returns are quite different. As a next step, it is worthwhile to delve further to understand the sources of these discrepancies. It will also be interesting to see whether the results reported in the replication report change the analysis on how investors can better leverage commodity futures as well as the overall conclusion that adding commodity futures can add value to a diversified portfolio from an asset allocation perspective.

Most of the codes in the vigenettes are written with few repetitions, but there are two potential repetitions that can be optimized with functions. 
First, in Which-factors.Rmd, there are a few lines of codes where a new data frame is created with "merge()" and filtered to keep rows with no missing data.

The following function can help organize the codes:
```
MergeAndSelect <- function(data.list, select.vars = NULL) {
  # 'data.list' is the list of data frames for merging
  # 'select.vars' is the list of variables to keep

  mergedData <- do.call(merge, data.list)
  cleanedData <- mergedData[complete.cases(mergedData), ]
  
  if (!is.null(select.vars) && all(select.vars %in% colnames(cleanedData))) {
    cleanedData <- cleanedData[, select.vars]
  } else if (!is.null(select.vars) && !all(select.vars %in% colnames(cleanedData))) {
    warning("Some of the selected variables are not in the merged data. Keeping all columns.")
  }
  
  return(cleanedData)
}
```

When calling the function, pass the data frames as a list() object to data.list and the selected variables as a c() object. If all variables as kept, the select.vars argument can be left blank.

Second, in Value--Devil-in-HMLs_Details.Rmd, the codes to run the two time-series regressions are quite repetitive. While it may not be as useful to create a function as the codes are repeated only twice, if similar regressions models are built in the future, one may consider a function such as:

```
TSRegression <- function(data, dep.vars, exp.vars, date.var) {
  # 'data' is the dataset containing all variables
  # 'dep.vars' is a vector of dependent variable names
  # 'exp.vars' is a matrix of explanatory variable names
  # 'date.var' is the name of the date column
  
  regressions <- lapply(1:length(dep.vars), function(i) {
    currentData <- data[, c(dep.vars[i], exp.vars[i, ], date.var)]
    
    y <- colnames(currentData)[1]
    X <- paste(exp.vars[i, ], collapse = '+')
    model.formula <- formula(paste(y, X, sep = '~'))
    
    regressionResult <- plm::plm(
      model.formula, data = currentData,
      model = 'pooling', index = date.var
    )
    
    return(regressionResult)
  })
  
  return(regressions)
}
```

In the HML^{a,c} time-series regressions case, the function can be called as
```
dep.vars.ac = regrds[, 1]
ts.regs.hml = TSRegression(factors.data, dep.vars.ac, hml.regrs, 'DATE')
```
