# At-risk-measure sampling
This repository is intended to accompany the manuscript entitled *At-risk-measure sampling in case-control studies with aggregated data* (under review).

There are three files of R code files located in the [code](https://github.com/michaeldgarber/at-risk-measure-sampling/tree/master/code) folder.

1. The first, [code-for-figures.md](https://github.com/michaeldgarber/at-risk-measure-sampling/blob/master/code/code-for-figures.md), produces the figures in the manuscript: 

2. The second, [sim1-simple.R](https://github.com/michaeldgarber/at-risk-measure-sampling/blob/master/code/sim1-simple.R), simulate a simple dataset to illustrate the condition (Equation 3 from the manuscript).

3. The third, link, simulates a more involved dataset that parallels aspects of the example in the manuscript. It induces correlation of segment-level variables within streets. With this grouped dataset, we illustrate the following procedures, which are described in the manuscript:
    
    + Calculation of the unadjused result.

    + Adjustment for selection bias using inverse-probability-of-selection weighting assuming some internal validation data are available.
    
    + Adjustment for confounding using a weighted geometric mean.
    
    + The production of 95% confidence intervals for each of the above using the percentile method from hierarchical bootstrapping.


