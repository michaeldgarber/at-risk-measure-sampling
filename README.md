# At-risk-measure sampling
This repository is intended to accompany the manuscript entitled *At-risk-measure sampling in case-control studies with aggregated data*.

There are three code files located in the [code](https://github.com/michaeldgarber/at-risk-measure-sampling/tree/master/code) folder
1. The first, [code-for-figures.md](https://github.com/michaeldgarber/at-risk-measure-sampling/blob/master/code/code-for-figures.md), produces the figures in the manuscript: 
2. Simulate a simple dataset illustrating Condition 3.
3. Simulate a more complicated but more realistic dataset with correlation of variables within groups of segments (streets). With this grouped dataset, we illustrate the following procedures, which are described in the manuscript:
    + Inverse-probability-of-selection weighting assuming some internal validation data are available
    + Adjustment for confounding via a weighted geometric mean
    + Throughout, because the data are assumed to be grouped and correlated, we produce 95% confidence intervals using the percentile method via a hierarchical bootstrap.


