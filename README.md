# At-risk-measure sampling
This repository is intended to accompany the manuscript entitled *At-risk-measure sampling in case-control studies with aggregated data*.

The repository contains three code files with, respectively, the following objectives:
1. Produces the figures in the manuscript.
2. Simualte a simple dataset illustrating Condition 3.
3. Simulate a more complicated but more realistic dataset with correlation of variables within groups of segments (streets). With this grouped dataset, we illustrate the following procedures, which are described in the manuscript:
    + Inverse-probability-of-selection weighting assuming some internal validation data are available
    + Adjustment for confounding via a weighted geometric mean
    + Throughout, because the data are assumed to be grouped and correlated, we produce 95% confidence intervals using the percentile method via a hierarchical bootstrap.


