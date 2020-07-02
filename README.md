# At-risk-measure sampling
This repository is intended to accompany the manuscript entitled *At-risk-measure sampling in case-control studies with aggregated data*.

1. Code to produce the two figures
2. Code to simulate a simple example to illustrate Condition 3
3. A more complicated but more realistic simulate dataset where there is correlation of variables within groups of segments (streets). With this grouped dataset, we illustrate the following procedures:
    + Inverse-probability-of-selection weighting assuming some internal validation data are available
    + Adjustment for confounding via a weighted geometric mean
    + Throughout, because the data are assumed to be grouped and correlated, we produce 95% confidence intervals using the percentile method via a hierarchical bootstrap.


