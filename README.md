# At-risk-measure sampling
This repository is intended to accompany the manuscript entitled *At-risk-measure sampling in case-control studies with aggregated data* (under review).

There are three code files located in the [code](https://github.com/michaeldgarber/at-risk-measure-sampling/tree/master/code) folder.

1. The first, [code-for-figures.md](https://github.com/michaeldgarber/at-risk-measure-sampling/blob/master/code/code-for-figures.md), produces the figures in the manuscript: 

2. The second, link, simulate a simple dataset to illustrate the condition (Equation 3 from the manuscript).

3. The third, link, simulates a more complicated but probably more realistic dataset that parallels the example in the manuscript. It induces correlation of segment-level variables within groups. Segments are nested within streets. Then, with this grouped dataset, we illustrate the following procedures, which are described in the manuscript:
    
    + Adjustment for selection bias using inverse-probability-of-selection weighting assuming some internal validation data are available.
    
    + Adjustment for confounding using a weighted geometric mean.
    
    + Throughout, because the data are assumed to be grouped and correlated, we produce 95% confidence intervals using the percentile method via a hierarchical bootstrap. The percentile method is described here: Efron B, Hastie T. Bootstrap Confidence Intervals. In: Computer Age Statistical Inference: Algorithms, Evidence, and Data Science. Cambridge University Press; 2016:181-2014.


