# multiSPC

<!-- badges: start -->
<!-- badges: end -->

**multiSPC** provides tools for teaching and applying statistical process
control (SPC) in R:

- estimation of process parameters and bias-correction constants
  (`c4()`, `d2()`, `d3()`, `sigma_I()`);
- univariate control charts: Shewhart charts (`plot_chart()`), power and
  average run length of the chart for the mean (`efficacite_Shewart_moy()`),
  EWMA charts (`ewma_cart()`);
- capability indices with confidence intervals (`capability()`);
- multivariate control charts: Hotelling *T²* for subgrouped and individual
  data (`T2_Hotelling_kn()`, `T2_Hotelling_k1()` and their control limits),
  MYT decomposition (`T2_Hotelling_kn_MYT()`), MEWMA (`T2_MEWMA_k1()`,
  `T2_MEWMA_kn()`), generalized variance (`generalized_variance()`) and
  charts on principal components (`Principal_Component_construction()`).

The package was written for a course on statistical process control
([course website, in French](https://galharret.github.io/MSP/)).

## Installation

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("GALHARRET/multiSPC")
```

## Data structure

- Subgrouped multivariate data are stored in three-way arrays of dimensions
  *k* × *p* × *n*: *k* subgroups, *p* quality characteristics and *n*
  observations per subgroup (e.g. `carbon1`).
- Individual multivariate data are stored in *k* × *p* matrices
  (e.g. `bimetal1`).

## Example: Hotelling *T²* chart

```r
library(multiSPC)
data(carbon1)   # Phase I: 30 subgroups of 8 tubes, 3 characteristics
data(carbon2)   # Phase II: 25 subgroups

# Phase I
T2  <- T2_Hotelling_kn(carbon1)
LSC <- LSC_T2_Hotelling_kn(carbon1, alpha = 0.01)
plot_chart(T2, LSC = LSC["PhaseI"], Type = "T2 chart - Phase I")

# Phase II, with the Phase I estimates
T2new <- T2_Hotelling_kn(carbon2, MoyT = Moy_T(carbon1),
                         S = covariance_X(carbon1))
plot_chart(T2new, LSC = LSC["PhaseII"], Type = "T2 chart - Phase II")
```

## Datasets

The datasets (`archery`, `bimetal`, `carbon`, `dowel`, `glass`, `indust`,
`kulpa`, `mech`, `rskewed`, `sabathia`, `water`) come from the MSQC package
by Edgar Santos-Fernández and accompany the book:

> Santos-Fernández, E. (2012). *Multivariate Statistical Quality Control
> Using R*. Springer. <https://doi.org/10.1007/978-1-4614-5453-3>

## License

GPL (>= 2)
