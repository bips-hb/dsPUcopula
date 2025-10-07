# dsPUcopula

`dsPUcopula` provides the server-side machinery that powers the PU copula based
synthetic data generator for [DataSHIELD](https://www.datashield.org/). The
package wraps the preprocessing, modelling and disclosure control checks that
need to run alongside the standard DataSHIELD aggregation interface. In
addition to the R components, the package exposes convenience wrappers around
the Python packages **syndat** and **anonymeter** so that privacy risk metrics
can be evaluated directly from the server session.

## Installation

The package is not yet on CRAN. You can install the development version from a
Git remote (for example GitHub) using:

```r
remotes::install_github("bips-hb/dsPUcopula")
```

Once installed on the DataSHIELD server you can set up the required Python
environment by running:

```r
dsPUcopula::setup_python()
```

## ToDo

- [ ] Introduce a privacy filter option that can force inference_check = TRUE, singling_out_check = TRUE and inference_check_ignore_na = FALSE in generateSyntheticDS.

## Overview of the server workflow

The typical order of operations initiated by the DataSHIELD client is:

1. `save_original_varnamesDS()` and `save_original_classesDS()` to capture the
   metadata of the training data set.
2. `preprocessDataDS()` to encode categorical variables while tracking their
   original levels.
3. `fitPUcopulaDS()` and `estimateMarginalsDS()` to fit the copula and marginal
   models.
4. `generateSyntheticDS()` to draw synthetic data, optionally requesting privacy
   and utility scores via `py_syndat_scores()`,
   `py_anonymeter_SinglingOut()` and `py_anonymeter_Inference()`.

See the function documentation for full details on the available arguments and
customisation options.
