<a href="https://github.com/haghish/h2otools"><img src='man/figures/logo.PNG' align="right" height="200" /></a>

`h2otools`: Machine Learning Model Evaluation for 'h2o' Package
===============================================================

This repository includes functions that my other h2o-related packages rely on. So far, I had to destribute practical functions in different packages and I intend to collect them in this repository, creating a comprehensive toolkit for working with [`h2o`](https://CRAN.R-project.org/package=h2o). 

More importantly, this package includes functions for evaluating machine learning models under severe class imbalance that are not implemented in `h2o` such as F3, F4, and F5 F-measures. `h2o` only provides `F0.5`, `F1`, and `F2`. However, this package will compute the F-measure for any value of _Beta_. Along the way, I will program other measures on model evaluation in this package. 

