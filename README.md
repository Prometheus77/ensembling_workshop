This repo contains material for a workshop that teaches how to create ensemble models in R using the `mlr` package. Click on ensembling_workshop.md above to see the output of the tutorial. If you would like to pull down the repo and run the algorithms yourself, use ensembling_workshop.Rmd.

# Getting started
You will need to install several packages in order to run the code:
```
install.packages("devtools")
install.packages("mlr", dependencies = TRUE, suggests = TRUE)
install.packages("mlrMBO", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
devtools::install_github("Prometheus77/ucimlr")
devtools::install_github("Prometheus77/actools")
```
Installing the `mlr` packages with all suggested dependences can take upwards of an hour. As an alternative, you can remove the `suggests = TRUE` code and then install packages as you go. Every time `mlr` needs a package you don't have installed, it will throw an error and tell you which package is missing.

# Additional resources
* mlr tutorial: https://mlr.mlr-org.com/
* mlr Github repo: https://github.com/mlr-org/mlr
* mlr cheat sheet: https://github.com/mlr-org/mlr/blob/master/addon/cheatsheet/MlrCheatsheet.pdf
* Ensemble selection white paper: http://www.cs.cornell.edu/~alexn/papers/shotgun.icml04.revised.rev2.pdf
