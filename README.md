## Bayesian Logistic Regression

### To run...

1. Open R
2. Install 'shiny' library
3. Place the three files from this repo (model.R, ui.R and server.R) in a folder called 'bayes-app'
4. Navigate to the parent directory of 'bayes-app'
5. In R, run runApp('bayes-app') to launch the application

## R Dependencies
The following R packages are needed: 
```
library(arm)
library(MCMCpack)
library(coda)
library(fBasics)
library(stats4)
library(MASS)
library(vcd)
library(caret)
library(pROC)
library(ROCR)
library(BoomSpikeSlab)
```

## Questions?** 
email: nicola.voyle@kcl.ac.uk

Please note, this application was not created in conjunction with the developers of MCMCpack.
Their package and documentation are fully acknowledged:

http://mcmcpack.berkeley.edu/index.html

https://cran.r-project.org/web/packages/MCMCpack/index.html

Andrew D. Martin, Kevin M. Quinn, Jong Hee Park (2011). MCMCpack: Markov Chain Monte Carlo in R. Journal of Statistical Software. 42(9): 1-21. URL http://www.jstatsoft.org/v42/i09/.

