## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ParSPaS)

## ---- eval = TRUE-------------------------------------------------------------
library(ParSPaS)

## Data simulation
set.seed(1234)
n = 100
p = 10
varNames = paste0("X", 1:p)
beta = c(2,-1,-1, rep(x = 0, 
                      times = p - 3))
x = matrix(rnorm(n*p),
           ncol = p)
colnames(x) = varNames
y = rbinom(n = n,
           size = 1,
           prob = expit(x %*% beta))


## ---- fig.width=9, fig.height=6-----------------------------------------------
output = ParSPaS::parspas(x = x,
                          y = y,
                          B = 100,
                          I = 20,
                          family = "binomial",
                          seed = 1234)
ParSPaS::combine_plot(output$plots)

