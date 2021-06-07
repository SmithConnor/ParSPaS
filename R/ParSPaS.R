#' The ParSPaS Function
#' @param

parspas = function(x,
                   y
                   B,
                   c,
                   family){
  n = base::nrow(x) # Number of observations
  p = bas::ncol(x) # Number of variables
  varNames = base::colanmes(X) # Variable names

  bootWeights = base::matrix(stats::rexp(n = n*B,
                                     rate = 1),
                         nrow = n,
                         ncol = B) #Exponential bootstrap weights

  modelFit = base::list()

  for(b in 1:B){
    GLMFit = glmnet::glmnet(x = x,
                            y = y,
                            family = family,
                            weights = bootWeights[,b])
    lambdaMin =
    lambdaMax =
    lambdaVector = 1 - c(base::seq(from = 0,
                                   to = 1,
                                   length.out = c),
                         base::exp(base::seq(from = 0,
                                             to = 1,
                                             length.out = c))/base::exp(1)) %>%
      base::unique(x = .) %>%
      base::sort(x = .)

    lambdaAdjust = lambdaVector*(lambdaMax - lambdaMin) + lambdaMin
    GLMAdjust = stats::coef(GLMFit,
                            s = lambdaAdjust)[-1,]
    modelFit[[b]] = GLMAdjust %>%
      base::as.matrix(data = .)
  }
}

##########

avg_val = function(modelFit, varNames, p, B){
  avgVal = base::matrix(data = NA_real_,
                        nrow = p,
                        ncol = B)
  rownames(avgVal) = varNames

  for( b in 1:B){
    avgVal[,b] = base::apply(X = modelFit[[b]],
                             MARGIN = 1,
                             FUN = base::mean)
  }

  base::return(avgVal)
}

##########

non_zero = function(modelFit, varNames, p, B){

}

##########

# Function to calculate the distance of a curve from the median curve.
med_dist = function(modelFit, varNames, p, b){
  medDist = base::matrix(data = NA_real_,
                        nrow = p,
                        ncol = B)
  base::rownames(medDist) = varNames
  medCur = med_curve(modelFit = modelFit,
                     B = B,
                     p = p)
  for(b in 1:B){
    distMat = base::abs(x = (medCur - modelFit[[b]]))
    medDist[,b] = base::apply(X = distMat,
                              MARGIN = 1,
                              FUN = base::mean)
  }
  base::return(medDist)
}

#####

# Function to find the median curve from a set of bootstrapped curves.
med_curve = function(modelFit, p, B){
  h = modelFit[[1]] %>%
    base::ncol(x = .)
  medCur = base::matrix(data = NA_real_,
                        nrow = p,
                        ncol = h)

  for(i in 1:p){
    for(j in 1:h){
      medVector = base::rep(x = 0,
                           times = B)
        for(b in 1:B){
          medVector[b] = modelFit[[b]][i,j]
        }
      medCur[i,j] = medVector %>%
        stats::median(x = .)
    }
  }
  base::return(medCur)
}

##########

avg_rank_var = function(modelFit, varNames, p, B){
  avgRankVar = base::matrix( data = NA_real_,
                             nrow = p,
                             ncol = B)
  base::rownames(avgRankVar) = varNames
  for(b in 1:B){
    rankMat = base::apply(X = base::abs(x = modelFit[[b]]),
                          MARGIN = 2,
                          FUN = base::rank,
                          ties.method = "random")
    avgRankVar[,b] = base::apply(X = rankMat,
                                 MARGIN = 1,
                                 FUN = base::var)
  }
  base::return(avgRankVar)
}

##########
