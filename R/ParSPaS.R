#' The ParSPaS Function
#' @param

parspas = function(x,
                   y,
                   B,
                   c,
                   family){
  n = base::nrow(x) # Number of observations
  p = base::ncol(x) # Number of variables
  varNames = base::colnames(x) # Variable names

  bootWeights = base::matrix(stats::rexp(n = n*B,
                                     rate = 1),
                         nrow = n,
                         ncol = B) #Exponential bootstrap weights

  rawModelFit = base::list()
  modelFit = base::list()

  AIC = base::rep(x = NA_real_,
                  times = B)
  BIC = base::rep(x = NA_real_,
                  times = B)

  AICd = base::rep(x = NA_integer_,
                  times = B)
  BICd = base::rep(x = NA_integer_,
                  times = B)

  for(b in 1:B){ # Calculate d_min and d_max
    GLMFit = glmnet::glmnet(x = x,
                            y = y,
                            family = family,
                            weights = bootWeights[,b])
    rawModelFit[[b]] = GLMFit

    }

  lambdaMin = base::rep(x = NA_real_,
                        times = B)
  lambdaMax = base::rep(x = NA_real_,
                        times = B)

  for(b in 1:B){
    GLMFit = rawModelFit[[b]]
    lambdaMin[b] = GLMFit$lambda[base::which.min(GLMFit$df < 6)]
    lambdaMax[b] = GLMFit$lambda[base::which.max(GLMFit$df > 1)]
    lambdaVector = 1 - c(base::seq(from = 0,
                                   to = 1,
                                   length.out = c),
                         base::exp(base::seq(from = 0,
                                             to = 1,
                                             length.out = c))/base::exp(1)) %>%
      base::unique(x = .) %>%
      base::sort(x = .)

    lambdaAdjust = lambdaVector*(lambdaMax[b] - lambdaMin[b]) + lambdaMin[b]
    GLMAdjust = stats::coef(GLMFit,
                            s = lambdaAdjust)[-1,]
    modelFit[[b]] = GLMAdjust %>%
      base::as.matrix(x = .)
  }

  avgVal = avg_val(modelFit = modelFit, # Finished
                   varNames = varNames,
                   p = p,
                   B = B)

  nonZero = non_zero(rawModelFit = rawModelFit,
                     lambdaMin = lambdaMin,
                     LambdaMax = lambdaMax,
                     varNames = varNames,
                     p = p,
                     B = B)

  medDist = med_dist(modelFit = modelFit, # Finished
                    varNames = varNames,
                    p = p,
                    B = B)

  avgRankVar = avg_rank_var(modelFit = modelFit, # Finished
                     varNames = varNames,
                     p = p,
                     B = B)

  base::return(list(lambdaMin = lambdaMin,
                    lambdaMax = lambdaMax,
                    avgVal = avgVal,
                    nonZero = nonZero,
                    medDist = medDist,
                    avgRankVar = avgRankVar))
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

non_zero = function(rawModelFit, lambdaMin, LambdaMax, varNames, p, B){
  nonZero = base::matrix(data = NA_real_,
                         nrow = p,
                         ncol = B)
  for(b in 1:B){
  count = which(rawModelFit[[b]]$lambda >= lambdaMin[b] & rawModelFit[[b]]$lambda <= lambdaMax[b])
  lambdaVector = rawModelFit[[b]]$lambda[count]
  coefMatrix = rawModelFit[[b]]$beta[,count]
  zeroLength = base::apply(X = coefMatrix,
                           MARGIN = 1,
                           FUN = count_zero,
                           lmbdVector = lambdaVector)
  nonZero[,b] = zeroLength
  }
  base::return(nonZero)
}

#####

count_zero = function(cfVector, lmbdVector){
  totalLength = base::sum(base::diff(lmbdVector))
  checkVector = base::diff(lmbdVector)
  whichZero = base::which(cfVector == 0)
  zeroLength = base::diff(lmbdVector[whichZero])
  checkedLength = base::sum(zeroLength[zeroLength %in% checkVector])
  base::return(1 - checkedLength/totalLength)
}

##########

# Function to calculate the distance of a curve from the median curve.
med_dist = function(modelFit, varNames, p, B){
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
                                 FUN = stats::var)
  }
  base::return(avgRankVar)
}

##########

metric_clust = function(metric){
  distMet = stats::dist(x = metric)
  hClustMet = stats::hclust(d = distMet) %>%
    ggdendro::dendro_data(model = .)
  base::return(hClustMet)
}
