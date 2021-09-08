#' The ParSPaS Function
#' @param x
#' @param y
#' @param B
#' @param I
#' @param family
#' @param seed
#'
#' @return
#'
#' @export

parspas = function(x,
                   y,
                   B,
                   I,
                   family,
                   seed = NA){
  if(is.na(seed)){
    seed = sample(1:10e6,
                  size = 1)
  }
  n = base::nrow(x) # Number of observations
  p = base::ncol(x) # Number of variables
  varNames = base::colnames(x) # Variable names

  set.seed(seed)
  bootWeights = base::matrix(stats::rexp(n = n*B,
                                     rate = 1),
                         nrow = n,
                         ncol = B) #Exponential bootstrap weights

  rawModelFit = base::list()
  modelFit = base::list()

  AICd = base::rep(x = NA_integer_,
                  times = B)
  BICd = base::rep(x = NA_integer_,
                  times = B)

  baseModel = base::rep(x = 0,
                        times = p)

  for(b in 1:B){
    GLMFit = glmnet::cv.glmnet(x = x,
                               y = y,
                               family = family,
                               weights = bootWeights[,b])
    rawModelFit[[b]] = GLMFit
    coef = coef(GLMFit, s = 'lambda.1se')[-1]
    coefBin = (coef != 0)/B
    baseModel = baseModel + coefBin
    LL = GLMFit$glmnet.fit$nulldev - deviance(GLMFit$glmnet.fit)
    AIC = -LL + 2*GLMFit$nzero
    BIC = -LL + log(n)*GLMFit$nzero
    AICd[b] = GLMFit$nzero[which.min(AIC)]
    BICd[b] = GLMFit$nzero[which.min(BIC)]
  }

  Dmin = max(c(min(BICd), 0))
  Dmax = max(AICd)

  lambdaMin = base::rep(x = NA_real_,
                        times = B)
  lambdaMax = base::rep(x = NA_real_,
                        times = B)

  for(b in 1:B){
    GLMFit = rawModelFit[[b]]
    lambdaMin[b] = GLMFit$glmnet.fit$lambda[base::which.min(GLMFit$nzero < Dmax)]
    lambdaMax[b] = GLMFit$glmnet.fit$lambda[base::which.max(GLMFit$nzero > Dmin)]
    lambdaVector = 1 - c(base::seq(from = 0,
                                   to = 1,
                                   length.out = I),
                         base::exp(base::seq(from = 0,
                                             to = 1,
                                             length.out = I))/base::exp(1)) %>%
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
  avgClust = metric_clust(avgVal)
  avgPlot = plot_parspas(hClustMet = avgClust,
                         baseModel = baseModel,
                         title = "Average",
                         varNames = varNames)

  nonZero = non_zero(rawModelFit = rawModelFit,
                     lambdaMin = lambdaMin,
                     LambdaMax = lambdaMax,
                     varNames = varNames,
                     p = p,
                     B = B)
  rownames(nonZero) = varNames
  nonClust = metric_clust(nonZero)
  nonPlot = plot_parspas(hClustMet = nonClust,
                         baseModel = baseModel,
                         title = "Non-Zero",
                         varNames = varNames)

  medDist = med_dist(modelFit = modelFit, # Finished
                    varNames = varNames,
                    p = p,
                    B = B)
  medClust = metric_clust(medDist)
  medPlot = plot_parspas(hClustMet = medClust,
                         baseModel = baseModel,
                         title = "Median Curve",
                         varNames = varNames)

  avgRankVar = avg_rank_var(modelFit = modelFit, # Finished
                     varNames = varNames,
                     p = p,
                     B = B)
  varClust = metric_clust(avgRankVar)
  varPlot = plot_parspas(hClustMet = varClust,
                         baseModel = baseModel,
                         title = "Rank Variance",
                         varNames = varNames)
  allMetrics = list(avgVal = avgVal,
                    nonZero = nonZero,
                    medDist = medDist,
                    avgRankVar = avgRankVar)
  allClust = cluster_all(allMetrics)
  allPlot = plot_parspas(hClustMet = allClust,
                         baseModel = baseModel,
                         title = "All Metrics",
                         varNames = varNames)
  finalPlot = grid.arrange(grid.arrange(avgPlot[[1]], nonPlot[[1]], nrow = 2),
                           allPlot[[1]],
                           grid.arrange(medPlot[[1]], varPlot[[1]], nrow = 2),
                           nrow = 1, widths = c(1,1.5,1))

  base::return(list(lambdaMin = lambdaMin,
                    lambdaMax = lambdaMax,
                    metrics = list(avgVal = avgVal,
                                   nonZero = nonZero,
                                   medDist = medDist,
                                   avgRankVar = avgRankVar),
                    clusts = list(avgClust = avgClust,
                                  nonClust = nonClust,
                                  medClust = medClust,
                                  varClust = varClust,
                                  allClust),
                    plots = list(avgPlot = avgPlot[[1]],
                                 nonPlot = nonPlot[[1]],
                                 medPlot = medPlot[[1]],
                                 varPlot = varPlot[[1]],
                                 allPlot = allPlot[[1]]),
                    fullPlot = finalPlot,
                    baseModel = baseModel,
                    varNames = varNames,
                    seed = seed))
}

#'Average Coef Function
#' @param modelFit
#' @param varNames
#' @param p
#' @param B

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


#'Non Zero Function
#'@param rawModelFit
#'@param lambdaMin
#'@param lambdaMax
#'@param varNames
#'@param p
#'@param B

non_zero = function(rawModelFit, lambdaMin, LambdaMax, varNames, p, B){
  nonZero = base::matrix(data = NA_real_,
                         nrow = p,
                         ncol = B)
  rawModelFit
  for(b in 1:B){
  count = which(rawModelFit[[b]]$lambda >= lambdaMin[b] & rawModelFit[[b]]$lambda <= lambdaMax[b])
  lambdaVector = rawModelFit[[b]]$lambda[count]
  coefMatrix = rawModelFit[[b]]$glmnet.fit$beta[,count]
  zeroLength = base::apply(X = coefMatrix,
                           MARGIN = 1,
                           FUN = count_zero,
                           lmbdVector = lambdaVector)
  nonZero[,b] = zeroLength
  }
  base::return(nonZero)
}

#'Count Function
#' @param cfVector
#' @param lmbdVector

count_zero = function(cfVector, lmbdVector){
  totalLength = base::sum(base::diff(lmbdVector))
  checkVector = base::diff(lmbdVector)
  whichZero = base::which(cfVector == 0)
  zeroLength = base::diff(lmbdVector[whichZero])
  checkedLength = base::sum(zeroLength[zeroLength %in% checkVector])
  base::return(1 - checkedLength/totalLength)
}

#'Average Median Dist Function
#' @param modelFit
#' @param varNames
#' @param p
#' @param B

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

#'Median Curve Function
#' @param modelFit
#' @param p
#' @param B

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

#'Average Rank Variance Function
#' @param modelFit
#' @param varNames
#' @param p
#' @param B

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

#'Metric Clustering Function
#' @param metric

metric_clust = function(metric){
  distMet = stats::dist(x = metric)
  hClustMet = stats::hclust(d = distMet) %>%
    ggdendro::dendro_data(model = .)
  base::return(hClustMet)
}

#'Multi-Metric Clustering Function
#'@param metricList

cluster_all = function(metricList){
  varNames = rownames(metricList[[1]])
  m = length(metricList)
  b = ncol(metricList[[1]])
  p = nrow(metricList[[1]])
  output = lapply(1:p,
                  matrix,
                  data = NA,
                  nrow = m,
                  ncol = b)
  for(i in 1:m){
    for(j in 1:p){
      output[[j]][i,] = metricList[[i]][j,]
    }
  }

  distMat = matrix(data = NA,
                   nrow = p,
                   ncol = p)
  colnames(distMat) = varNames
  rownames(distMat) = varNames

  for(i in 1:p){
    for(j in 1:p){
      distMat[i,j] = base::sqrt(psych::tr((output[[i]]-output[[j]]) %*% t(output[[i]]-output[[j]])))
    }
  }
  distMat = stats::as.dist(distMat)

  hClustMet = stats::hclust(d = distMat) %>%
    ggdendro::dendro_data(model = .)
  base::return(hClustMet)
}
