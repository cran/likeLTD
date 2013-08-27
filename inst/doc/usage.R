### R code from vignette source 'usage.Rnw'

###################################################
### code chunk number 1: setthreads
###################################################
  if(.Call(likeLTD::.cpp.nbthreads) > 2) {
    .Call(likeLTD::.cpp.set_nbthreads, as.integer(2))
  }


###################################################
### code chunk number 2: verystart (eval = FALSE)
###################################################
##   require(likeLTD)
##   require(DEoptim)
## 
##   # Case we are going to be looking at.
##   caseName = 'hammer'
##   datapath = file.path(system.file("extdata", package="likeLTD"), 
##                        caseName)
##   args = list(
##     databaseFile = NULL,
##     cspFile    = file.path(datapath, 'hammer-CSP.csv'),
##     refFile      = file.path(datapath, 'hammer-reference.csv'),
##     nUnknowns    = 0,
##     doDropin     = TRUE,
##     ethnic       = "EA1",
##     adj          = 1.0,
##     fst          = 0.02,
##     relatedness  = c(0, 0)/4
##   )
##   # Create hypothesis for defence and prosecution.
##   defenceHyp = do.call(defence.hypothesis, args)
##   prosecuHyp = do.call(prosecution.hypothesis, args)


###################################################
### code chunk number 3: models (eval = FALSE)
###################################################
##   defenceModel <- create.likelihood(defenceHyp)
##   prosecuModel <- create.likelihood(prosecuHyp)


###################################################
### code chunk number 4: usage.Rnw:79-88 (eval = FALSE)
###################################################
##   defenceModel(rcont=c(1, 1e-8, 1.63), 
##                degradation=c(10^-2.27, 10^-2.74, 10^-2.47),
##                locusAdjustment=list(D3=0.983, vWA=1.010, D16=1.028, 
##                                     D2=1.072, D8=1.020, D21=0.930,
##                                     D18=0.850, D19=0.932, 
##                                     TH01=1.041, FGA=0.916),
##                dropout=c(0.5072, 1e-8), 
##                dropin=1.0216,
##                power=-4.4462)


###################################################
### code chunk number 5: plotscalarWoE (eval = FALSE)
###################################################
##   require(ggplot2)
##   require(scales)
##   # Function that winnows down to a single value
##   scalarWoE <- function(x) {
##      defenceModel(locusAdjustment=list(D3=0.983, vWA=1.010,
##                                        D16=1.028, D2=1.072,
##                                        D8=1.020, D21=0.930,
##                                        D18=0.850, D19=0.932, 
##                                        TH01=1.041, FGA=0.916),
##                   dropout=c(0.5072, 1e-8), 
##                   degradation=c(10^-2.27, 10^-2.74, 10^-2.47),
##                   rcont=c(x, 1e-8, 1), 
##                   dropin=1.0216,
##                   power=-4.4462)
##   }
## 
##   x = 0:30/30 * 3e0
##   data = data.frame(x=x, y=sapply(x, scalarWoE))
##   plots <- ggplot(data, aes(x=x, y=y))                  +
##               geom_line()                               +
##               xlab("Relative contribution of Victim 1") +
##               ylab("Weight of Evidence")                +
##               scale_y_log10(
##                 labels=trans_format("log10", math_format(10^.x)))
##   print(plots)


###################################################
### code chunk number 6: plotme
###################################################
  require(likeLTD)
  require(DEoptim)

  # Case we are going to be looking at.
  caseName = 'hammer'
  datapath = file.path(system.file("extdata", package="likeLTD"), 
                       caseName)
  args = list(
    databaseFile = NULL,
    cspFile    = file.path(datapath, 'hammer-CSP.csv'),
    refFile      = file.path(datapath, 'hammer-reference.csv'),
    nUnknowns    = 0,
    doDropin     = TRUE,
    ethnic       = "EA1",
    adj          = 1.0,
    fst          = 0.02,
    relatedness  = c(0, 0)/4
  )
  # Create hypothesis for defence and prosecution.
  defenceHyp = do.call(defence.hypothesis, args)
  prosecuHyp = do.call(prosecution.hypothesis, args)
  defenceModel <- create.likelihood(defenceHyp)
  prosecuModel <- create.likelihood(prosecuHyp)
  require(ggplot2)
  require(scales)
  # Function that winnows down to a single value
  scalarWoE <- function(x) {
     defenceModel(locusAdjustment=list(D3=0.983, vWA=1.010,
                                       D16=1.028, D2=1.072,
                                       D8=1.020, D21=0.930,
                                       D18=0.850, D19=0.932, 
                                       TH01=1.041, FGA=0.916),
                  dropout=c(0.5072, 1e-8), 
                  degradation=c(10^-2.27, 10^-2.74, 10^-2.47),
                  rcont=c(x, 1e-8, 1), 
                  dropin=1.0216,
                  power=-4.4462)
  }

  x = 0:30/30 * 3e0
  data = data.frame(x=x, y=sapply(x, scalarWoE))
  plots <- ggplot(data, aes(x=x, y=y))                  +
              geom_line()                               +
              xlab("Relative contribution of Victim 1") +
              ylab("Weight of Evidence")                +
              scale_y_log10(
                labels=trans_format("log10", math_format(10^.x)))
  print(plots)


###################################################
### code chunk number 7: skel (eval = FALSE)
###################################################
##   skeleton = initial.arguments(defenceHyp)
##   vector.model <- function(x) {
##     args <- relist(x, skeleton)
##     args[["degradation"]] = 10^args[["degradation"]]
##     result <- do.call(defenceModel, args)
##     log10(result)
##   }
## 
##   # Call vector.model with vector argument.
##   arguments = skeleton
##   arguments[["degradation"]] = log10(arguments[["degradation"]])
##   vector.model( as.vector(unlist(arguments)) )


###################################################
### code chunk number 8: maxiskel (eval = FALSE)
###################################################
##   require(stats)
##   # define upper and lower bounds for constrained maximization
##   nloci = ncol(defenceHyp$cspProfile)
##   upper = list(locusAdjustment = rep(1.5, nloci),
##                dropout         = c(1-1e-3, 1-1e-3),
##                degradation     = rep(-1e-3, 3),
##                rcont           = rep(100, 2),
##                dropin          = 1,
##                power      = -2 )[names(arguments)]
##   lower = list(locusAdjustment = rep(0.5, nloci),
##                dropout         = c(1e-3, 1e-3),
##                degradation     = rep(-20, 3),
##                rcont           = rep(1e-3, 2),
##                dropin          = 1e-3,
##                power      = -6 )[names(arguments)]
## 
##   # perform maximization
##   result <- DEoptim(fn  = vector.model,
##                   upper = unlist(upper),
##                   lower = unlist(lower),
##                   control = list(strategy=3, itermax=500)
##   		)
##   opti = relist(result$optim$bestmem, skeleton)
##   cat(sprintf("Resulting Weight of Evidence: 10^%f\n",
##               -result$optim$bestval))


###################################################
### code chunk number 9: alltheabove
###################################################
  require(likeLTD)
  require(DEoptim)

  # Case we are going to be looking at.
  caseName = 'hammer'
  datapath = file.path(system.file("extdata", package="likeLTD"), 
                       caseName)
  args = list(
    databaseFile = NULL,
    cspFile    = file.path(datapath, 'hammer-CSP.csv'),
    refFile      = file.path(datapath, 'hammer-reference.csv'),
    nUnknowns    = 0,
    doDropin     = TRUE,
    ethnic       = "EA1",
    adj          = 1.0,
    fst          = 0.02,
    relatedness  = c(0, 0)/4
  )
  # Create hypothesis for defence and prosecution.
  defenceHyp = do.call(defence.hypothesis, args)
  prosecuHyp = do.call(prosecution.hypothesis, args)
  defenceModel <- create.likelihood(defenceHyp)
  prosecuModel <- create.likelihood(prosecuHyp)
  skeleton = initial.arguments(defenceHyp)
  vector.model <- function(x) {
    args <- relist(x, skeleton)
    args[["degradation"]] = 10^args[["degradation"]]
    result <- do.call(defenceModel, args)
    log10(result)
  }

  # Call vector.model with vector argument.
  arguments = skeleton
  arguments[["degradation"]] = log10(arguments[["degradation"]])
  vector.model( as.vector(unlist(arguments)) )
  require(stats)
  # define upper and lower bounds for constrained maximization
  nloci = ncol(defenceHyp$cspProfile)
  upper = list(locusAdjustment = rep(1.5, nloci),
               dropout         = c(1-1e-3, 1-1e-3),
               degradation     = rep(-1e-3, 3),
               rcont           = rep(100, 2),
               dropin          = 1,
               power      = -2 )[names(arguments)]
  lower = list(locusAdjustment = rep(0.5, nloci),
               dropout         = c(1e-3, 1e-3),
               degradation     = rep(-20, 3),
               rcont           = rep(1e-3, 2),
               dropin          = 1e-3,
               power      = -6 )[names(arguments)]

  # perform maximization
  result <- DEoptim(fn  = vector.model,
                  upper = unlist(upper),
                  lower = unlist(lower),
                  control = list(strategy=3, itermax=500)
  		)
  opti = relist(result$optim$bestmem, skeleton)
  cat(sprintf("Resulting Weight of Evidence: 10^%f\n",
              -result$optim$bestval))


###################################################
### code chunk number 10: optim (eval = FALSE)
###################################################
##   params = optimisation.params(defenceHyp, verbose=FALSE)
##   params$control$itermax=50 # Less strict convergence, for demo purposes.
##   results <- do.call(DEoptim, params)
##   arguments <- relistArguments(results$optim$bestmem, defenceHyp)


###################################################
### code chunk number 11: tabopt
###################################################
  require(likeLTD)
  require(DEoptim)

  # Case we are going to be looking at.
  caseName = 'hammer'
  datapath = file.path(system.file("extdata", package="likeLTD"), 
                       caseName)
  args = list(
    databaseFile = NULL,
    cspFile    = file.path(datapath, 'hammer-CSP.csv'),
    refFile      = file.path(datapath, 'hammer-reference.csv'),
    nUnknowns    = 0,
    doDropin     = TRUE,
    ethnic       = "EA1",
    adj          = 1.0,
    fst          = 0.02,
    relatedness  = c(0, 0)/4
  )
  # Create hypothesis for defence and prosecution.
  defenceHyp = do.call(defence.hypothesis, args)
  prosecuHyp = do.call(prosecution.hypothesis, args)
  params = optimisation.params(defenceHyp, verbose=FALSE)
  params$control$itermax=50 # Less strict convergence, for demo purposes.
  results <- do.call(DEoptim, params)
  arguments <- relistArguments(results$optim$bestmem, defenceHyp)


###################################################
### code chunk number 12: testing (eval = FALSE)
###################################################
## library(svUnit)
## library(likeLTD)
## 
## runTest( svSuite("package:likeLTD") )
## Log()


