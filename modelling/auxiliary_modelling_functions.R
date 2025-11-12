## Criando a função que será utilizada para a tunagem no pacote ranger (adaptada da função tune, do pacote randomForestSRC)
tune_ranger <- function (formula, data, mtryStart = ncol(data)/2, nodesizeTry = c(1:9, seq(10, 100, by = 5)),
                         ntreeTry = 100, sampsize = function(x) {  min(x * 0.632, max(150, x^(3/4))) }, 
                         nsplit = 1, stepFactor = 1.25, improve = 0.001, strikeout = 3, maxIter = 25,
                         trace = TRUE, doBest = TRUE, splitrule = "logrank", time.interest = 50,
                         num.threads = parallel::detectCores(), seed = NULL, respect.unordered.factors = FALSE) 
{
  if (improve < 0) {
    stop("improve must be non-negative.")
  }
  if (stepFactor <= 1) {
    stop("stepFactor must be great than 1.")
  }
  if (missing(formula)) {
    stop("a formula must be supplied (only supervised forests allowed).")
  }
  stump <- rfsrc(formula, data, nodedepth = 0, perf.type = "none", 
                 save.memory = TRUE, ntree = 1, splitrule = "random")
  n <- stump$n
  yvar.names <- stump$yvar.names
  data <- data.frame(stump$yvar, stump$xvar)
  colnames(data)[1:length(yvar.names)] <- yvar.names
  rm(stump)
  if (is.function(sampsize)) {
    ssize <- sampsize(n)
  }
  else {
    ssize <- sampsize
  }
  if ((2 * ssize) < n) {
    tst <- sample(1:n, size = ssize, replace = FALSE)
    trn <- setdiff(1:n, tst)
    newdata <- data[tst, , drop = FALSE]
  }
  else {
    trn <- 1:n
    newdata <- NULL
  }
  res <- list()
  counter1 <- 0
  for (nsz in nodesizeTry) {
    counter1 <- counter1 + 1
    if (is.null(newdata)) {
      o <- ranger(formula, data, num.trees = ntreeTry, mtry = mtryStart, 
                  min.node.size = nsz, replace = FALSE, num.random.splits = nsplit,
                  num.threads = num.threads, splitrule = splitrule,
                  time.interest = time.interest, seed = seed, 
                  respect.unordered.factors = respect.unordered.factors)
      
      errorOld <- o$prediction.error
    }
    else {
      o <- ranger(formula, data[trn, , drop = FALSE], num.trees = ntreeTry,
                  mtry = mtryStart, min.node.size = nsz, replace = FALSE, 
                  num.random.splits = nsplit, num.threads = num.threads, 
                  splitrule = splitrule, time.interest = time.interest, seed = seed,
                  respect.unordered.factors = respect.unordered.factors)
      
      errorOld <- o$prediction.error
      
    }
    mtryStart <- o$mtry
    mtryMax <- length(o$num.independent.variables)
    if (trace) {
      cat("nodesize = ", nsz, " mtry =", mtryStart, "error =", 
          paste(100 * round(errorOld, 4), "%", sep = ""), 
          "\n")
    }
    oobError <- list()
    oobError[[1]] <- errorOld
    names(oobError)[1] <- mtryStart
    for (direction in c("left", "right")) {
      if (trace) 
        cat("Searching", direction, "...\n")
      Improve <- 1.1 * improve
      mtryBest <- mtryStart
      mtryCur <- mtryStart
      counter2 <- 1
      strikes <- 0
      while (counter2 <= maxIter && (Improve >= improve || 
                                     (Improve < 0 && strikes < strikeout))) {
        counter2 <- counter2 + 1
        if (Improve < 0) {
          strikes <- strikes + 1
        }
        mtryOld <- mtryCur
        if (direction == "left") {
          mtryCur <- max(1, min(ceiling(mtryCur/stepFactor), 
                                mtryCur - 1))
        }
        else {
          mtryCur <- min(mtryMax, max(floor(mtryCur * 
                                              stepFactor), mtryCur + 1))
        }
        if (mtryCur == mtryOld) {
          break
        }
        if (is.null(newdata)) {
          errorCur <- ranger(formula, data, num.trees = ntreeTry, mtry = mtryCur,
                             min.node.size = nsz, replace = FALSE, num.random.splits = nsplit, 
                             num.threads = num.threads, splitrule = splitrule,
                             time.interest = time.interest, seed = seed, 
                             respect.unordered.factors = respect.unordered.factors)$prediction.error
        }
        else {
          errorCur <- ranger(formula, data[trn, , drop = FALSE], num.trees = ntreeTry,
                             mtry = mtryCur, min.node.size = nsz, replace = FALSE, 
                             num.random.splits = nsplit, num.threads = num.threads,
                             splitrule = splitrule, time.interest = time.interest, seed = seed, 
                             respect.unordered.factors = respect.unordered.factors)$prediction.error
        }
        if (trace) {
          cat("nodesize = ", nsz, " mtry =", mtryCur, 
              " error =", paste(100 * round(errorCur, 4), 
                                "%", sep = ""), "\n")
        }
        oobError[[as.character(mtryCur)]] <- errorCur
        Improve <- 1 - errorCur/errorOld
        if (trace) {
          cat(Improve, improve, "\n")
        }
        if (Improve > improve) {
          errorOld <- errorCur
          mtryBest <- mtryCur
        }
      }
    }
    mtry <- sort(as.numeric(names(oobError)))
    err <- unlist(oobError[as.character(mtry)])
    res[[counter1]] <- cbind(nodesize = nsz, mtry = mtry, 
                             err = err)
  }
  if (is.null(res)) {
    stop("NULL results - something is wrong, check parameter (tuning) settings\n")
  }
  res <- do.call(rbind, res)
  res <- res[order(res[, 1], res[, 2]), ]
  rownames(res) <- 1:nrow(res)
  opt.idx <- which.min(res[, 3])
  rf <- NULL
  list(results = res, optimal = res[opt.idx, -3], rf = rf)
}

## Criando a função que será utilizada para o cálculo do integrated Brier Score
ibsRSF <- function(obj, self = 0, ave.flag = 1, package = "randomForestSRC", data = NULL, y = "time_years", event = "falha"){
  if (package == "randomForestSRC") {
    nsample <- obj$n
    time <- obj$time.interest
    ntime <- length(time)
    
    deadStatus <- obj$yvar[,2]
    deadTime <- obj$yvar[deadStatus==1,1]
    censorTime <- obj$yvar[deadStatus==0,1]
    
    if(self==0){
      survP <- obj$survival.oob
    }
    else{
      if(self==1){
        survP <- obj$survival
      }}
    
  } else if (package == "ranger") {
    nsample <- obj$num.samples
    time <- obj$unique.death.times
    ntime <- length(time)
    
    deadStatus <- data[[event]]
    deadTime <- data[[y]][deadStatus==1]
    censorTime <- data[[y]][deadStatus==0]
    
    survP <- obj$survival
  }
  
  if(length(survP) == (nsample*ntime)){
    
    tmp <- matrix(survP, ncol=ntime)
    tmp1 <- tmp[deadStatus==1, ]
    tmp0 <- tmp[deadStatus==0, ]
    bs1 <- matrix(nrow=nrow(tmp1), ncol=ntime)
    bs0 <- matrix(nrow=nrow(tmp0), ncol=ntime)
    
    # go through dead 
    for(si in 1:nrow(tmp1)){
      coreBefore <- c(1:ntime)[ time < deadTime[si] ]
      coreAfter <- c(1:ntime)[ time >= deadTime[si] ]
      bs1[si, coreBefore] <- (1-tmp1[si,coreBefore])^2
      bs1[si, coreAfter] <- tmp1[si,coreAfter]^2
    }
    
    # go through censored
    for(si in 1:nrow(tmp0)){
      coreBefore <- c(1:ntime)[ time < censorTime[si] ]
      coreAfter <- c(1:ntime)[ time >= censorTime[si] ]
      bs0[si, coreBefore] <- (1-tmp0[si,coreBefore])^2
      # it should not be zero, but unknown (not contributing)
      # bs0[si, coreAfter] <- 0
      bs0[si, coreAfter] <- NA
    }
    
    bs <- rbind(bs1, bs0)
    ibs.time1 <- apply(bs1, 2, mean)
    ibs.time0 <- apply(bs0, 2, function(x){ mean( sort(x))  })
    ibs.time <- apply(bs, 2, function(x){ mean( sort(x))  } )
    ibs.time.all <- rbind(ibs.time1,ibs.time0,ibs.time)
    
    if(ave.flag==1){
      aveibs <- mean(ibs.time.all[3,])
      return(aveibs)
    } else if(ave.flag==0){
      return(ibs.time.all)
    }
    
  }}