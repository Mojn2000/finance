library("nloptr")

markovicOptim <- function(tickerNames, minReturn, minExposure){
  print("Running, this might take a while...")
  deciding = T
  while (deciding==T) {
    covMat = matrix(0,length(names),length(names))
    growthRates = rep(0,length(names))
    for (k in 1:(length(names)-1)) {
      for (l in (k+1):length(names)) {
        data1 <- getDat(names[[k]])
        data2 <- getDat(names[[l]])

        data1 = data1[ data1$Date %in% data2$Date , ]
        data2 = data2[ data2$Date %in% data1$Date , ]
        
        fit1 <- lm(data1$logAdjRes ~ data1$Date)
        fit2 <- lm(data2$logAdjRes ~ data2$Date)
        res1 = fit1$residuals
        res2 = fit2$residuals
        
        growthRates[k] = exp(fit1$coefficients[[2]])^365.25
        growthRates[l] = exp(fit2$coefficients[[2]])^365.25
        covMat[k,k] = var(res1)
        covMat[l,l] = var(res2)
        covMat[k,l] = cov(res1,res2)
        covMat[l,k] = cov(res1,res2)
      }
    }
    
    # Objective function
    eval_f <- function(x){
      return ( (-t(x) %*% growthRates)/10000 + t(x) %*% covMat %*% x )
    }
    
    # Inequality constraints
    eval_g_ineq <- function(x) {
      return ( -(t(x) %*% growthRates) + minReturn )
    }
    
    # Equality constraints
    eval_g_eq <- function(x){
      return ( sum(x) - 1 )
    }
    
    # Lower and upper bounds
    lb <- rep(0,length(names))
    ub <- rep(1,length(names))
    
    # Initial values
    x0 <- rep(0,length(names))
    
    # Set optimization options.
    local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-8 )
    opts <- list( "algorithm"= "NLOPT_GN_ISRES",
                  "xtol_rel"= 1.0e-8,
                  "maxeval"= 1000,
                  "local_opts" = local_opts,
                  "print_level" = 0 )
    
    res <- nloptr ( x0 = x0,
                    eval_f = eval_f,
                    lb = lb,
                    ub = ub,
                    eval_g_ineq = eval_g_ineq,
                    eval_g_eq = eval_g_eq,
                    opts = opts
    )
    print("Don't worry, I'm still alive :)")
    if (sum(res$solution < minExposure) > 0) {
      names = names[!(res$solution == min(res$solution))]
    }else {
      deciding = F
    }
  }
  elm1 = list( "Composition", names, res$solution )
  elm2 = list( "Variance", sqrt(t(res$solution) %*% covMat %*% res$solution))
  elm3 = list( "AROR", (res$solution %*% growthRates) )
  return( list( elm1, elm2, elm3 ) )
}