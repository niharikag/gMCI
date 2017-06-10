mciModel <- function(x, statistic="DEV", ...){
  UseMethod("mciModel")
}

mciModel.table <- function(x, statistic="DEV", ...){
  mciModel_table(x, statistic, ...)
}

mciModel.data.frame <- function(x, statistic="DEV", ...){
  set <- names(x)
  wdata       <- x[,set]
  varTypes    <- uniquePrim(unlist(lapply(wdata, class)))
  has.factor  <- "factor" %in% varTypes
  
  
  if (has.factor){
    mciModel_table(xtabs(count~., data=x), statistic,...)
  } else {  
    stop("NOt a contingency table...\n")
  }    
}


print.mciModel <- function(x, ...){
  
    #cat("Modeling \n")  
  #model = unlist(x$model )
  cat("Model:\n")  
  cat(sprintf( "%s", x$model ))
  if(x$statistic == "DEV"){
    cat(sprintf("\n Statistic (%s): %8.3f df: %s p-value: %6.4f method: %s\n",
               x$statistic, x$dev, x$df, x$p.value, x$method))
  }
  else{
    cat(sprintf("\n Statistic (%s): %8.3f ",
                x$statistic, x$aic))
  }
}

summary.mciModel <- function(object,...){
  str(object,max.level=1)
  return(invisible(object))
}