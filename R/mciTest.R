mciTest <- function(x,mciset=NULL,cfset=NULL,statistic="DEV",...){
  UseMethod("mciTest")
}

mciTest.table <- function(x, mciset=NULL,cfset=NULL, statistic="DEV", ...){
  mciTest_table(x, mciset, cfset, statistic="DEV", ...)
}

mciTest.data.frame <- function(x, mciset=NULL,cfset=NULL, statistic="DEV", ...){
  mciTest_dataframe(x, mciset, cfset, statistic="DEV",...)
}


print.mciTest <- function(x, ...){
  if (length(x$mset) >= 2){
   ## cat("Testing", x$varNames[1], "_|_", x$varNames[2], "|",x$varNames[-(1:2)],"\n")
    cat("Testing ")
    cat(x$mset[1])
    for(i in 2:length(x$mset)){
      cat("_||_",x$mset[i])
    }
    
    if(!is.null(x$cset)){      
      cat("|", x$cset[1])
      if(length(x$cset) > 1){
        for(i in 2:length(x$cset)){
          cat(",",x$cset[i])
        }  
      }
    }
    cat("\n")  
  }
  cat(sprintf("Statistic (%s): %8.3f df: %s p-value: %6.4f method: %s\n",
              x$statname, x$statistic, x$df, x$p.value, x$method))
}

summary.mciTest <- function(object,...){
  str(object,max.level=1)
  return(invisible(object))
}