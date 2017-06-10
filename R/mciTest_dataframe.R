## ########################################################
##
## Testing for mutual condional independence in a dataframe
## <x>  : dataframe
## <miset>: NULL, a vector of variables to be tested for 
##          MCI(mutual conditional independence)
## <cfset>: NULL, a vector of conditioning variable
## <statistic>: DEV, deviance or chi-squae based test
## ########################################################
mciTest_dataframe <- function(x, miset=NULL,cfset=NULL,statistic="DEV",...){
 
  set <- names(x)
  wdata       <- x[,set]
  varTypes    <- uniquePrim(unlist(lapply(wdata, class)))
  has.factor  <- "factor" %in% varTypes
  
  if (has.factor){
    mciTest_table(xtabs(count~., data=x),miset=miset,cfset=cfset,...)
  } else {  
    stop("NOt a contingency table...\n")
  }    
  
  ##convert a data frame to contingency table and then call mciTest_table
}