## ########################################################
##
## Testing for mutual condional independence in a contingency table
## test based on either deviance or Pearsons chi square
## <x>  : table
## <miset>: NULL, a vector of factors to be tested for 
##          MCI(mutual conditional independence)
## <cfset>: NULL, a vector of conditioning factors
## <statistic>: DEV, deviance or chi-squae based test
## ########################################################
mciTest_table <- function(x, miset=NULL, cfset=NULL, statistic="DEV")
{
  statistic <- match.arg(toupper(statistic), c("DEV",   "X2"))
  
  if (is.null(miset)){
    #testing for mutual independence amongst factors of contingency table, 
    # as miset and conditioning set is NULL    
    miset = names(dimnames(x))
  } 
  else {
    if (is.null(cfset) || (cfset[1] == "REST")){
      #testing for mutual independence amongst factors in miset, 
      # as conditioning set is NULL , it will be conditioned on the rest     
      set1 <- names(dimnames(x))
      cfset = setdiff(set1,miset)
      
      if(length(cfset) == 0){
        cfset = NULL
      }
    }
    else{       
      #testing for mutual conditional independence amongst factors in miset, 
      # conditioned on cf set, marginalizing over remaining factors
      x   <- tableMargin(x, c(miset,cfset))       
    }            
  }
  
  if (is.null(cfset)){
    dn = dim(x)
    
    tab1 =  tableMargin(x, miset[1])
    dof = dn[1]
    for(i in 2:length(miset)){
      tab2 <-   tableMargin(x, miset[i])
      tab1 = tableOp(tab1, tab2)
      dof = dof+dn[i]
    }
    
    fit.table <- tablePerm(tab1, miset)/(sum(x))^(length(miset)-1)
    dof = prod(dn) - dof + (length(miset)-1) #compute DOF
  } 
  else{
    vn <- names(dimnames(x))
    
    margin1 = tableMargin(x, miset)
    margin2 = tableMargin(x, cfset)
    dm1 = dim(margin1)
    dm2 = dim(margin2)
    
    marTab1 =  tableMargin(x, c(miset[1],cfset))
    
    cf.dof = prod(dm2)
    mi.dof = dm1[1]*cf.dof
    for(i in 2:length(miset)){
      marTab2 <-   tableMargin(x, c(miset[i],cfset))    
      marTab1 = tableOp(marTab1, marTab2)      
      marTab1 = tableOp(marTab1, margin2,"/")
      mi.dof = mi.dof+(dm1[i]*cf.dof)
    }
     
    fit.table <- tablePerm(marTab1, vn)
    cf.dof = cf.dof*(length(miset)-1)
    dof = prod(dim(x)) - mi.dof+cf.dof #compute DOF
  }
  
  
  #Evaluate test statistic
  if (statistic=="DEV"){             ## Deviance
    testStat  <- 2* x * log(x/fit.table)
  } else {                           ## Pearson chi square
    testStat <- (x-fit.table)^2/fit.table
  }
  testStat[!is.finite(testStat)] <- 0
  testSum <- sum(testStat)
  
  pValue   <- 1-pchisq(testSum, dof)
  
  ans <- list(statistic=testSum, p.value=pValue, df=dof, 
              statname=statistic, method="CHISQ", mset = miset, cset = cfset)
  class(ans) <- "mciTest"
  ans
}