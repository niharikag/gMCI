## ########################################################
##
## Forward Model Selection based mutual condional independence 
## test based on deviance 
## <x>  : table
## ########################################################
mciModel_table <- function(x, statistic, ...)
{
  if(statistic == "DEV"){
    ans <- .mciModel.dev(x, ...)
  }
  else{
    ans <- .mciModel.aic(x, ...)
  }
  
  return(ans)
}

.mciModel.dev <- function(x, ...){
  varNames = names(dimnames(x))
  misTemp = list(varNames) #temp Maximal Ind Set
  misFinal = NULL  #Final Maximal Ind Set
  alpha = 0.05
  curMod.terms = varNames #initialize current interaction terms to main effects 
  curMod.lrt = NULL #current model fit
  curMod.aic = 0
  
  while(length(misTemp)>0){
    mciSet = unlist(misTemp[1])
    
    if(length(mciSet)< 2 ){
      #singleton 
      misFinal = union(misFinal,misTemp[1])
      misTemp = misTemp[-1]
    }
    else{
      mc = mciTest(x,mciSet)
      
      if(is.null(curMod.lrt)){
        curMod.lrt = mc$statistic
        curMod.df = mc$df
      }
      
      if(mc$p.value >= alpha){
        ## Mutual conditional relation holds among MIS, remove it from tempMIS
        misFinal = union(misFinal,misTemp[1])
        misTemp = misTemp[-1]
      }
      else{
        edgeSet = .vec2pairs(unlist(misTemp[1]))
        devDiff = rep(0,length(edgeSet))
        df = rep(0,length(edgeSet))
        lrt =  rep(0,length(edgeSet))
        aic = rep(0,length(edgeSet))
        for(i in 1:length(edgeSet)){
          edge = unlist(edgeSet[i])
          interact = paste0(edge[1],"*",edge[2])
          tempTerms = union(curMod.terms,interact)
          
          #fm = as.formula(paste("~",paste(tempTerms,collapse="+")))        
          #lm = loglm(fm,x)
          maxCls = .terms2MaxCliques(tempTerms)
          lm = loglin(x, maxCls, print=FALSE,fit = TRUE)
          aicInfo = computAIC(x,maxCls,lm$fit)
          aic[i] = aicInfo$aic
          lrt[i] =  lm$lrt 
          devDiff[i] = curMod.lrt- lm$lrt #deviance difference
          df[i] =  lm$df
        }      
        indx = which.max(devDiff)
        p.value = 1-pchisq(devDiff[indx],(curMod.df - df[indx]) )
       # if(p.value < alpha){
          curMod.lrt = lrt[indx]
          curMod.df = df[indx]
          edge = unlist(edgeSet[indx])
          misTemp = .add.term(misTemp,edge)
          
          interact = paste0(edge[1],"*",edge[2])
          curMod.terms = union(curMod.terms,interact)
          curMod.aic = aic[indx]
       # }
      #  else{
      #    misFinal = union(misFinal,misTemp[1])
       #   misTemp = misTemp[-1]
     #   }            
      }
    ##fixme 
    }
  }
  #model = (paste("~",paste(curMod.terms,collapse="+")))
  model =  .terms2MaxCliques(curMod.terms)
  adMat = .terms2adjMat(curMod.terms)
  p.value = 1-pchisq(curMod.lrt,curMod.df)
  
  aic = curMod.aic
  ans <- list(model=model, statistic = "DEV", dev=curMod.lrt, df=curMod.df, 
              p.value = p.value, method="CHISQ",adjMat = adMat,aic = aic)
  class(ans) <- "mciModel"
  ans
  
}

.mciModel.aic <- function(x,...){
  varNames = names(dimnames(x))
  misTemp = list(varNames) #temp Maximal Ind Set
  misFinal = NULL  #Final Maximal Ind Set
  curMod.terms = varNames #initialize current interaction terms to main effects 
  curMod.aic = NULL #current model fit
  alpha = 0.05
  sat.aic = comput.sat.AIC(x)
  
  while(length(misTemp)>0){
    mciSet = unlist(misTemp[1])
    #mc.aic = mci.aic(x,mciSet)
    if(length(mciSet)< 2 ){
      #singleton 
      misFinal = union(misFinal,misTemp[1])
      misTemp = misTemp[-1]
    }
    else{
      mc = mciTest(x,mciSet)
      
      if(is.null(curMod.aic)){
        curMod.aic = sat.aic
        #curMod.df = mc$df
      }
      
      #if(mc.aic <= curMod.aic){
      if(mc$p.value >= alpha){
        ## Mutual conditional relation holds among MIS, remove it from tempMIS
        misFinal = union(misFinal,misTemp[1])
        misTemp = misTemp[-1]
      }
      else{
        edgeSet = .vec2pairs(unlist(misTemp[1]))
        #devDiff = rep(0,length(edgeSet))
        #df = rep(0,length(edgeSet))
        aic =  rep(0,length(edgeSet))
        for(i in 1:length(edgeSet)){
          edge = unlist(edgeSet[i])
          interact = paste0(edge[1],"*",edge[2])
          tempTerms = union(curMod.terms,interact)
          
          #fm = as.formula(paste("~",paste(tempTerms,collapse="+")))        
          #lm = loglm(fm,x)
          maxCls = .terms2MaxCliques(tempTerms)
          lm = loglin(x,maxCls,fit=TRUE,print=FALSE)
          
          aicInfo = computAIC(x,maxCls,lm$fit)
          aic[i] =  aicInfo$aic
          #devDiff[i] = curMod.lrt- lm$lrt #deviance difference
          #df[i] =  lm$df
        }      
        indx = which.min(aic)
        
        curMod.aic = aic[indx]
        #curMod.df = df[i]
        edge = unlist(edgeSet[indx])
        misTemp = .add.term(misTemp,edge)
        
        interact = paste0(edge[1],"*",edge[2])
        curMod.terms = union(curMod.terms,interact)          
      }
      ##fixme 
    }
    
  }
  model =  .terms2MaxCliques(curMod.terms)
  adMat = .terms2adjMat(curMod.terms)
  aic = curMod.aic
  
  ans <- list(model=model, statistic = "AIC", aic = aic, adjMat = adMat)
  class(ans) <- "mciModel"
  ans
}

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
mci.aic <- function(x, miset=NULL)
{
  set1 <- names(dimnames(x))
  cfset = setdiff(set1,miset)
       
  if(length(cfset) == 0){
    cfset = NULL
  }  
  
  if (is.null(cfset)){
    dn = dim(x)
    
    tab1 =  tableMargin(x, miset[1])
    dof = dn[1]
    maxCls = lapply(set1,function(ll){ll})#independence model
    
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
    
    maxCls = lapply(miset, function(ll) {
      union(ll, cfset) })
    
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
  
  ans = computAIC(x,maxCls,fit.table)
 
  ans$aic
}

