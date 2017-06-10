.splitSet <- function(mset,e){  
  ans = mset
  if(is.element(e[1],mset) && is.element(e[2],mset)){
    i <- match(e,mset)
    ans <- list(mset[-i[2]], mset[-i[1]])
  }
  ans
}

.add.term <-  function(mlist, e){
  idx<-isin(mlist,e,TRUE)
  zzz <- unlist(
    lapply(1:length(mlist), function(i){
      if (idx[i]==1)
        .splitSet(mlist[[i]],e)
      else
        mlist[i]
    }), recursive=FALSE)
  ans <- unique(zzz)
  ans
}

.list2pairs <- function(x){
  if (length(x)>0){
    if (!inherits(x,"list"))
      x <- list(x)
    x <- lapply(x, function(zzz) lapply(combn(zzz,2, simplify=FALSE), sort))
    x <- unlist(x, recursive=FALSE)
  }
  x
}

.vec2pairs <- function(x){
  if (length(x)>2){
    x = combn(x,2, simplify=FALSE)
  }else{
    x= list(x)
  }
  x
}

fitNullModel<- function(x){
  varNames = names(dimnames(x))
  fm = as.formula(paste("~",paste(varNames,collapse="+")))
  ans = loglm(fm,reinis)
  ans
}

.terms2adjMat <- function(terms){
  fm = as.formula(paste("~",paste(terms,collapse="+")))
  um = ug(fm)
  adMat = as.adjMAT(um)
  adMat
}

.terms2MaxCliques <- function(terms){
  fm = as.formula(paste("~",paste(terms,collapse="+")))
  um = ug(fm)
  adMat = as.adjMAT(um)
  mc = maxCliqueMAT(adMat)
  mc$maxCliques
}

.getNumList<- function(x, varNames){
  numList <- lapply(x, function(ll) {
    match(ll, varNames) })
 return(numList)   
}

#compute AIC for a given fit
computAIC <- function(x, maxCls,fit.table){
  varNames = names(dimnames(x))
  iii <- x* fit.table > 0
  logL      <- sum(x[iii] * log(fit.table[iii]/sum(fit.table)))
  
  numList <- lapply(maxCls, function(ll) {
    match(ll, varNames) })
  
  dm = loglinGenDim(numList,x)  
  ans <- list(logL = logL, aic = -2*logL + 2*dm,dim = dm)
  #lgfit$bic       <- -2*lgfit$logL + log(sum(x))*lgfit$df
  return(ans)
}

#compute AIC of saturated model
comput.sat.AIC <- function(x){
  varNames = names(dimnames(x))
  iii <- x > 0
  logL      <- sum(x[iii] * log(x[iii]/sum(x)))
  
  numList <- lapply(list(varNames), function(ll) {
    match(ll, varNames) })
  
  dm = loglinGenDim(numList,x)  
  aic       <- -2*logL + 2*dm
  #lgfit$bic       <- -2*lgfit$logL + log(sum(x))*lgfit$df
  return(aic)
}

#compute KL for a given fit
compute.kl <- function(x, fit.table){
  iii <- x* fit.table > 0
  logL      <- sum(x[iii] * log(x[iii]/fit.table[iii]))
  return(logL)
}

#compute KL of complete Independence model
compute.indp.kl <- function(x){
  varNames = names(dimnames(x))
  maxCls = .terms2MaxCliques(varNames)
  lm = loglin(x,maxCls,fit=TRUE,print=FALSE)
  
  iii <- x > 0
  logL      <- sum(x[iii] * log(x[iii]/lm$fit[iii]))
  
  return(logL)
}