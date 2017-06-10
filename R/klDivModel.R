mciModel.kl <- function(x,...){
  varNames = names(dimnames(x))
  misTemp = list(varNames) #temp Maximal Ind Set
  misFinal = NULL  #Final Maximal Ind Set
  curMod.terms = varNames #initialize current interaction terms to main effects 
  curMod.kl = NULL #current model fit
  #alpha = 0.05
  #sat.aic = comput.sat.AIC(x)
  #curr.kl = compute.kl(x)
  while(length(misTemp)>0){
    mciSet = unlist(misTemp[1])
    #mc.aic = mci.aic(x,mciSet)
    if(length(mciSet)< 2 ){
      #singleton 
      misFinal = union(misFinal,misTemp[1])
      misTemp = misTemp[-1]
    }
    else{
      
      
      
      
      {
        if(is.null(curMod.kl)){
          #curMod.kl = compute.indp.kl(x) #compute inependence model kl
          mc.kl = compute.indp.kl(x)
          #curMod.df = mc$df
          curMod.kl = 0
        }
        else{
          mc.kl = mci.kl(x,mciSet)
        }
        
        if(mc.kl <= curMod.kl){
        #if(mc$p.value >= alpha){
          ## Mutual conditional relation holds among MIS, remove it from tempMIS
          misFinal = union(misFinal,misTemp[1])
          misTemp = misTemp[-1]
        }
        else{
          edgeSet = .vec2pairs(unlist(misTemp[1]))
          #devDiff = rep(0,length(edgeSet))
          #df = rep(0,length(edgeSet))
          kl =  rep(0,length(edgeSet))
          for(i in 1:length(edgeSet)){
            edge = unlist(edgeSet[i])
            interact = paste0(edge[1],"*",edge[2])
            tempTerms = union(curMod.terms,interact)
            
            #fm = as.formula(paste("~",paste(tempTerms,collapse="+")))        
            #lm = loglm(fm,x)
            maxCls = .terms2MaxCliques(tempTerms)
            lm = loglin(x,maxCls,fit=TRUE,print=FALSE)
            
            kl[i] = compute.kl(x,lm$fit)
            
            #devDiff[i] = curMod.lrt- lm$lrt #deviance difference
            #df[i] =  lm$df
          }      
          indx = which.min(kl)
          
          curMod.kl = kl[indx]
          #curMod.df = df[i]
          edge = unlist(edgeSet[indx])
          misTemp = .add.term(misTemp,edge)
          
          interact = paste0(edge[1],"*",edge[2])
          curMod.terms = union(curMod.terms,interact)          
        }
      }
      ##fixme 
    }
    
  }
  model =  .terms2MaxCliques(curMod.terms)
  adMat = .terms2adjMat(curMod.terms)
  kl = curMod.kl
  
  ans <- list(model=model, statistic = "AIC", kl = kl, adjMat = adMat)
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
mci.kl <- function(x, miset=NULL)
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
  
  ans = compute.kl(x,fit.table)
  
  ans
}
