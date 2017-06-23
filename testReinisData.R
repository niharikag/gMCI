library(gMCI)
library(gtools)
data(reinis)
data.reinis = data.frame(reinis)
colnames(data.reinis) = c("a","b","c","d","e","f","count")

#Step1: test for complete independence
mciTest.data.frame(data.reinis)
null.model = paste(c("a","b","c","d","e","f"), collapse=" + ")
prev.model = paste(c("a","b","c","d","e","f"), collapse=" + ")
print("step 1")
print(prev.model)
cat("\n") 
#Step2: Look for the most significant edge
cand.set = c("a","b","c","d","e","f")
cand.edge = combinations(n=6,r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(prev.model,"+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 2")
edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#print("Result of step 2")
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)

#Step3: First test for MCI
mciTest.data.frame(data.reinis, c("a","b","d","e" , "f"))
mciTest.data.frame(data.reinis, c("a","c","d","e", "f"))
#Look for the most significant edge
cand.set = c("a","b","d","e","f")
cand.edge = combinations(n=5,r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f+b*c, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(paste(c("a","b","c","d","e","f"), collapse=" + "),
                                       "+b*c+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 3")

edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)

#Step4: MCI test
mciTest.data.frame(data.reinis, c("a","b","d", "f"))
mciTest.data.frame(data.reinis, c("a","c","d","e", "f"))
#Look for the most significant edge
cand.set = c("a","c","d","e","f")
cand.edge = combinations(n=5,r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f+b*c+b*e, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(paste(c("a","b","c","d","e","f"), collapse=" + "),
                                       "+b*c+b*e+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 4")
edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)



#Step5: MCI test
mciTest.data.frame(data.reinis, c("a","b","d" ,"f"))
mciTest.data.frame(data.reinis, c("a", "d","e", "f"))
mciTest.data.frame(data.reinis, c("c","d", "e","f"))
#Look for the most significant edge
cand.set = c("a","b","d","f")
cand.edge = combinations(n= length(cand.set),r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f+a*c+b*c+b*e, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(paste(c("a","b","c","d","e","f"), collapse=" + "),
                                       "+a*c+b*c+b*e+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 5")
edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)

#Step6: MCI test
mciTest.data.frame(data.reinis, c("a","b" ,"f"))
mciTest.data.frame(data.reinis, c("b", "d", "f"))
mciTest.data.frame(data.reinis, c("a", "e","f"))
#Look for the most significant edge
cand.set = c("c","d","e","f")
cand.edge = combinations(n= length(cand.set),r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f+a*c+a*d+b*c+b*e, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(paste(c("a","b","c","d","e","f"), collapse=" + "),
                                       "+a*c+a*d+b*c+b*e+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 6")
edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)


#Step7: MCI test
mciTest.data.frame(data.reinis, c("c","d" ,"f"))
mciTest.data.frame(data.reinis, c("c", "e", "f"))
mciTest.data.frame(data.reinis, c("a", "e","f"))
#Look for the most significant edge
cand.set = c("a","e","f")
cand.edge = combinations(n= length(cand.set),r=2,v=cand.set)
lrt.list = rep(0, nrow(cand.edge))
lrt.diff = rep(0, nrow(cand.edge))
p.value = rep(0, nrow(cand.edge))

fit.base = loglm(formula = count ~ a+b+c+d+e+f+a*c+a*d+b*c+b*e+d*e, data = data.reinis)
lrt.base = fit.base$lrt

for(i in 1: nrow(cand.edge))
{
  loglm.formula = paste("count", paste(paste(c("a","b","c","d","e","f"), collapse=" + "),
                                       "+a*c+a*d+b*c+b*e+d*e+", paste(cand.edge[i,1],"*",cand.edge[i,2])), sep=" ~ ")
  fit.lm = loglm(formula = as.formula(loglm.formula), data = data.reinis)
  lrt.list[i] = fit.lm$lrt
  lrt.diff[i] = lrt.base - fit.lm$lrt
  p.value[i] = pchisq(lrt.diff[i], 1, lower.tail = FALSE)
}
print("Result of step 7")
edge.indx = which(lrt.diff==max(lrt.diff))
print("most significant edge")
print(cand.edge[edge.indx,])
print("current model")
curr.model = paste(prev.model,"+", paste(cand.edge[edge.indx,1],"*",cand.edge[edge.indx,2]))
print(curr.model)
cat("\n")
prev.model= curr.model
#round(lrt.list,4)
#round(lrt.diff,4)
#round(p.value,4)
