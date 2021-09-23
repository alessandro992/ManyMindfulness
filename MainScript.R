
library(ggplot2)
library(BayesFactor)
f=function(n){
  
  id=seq(1,n,1)
  Id=c(id,id,id,id,id)
  
  v1=rnorm(n,21.88,4.78)
  v2=rnorm(n,19.85,5.48)
  v3=rnorm(n,20.5,4.5)
  v4=rnorm(n,20.5,4.5)
  v5=rnorm(n,18,4.5)
  v=c(v1,v2,v3,v4,v5)
  
  treatA=rep("mindful breathing",n)
  treatB=rep("loving kindness",n)
  treatC=rep("mindful movements",n)
  treatD=rep("body scan",n)
  treatE=rep("active control",n)
  treat=c(treatA,treatB,treatC,treatD,treatE)
  
  data=data.frame(Id,v,treat)
  data$treat=as.factor(data$treat)
  
  return(data)
}
AB=c()
AC=c()
AD=c()
AE=c()
BC=c()
BD=c()
BE=c()
CD=c()
CE=c()
DE=c()

AB[1]=1
AC[1]=1
AD[1]=1
AE[1]=1
BC[1]=1
BD[1]=1
BE[1]=1
CD[1]=1
CE[1]=1
DE[1]=1
n=800
for (i in 2:n){
  d=f(i)
  AB[i]=ttestBF(x = d$v[d$treat=="mindful breathing"], y = d$v[d$treat=="loving kindness"])@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(AB,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful breathing v loving kindness")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  AC[i]=ttestBF(x = d$v[d$treat=="mindful breathing"], y = d$v[d$treat=="mindful movements"])@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(AC,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful breathing v mindful movements")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  AD[i]=ttestBF(x = d$v[d$treat=="mindful breathing"], y = d$v[d$treat=="body scan"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(AD,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful breathing v body scan")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  AE[i]=ttestBF(x = d$v[d$treat=="mindful breathing"], y = d$v[d$treat=="active control"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(AE,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful breathing v active control")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  BC[i]=ttestBF(x = d$v[d$treat=="loving kindness"], y = d$v[d$treat=="mindful movements"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(BC,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="loving kindness v mindful movements")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  BD[i]=ttestBF(x = d$v[d$treat=="loving kindness"], y = d$v[d$treat=="body scan"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(BD,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="loving kindness v body scan")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  BE[i]=ttestBF(x = d$v[d$treat=="loving kindness"], y = d$v[d$treat=="active control"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(BE,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="loving kindness v active control")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  CD[i]=ttestBF(x = d$v[d$treat=="mindful movements"], y = d$v[d$treat=="body scan"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(CD,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful movements v body scan")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  CE[i]=ttestBF(x = d$v[d$treat=="mindful movements"], y = d$v[d$treat=="active control"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(CE,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="mindful movements v active control")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)


n=800
for (i in 2:n){
  d=f(i)
  DE[i]=ttestBF(x = d$v[d$treat=="body scan"], y = d$v[d$treat=="active control"] )@bayesFactor$bf
  
}
id=seq(1,n,1)
d1=data.frame(DE,id)
plot(d1[, 2], d1[, 1], type = "l", xlab = "Sample Size", 
     ylab = "(Log) Bayes Factor", pch = 18, 
     col = "gray48", lwd = 1,main="body scan v active control")
abline(h = 0, lwd = 1)
abline(h = log(6), col = "black", lty = 2, lwd = 1)
abline(h = log(1/6), col = "black", lty = 2, lwd = 1)
