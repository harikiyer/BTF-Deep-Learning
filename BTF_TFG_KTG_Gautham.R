
require(igraph)

checktfg <-function(x){
  diag(x)=0
  x=pmax(x,t(x))
  sum(diag(x%*%x%*%x>0))
}


# Base function
ktg <- function(n=25,k=1,p=0.7, PLOT="FALSE"){
m = sample((5*n):(11*n),1)
t1 = ceiling(n/2)
t2 = t1:(t1-2)
n1 = sample(t2,1)
n2 = n-n1
types = c(rep(0,n1),rep(1,n2))
c1 = sample(1:n1,m,replace=TRUE)
c2 = sample((n1+1):n,m,replace=TRUE)
temp0 = cbind(c1,c2)
ss=NULL
for (i in 1:k){
a1=sample(1:n1,2)
a2=sample((n1+1):n,1)
ss = rbind(ss,c(a1[1],a1[2]),c(a1[1],a2[1]),c(a1[2],a2[1]))
}
temp=unique(rbind(temp0,ss))
edges=c(t(temp))

g <- make_graph(edges,directed=FALSE)
A = 1*(as.matrix(as_adjacency_matrix(g))>=1)
perm = sample(1:n,n)
B = A[perm,perm]
#image(B)
for (i in 1:n){
  for (j in i:n){
    z=sample(1:2,1)
    if (z==1&B[i,j]==1) B[i,j]=rbinom(1,1,p) 
    if (z==2&B[i,j]==1) B[j,i]=rbinom(1,1,p)
    if (i==j & z==1) B[i,j]=rbinom(1,1,.5)
  }
}
if (PLOT == "TRUE") image(B,axes=FALSE,asp=1)
ntri = checktfg(B)%%2
return(list(B,ntri))
}


# Generate as many examples as you want. Last column is class label. The rest is 25 by 25 image flattened.
set.seed(1234)
nexamples = 1000
dat=NULL
for (i in 1:nexamples){
  print(i);flush.console()
  out=ktg()
  row = c(out[[1]],out[[2]])
  dat=rbind(dat,row)
}





