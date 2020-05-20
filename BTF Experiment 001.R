

# Want to build a classifier (NN) to discriminate between 
# f(x) = 3*x^2     x in [0,1] and
# g(x) = 2*x       x in [0,1]

require(mlbench)
require(deepnet)
require(neuralnet)
require(pROC)
require(schumaker)
require(ConSpline)


#require(h2o)
#localH2O = h2o.init(ip="localhost", port = 54321, 
#                    startH2O = TRUE, nthreads=-1)


simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


H <- function(n,alpha){
  runif(n)^(1/(alpha+1))
}

S <- function(x,alpha){
  1-x^(alpha+1)
}


#############
set.seed(1)
n=10000
alpha=4
beta=1

x1=H(n,alpha)
x2=H(n,beta)
x = c(x1,x2)
y = c(rep(1,n),rep(0,n))
dat = data.frame(x,y,stringsAsFactors = FALSE)

xat=seq(0,1,.01)
u=S(xat,alpha)
v=S(xat,beta)

# deepnet

y = as.numeric(y)
x = matrix(x,ncol=1)


nn <- nn.train(x, y, hidden = c(5), activationfun = "sigm")
yy = nn.predict(nn, x)
print(head(yy))
auc(as.numeric(y),as.numeric(yy))


#s1 = yy[y==1]
#s0 = yy[y==0]


out=simple_roc(y,yy)
plot(out$FPR,out$TPR,type="l")
lines(v,u,lwd=3,col=rgb(1,0,0,0.2))
abline(0,1,lty=2)


# Schumaker
ord=order(out$FPR)
aa = out$FPR[ord]
bb = out$TPR[ord]
junk=aggregate(bb~aa,FUN=mean)
aa = junk[,1]
bb = junk[,2]

res = Schumaker(aa,bb)
r1 = res$Spline(xat)
s1 = res$DerivativeSpline(xat)
t1 = res$SecondDerivativeSpline(xat)

lines(xat,r1,col=4,lwd=4)
lines(xat,s1,col=5,lwd=2)
lines(xat,t1,col=6,lwd=2)

LR = function(x,alpha,beta){
  ((alpha+1)/(beta+1))*x^(alpha-beta)
}

ans = conspline(bb,aa,type=7)
bbhat = ans$muhat
ss = ans$fslope
lines(aa,bbhat,lwd=5,col=rgb(1,0,0))
plot(aa,slopes,type="l")

#######  next method  #######


df = data.frame(x,y)
nn = neuralnet(y~x,data=df,hidden = 5)
yy = nn$net.result[[1]]

yhat = (yy> median(yy))*1
table(y,yhat)

s1 = yy[y==1]
s0 = yy[y==0]


out=simple_roc(y,yy)
plot(out$FPR,out$TPR,type="l")
abline(0,1,lty=2)

auc(as.numeric(y),as.numeric(yy))

