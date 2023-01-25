##Que.1
n=50
lambda=5
x=rpois(n,lambda)
l_=seq(3,7,length.out=500)
like_lamda=-n*l_+sum(x)*log(l_)
par(mfrow=c(1,1))
plot(l_,like_lamda)
abline(v=mean(x),col='green')
abline(v=5,col='red')
max(like_lamda)
which.max(like_lamda)

## Que.2
n=50 
p=0.8
x=rbinom(n,1,p)
p_=seq(0.6,0.9,length.out=500)
like_lamda=sum(x)*log(p_)+(n-sum(x))*log(1-p_)
plot(p_,like_lamda)
abline(v=mean(x),col='red')
abline(v=0.8,col='green')

## Que.3
n=20
mu=10
sigma=1
x=rnorm(n,mu,sigma)
mu_=seq(9,11,length.out=1000)
like_lamda=-1/2*(sum(x^2)-2*mu_*sum(x)+n*mu_^2)
plot(mu_,like_lamda)
abline(v=mean(x),col='red')
abline(v=10,col='yellow')
