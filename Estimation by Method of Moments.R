##Que.1
k=100
n=6
p=0.86
x=rbinom(k,n,p)
x
#moment estimate of p when n is known
p_hat=mean(x)/n
#moment estimate of n when p is known
n_hat=mean(x)/p
##moment estimate of n & p when both are unknown
p_hat1=1-var(x)/mean(x)
n_hat1=mean(x)/p_hat1

##Que.2
n=100
alpha=7
beta=4
x=rgamma(n,7,1/4)
#method of moment estimate of alpha when beta is known
alpha_hat=mean(x)/beta
#method of moment estimate of beta when alpha is known 
beta_hat =mean(x)/alpha
##method of moment estimate when both parameters are unknown
alpha_hat1=mean(x)^2*n/((n-1)*var(x))
beta_hat1=((n-1)*var(x))/(mean(x)*n)

##Que.3
x=c(0.133904, 0.224011, 0.062282, 0.122690, 0.051213, 0.620577, 0.000650, 0.271087, 0.827109, 0.929479,0.561905, 0.160865, 0.362565, 0.001039, 0.883135, 0.393477, 0.990179, 0.045737, 0.243155, 0.080471)
n=length(x)
m=mean(x)
var_=var(x)*(n-1)/n
#method of moment estimate of a and b
a_hat=m*(m-1/n*sum(x^2))/var_
b_hat=(1-m)*(m-1/n*sum(x^2))/var_

