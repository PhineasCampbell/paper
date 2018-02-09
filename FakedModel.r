# Build the data with which we will work

library(MASS)
set.seed(12345)
Mu<-c(0,0,0,0,0,0,0,0,0)
N<-252





#Import the data
X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S)
L<-EG$values
H<-EG$vectors

# Reform the covariance matrix
L_new<-c(0.024,0.00836,0.0051,0.00446,0.00324,0.002873,0.002692,0.002064,0.001007)
S_new<-H%*%diag(L_new)%*%t(H)
X_new<-MASS::mvrnorm(n=N, mu=Mu, Sigma=S_new)
S_new<-cov(X_new)
EG_new<-eigen(S_new)
L<-EG_new$values
print('Top down')
for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}
print('bottom up')
for(m in 9:1){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}


