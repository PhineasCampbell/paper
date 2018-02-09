

# This is some R
library(MASS)
set.seed(12345)
M<-8
Sig<-diag(c(1,1,1,1,1,1,1,1))
Mu<-c(0,0,0,0,0,0,0,0)
N<-252
X<-MASS::mvrnorm(n=N, mu=Mu, Sigma=Sig)
X[,7]<-X[,7]*0.05+X[,1]+X[,2]
X[,8]<-X[,8]*0.05+X[,7]+X[,3]
X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S)
L<-EG$values
L<-sort(L)
print(L)

for(m in 1:8){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-prod(L[1:m])/(((1/m)*sum(L[1:m]))^m)
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(0.95, df)
    if(stat>tv){
        res<-"Reject H0"
    }else{
        res<-"Accept H0"
    }
    print(c(stat, tv, res))
}





