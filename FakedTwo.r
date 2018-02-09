# Take the FX data build a data set with all latent roots equal and
# with all latent roots different then calculate the probabilities
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
print("FX Data")
for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}




# Reform the covariance matrix
print("Reformed Data")
L_new<-L
S_new<-H%*%diag(L_new)%*%t(H)
X_new<-MASS::mvrnorm(n=N, mu=Mu, Sigma=S_new)
S_Rnew<-cov(X_new)
EG_Rnew<-eigen(S_new)
L<-EG_Rnew$values

for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}

#print("All the same")
L_new<-c(1,1,1,1,1,1,1,1,1)
S_new<-H%*%diag(L_new)%*%t(H)
X_new<-MASS::mvrnorm(n=N, mu=Mu, Sigma=S_new)
S_new<-cov(X_new)
EG_new<-eigen(S_new)
L<-EG_new$values
for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    #print(tv)
}

print("Faked Data")
L_new<-c(0.024452510, 0.008358804, 0.005064643, 0.004461790, 0.003243962, 0.002873042, 0.002692412, 0.002064189, 0.001007395)
S_new<-H%*%diag(L_new)%*%t(H)
X_new<-MASS::mvrnorm(n=N, mu=Mu, Sigma=S_new)
S_new<-cov(X_new)
EG_new<-eigen(S_new)
L<-EG_new$values
for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}



