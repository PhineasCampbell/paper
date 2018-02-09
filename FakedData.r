# This fakes the FX data so as to get two 0 latent roots
# We test the null that the latent roots are equal


library(MASS)
set.seed(12345)
Mu<-c(0,0,0,0,0,0,0,0,0)
N<-252
# Firstly we do the experiment with a well behaved distribution: IN(0,I)
Sig<-diag(c(1,1,1,1,1,1,1,1,1))
X_hat<-MASS::mvrnorm(n=N, mu=Mu, Sigma=Sig)
S_hat<-cov(X_hat)
PC_hat<-eigen(S_hat)
LR<-PC_hat$values
print(LR)
LR<-sort(LR)
for(m in 2:9){
    U<-prod(LR[1:m])
    V<-(((1/m)*sum(LR[1:m]))^m)
    V0<-prod(LR[1:m])/(((1/m)*sum(LR[1:m]))^m)
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


# Now we do the experiment on the FX data
# Load the FX data to get the covariance matrix
X<-(read.table("FX_Data_Tab_Delimited.txt", header=TRUE))
S_hat<-cov(X)
PC_hat<-eigen(S_hat)
LR<-PC_hat$values
print(LR)
LR<-sort(LR)

for(m in 2:9){
    U<-prod(LR[1:m])
    V<-(((1/m)*sum(LR[1:m]))^m)
    V0<-prod(LR[1:m])/(((1/m)*sum(LR[1:m]))^m)
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

# We now fake data
Sig<-diag(c(1,1,1,1,1,1,1,1,1))
X_hat<-MASS::mvrnorm(n=N, mu=Mu, Sigma=Sig)
X_hat[,8]<-0.5*X_hat[,8]+X_hat[,1]+X_hat[,2]
X_hat[,9]<-0.5*X_hat[,9]+X_hat[,8]+X_hat[,3]
S_hat<-cov(X_hat)
PC_hat<-eigen(S_hat)
LR<-PC_hat$values
print(LR)
LR<-sort(LR)

for(m in 2:9){
    U<-prod(LR[1:m])
    V<-(((1/m)*sum(LR[1:m]))^m)
    V0<-prod(LR[1:m])/(((1/m)*sum(LR[1:m]))^m)
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(0.95, df)
    if(stat>tv){
        res<-"Reject H0"
    }else{
        res<-"Accept H0"
    }
    print(c(stat, tv))
}




