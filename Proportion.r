X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S)
L<-EG$values
M<-9
Mu<-c(0,0,0,0,0,0,0,0,0)
N<-252


total_variation<-sum(L)

for(k in 0:M){
   s<-sum(L[(k+1):M])
   h<-s/total_variation
   print(h)
}

Sig<-diag(c(1,1,1,1,1,1,1,1,1))


X_hat<-MASS::mvrnorm(n=N, mu=Mu, Sigma=Sig)
X_hat[,8]<-0.5*X_hat[,8]+X_hat[,1]+X_hat[,2]
X_hat[,9]<-0.5*X_hat[,9]+X_hat[,8]+X_hat[,3]
S_hat<-cov(X_hat)
PC_hat<-eigen(S_hat)
LR<-PC_hat$values
total_variation<-sum(LR)

for(k in 0:M){
   s<-sum(LR[(k+1):M])
   h<-s/total_variation
   print(h)
}
