X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S, "True")
SV<-svd(S)
print(EG$values)
print(SV$d)
print("EG$vectors")
print(EG$vectors)
print("SV$u")
print(SV$u)
print("SV$u")
print(SV$v)
V<-EG$vectors
lam<-EG$values
S_Hat<-V%*%diag(lam)%*%V^(-1)
#print(S_Hat)
#print(S)
d<-t(V)%*%S%*%V
print(diag(d))
print(EG$values)


