# Take the FX data calculate the covariance matrix, get the eigen values, vectors
# then rebuild the covariance matrix

X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
SV = svd(S)
#print(SV)
EG<-eigen(S)
#print(EG)
print("SVD")
print(SV$d)
print("Eigen")
print(EG$values)
print("SVD")
print(SV$u)
print(SV$v)
print("EG")
print(EG$vectors)
S_H = SV$u%*%diag(SV$d)%*%t(SV$v)
print("SV$u%*%diag(SV$d)%*%t(SV$v)")
print(S_H)
print(S)
D_H = t(SV$u)%*%S%*%SV$v
print(diag(D_H))
print(SV$d)





