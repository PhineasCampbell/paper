X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S, "True")
EG.LR<-EG$values
EG.V<-EG$vectors
PR<-prcomp(X)
PR.LR<-PR$sdev
PR.ROT<-PR$rotation
PI<-princomp(X)


print("EG values")
print(EG.LR)
print("PR Values")
print(PR.LR)
print("PI sdev")
print(PI$sdev)
print("EG.V")
print(EG.V)
print("PR.ROT")
print(PR.ROT)
print("PI$loadings")
print(PI$loadings)
