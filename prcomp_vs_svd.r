X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
P<-prcomp(X)
S<-cov(X)
SV<-svd(S)
print("P$sdev")
print(P$sdev)
print("P$sdev^2")
print(P$sdev^2)
print("SV$d")
print(SV$d)
print("P$rotation")
print(P$rotation)
print("SV$u")
print(SV$u)
print("SV$v")
print(SV$v)
