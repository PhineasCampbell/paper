# Load the data
X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
print(S)
PR<-prcomp(X)
rot<-PR$rotation
print(rot)
sd<-PR$sdev
m_rot = data.matrix(rot)
print('m_rot:')
print(m_rot)
lambda <- t(m_rot)%*%S%*%m_rot
print(lambda)
print(sd)
print(sd^2)
print(diag(lambda))







