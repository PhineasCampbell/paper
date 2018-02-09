X<-data.matrix(read.table("FX_Data_Tab_Delimited_No_head.txt", header=FALSE))
#X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
M<-mean(X)
Sig<-cov(X)
EG<-eigen(Sig, TRUE)
lambda<-EG$values
H<-EG$vectors
L<-t(H)%*%Sig%*%H
print(diag(L))
print(lambda)
U<-t(H)%*%X
U<-H%*%t(X)
#(9*9)*



