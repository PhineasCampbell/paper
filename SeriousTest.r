M<-9
N<-252

X<-read.table("FX_Data_Tab_Delimited.txt", header=TRUE)
S<-cov(X)
EG<-eigen(S)
L<-EG$values
H<-EG$vectors



for(m in 1:9){
    U<-prod(L[1:m])
    V<-(((1/m)*sum(L[1:m]))^m)
    V0<-U/V
    stat<--1*(N-((2*m^2+m+2)/(6*m)))*log(V0)
    df<-0.5*(m+2)*(m-1)
    tv<-pchisq(df, stat)
    print(tv)
}

