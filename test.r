
ITERATIONS = 100

x<-rnorm(100)
y = sum(x)
print(y)


x = matrix(nrow = 100, ncol = 3)
x[,1] = rnorm(100)
x[,2] = rnorm(100)
x[,3] = x[,1] + x[,2]
S = cov(x)
print(y)

res = vector(length = ITERATIONS)

for(i in (1:ITERATIONS)){
    res[i] = rnorm(1)
}

qqnorm(res)

pdf('chart.pdf')

dev.off()







