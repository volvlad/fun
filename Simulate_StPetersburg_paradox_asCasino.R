entry_fee = 20
payoffs<-c()
for (i in 1:10000) {
  k<-1
  while (sample(c(0,1),1)>0) {
    k=k+1
  }
  payoffs<-c(payoffs, entry_fee - 2**k)
}
summary(payoffs)
sum(payoffs)

