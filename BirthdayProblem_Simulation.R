# number of simulations
M <- 10000
# size of group
N <- 37
#Days
days <- 365*1

##### S-Curve
K <- 500
calc_mean <- rep(0,K)
theor_mean <- rep(0,K)
for (i in 2:K) {
  res <- rep(0,M)
  for (j in 1:M) {
    x <- sample(1:days,i,replace=TRUE)
    res[j]<-ifelse(sum(duplicated(x))>0,1,0)
  }
  calc_mean[i] <- mean(res)
  x <- 1
  for (j in 1:i-1) {
    x <- x*(days-j)/days
  }
  theor_mean[i]<- 1-x
}

plot(theor_mean,main="Theoretical and Simulated S-curves", 
     xlab = "# of people in Group", ylab="Probability", ylim=c(0,1), col="black")
points(calc_mean, type="l", col="red")


outcomes <- rep(0,M)
for (i in 1:M) {
  x = sample(1:days,N, replace = TRUE)
  outcomes[i] <- ifelse(sum(duplicated(x))>0,1,0)
}
outcome_mean <-mean(outcomes)

# Theoretical calculation
mean_theor <- 1
for (i in 1:N-1) {
  mean_theor <- mean_theor*(days-i)/days
}
mean_theor<- 1-mean_theor

#d1 <- "\n Monty Hall Problem Simulation\n\n\"Don't Switch\"\n"
#d2 <- "\n                           Wins = "
#d3 <- "\n               Number of trials = "
#d4 <- "\n    Observed winning proportion = "
#d5 <- "\n Theoretical Winning Proportion = "
#d6 <- "\n\n\"Switch to Other Door\"\n"

#cat(d1,d2, sum(stay_win),d3,N,d4, stay_perc, d5, stay_theor,
#    d6,d2, sum(change_win), d3, N, d4, change_perc, d5, change_theor)

#plot.new()
plot(cumsum(outcomes)/c(1:M), main ="'Convergence' to true probability",
     xlab = "Trial", ylab = "Win percentage", ylim = c(0,1), col ="blue")
text(M,mean_theor,cat("Mean: ",mean_theor))
abline(h=mean_theor)

mtext(mean_theor,3)

