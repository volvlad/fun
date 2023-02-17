library(tictoc)
library(ggplot2)

log10Tck <- function(side, type){
  lim <- switch(side, 
                x = par('usr')[1:2],
                y = par('usr')[3:4],
                stop("side argument must be 'x' or 'y'"))
  at <- floor(lim[1]) : ceiling(lim[2])
  return(switch(type, 
                minor = outer(1:9, 10^(min(at):max(at))),
                major = 10^at,
                stop("type argument must be 'major' or 'minor'")
  ))
}

sims <- 5500000 # Number of simulations
W <- 1000000000 # size of bank
N <- floor(log(W, 2)) # maximum number of rounds
fee <- 50
tic()
payoffs<-matrix(0,sims,6)
#1: unlimited payoffs
#2: limited payoffs
#3: mean unlimited
#4: mean limited
#5: casino profits
#6: cum. profit
for (i in 1:sims) {
  k<-1
  #while (sample(c(0,1),1)>0) {
  while (runif(1)>=.5) {
    k=k+1
  }
  payoffs[i,1] <- 2**k
  payoffs[i,2] <- min(2**k,W)
  payoffs[i,5] <- fee-payoffs[i,2]
  #payoffs[i,2] <- mean(payoffs[1:i,1])
  #if (i %% 10000 == 0) { 
  #  #tmp <- c(i %/% 10000, payoffs[i,2])
  #  print(i)
  #}
}
toc()
summary(payoffs[,1])
quantile(payoffs[,1], c(.1,.5,.9))
summary(payoffs[,2])
quantile(payoffs[,2], c(.1,.5,.9))

payoffs[1,3] <- payoffs[1,1]
payoffs[1,4] <- payoffs[1,2]
payoffs[1,6] <- payoffs[1,5]
for (i in 2:sims) {
  payoffs[i,3] <- (payoffs[i-1,3]*(i-1)+payoffs[i,1])/i
  payoffs[i,4] <- (payoffs[i-1,4]*(i-1)+payoffs[i,2])/i
  payoffs[i,6] <- payoffs[i-1,6]+payoffs[i,5]
}
payoffs_plot <- payoffs[seq(1,sims,100),2]
plot(payoffs_plot)
mean_payoff_plot <- payoffs[seq(1,sims,100),3]
plot(mean_payoff_plot)
mean_payoff_lim_plot <- payoffs[seq(1,sims,100),4]
lines(mean_payoff_lim_plot, col="red")
cum_profit_plot = payoffs[seq(1,sims,100),6]/1e6
plot(cum_profit_plot)
p <- c(0, 10^(seq(-12,-3,.01)), ppoints(1000), 1)
#p <- ppoints(100)
#p <- c(p, ppoints(100))
X <- payoffs[,1]
#CDF <- ecdf(payoffs[,1])
CDF <- c(0,1)
plot(p,
     xlim=c(.1,9),
     xlab="Payoffs",
     ylab="CDF",
     main="Payoffs Distribution",
     col="seashell2")
     #axes=FALSE)
lines(log10(quantile(X,p=p)),1-p,
     ylab="[P(X > x)]",xlab="log(x)",main="CCDF:log", col='blue')
#axis(1, at=log10Tck('x','major'), tcl= 0.2) # bottom
#axis(3, at=log10Tck('x','major'), tcl= 0.2, labels=NA) # top
#axis(1, at=log10Tck('x','minor'), tcl= 0.1, labels=NA) # bottom
#axis(3, at=log10Tck('x','minor'), tcl= 0.1, labels=NA) # top
#axis(2) # normal y axis
#axis(4) # normal y axis on right side of plot
#box()

payoffs_plot <- payoffs[payoffs[,2]>1000,2]
plot(payoffs_plot)
length(payoffs_plot)
c(length(payoffs_plot),sum(payoffs_plot))
payoffs_plot <- payoffs[payoffs[,2]>1e6,2]
c(length(payoffs_plot),sum(payoffs_plot))
########################

tic() # start timing
output <- matrix(0, sims, 2)
for(j in 1:sims) { #outer loop for many trials
  for(i in 1:(N+1)){ # inner loop for each trial
    if(runif(1) >= 0.5) {output[j,1] <- 2^i} # payoff = 2^round, runif = r uniform
    if(i == (N+1)){output[j,1] <- W} # Or whole bank
    # if we reach the last round
    if(output[j,1]>=2){break} # if we win, stop flipping
  }
}
mean(output[,1])
output[1,2] <- output[1,1]
for (i in 2:sims) {
  output[i,2] <- (output[i-1,2]*(i-1)+output[i,1])/i
}
toc()
mean_payoff_lim_plot <- output[seq(1,sims,100),2]
plot(mean_payoff_lim_plot, col="red")

tic()
payoffs <- c(2^seq(1:N),W)
probabilities <- c(1/(2^seq(1:(N+1))))
winnings <- sample(payoffs, sims, prob = probabilities,
                   replace = TRUE)
mean(winnings)
toc()

tic()
mean(sample(c(2^seq(1:N),W), sims, prob = c(1/(2^seq(1:(N+1)))), replace = TRUE))
toc()
