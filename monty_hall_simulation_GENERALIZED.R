# number of simulations
N <- 10000
# number of doors
ND <- 3
# number of doors to be revealed
NR <- 1

# randomly select the doors with a car
prize <- sample(1:ND,N,replace=TRUE)

# randomly select a player's choice
stay <- sample(1:ND,N,replace=TRUE)

#init
reveal <- rep(0, N)
change <- rep(0, N)

for (i in 1:N) {
  x <- c(1:ND)[-c(prize[i],stay[i])]
  rev_tmp <- sample(x, size=NR)
  if (NR < ND-2) {
    change[i] <- sample(c(1:ND)[-c(rev_tmp,stay[i])], size=1)
  } else if (NR == ND-2) {
    reveal[i] <- x[sample.int(length(x), size=1)]
    change[i] <- c(1:ND)[-c(rev_tmp,stay[i])]
  }
  
}

stay_win <- ifelse(stay==prize,1,0)
change_win <- ifelse(change==prize,1,0)


change_perc <- mean(change_win)
stay_perc <- mean(stay_win)

stay_theor <- 1/ND
change_theor <- (ND-1)/ND/(ND-NR-1)

d1 <- "\n Monty Hall Problem Simulation\n\n\"Don't Switch\"\n"
d2 <- "\n                           Wins = "
d3 <- "\n               Number of trials = "
d4 <- "\n    Observed winning proportion = "
d5 <- "\n Theoretical Winning Proportion = "
d6 <- "\n\n\"Switch to Other Door\"\n"

cat(d1,d2, sum(stay_win),d3,N,d4, stay_perc, d5, stay_theor,
    d6,d2, sum(change_win), d3, N, d4, change_perc, d5, change_theor)

plot.new()
plot(cumsum(change_win)/c(1:N), main ="'Convergence' to true Winning Proportions",
     xlab = "Trial", ylab = "Win percentage", ylim = c(0,1), col ="blue")
abline(h=change_theor)
points(cumsum(stay_win)/c(1:N), type="p", col="red")
abline(h=stay_theor)