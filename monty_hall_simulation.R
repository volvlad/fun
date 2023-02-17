# number of simulations
N <- 10000

# randomly select the doors with a car
prize <- sample(1:3,N,replace=TRUE)

# randomly select a player's choice
stay <- sample(1:3,N,replace=TRUE)

#init
reveal <- rep(0, N)
change <- rep(0, N)

for (i in 1:N) {
  x <- c(1:3)[-c(prize[i],stay[i])]
  # if wrong door is chosen by the contensant then the only choice to reveal is another door with a goat
  # if consenant chose the right door then randomly select one of two doors with the goats
  reveal[i] <- x[sample.int(length(x), size=1)]
  
  change[i] <- c(1:3)[-c(reveal[i],stay[i])]
}

win_stay <- ifelse(stay==prize,1,0)
win_change <- ifelse(change==prize,1,0)

change_perc <- mean(win_change)
stay_perc <- mean(win_stay)

d1 <- "\n Monty Hall Problem Simulation\n\n\"Don't Switch\"\n"
d2 <- "\n                           Wins = "
d3 <- "\n               Number of trials = "
d4 <- "\n    Observed winning proportion = "
d5 <- "\n Theoretical Winning Proportion = "
d6 <- "\n\n\"Switch to Other Door\"\n"

cat(d1,d2, sum(win_stay),d3,N,d4,stay_perc, d5, 1/3,
    d6,d2, sum(win_change), d3, N, d4, change_perc, d5, 2/3)
    
plot.new()
plot(cumsum(win_change)/c(1:N), main ="'Convergence' to true Winning Proportions",
     xlab = "Trial", ylab = "Win percentage", ylim = c(0,1), col ="blue")
abline(h=2/3)
points(cumsum(win_stay)/c(1:N), type="p", col="red")
abline(h=1/3)