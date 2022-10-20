#XXXXXXXXXXXXXXXXXXXXXXXX proj2.r XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Group member
# Biwei Zhu, s2325784
# Guanhao Su, s2301705
# Shuying Liu, s2436365

#XXXXXXXXXXXXXXXXXXXXXXXXXXX Contribution XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Biwei Zhu completed the function Pone
# Guanhao Su completed the function Pall and the visualization
# Biwei Zhu and Guanhao Su completed together the example code to assess success probabilities under each strategy
# Shuying Liu completed the function dloop and the assessment of probability, and modified the whole code. 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXX Simulation XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# This project aims to estimate the success probability of the prisoner problem by simulations
# under three strategies:

# 1. The prisoner first opens the box with their number on it, 
# reads the number in the box then opens the box with that number,
# repeat until finding their own number in the box or reach the maximum of n open boxes.


# 2. As strategy 1, but starting from a randomly selected box.

# 3. They open n boxes at random to look for their number.
# Note: The cards in 2n boxes are in random order.


sim <- function(n,k,strategy,nprisoner=1,nreps=10000){
  # sim estimates the probability of prisoner succeeding in finding his number in 2*n boxes
  # takes argument: n; k - the list of prisoners' numbers; strategy - 1,2,3;
  #                 nprisoner - number of prisoners; nreps - number of simulations
  # returns the estimate probability
  
  # the total numer of success in nreps simulations
  success <- 0
  
  # Run nreps simulations
  for (irep in 1:nreps) {
    cards <- sample(2*n)
    # Elements of flag is whether the prisoner succeeds: TRUE if succeeds.
    flag <- array(FALSE,nprisoner)
    
    # Check if each prisoner succeeds
    for (prisoner in 1:nprisoner) {
      if (strategy == 3) {
        open_boxes <- sample(2*n,n)
        check_cards <- cards[open_boxes]
        if (k[prisoner] %in% check_cards) flag[prisoner] <- TRUE
      }
      else {
        # Different initially selected box number for strategy 2 & 3
        if (strategy == 1) selected_box <- k[prisoner]
        else selected_box <- sample(2*n,size=1)
        
        for (box in 1:n){
          if (cards[selected_box] == k[prisoner]){
            flag[prisoner] <- TRUE
            break
          }
          selected_box <- cards[selected_box]
        }
      }
    }
    success <- success+all(flag)
  }
  success/nreps
}


Pone <- function(n,k,strategy,nreps=10000){
  # Pone estimates the probability of one prisoner succeeding in finding his number in 2*n boxes
  # Takes argument: n; k - the prisoner's number; strategy - 1,2,3;
  #                 nreps - number of simulations
  # Returns the estimate probability
  sim(n,k,strategy)
}

Pall <- function(n,strategy,nreps=10000){
  # Pone estimates the probability of all 2*n prisoners succeeding in finding their number in 2*n boxes
  # Takes argument: n; strategy - 1,2,3; nreps - number of simulations
  # Returns the estimate probability
  k <- 1:(2*n)
  sim(n,k,strategy,nprisoner=2*n)
}



# Estimate the individual success probability under n = 5 and n = 50
cat("Individual success probability under the three strategies when n = 5\n")
for (strategy in 1:3) cat(sprintf('strategy %d: %f\n',strategy,Pone(5,1,strategy,10000)))

cat("Individual success probability under the three strategies when n = 50\n")
for (strategy in 1:3) cat(sprintf('strategy %d: %f\n',strategy,Pone(50,1,strategy,10000)))

# Estimate the joint success probability under n = 5 and n = 50
cat("Joint success probability under the three strategies when n = 5\n")
for (strategy in 1:3) cat(sprintf('strategy %d: %f\n',strategy,Pall(5,strategy,10000)))

cat("Joint success probability under the three strategies when n = 50\n")
for (strategy in 1:3) cat(sprintf('strategy %d: %f\n',strategy,Pall(50,strategy,10000)))


#XXXXXXXXXXXXXXXXXXXXXXXXXXXX comments XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# If we run enough simulations, we can estimate the probability by the relative frequency.
# The only difference between strategy 2 and strategy 1 is the initial choice of boxes,
# but strategy 2 (prob = 0.4) will have a lower probability of success for one prisoner than strategies 1 & 3 (prob = 0.5).
# The difference of success rate becomes greater as we run simulations on 2*n prisoners,
# strategy 1 still has a surprisingly stable success rate (Prob = 0.3),
# while the success rates of strategy 2 & 3 drop to 0.

# Speculation: Strategy 1 works surprisingly well because there is a decent chance that
# the length of a loop in a 2*n permutation is under n,
# thus if the prisoner first opens the box with his own number, then he is on the unique loop containing his number,
# so he is likely to find his number in the sequence of opening n boxes.
# Strategy 2 fails because he initially opens a random box, so loop wouldn't help.

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dloop <- function(n,nreps=10000){
  # dloop estimates the probability of each loop length from 1 to 2*n ocurring in a random shuffle of 2*n cards
  # Take argument: n; nreps - number of simulations
  # Returns a matrix of diminsion (2,2*n), 
  #   the first row being the probability distribution of each loop length, 
  #   the second being the probability distribution of the longest loop.
  
  loops <- array(0,2*n)
  longest_loops <- array(0,2*n)
  
  # run nreps simulations
  for (irep in 1:nreps){
    # ramdom shuffle of cards
    shuffle <- sample(2*n)
    # elements of loop_check are whether each loop length ocurrs in shuffle: 1 if occurrs
    loop_check <- array(0,2*n)
    # elements of unchecked_list are whether indices of shuffle haven't been searched for loop: TRUE if not searched
    unchecked_list <- array(TRUE,2*n)
    
    # search all loops until all the elements in shuffle have been searched
    while (length(shuffle[unchecked_list])){
      # pick an unchecked element in shuffle to start
      start <- shuffle[unchecked_list][1]
      k <- start
      unchecked_list[k] <- FALSE
      loop_len <- 1
      # find the loop that begins with 'start'
      while (shuffle[k] != start){
        k <- shuffle[k]
        unchecked_list[k] <- FALSE
        loop_len <- loop_len+1
      }
      loop_check[loop_len] <- 1
    }
    
    loops <- loops+loop_check
    longest_loop <- tail((1:(2*n))[as.logical(loop_check)],1)
    longest_loops[longest_loop] <- longest_loops[longest_loop]+1
  }
  rbind(loops/nreps,longest_loops/nreps)
}

# Assess the probability of loop
y <- dloop(50)
# The probability of each loop length from 1 to 2n occurring at least once.
cat('The probability of each loop length is\n',(y[1,]),'\n')
# the probability that there is no loop longer than 50 is roughly 0.31
cat('Probability of longest loop length being 50 is',sum(y[2,1:50]))


#Visualising the probability
n <- 50
color1 <- c(rgb(250,220,120,150,max = 255), rgb(160,150,180,150,max = 255))
color2 <- c(rep(rgb(250,220,120,150,max = 255),n), rep(rgb(160,150,180,150, max = 255), n))
par(mfrow = c(1,3))#three subplot

barplot(y[1,],col = rgb(255,100,100,150,max = 255), space = 0,xaxs="i"
        , xlab = "Length of the loop", ylab = "Probability")
axis(1,c(0,50,100))

barplot(c((y[2,1:50]),(y[2,51:100])), space = 0, col = color2, xaxs="i"
        , xlab = "Length of the longest loop", ylab = "Probability")
axis(1,c(0,50,100))

barplot(c(sum(y[2,1:50]), sum(y[2,51:100])), space = 0, col = color1, xaxs="i"
        , xlab = "Length of the loop \nlonger than 50 or not", ylab = "Cumulative Probability")
axis(1,c(0,1,2),c(0,50,100))
