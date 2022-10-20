#XXXXXXXXXXXXXXXXXXXXXXXX proj2.r XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Group member
# Biwei Zhu, s2325784
# Guanhao Su, s2301705
# Shuying Liu, s2436365

#XXXXXXXXXXXXXXXXXXXXXXXXXXX Contribution XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Biwei Zhu completed the Question 1
# Guanhao Su completed the Question 2
# Biwei Zhu and Guanhao Su completed the question 3 and 4 together
# Shuying Liu completed the Question 5,6 and modified the whole code. 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXX Variables XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# This project aims to calculate the surviving probability of 2n prisoners
# under three strategies:

# 1. The prisoner starts at the box with their number on it, opens it and 
# reads the number on the card: k, say. If k is not their prisoner number, 
# they go to box number k, open it and repeat the process until they have either
# found the card with their number on it, or opened n boxes without finding it.

# 2. As strategy 1, but starting from a randomly selected box.

# 3. They open n boxes at random, checking each card for their number.
# Note: The card in 2n boxes are in random order.


# Estimate the probability of a single prisoner succeeding in finding his number
# under three strategies.
# nreps is the number of replicate simulations.
# success_one is used to count the overall success times.

sim <- function(n,k,strategy,nprisoner=1,nreps=10000){
  
  success <- 0
  
  for (irep in 1:nreps) {
    cards <- sample(2*n)
    # check_cards <- array(0,n)
    flag <- array(FALSE,nprisoner)
    
    for (prisoner in 1:nprisoner) {
      if (strategy == 3) {
        open_boxes <- sample(2*n,n)
        check_cards <- cards[open_boxes]
        if (k[prisoner] %in% check_cards) flag[prisoner] <- TRUE
      }
      else {
        # if (strategy == 1) check_cards[1] <- cards[k[prisoner]]
        # else check_cards[1] <- cards[sample(2*n,1)]
        if (strategy == 1) selected_box <- k[prisoner]
        else selected_box <- sample(2*n,size=1)
        
        # for (box in 2:n) check_cards[box] <- cards[check_cards[box-1]]
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

# time <- function(){
#   start.time <- Sys.time()
#   sim(50,1:100,1,100)
#   end.time <- Sys.time()
#   time.taken <- end.time - start.time
#   time.taken
# }
# time()

Pone <- function(n,k,strategy,nreps=10000){
  sim(n,k,strategy)
}



# Estimate the probability of all prisoners finding their number, so that all 
# are released, under three strategies.
# success_all is used to note which prisoner has succeed.
# final_success_all is used to count if 2n prisoners are released in jth experiment.

Pall <- function(n,strategy,nreps=10000){
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
# For one prisoner
# When a prisoner is simulated enough times, the prisoner can be successfully released is independent of the number of boxes.
# Surprisingly, the only difference between the strategy 2 and strategy 1 is the initial choice of boxes,
# but strategy 2 (prob = 0.4) will have a lower probability of success than strategies 1 & 3 (prob = 0.5).
# Speculation: If strategy 1 is followed, the box will definitely be opened in this prisoner's own loop.
# However, strategy 2 is not guaranteed to be in this prisoner's own loop, so the probability will be lower.

# For 2*n prisoners
# As the number of prisoners increases,
# strategy 1 (Prob = 0.318) is more stable compared to strategies 2,3 (which converge to 0).
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
