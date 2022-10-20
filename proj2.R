#XXXXXXXXXXXXXXXXXXXXXXXX proj2.R XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Biwei Zhu, s2325784
# Guanhao Su, s2301705
# Shuying Liu, s2436365

#XXXXXXXXXXXXXXXXXXXXXXXXXXX Contribution XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Biwei Zhu completed the Question 1
# Guanhao Su completed the Question 2
# Biwei Zhu and Guanhao Su completed the question 3 and 4 together
# Shuying Liu completed the Question 5,6 and modified the whole code. 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXX Variables XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Estimate the probability of a single prisoner succeeding in finding their number
# 'n' is the maximum number of boxes a prisoner is allowed to open
# 'k' is the serial number of the prisoner conducting the experiment
# 'strategy' <- 1/2/3 is the strategy chosen by the prisoner to open the box
# 'nreps' is the number of times a prisoner can simulate the experiment, the default value is 10000.
# 'times' stores the number of times the prisoner has been successful
# 'box' the box is chosen by the prisoner
# 'card_new' card numbers randomly placed in each box
# 'Prob' The probability of success is the number of successful frequencies divided by the total number of simulated experiments nreps


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# When strategy 1 is selected, the prisoner first opens the box with the same serial number as his own, and then continues to open the next box according to the card number in the box
# Strategy 2 is that the first time the prisoner opens the box is random and the subsequent actions are the same as strategy 1
# Strategy 3 is to open each box is completely random, until the opened box and its own number is the same, The choice of the next box will not be influenced by the previous one

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Process:
# 1.First create 1-2n boxes
# 2.Randomly put the cards labeled 1-2n into the boxes
# 3.Open the boxes according to each strategy to extract cards
# 4.A prisoner has n chances to open the box (Looping with 'while' statements)
# 5.Only when the prisoner find a card with the same number as your own in n chances, they can be released

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Pone <- function(n,k,strategy,nreps = 10000){ 
  
  box <- array(1:(2*n))  
  times <- 0 
  
  if (strategy == 1){ 
    for(i in 1:nreps){ 
      m <- n 
      card_new <- sample(box, length(box)) # Put the cards printed with 1-2n into 2n boxes randomly
      num <- k 
      # Each prisoner has m = n attempts, and if that number is exceeded, the prisoner cannot be released
      while (m > 0) { 
        num <- card_new[num] 
        # If the number of the card in the opened box matches that of the prisoner, the prisoner can be successfully released
        if (num == k){
          times <- times + 1 # If the prisoner is successfully released, the success of the simulation is recorded
          break 
        }
        m <- m - 1 # If a box is opened with the wrong number in that cycle, the number of attempts available is reduced by one
      }
    }
  }
  else if(strategy == 2){
    for(i in 1:nreps){
      m <- n
      card_new <- sample(box, length(box))  
      num <- sample(box,length(1)) # Randomly select one as the first box number to be opened
      while (m > 0) {
        num <- card_new[num]
        if (num == k){
          times <- times + 1
          break
        }
        m <- m - 1
      }
    }
  }
  else if(strategy == 3){ 
    for(i in 1:nreps){
      box <- array(1:(2*n))
      card_new <- sample(box,length(box))
      box_num <- sample(box,n) # In order to improve the efficiency of the algorithm, we choose n boxes arbitrarily in the first step of this experiment
      box_prison_num <- card_new[box_num]  
      if(k %in% box_prison_num){ # If the prisoner's own serial number, is in one of these n boxes, then the prisoner can be successfully released
        times <- times + 1
      }
    }
  }
  # If the sequence number of the strategy entered is not 1,2,3, then the input is wrong
  else{ 
    print("The input is error.")
  }
  Prob <- times / nreps 
  return(Prob)  
}
