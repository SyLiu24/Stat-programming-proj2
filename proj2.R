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

Pone <- function(n,k,strategy,nreps){
  success_one <- 0
  for (j in 1:nreps){
    box_prison <- sample(1:(2*n),2*n,replace = FALSE)
    a <- array(0,c(n))
    #strategy 1
    if (strategy == 1){
      a[1] <- box_prison[k]
      for (i in 2:n){
        a[i] <- box_prison[a[i-1]]
      }
      if (k %in% a){
        success_one = success_one + 1
      }
    }
    #strategy 2
    if (strategy == 2){
      random_select <- sample(1:(2*n),1,replace = FALSE)
      a[1] <- box_prison[random_select]
      for (i in 2:n){
        a[i] <- box_prison[a[i-1]]
      }
      if (k %in% a){
        success_one = success_one + 1
      }
    }
    #strategy 3
    if (strategy == 3){
      n_box <- sample(1:(2*n),n,replace = FALSE)
      box_prison_new <- box_prison[n_box]
      if (k %in% box_prison_new){
        success_one = success_one + 1
      }
    }
  }
  cat('the survive probability is:')
  success_one/nreps
}



# Estimate the probability of all prisoners finding their number, so that all 
# are released, under three strategies.
# success_all is used to note which prisoner has succeed.
# final_success_all is used to count if 2n prisoners are released in jth experiment.

Pall <- function(n,strategy,nreps){
  final_success_all <- array(0,c(nreps))
  for (j in 1:nreps){
    a <- array(0,c(n))
    box_prison <- sample(1:(2*n),2*n,replace = FALSE)
    success_all <- array(0,c(2*n))
    if (strategy == 1){
      for (g in 1:(2*n)){
        #strategy 1
        a[1] <- box_prison[g]
        for (i in 2:n){
          a[i] <- box_prison[a[i-1]]
        }
        if (g %in% a){
          success_all[g] = 1
        }
      }
    }
    #strategy 2
    if (strategy == 2){
      for (g in 1:(2*n)){
        random_select <- sample(1:(2*n),1,replace = FALSE)
        a[1] <- box_prison[random_select]
        for (i in 2:n){
          a[i] <- box_prison[a[i-1]]
        }
        if (g %in% a){
          success_all = success_all + 1
        }
      }
    }
    #strategy 3
    if (strategy == 3){
      for (g in 1:(2*n)){
        n_box <- sample(1:(2*n),n,replace = FALSE)
        box_prison_new <- box_prison[n_box]
        if (g %in% box_prison_new){
          success_all = success_all + 1
        }
      }
    }
    part_success_all = success_all[1:n]
    if  (sum(part_success_all) == n){
      final_success_all[j] = 1
    }
  }
  cat('the survive probability is:')
  sum(final_success_all)/(nreps)
}



# Estimate the individual success probability under n = 5 and n = 50
print("Individual success probability under the three strategies when n = 5")
Pone(5,1,1,10000)
Pone(5,1,2,10000)
Pone(5,1,3,10000)

print("Individual success probability under the three strategies when n = 50")
Pone(50,1,1,10000)
Pone(50,1,2,10000)
Pone(50,1,3,10000)

# Estimate the joint success probability under n = 5 and n = 50
print("Joint success probability under the three strategies when n = 5")
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)

print("Joint success probability under the three strategies when n = 50")
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)

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
