

#Lineup With One Player########

one <- rep(1,9)
two <- rep(2,9)
three <- rep(3,9)
four <- rep(4,9)
five <- rep(5,9)
six <- rep(6,9)
seven <- rep(7,9)
eight <- rep(8,9)
nine <- rep(9,9)

stats10 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats10$batter <- one
stats11 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats11$batter <- two
stats12 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats12$batter <- three
stats13 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats13$batter <- four
stats14 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats14$batter <- five
stats15 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats15$batter <- six
stats16 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats16$batter <- seven
stats17 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats17$batter <- eight
stats18 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats18$batter <- nine

data2 <- cbind(game(stats10), game(stats11), game(stats12), game(stats13), 
               game(stats14), game(stats15), game(stats16), game(stats17), game(stats18))
data2 <- as.data.frame(data2)

game <- function(lineup) {
  for (j in 1:1000) {
    
    
    
    outs <- 0
    runs <- 0
    first <- 0
    second <- 0
    third <- 0
    
    
    while (outs < 27) {
      
      for (i in lineup$batter) {
        
        play <- round(runif(1, 0, 1000))
        
        # out
        if (play < lineup$OBP[i]) {
          outs = outs + 1
          print("OUT!")
          #play <- round(runif(1, 1, 1000))
          if (outs %% 3 == 0) {
            first = 0
            second = 0
            third = 0
            print('Inning Over')
          }
          # walk
        } else if (play > lineup$OBP[i] &
                   play <= lineup$walk_pct[i]) {
          print("WALK")
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 1) {
            second <- 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
          }
          #play <- round(runif(1, 1, 1000))
          # single
        } else if (play > lineup$walk_pct[i] &
                   play <= lineup$single_pct[i]) {
          print("SINGLE")
          
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
            second <- 0
            third <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 1
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
            second <- 0
            third <- 1
            runs <- runs + 1
          }
          #play <- round(runif(1, 1, 1000))
          #double
        } else if (play > lineup$single_pct[i] &
                   play <= lineup$double_pct[i]) {
          print("DOUBLE")
          if (first == 0 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 1
            third <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            second <- 1
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 1) {
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # triple
        } else if (play > lineup$double_pct[i] &
                   play <= lineup$triple_pct[i]) {
          print("TRIPLE")
          if (first == 0 & second == 0 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # homerun
        } else {
          print("HOMERUN!!!")
          if (first == 0 & second == 0 & third == 0) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 0 & third == 1) {
            third <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            third <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            third <- 0
            runs <- runs + 4
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          
        }
        
      }
      
      
    }
    
    if (outs == 27) {
      print('')
      print("Ball game!")
      print(runs)
      print('')
    }
    
    total_runs[j] <- runs
    
    
  }
  return(total_runs)
}

x <- c()
for (i in 1:9) {
  x[i] <- mean(data2[,i])
}   

names <- batter_names
barplot(x, names.arg = names, border = NA, xpd = F, axis.lty = 0, ylim = c(0,10), main = "If Lineup Was Entirely One Player", ylab = "Mean Runs Scored")


#Optimized Lineup########

# set lineup
batter <- seq(1, 9, 1)
OBP <- c(657, 662, 604, 594, 639, 579, 655, 634, 870)
walk_pct <- c(711, 709, 727, 718, 751, 761, 750, 729, 895)
single_pct <- c(929, 862, 886, 904, 911, 899, 936, 920, 985)
double_pct <- c(983, 928, 958, 955, 968, 932, 986, 972, 990)
triple_pct <- c(985, 942, 966, 957, 979, 939, 988, 981, 991)
homer_pct <- c(996, 998, 999, 1001, 1000, 1000, 1000, 999, 1000)

stats <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))

total_runs <- 0 
# updated game function with only 100 simulations to save time
game <- function(lineup) {
  for (j in 1:100) {
    
    
    
    outs <- 0
    runs <- 0
    first <- 0
    second <- 0
    third <- 0
    
    
    while (outs <= 27) {
      
      for (i in lineup$batter) {
        
        play <- round(runif(1, 0, 1000))
        
        # out
        if (play < lineup$OBP[i]) {
          outs = outs + 1
          print("OUT!")
          #play <- round(runif(1, 1, 1000))
          if (outs %% 3 == 0) {
            first = 0
            second = 0
            third = 0
            print('Inning Over')
          }
          # walk
        } else if (play > lineup$OBP[i] &
                   play <= lineup$walk_pct[i]) {
          print("WALK")
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 1) {
            second <- 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
          }
          #play <- round(runif(1, 1, 1000))
          # single
        } else if (play > lineup$walk_pct[i] &
                   play <= lineup$single_pct[i]) {
          print("SINGLE")
          
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
            second <- 0
            third <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 1
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
            second <- 0
            third <- 1
            runs <- runs + 1
          }
          #play <- round(runif(1, 1, 1000))
          #double
        } else if (play > lineup$single_pct[i] &
                   play <= lineup$double_pct[i]) {
          print("DOUBLE")
          if (first == 0 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 1
            third <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            second <- 1
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 1) {
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # triple
        } else if (play > lineup$double_pct[i] &
                   play <= lineup$triple_pct[i]) {
          print("TRIPLE")
          if (first == 0 & second == 0 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # homerun
        } else {
          print("HOMERUN!!!")
          if (first == 0 & second == 0 & third == 0) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 0 & third == 1) {
            third <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            third <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            third <- 0
            runs <- runs + 4
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          
        }
        
      }
      
      
    }
    
    if (outs == 27) {
      print('')
      print("Ball game!")
      print(runs)
      print('')
    }
    
    total_runs[j] <- runs
    
    
  }
  return(total_runs)
}

#setting order to original order
stats$batter <- seq(1, 9, 1)
order <- c()

#initialize starting minimum
avg_runs0 <- 0
scores <- c()

# 1000 simulations of the mean of 100 games
# if the new average is greater than the average of the previous lineup, update lineup
for (i in 1:1000) {
  stats$batter <- sample(1:9, 9, replace = F)
  scores <- game(stats)
  avg_runs1 <- mean(scores)
  if (avg_runs1 > avg_runs0) {
    order <- stats$batter
    avg_runs0 <- avg_runs1
  }
}

# view optimized order
# this will be used in its own lineup later
order

#reset original lineup
stats$batter <- seq(1, 9, 1)


#Optimized Lineup########

batter_names <- c("Almora", "Baez", "Bryant", "Rizzo", "Contreras", "Schwarber", "Russel", "Heyward", "Pitcher")

batter <- seq(1, 9, 1)
OBP <- c(657, 662, 604, 594, 639, 579, 655, 634, 870)
walk_pct <- c(711, 709, 727, 718, 751, 761, 750, 729, 895)
single_pct <- c(929, 862, 886, 904, 911, 899, 936, 920, 985)
double_pct <- c(983, 928, 958, 955, 968, 932, 986, 972, 990)
triple_pct <- c(985, 942, 966, 957, 979, 939, 988, 981, 991)
homer_pct <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000)

# six lineups

#original
stats <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))

#pitcher 8th
stats2 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats2$batter <- c(1, 2, 3, 4, 5, 6, 7, 9, 8)

#pitcher first
stats3 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats3$batter <- c(9, 1, 2, 3, 4, 5, 6, 7, 8)

#6th -> 1st
stats4 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats4$batter <- c(6, 1, 2, 3, 4, 5, 7, 8, 9)

#optimized
stats5 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats5$batter <- order
order

#dereasing OBP
stats6 <- data.frame(cbind(batter, OBP, walk_pct, single_pct, double_pct, triple_pct))
stats6$batter <- c(3,4,6,8,5,7,1,2,9)

#updated game(): 1000 simulations
game <- function(lineup) {
  for (j in 1:1000) {
    
    
    
    outs <- 0
    runs <- 0
    first <- 0
    second <- 0
    third <- 0
    
    
    while (outs < 27) {
      
      for (i in lineup$batter) {
        
        play <- round(runif(1, 0, 1000))
        
        # out
        if (play < lineup$OBP[i]) {
          outs = outs + 1
          print("OUT!")
          #play <- round(runif(1, 1, 1000))
          if (outs %% 3 == 0) {
            first = 0
            second = 0
            third = 0
            print('Inning Over')
          }
          # walk
        } else if (play > lineup$OBP[i] &
                   play <= lineup$walk_pct[i]) {
          print("WALK")
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 1) {
            second <- 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
          }
          #play <- round(runif(1, 1, 1000))
          # single
        } else if (play > lineup$walk_pct[i] &
                   play <= lineup$single_pct[i]) {
          print("SINGLE")
          
          if (first == 0 & second == 0 & third == 0) {
            first <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            first <- 1
            second <- 0
            third <- 1
          } else if (first == 0 & second == 0 & third == 1) {
            first <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 1
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 1 & third == 1) {
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 1) {
            first <- 1
            second <- 0
            third <- 1
            runs <- runs + 1
          }
          #play <- round(runif(1, 1, 1000))
          #double
        } else if (play > lineup$single_pct[i] &
                   play <= lineup$double_pct[i]) {
          print("DOUBLE")
          if (first == 0 & second == 0 & third == 0) {
            second <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 1
            third <- 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            second <- 1
            third <- 0
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            second <- 1
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 1) {
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # triple
        } else if (play > lineup$double_pct[i] &
                   play <= lineup$triple_pct[i]) {
          print("TRIPLE")
          if (first == 0 & second == 0 & third == 0) {
            third <- 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            third <- 1
            runs <- runs + 1
          } else if (first == 0 & second == 0 & third == 1) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            third <- 1
            runs <- runs + 2
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          # homerun
        } else {
          print("HOMERUN!!!")
          if (first == 0 & second == 0 & third == 0) {
            runs <- runs + 1
          } else if (first == 1 & second == 0 & third == 0) {
            first <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 1 & third == 0) {
            second <- 0
            runs <- runs + 2
          } else if (first == 0 & second == 0 & third == 1) {
            third <- 0
            runs <- runs + 2
          } else if (first == 1 & second == 0 & third == 1) {
            first <- 0
            third <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 0) {
            first <- 0
            second <- 0
            runs <- runs + 3
          } else if (first == 1 & second == 1 & third == 1) {
            first <- 0
            second <- 0
            third <- 0
            runs <- runs + 4
          } else if (first == 0 & second == 1 & third == 1) {
            second <- 0
            third <- 0
            runs <- runs + 2
          }
          #play <- round(runif(1, 1, 1000))
          
        }
        
      }
      
      
    }
    
    if (outs == 27) {
      print('')
      print("Ball game!")
      print(runs)
      print('')
    }
    
    total_runs[j] <- runs
    
    
  }
  return(total_runs)
}

#data frame of results from each lineup
data <- cbind(game(stats), game(stats2), game(stats3), game(stats4), game(stats5), game(stats6))
data <- as.data.frame(data)

#check the means
mean(data$V1)
mean(data$V2)
mean(data$V3)
mean(data$V4)
mean(data$V5)
mean(data$V6)

#boxplot
par(mfrow = c(1,1))
boxplot(data)

#pvalue for each pairwise relationship
t.test(data = data, x = data$V1, y = data$V2)$p.value
t.test(data = data, x = data$V1, y = data$V3)$p.value
t.test(data = data, x = data$V1, y = data$V4)$p.value
t.test(data = data, x = data$V1, y = data$V5)$p.value
t.test(data = data, x = data$V1, y = data$V6)$p.value
t.test(data = data, x = data$V2, y = data$V3)$p.value
t.test(data = data, x = data$V2, y = data$V4)$p.value
t.test(data = data, x = data$V2, y = data$V5)$p.value
t.test(data = data, x = data$V2, y = data$V6)$p.value
t.test(data = data, x = data$V3, y = data$V4)$p.value
t.test(data = data, x = data$V3, y = data$V5)$p.value
t.test(data = data, x = data$V3, y = data$V6)$p.value
t.test(data = data, x = data$V4, y = data$V5)$p.value
t.test(data = data, x = data$V4, y = data$V6)$p.value
t.test(data = data, x = data$V5, y = data$V6)$p.value

#histogram of each lineup results
par(mfrow = c(2,3))
plot(table(data$V1), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Original Order", xlab = "Runs Scored", ylab = "freq")
plot(table(data$V2), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Pitcher 8th", xlab = "Runs Scored", ylab = "freq")
plot(table(data$V3), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Pitcher First", xlab = "Runs Scored", ylab = "freq")
plot(table(data$V4), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Number 6th -> First", xlab = "Runs Scored", ylab = "freq")
plot(table(data$V5), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Optmized Order", xlab = "Runs Scored", ylab = "freq")
plot(table(data$V6), ylim = c(0,165), xlim = c(0,25), lwd = 3, main = "Decreasing OBP", xlab = "Runs Scored", ylab = "freq")

#total mean runs scored
total_mean <- sum(data[,1], data[,2], data[,3], data[,4], data[,5], data[,6])/6000

#barplot of total games scoring above the mean
par(mfrow=c(1,1))
par(las=2)
par(mar=c(7.5,6,4,6))
barplot(c(sum(data$V1 > total_mean),
          sum(data$V2 > total_mean),
          sum(data$V3 > total_mean),
          sum(data$V4 > total_mean),
          sum(data$V5 > total_mean),
          sum(data$V6 > total_mean)), border = NA, names.arg = c("Original", "Pitcher 8th", "Pitcher 1st", "6th -> 1st", "Optimized", "Increasing OBP"),
        main = "Number of Games With Runs Above the Mean",
        ylim = c(0,500))




