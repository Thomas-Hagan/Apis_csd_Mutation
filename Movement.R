Movement <- function(alpha_jump, beta_jump, Max_Jump, Jump_sd, Queens, Push, Bounded, Max_Dist, Min_Dist) {
  Movement_Dist = c()
  for (i in 1:length(Queens$Position)) {
    
    #This is the "Pressure" to be pushed away. THIS IS A HEURISTIC
    Travel = alpha_jump*rnorm(1, beta_jump*Push[i], Jump_sd)
    
    # This pressure is a random number, NOT from a normal distribution, with a small influence from the surrounding colonies.
    #Travel = alpha_jump*(beta_jump*Push[i] + runif(1, min = -Max_Jump, max = Max_Jump))
    
    #Completely random number
    #Travel = runif(1, min = -Max_Jump, max = Max_Jump)
    
    if (isTRUE(Travel >= Max_Jump)) {
      Travel = Max_Jump
    }
    
    
    if (isTRUE(Travel <= -Max_Jump)) {
      Travel = -Max_Jump
    }
    
    #Create distance that the bee travels. HEURISTIC HERE of limiting distance to 3 units
    Queens$Position[i] = Queens$Position[i] + Travel
    if (Bounded == "YES") {
      if (Queens$Position[i] > Max_Dist) {
        Queens$Position[i] = Max_Dist - (Queens$Position[i]-Max_Dist)
      } else if (Queens$Position[i] < Min_Dist) {
        Queens$Position[i] = Min_Dist - (Queens$Position[i]-Min_Dist)
      }
    }
    Movement_Dist[i] = Travel
  }
  return(Queens)
}