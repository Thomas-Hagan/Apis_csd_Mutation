Carrying_Capacity <- function(K_Dist, K_Num, Queens) {
  FuncF= FunctionalFitness(Queens)
  for (i in 1:length(Queens$Position)) {
    Up_K = Queens$Position[i] + K_Dist
    Low_K = Queens$Position[i] - K_Dist
    Kdf = FuncF[between(Queens$Position, Low_K, Up_K)]
    if (length(Kdf > 1)) {
      aveF = mean(Kdf)
    } else {
      aveF = 0
    }
    
    #This needs to be normalised, as normally speaking this probability will produce a number of queens; 
    # = K_Num*(MinFitValue + (1-MinFitValue)/2), where in this case minFitValue is obviously 0.5
    #This is a simply normalisation, as this is equal to dividing through by 0.75
    prob = FuncF[i]*K_Num/(aveF*length(Kdf))
    
    if (runif(1) > prob) {
      Queens$Allele_1[i] = 0
    }
  }
  
  Queens <- Queens[Queens$Allele_1 != 0,]
  
  if (length(Queens$Position) == 0) {
    print("Stop. Stop, they're already dead!")
    break
  }
  
  return(Queens)
}