Reproduce <- function(Max_Swarms, GQL, Queens, Chance_Mutate, Queenless_Chance, Mutant_Number) {
  
  #Creates a bunch of names, creates a new data frame with columns equal to those number of names and then calls those columns those names
  Column_Names = names(Queens)
  New_Queens = data.frame(matrix(nrow = 0, ncol = length(Column_Names)))
  FuncF = FunctionalFitness(Queens)
  New_Allele_Counter = 1
  Allele_N = as.integer(str_remove(names(Queens)[-(1:6)], "Aus"))
  
  for (i in 1:length(Queens$Position)) {
    
    #Creates a random number, with the average being the max number of swamrs multiplied by fitness
    Num_Swarms = rpois(1, Max_Swarms*FuncF[i])
    
    #Starts making new colonies
    if (Num_Swarms > 0) {
      for (j in 1:Num_Swarms) {
        
        if (runif(1)<Queenless_Chance) {
          Current_New_Queen <- Queens[i,]
          Current_New_Queen$Age = GQL
          Current_New_Queen = unlist(Current_New_Queen)
          names(Current_New_Queen) = NULL
          
          if (length(Current_New_Queen)<length(New_Queens)) {
            number0 = length(New_Queens) - length(Current_New_Queen)
            Current_New_Queen = c(Current_New_Queen, rep(0, number0))
          }
          
          New_Queens <- rbind(New_Queens, Current_New_Queen) 
        } else {
          #Takes mothers alleles
          MotherAllele <- c(Queens$Allele_1[i], Queens$Allele_2[i])
          
          #Figures out how many alleles there are in the population and makes a new vector
          FatherAllele = c()
          
          #Takes fathers alleles
          for (k in 1:length(Allele_N)) {
            FatherAllele = c(FatherAllele, rep(Allele_N[k], Queens[i, k+6]))
          }
          
          #Randomly takes one of the mothers alleles and on of the fathers, then binds this into the New_Queens data table at the same position
          if (runif(1) <= Chance_Mutate) {
            #This creates a new allele, it's value is equal to one more than the number of alleles
            Allele1 = Mutant_Number + New_Allele_Counter
            New_Allele_Counter = New_Allele_Counter + 1
          } else {
            Allele1 = sample(MotherAllele, 1)
          }
          
          if (isTRUE(length(FatherAllele) == 1)) {
            Allele2 = FatherAllele
          } else {
            Allele2 <- sample(FatherAllele, 1)
          }
          
          Current_New_Queen <- c(Queens$Position[i], rep(0, 3), Allele1, Allele2, rep(0, length(Allele_N)))
          New_Queens <- rbind(New_Queens, Current_New_Queen) 
        }
      }
    }
  }
  
  colnames(New_Queens) = Column_Names
  
  #Creates a new column for alleles if the queen has mutated
  if (isTRUE(max(New_Queens$Allele_1) > Mutant_Number)) {
    Dif = max(New_Queens$Allele_1) - Mutant_Number
    k = c((Mutant_Number+1):max(New_Queens$Allele_1))
    New_Allele = data.frame(matrix(data = 0, nrow = length(New_Queens$Allele_1), ncol = Dif))
    Name = paste("Aus", k, sep="")
    names(New_Allele) = Name
    New_Queens = cbind(New_Queens, New_Allele)
  }
  
  #Removes the "extra" queens that the large vector produced, or removes any queens that are impossible, ie, their two sex alleles are the same.
  New_Queens <- New_Queens[New_Queens$Allele_1 != 0,]
  New_Queens <- New_Queens[New_Queens$Allele_1 != New_Queens$Allele_2,]
  
  #Kills this if they're all dead
  if (length(New_Queens$Position) == 0) {
    print("Stop. Stop, they're already dead!")
    break
  } 
  
  return(New_Queens)
}