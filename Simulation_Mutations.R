Simulation <- function(Iterations, K_Num, Max_Swarms) {
  
  Queens <- Create_Queens(Start_D, End_D, Sep, Mean_Mates, N_Initial_Alleles)
  
  Position_Birth = c()
  Allele = c()
  Time = c()
  Fit = c()
  Position_Moved = c()
  Dead_Or_Alive = c()
  Mutants <- data.frame(matrix(nrow = 0, ncol = 6))
  names(Mutants) = c("Allele", "Time", "Fit", "Position_Birth", "Position_Moved", "Dead_Or_Alive")
  
  Mutant_Number = N_Initial_Alleles
  
  t=1
  
  Push <- Pressure_To_Move(radius_sight, K_Num, K_Dist, Queens, Bounded, Max_Dist, Min_Dist)
  
  Queens <- Movement(alpha_jump, beta_jump, Max_Jump, Jump_sd, Queens, Push, Bounded, Max_Dist, Min_Dist)
  
  Queens <- Carrying_Capacity(K_Dist, K_Num, Queens)
  
  CurrentGen = CreateStore(t, Queens)
  
  StoredValues = CurrentGen
  
  repeat {
    
    New_Queens <- Reproduce(Max_Swarms, GQL, Queens, Chance_Mutate, Queenless_Chance, Mutant_Number)
    
    Old_Mutant_Number = Mutant_Number
    
    Mutant_Number = max(c(max(as.integer(str_remove(names(New_Queens)[-(1:6)], "Aus"))), Mutant_Number))
    
    Queenless_Colonies = New_Queens[New_Queens$Age == GQL, ]
    
    New_Queens = New_Queens[New_Queens$Age == 0, ]
    
    Queens <- AddColoumn(Queens, New_Queens)
    
    New_Queens <- Mate_New_Queens(Male_Flight, Mean_Mates, SD_Mates, WLColony, WBD_Chance, A_Norm_Drones, Max_Drones, Drone_Spread, 
                                  Queens, New_Queens, Samp_Cutoff, Chance_Mutate, Queenless_Colonies, Mutant_Number)
    
    Mutant_Number = max(c(max(as.integer(str_remove(names(New_Queens)[-(1:6)], "Aus"))), Mutant_Number))
    
    Queens$Age = Queens$Age + 1
    
    for (i in 1:length(Queens$Age)) {
      if (Queens$Age[i] == GQL) {
        Queens$Allele_1[i] = 0
      }
    }
    
    Queens <- Queens[Queens$Allele_1 != 0,]
    New_Queens = New_Queens[New_Queens$Allele_1 != 0,]
    New_Queens = New_Queens[New_Queens$Allele_2 != 0,]
    
    Fitness <- New_Queen_Fitness(New_Queens)
    
    New_Queens$Fitness = Fitness
    
    Position_Birth = c()
    Allele = c()
    Time = c()
    Fit = c()
    Position_Moved = c()
    Dead_Or_Alive = c()
    counter = 0
    if (isTRUE(Mutant_Number > Old_Mutant_Number)) {
      for (i in (Old_Mutant_Number+1):Mutant_Number) {
        counter = counter + 1
        Allele = c(Allele, (Old_Mutant_Number+counter))
        ThisAllele = (Old_Mutant_Number+counter)
        Distance = Mutant_Number - Old_Mutant_Number
        ThisAllelePos = length(New_Queens) + counter - Distance
        Position_Birth = c(Position_Birth, New_Queens$Position[xor(New_Queens[,ThisAllelePos]==1, xor(New_Queens$Allele_1==ThisAllele, New_Queens$Allele_2==ThisAllele))])
        Time = c(Time, (t+1))
        Fit = c(Fit, New_Queens$Fitness[xor(New_Queens[,ThisAllelePos]==1, xor(New_Queens$Allele_1==ThisAllele, New_Queens$Allele_2==ThisAllele))])
      }
    }
    
    Queens <- AddColoumn(Queens, New_Queens)
    
    Queens <- rbind.data.frame(Queens, New_Queens)
    
    Queens = Queens[order(Queens$Position), ]
    
    Queenless_Colonies = c()
    
    New_Queens = c()
    
    Push <- Pressure_To_Move(radius_sight, K_Num, K_Dist, Queens, Bounded, Max_Dist, Min_Dist)
    
    Queens <- Movement(alpha_jump, beta_jump, Max_Jump, Jump_sd, Queens, Push, Bounded, Max_Dist, Min_Dist)
    
    counter = 0
    if (isTRUE(Mutant_Number > Old_Mutant_Number)) {
      for (i in (Old_Mutant_Number+1):Mutant_Number) {
        counter = counter + 1
        Distance = Mutant_Number - Old_Mutant_Number
        ThisAllelePos = length(Queens) + counter - Distance
        ThisAllele = (Old_Mutant_Number+counter)
        Position_Moved = c(Position_Moved, Queens$Position[xor(Queens[,ThisAllelePos]==1, xor(Queens$Allele_1==ThisAllele, Queens$Allele_2==ThisAllele))])
      }
    }
    
    Queens = Queens[order(Queens$Position),]
    
    Queens <- Carrying_Capacity(K_Dist, K_Num, Queens)
    
    counter = 0
    if (isTRUE(Mutant_Number > Old_Mutant_Number)) {
      for (i in (Old_Mutant_Number+1):Mutant_Number) {
        counter = counter + 1
        Distance = Mutant_Number - Old_Mutant_Number
        ThisAllelePos = length(Queens) + counter - Distance
        ThisAllele = (Old_Mutant_Number+counter)
        Number1 = which(Queens$Position == Position_Moved[counter])
        Number2 = which(xor(Queens[,(ThisAllelePos)]==1, xor(Queens$Allele_1==ThisAllele, Queens$Allele_2==ThisAllele)))
        Dead_Or_Alive = c(Dead_Or_Alive, sum(isTRUE(Number2 %in% Number1)))
      }
      New_Mutants = data.frame(Allele, Time, Fit, Position_Birth, Position_Moved, Dead_Or_Alive)
      
      Mutants = rbind(Mutants, New_Mutants)
    }
    
    Mutant_Number = max(c(max(as.integer(str_remove(names(Queens)[-(1:6)], "Aus"))), Mutant_Number))
    
    Allele_N = as.integer(str_remove(names(Queens)[-(1:6)], "Aus"))
    
    Dead_Allele_Counter = c()
    for (i in 7:length(Queens)) {
      if (sum(sum(Queens$Allele_1 == Allele_N[i-6]), sum(Queens$Allele_2 == Allele_N[i-6]), sum(Queens[,i])) == 0) {
        Dead_Allele_Counter = c(Dead_Allele_Counter, i)
      }
    }
    if (isTRUE(length(Dead_Allele_Counter) > 0)) {
      Queens = Queens[,-(Dead_Allele_Counter)]
    }
    
    if (t+1 == Iterations) {
      break
    }
    t = t+1
    
    Only_alleles = Queens[,-(1:6)]
    A = (1:Mutant_Number)
    colnames = c(paste("Aus", A, sep=""))
    Allele_List = data.frame(matrix(data=0, nrow = length(Queens[,1]), ncol = Mutant_Number))
    names(Allele_List) = colnames
    check = names(Queens)
    crossreference = which(colnames %in% check)
    Allele_List[,crossreference] = Only_alleles
    Extra_Queens = data.frame(Queens[,1:6], Allele_List)
    CurrentGen = CreateStore(t, Extra_Queens)
    
    StoredValues <- AddColoumn(StoredValues, CurrentGen)
    StoredValues = StoreValue(StoredValues, CurrentGen)
  }
  
  t = t+1
  
  Only_alleles = Queens[,-(1:6)]
  A = (1:Mutant_Number)
  colnames = c(paste("Aus", A, sep=""))
  Allele_List = data.frame(matrix(data=0, nrow = length(Queens[,1]), ncol = Mutant_Number))
  names(Allele_List) = colnames
  check = names(Queens)
  crossreference = which(colnames %in% check)
  Allele_List[,crossreference] = Only_alleles
  Extra_Queens = data.frame(Queens[,1:6], Allele_List)
  CurrentGen = CreateStore(t, Extra_Queens)
  
  StoredValues <- AddColoumn(StoredValues, CurrentGen)
  StoredValues = StoreValue(StoredValues, CurrentGen)
  
  Output = list(StoredValues, Mutants)
  return(Output)
}