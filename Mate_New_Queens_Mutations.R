Mate_New_Queens <- function(Male_Flight, Mean_Mates, SD_Mates, WLColony, WBD_Chance, A_Norm_Drones, Max_Drones, 
                            Drone_Spread, Queens, New_Queens, Samp_Cutoff, Chance_Mutate, Queenless_Colonies, Mutant_Number) {
  Pop_Alleles = Mutant_Number
  for (i in 1:length(New_Queens$Position)) {
    Counter_Mutant_Alleles = 1
    EligibleQueens = rbind(Queens, Queenless_Colonies)
    
    Up_Dist = New_Queens$Position[i] + Male_Flight
    Low_Dist = New_Queens$Position[i] - Male_Flight
    MatingDist_df = EligibleQueens[between(EligibleQueens$Position, Low_Dist, Up_Dist),]
    #MatingDist_df = MatingDist_df[MatingDist_df$Position != Queens$Position[i]]
    
    Rev_Drones_Up = MatingDist_df$Position + A_Norm_Drones
    Rev_Drones_Low = MatingDist_df$Position - A_Norm_Drones
    
    Up_Area = pnorm(Rev_Drones_Up, mean = New_Queens$Position[i], sd = Drone_Spread, lower.tail = FALSE)
    Low_Area = pnorm(Rev_Drones_Low, mean = New_Queens$Position[i], sd = Drone_Spread, lower.tail = FALSE)
    Area_Under_Curve = abs(Up_Area - Low_Area)
    Num_Drones = round(Area_Under_Curve*MatingDist_df$Fitness*Max_Drones)
    
    #Create a New_drones vector of the right length, very happy to have done this earlier for Num_Drones
    New_Drones = rep(NaN, sum(Num_Drones))
    counter = 0
    
    #IMPLEMENT SOME SORT OF CUT OFF FUNCTION FOR THE NUMBER OF DRONES BEING RANDOMLY SAMPLED. IE, IF THERE ARE MORE THAN 50 DRONES,
    #ONLY SAMPLE 50 AND THEN JUST SPLIT THE REST, IF THERE ARE LESS THAN 50 THEN SAMPLE ALL OF THEM, KEEPS THE  VARIATION WHEN SMALL
    #BUT TENDS TO 1/2 WHEN LARGE< AS IT ALWAYS DOES!!!!
    
    #Looks for whether this is 0 or not
    if (isTRUE(length(MatingDist_df$Position) > 0)) {
      
      #Goes through the above list
      for (j in 1:length(MatingDist_df$Position)) {
        
        if (MatingDist_df$Age[j] == GQL) {
          
          Num_Work_Drones = round(Num_Drones[j]/2)
          
          #Finds the inverse of this number, therefore the number of drones produced by the queen, or the drones produced by the queen with her alleles
          Inverse = Num_Drones[j] - Num_Work_Drones
          
          #Finds the alleles the colony has from fathers, and the distribution of these alleles. It then removes any alleles not found in the colony
          Allele_Distribution <- unname(unlist(MatingDist_df[j, (7:length(MatingDist_df))]))
          Relevant_Alleles = c(1:(length(MatingDist_df)-6))
          Relevant_Alleles = Relevant_Alleles[Allele_Distribution != 0]
          Allele_Distribution = Allele_Distribution[Allele_Distribution != 0]
          
          #This was inside the if statement, but I moved it outside to implement the ability to skip directly to sampling if Num_Each_All is all 0's
          New_Num_Drones = Num_Work_Drones - Samp_Cutoff
          Num_Each_All = round(New_Num_Drones*Allele_Distribution/sum(Allele_Distribution))
          
          #This if and statement implements a cutoff function, such that if there are loads of drones, the function only samples some of them
          #It will also check if a vector is all 0's, otherwise it will just sample all of them
          if (Num_Work_Drones > Samp_Cutoff && isTRUE(sum(Num_Each_All) > 0)) {
            
            New_Drones[(counter+1):(counter+Samp_Cutoff)] = SampleDrones(Samp_Cutoff, Relevant_Alleles, Allele_Distribution)
            
            New_Drones[(counter+Samp_Cutoff+1):(counter+Samp_Cutoff+sum(Num_Each_All))] = rep(Relevant_Alleles, Num_Each_All)
            
            counter = counter + Num_Work_Drones
            
          } else if (Num_Work_Drones > 0) {
            
            New_Drones[(counter+1):(counter+Num_Work_Drones)] = SampleDrones(Num_Work_Drones, Relevant_Alleles, Allele_Distribution)
            
            counter = counter + Num_Work_Drones
          }
          
          #Now it does as above, but much more simply as it's just considering the queens alleles. The probability is just all 1's now
          Relevant_Alleles <- unname(unlist(MatingDist_df[j, (5:6)]))
          Probs = rep(1, length(Relevant_Alleles))
          if (Inverse > Samp_Cutoff) {
            New_Drones[(counter+1):(counter+Inverse)] = SampleDronesCutOff(Inverse, Relevant_Alleles, Samp_Cutoff, Probs)
            
            counter = counter + Inverse
          } else {
            New_Drones[(counter+1):(counter+Inverse)] = SampleDrones(Inverse, Relevant_Alleles, Probs)
            
            counter = counter + Inverse
          }
          
        } else if (runif(1) <= WBD_Chance) {
          #First checks to see if the colony is randomly a worker producing colony, this needs to be fixed
          
          #If worker producing, figures out the proportion of drones produced by workers.
          #This need to be divided by 2, as the workers produce their own mothers allele 50% of the time.
          Num_Work_Drones = round(Num_Drones[j]*WLColony/2)
          
          #Finds the inverse of this number, therefore the number of drones produced by the queen, or the drones produced by the queen with her alleles
          Inverse = Num_Drones[j] - Num_Work_Drones
          
          #Finds the alleles the colony has from fathers, and the distribution of these alleles. It then removes any alleles not found in the colony
          Allele_Distribution <- unname(unlist(MatingDist_df[j, (7:length(MatingDist_df))]))
          Relevant_Alleles = c(1:(length(MatingDist_df)-6))
          Relevant_Alleles = Relevant_Alleles[Allele_Distribution != 0]
          Allele_Distribution = Allele_Distribution[Allele_Distribution != 0]
          
          #This was inside the if statement, but I moved it outside to implement the ability to skip directly to sampling if Num_Each_All is all 0's
          New_Num_Drones = Num_Work_Drones - Samp_Cutoff
          Num_Each_All = round(New_Num_Drones*Allele_Distribution/sum(Allele_Distribution))
          
          #This if and statement implements a cutoff function, such that if there are loads of drones, the function only samples some of them
          #It will also check if a vector is all 0's, otherwise it will just sample all of them
          if (Num_Work_Drones > Samp_Cutoff && isTRUE(sum(Num_Each_All) > 0)) {
            
            New_Drones[(counter+1):(counter+Samp_Cutoff)] = SampleDrones(Samp_Cutoff, Relevant_Alleles, Allele_Distribution)
            
            New_Drones[(counter+Samp_Cutoff+1):(counter+Samp_Cutoff+sum(Num_Each_All))] = rep(Relevant_Alleles, Num_Each_All)
            
            counter = counter + Num_Work_Drones
          } else if (Num_Work_Drones > 0) {
            New_Drones[(counter+1):(counter+Num_Work_Drones)] = SampleDrones(Num_Work_Drones, Relevant_Alleles, Allele_Distribution)
            
            counter = counter + Num_Work_Drones
          }
          
          #Now it does as above, but much more simply as it's just considering the queens alleles. The probability is just all 1's now
          Relevant_Alleles <- unname(unlist(MatingDist_df[j, (5:6)]))
          Probs = rep(1, length(Relevant_Alleles))
          if (Inverse > Samp_Cutoff) {
            New_Drones[(counter+1):(counter+Inverse)] = SampleDronesCutOff(Inverse, Relevant_Alleles, Samp_Cutoff, Probs)
            
            counter = counter + Inverse
          } else {
            New_Drones[(counter+1):(counter+Inverse)] = SampleDrones(Inverse, Relevant_Alleles, Probs)
            
            counter = counter + Inverse
          }
          
          #This does the exact same thing again, it just doesn't consider the chance the colony produces drones
        } else {
          Relevant_Alleles <- unname(unlist(MatingDist_df[j, (5:6)]))
          Probs = rep(1, length(Relevant_Alleles))
          if (Num_Drones[j] > Samp_Cutoff) {
            New_Drones[(counter+1):(counter+Num_Drones[j])] = SampleDronesCutOff(Num_Drones[j], Relevant_Alleles, Samp_Cutoff, Probs)
            
            counter = counter + Num_Drones[j]
          } else {
            New_Drones[(counter+1):(counter+Num_Drones[j])] = SampleDrones(Num_Drones[j], Relevant_Alleles, Probs)
            
            counter = counter + Num_Drones[j]
          }
        }
      }
    }
    
    New_Drones = New_Drones[!is.na(New_Drones)]
    
    N_of_Mates = RandomMateNumber(Mean_Mates, SD_Mates)
    
    if (length(New_Drones) > 0) {
      if (length(New_Drones) >= (N_of_Mates+1)) {
        #This if statement controls for if a queen is inundated by drones. It takes a sample of drones, equal to N_of_Mates, and then sorts them into the
        #right coloumns. Implements another for loop but it's really tiny
        Mated_Drones = sample(New_Drones, N_of_Mates)
      } else {
        #This is only true in cases in which the number of drones is low, then it just sorts them into the right columns, rather than sampling them first.
        Mated_Drones = New_Drones
      }
      
      for (l in 1:length(Mated_Drones)) {
        if (runif(1) <= Chance_Mutate) {
          Mated_Drones[l] = Pop_Alleles + Counter_Mutant_Alleles
          Counter_Mutant_Alleles = Counter_Mutant_Alleles + 1
        }
      }
      
      if (isTRUE(max(Mated_Drones) > Pop_Alleles)) {
        Dif = max(Mated_Drones) - Pop_Alleles
        k = c((Pop_Alleles+1):max(Mated_Drones))
        New_Allele = data.frame(matrix(data = 0, nrow = length(New_Queens$Allele_1), ncol = Dif))
        Name = paste("Aus", k, sep="")
        names(New_Allele) = Name
        New_Queens = cbind(New_Queens, New_Allele)
        Pop_Alleles = max(Mated_Drones)
      }
      
      Allele_Positions = as.integer(str_remove(names(New_Queens)[-(1:6)], "Aus"))
      
      for (k in 1:length(Allele_Positions)) {
        Allele = Allele_Positions[k]
        New_Queens[i, (k+6)] = length(which(Mated_Drones == Allele))
      }
    } else if (length(New_Drones) == 0) {
      #Hopefullthisfixes the issue with no mates, if and when it rearely comes up
      New_Queens$Allele_1[i] = 0
    }
    
    #Had some issues with this, so just here to double check
    
    if (sum(New_Queens[i,7:length(New_Queens)]) == 0) {
      New_Queens$Allele_1[i] = 0
    }
  }
  
  New_Queens <- New_Queens[New_Queens$Allele_1 != 0,]
  return(New_Queens)
  #Write file here with old queens
  #Potentially figure out way to get old queens to stay in system for few years here too
}