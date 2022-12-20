Pressure_To_Move <- function(radius_sight, K_Num, K_Dist, Queens, Bounded, Max_Dist, Min_Dist) {
  Push=c()
  for (i in 1:length(Queens$Position)) {
    
    if (Bounded == "YES") {
      if (xor(Queens$Position[i] < (Min_Dist + radius_sight), Queens$Position[i] > (Max_Dist - radius_sight))) {
        
        Push[i] = 0
        
      } else {
        
        Dens = K_Num*(radius_sight/K_Dist)
        
        Up_Dist = Queens$Position[i] + radius_sight
        Low_Dist = Queens$Position[i] - radius_sight
        Position_df = Queens$Position[between(Queens$Position, Low_Dist, Up_Dist)]
        
        Position_Pos = Position_df[Position_df>Queens$Position[i]]
        Position_Neg = Position_df[Position_df<Queens$Position[i]]
        
        if (length(Position_Pos) == 0){
          DistPos = 0
        } else if (mean(Position_Pos) == 0) {
          DistPos = 0
        } else {
          DistPos = mean(Position_Pos)
        }
        
        if (length(Position_Neg) == 0){
          DistNeg = 0
        } else if (mean(Position_Neg) == 0) {
          DistNeg = 0
        } else {
          DistNeg = mean(Position_Neg)
        }
        
        if (DistNeg == 0 && DistPos == 0) {
          Push[i] = 0
        } else if (DistNeg == 0) {
          Push[i] = ((Queens$Position[i]-DistPos)*exp(-length(Position_Pos)/Dens))
        } else if (DistPos == 0) {
          Push[i] = ((Queens$Position[i]-DistNeg)*exp(-length(Position_Neg)/Dens))
        } else {
          Push[i] = (((Queens$Position[i]-DistPos)*exp(-length(Position_Pos)/Dens)) + ((Queens$Position[i]-DistNeg)*exp(-length(Position_Neg)/Dens)))/2 
        }
        
        Push[i] = Push[i]/radius_sight
        #Push[i] = (Queens$Position[i] - Dist)
        
        #AvgThis = Dist_df/(radius_sight^2)
        
        #AvgThis = (abs(Dist_df)/Dist_df)*(radius_sight - abs(Dist_df))/(radius_sight^2)
        
        #if (length(AvgThis) == 0){
        # Push[i] = 0
        #} else if (mean(AvgThis) == 0) {
        # Push[i] = 0
        #} else {
        # Push[i] = mean(AvgThis)
        #}
        
      }
    } else {
      
      Dens = K_Num*(radius_sight/K_Dist)
      
      Up_Dist = Queens$Position[i] + radius_sight
      Low_Dist = Queens$Position[i] - radius_sight
      Position_df = Queens$Position[between(Queens$Position, Low_Dist, Up_Dist)]
      
      Position_Pos = Position_df[Position_df>Queens$Position[i]]
      Position_Neg = Position_df[Position_df<Queens$Position[i]]
      
      if (length(Position_Pos) == 0){
        DistPos = 0
      } else if (mean(Position_Pos) == 0) {
        DistPos = 0
      } else {
        DistPos = mean(Position_Pos)
      }
      
      if (length(Position_Neg) == 0){
        DistNeg = 0
      } else if (mean(Position_Neg) == 0) {
        DistNeg = 0
      } else {
        DistNeg = mean(Position_Neg)
      }
      
      if (DistNeg == 0 && DistPos == 0) {
        Push[i] = 0
      } else if (DistNeg == 0) {
        Push[i] = ((Queens$Position[i]-DistPos)*exp(-length(Position_Pos)/Dens))
      } else if (DistPos == 0) {
        Push[i] = ((Queens$Position[i]-DistNeg)*exp(-length(Position_Neg)/Dens))
      } else {
        Push[i] = (((Queens$Position[i]-DistPos)*exp(-length(Position_Pos)/Dens)) + ((Queens$Position[i]-DistNeg)*exp(-length(Position_Neg)/Dens)))/2 
      }
      
      Push[i] = Push[i]/radius_sight
      #Push[i] = (Queens$Position[i] - Dist)
      
      #AvgThis = Dist_df/(radius_sight^2)
      
      #AvgThis = (abs(Dist_df)/Dist_df)*(radius_sight - abs(Dist_df))/(radius_sight^2)
      
      #if (length(AvgThis) == 0){
      # Push[i] = 0
      #} else if (mean(AvgThis) == 0) {
      # Push[i] = 0
      #} else {
      # Push[i] = mean(AvgThis)
      #}
      
    }
  }
  return(Push)
}