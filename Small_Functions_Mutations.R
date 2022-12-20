SampleDrones <- function(N, df, probability) {
  if (length(df) == 1) {
    New_Drones <- rep(df, N)
  } else {
    New_Drones <- sample(c(df), N, replace=TRUE, prob = probability)
  }
  return(New_Drones)
}

SampleDronesCutOff <- function(N, df, Cutoff, probability) {
  Remain = (N-Cutoff)%%length(df)
  Split = (N-Cutoff-Remain)/length(df)
  New_Drones <- rep(df, Split)
  N2 = Cutoff+Remain
  New_Drones <- c(New_Drones, SampleDrones(N2, df, probability))
  return(New_Drones)
}

AddColoumn <- function(OlderFrame, NewerFrame) {
  if (length(OlderFrame) < length(NewerFrame)) {
    Empty_Frame = data.frame(matrix(0, nrow = length(OlderFrame[,1]), ncol = length(NewerFrame[1,])))
    Empty_Frame = Empty_Frame[,-(1:length(OlderFrame))]
    OlderFrame = cbind(OlderFrame, Empty_Frame)
    names(OlderFrame) = names(NewerFrame)
  }
  return(OlderFrame)
}

CreateStore <- function(t, Queens) {
  namesdf = names(Queens)
  namesdf = c("Generation", namesdf)
  Generation = rep(t, length(Queens[,1]))
  col = length(Queens[1,]) + 1
  row = length(Queens[,1])
  CurrentGen = data.frame(matrix(NaN, nrow = row, ncol = col))
  names(CurrentGen) = namesdf
  CurrentGen[,1] = Generation
  CurrentGen[1:row, 2:col] = Queens
  return(CurrentGen)
}

StoreValue <- function(OlderFrame, NewerFrame) {
  col = length(OlderFrame[1,])
  rowold = length(OlderFrame[,1])
  rownew = length(NewerFrame[,1])
  totalrow = rowold+rownew
  namesdf = names(OlderFrame)
  Store = data.frame(matrix(NaN, nrow = totalrow, ncol = col))
  names(Store) = namesdf
  Store[1:rowold, 1:col] = OlderFrame
  Store[(rowold+1):totalrow, 1:col] = NewerFrame
  return(Store)
}

PrintValue <- function(df, name) {
  name = name
  filename = paste(name, ".txt", sep = "")
  write.table(df, filename, sep="\t", row.names = FALSE)
}

RandomMateNumber <- function(mean, sd) {
  Num = round(rnorm(1, mean, sd))
  if (Num < 1) {
    Num = 1
  }
  return(Num)
}

FunctionalFitness <- function(Queens) {
  Mid = 0.75
  Str = 25
  MinF = 0.5
  x = Queens$Fitness
  fx = MinF + (1-MinF)/(1 + exp(-Str*(x-Mid)))
  return(fx)
}