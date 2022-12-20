library(doParallel)
library(foreach)
library(reshape2)
library(tibble)

source("Simulation_Mutations.R")
source("Create_Queens.R")
source("Pressure_To_Move.R")
source("Movement.R")
source("Mate_New_Queens_Mutations.R")
source("New_Queen_Fitness_Mutations.R")
source("Growth_Rates.R")
source("Small_Functions_Mutations.R")

#For Soft Selection
source("Carrying_Capacity_Random.R")
source("Reproduce_Mutations.R")
#For Medium Selection
source("Carrying_Capacity_Random_NewFitness.R")
source("Reproduce_NewFitness_Mutations.R")

Start_D = -1
End_D = 1
N_Ran_Queens = 10
Sep = (End_D - Start_D)/(N_Ran_Queens)
N_Unequal_Alleles = 2
Proportion_Unequal = 0.8
Mean_Mates = 29
SD_Mates = 8
Max_Jump = 10
radius_sight = 4
alpha_jump = 1
Jump_sd = 4
K_Dist = 0.5
Male_Flight = 5
WLColony = 0
WBD_Chance = 0
A_Norm_Drones = .5
Max_Drones = 1000
Drone_Spread = 2
GQL = 5
Samp_Cutoff = 50
Queenless_Chance = 0.3

registerDoParallel(cores=detectCores())

#CHance Mutate = 10^-4, 10^-5 or 10^-6
Chance_Mutate = 1*10^-4
Iterations = 30
#K = 30 or 100
K_Num = 100
#N_Initial_Allele = 7 or 14
N_Initial_Alleles = 7
Max_Swarms = 3
beta_jump = 0.5*Max_Jump
#This function can bound the population to a certain distance limit, to do so make BOUNDED = "YES"
Max_Dist = 100
Min_Dist = -Max_Dist
Bounded = "NO"

foreach (z=01:96, .packages = "tidyverse") %dopar% {
  Output = Simulation(Iterations, K_Num, Max_Swarms)
  Run = Output[[1]]
  Mutants = Output[[2]]
  filename = paste(paste("Name", sprintf("%02d", z), sep="_"), ".txt", sep= "")
  filename2 = paste(paste("Name_Mutants", sprintf("%02d", z), sep="_"), ".txt", sep= "")
  write.table(Run, filename, sep="\t", row.names = FALSE)
  write.table(Mutants, filename2, sep="\t", row.names = FALSE)
}