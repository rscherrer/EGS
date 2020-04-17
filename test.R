rm(list = ls())

library(EGS)

# Test stuff here

library(tidyverse)
simulations <- paste0("data/", list.files("data"))

# Functions for individual simulations

# Function to read the parameter file -- DONE
read_paramfile(paste0(simulations[1], "/paramlog.txt"), c("hsymmetry", "ecosel"))

# Function to read parameters from the simulation folder -- DONE
read_parameters(simulations[2], c("hsymmetry", "ecosel"))

# Function to read a binary file read_data -- DONE
read_binary(paste0(simulations[1], "/time.dat"))

# Function to know if a simulation went extinct is_exinct -- DONE
is_extinct(simulations[1])

# Function to know of a simulation did not run is_missing -- DONE
is_missing(simulations[2])

# Function to read data -- DONE
read_data(simulations[2], "time")

# Function to read the time record -- DONE
read_time(simulations[2])

# Read population data -- DONE
read_population(simulations[2], "EI")

# Read counts from a given habitat and ecotype -- DONE
read_counts(simulations[2], habitat = 0, ecotype = 1)

# Read total population sizes -- DONE
read_total_counts(simulations[2])

# Repeat multiple vector elements multiple times -- DONE
mrep(c(1, 2, 3), c(1, 2, 3))
mrep(c(1, 2, 3), c(10, 10, 10))

# Read individual data -- DONE
read_individuals(simulations[2], "population_x") # returns a list of generation-vectors of individual values

# Read locus-specific data -- DONE
read_loci(simulations[2], "genome_Fst") # returns a list of generation-genome scans

# Read edge-specific data -- DONE
read_edges(simulations[2], "network_corbreed")

# String to numeric vector -- DONE
str2vec(factor("1 2 3 4"))

# Function to read an architecture -- DONE
out <- read_archfile(paste0(simulations[2], "/architecture.txt"))
str(out)

out$networks$ecotrait$weights %>% hist

# Read architecture from the folder -- DONE
arch <- read_architecture(simulations[1])
arch$traits[arch$networks$ecotrait$edges0]

# Find extinct simulations -- DONE
find_extinct(simulations, pb = FALSE)
extincts <- find_extinct("/media/raphael/bigass/simulations/EGS/EGS_sim1")

# Find missing simulations -- DONE
find_missing("/media/raphael/bigass/simulations/EGS/EGS_sim1")

