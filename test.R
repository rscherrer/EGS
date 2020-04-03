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

# Function to read some data -- DONE
read_data <- function(folder, variable) {

  read_binary(paste0(folder, "/", variable, ".dat"))

}

read_data(simulations[2], "time")

# Function to read the time record -- DONE
read_time(simulations[2])

# Read population data
read_population <- function(folder, variable) {

  read_data(folder, variable)

}

read_population(simulations[2], "EI")

# Read counts from a given habitat and ecotype
read_counts <- function(folder, habitat, ecotype) {

  read_data(folder, paste0("count", habitat, ecotype))

}

read_counts(simulations[2], habitat = 0, ecotype = 1)

# Read total population sizes
read_total_counts <- function(folder) {

  n00 <- read_counts(folder, habitat = 0, ecotype = 0)
  n01 <- read_counts(folder, habitat = 0, ecotype = 1)
  n10 <- read_counts(folder, habitat = 1, ecotype = 0)
  n11 <- read_counts(folder, habitat = 1, ecotype = 1)

  mapply(sum, n00, n01, n10, n11)

}

read_total_counts(simulations[2])

# Repeat multiple vector elements multiple times -- DONE
mrep <- function(x, n) {

  if (all(n == n[1])) return (rep(x, each = n[1]))
  do.call("c", mapply(rep, x, n))

}
mrep(c(1, 2, 3), c(1, 2, 3))
mrep(c(1, 2, 3), c(10, 10, 10))

# Read individual data
read_individuals <- function(folder, variable) {

  t <- read_time(folder)
  n <- read_total_counts(folder)
  t_indiv <- mrep(t, n) # time at which each individual lived

  data <- read_data(folder, variable)
  split(data, t_indiv)

}

read_individuals(simulations[2], "population_x") # returns a list of generation-vectors of individual values

# Read locus-specific data
read_loci <- function(folder, variable) {

  t <- read_time(folder)
  data <- read_data(folder, variable)
  nloci <- length(data) / length(t)
  timepoints <- rep(t, each = nloci)
  split(data, timepoints)

}

out <- read_loci(simulations[2], "genome_Fst") # returns a list of generation-genome scans

# Read edge-specific data
read_edges <- function(folder, variable) {

  t <- read_time(folder)
  data <- read_data(folder, variable)
  nedges <- length(data) / length(t)
  timepoints <- rep(t, each = nedges)
  data <- split(data, timepoints)

  # But how many edges per trait?
  nedges_per_trait <- as.numeric(read_parameters(folder, "nedges")$nedges)
  nedges_per_trait
  traits <- mrep(0:2, nedges_per_trait)

  lapply(data, split, traits) # returns a list of generation-lists of trait-vectors of edge properties

}

read_edges(simulations[2], "network_corbreed")

# Functions across simulations

# Functio tell what simulations did have problems find_extinct / find_missing

# Function to read all parameter files and make a table with what is there



# Save whole genomes every now and then?

