# Study a single simulation

rm(list = ls())

library(EGS)
library(tidyverse)
library(pbapply)
library(GGally)
library(cowplot)

root <- "/media/raphael/bigass/simulations/EGS/EGS_sim2"
simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

# Keep only non-extinct simulations
extincts <- find_extinct(root)
simulations <- simulations[!simulations %in% extincts]

# Filter simulations that reached ecological isolation
finals <- pbsapply(simulations, function(simulation) read_population(simulation, "EI") %>% unlist() %>% last())
simulations <- simulations[finals > 0.9]

# Choose one
simulation <- simulations[400]

# Diagnose the simulation
read_parameters(simulation, c("hsymmetry", "ecosel")) %>% unlist()
data <- data.frame(
  time = read_time(simulation),
  ei = read_population(simulation, "EI"),
  si = read_population(simulation, "SI"),
  ri = read_population(simulation, "RI")
)

ggplot(data, aes(x = time)) +
  geom_line(aes(y = ei), color = "green") +
  geom_line(aes(y = si), color = "red") +
  geom_line(aes(y = ri), color = "blue") +
  ylim(0, 1)

# Read the genome scans through time and make a long table
data <- read_loci(simulation, "genome_Fst")
nloci <- length(data[[1]])
ntimes <- length(data)
data <- as.data.frame(do.call("cbind", data))
data <- data %>% gather_("time", "Fst", colnames(data)) %>% mutate(time = as.numeric(time))
data$locus <- rep(seq_len(nloci), ntimes)

head(data)

# Read the genetic architecture for that simulation
architecture <- read_architecture(simulation)

# Add traits encoded by the loci to the table
traits <- architecture$traits
data$trait <- factor(rep(traits, ntimes))

# Get locus effect size
effects <- architecture$effects
data$effect <- rep(effects, ntimes)

# Count the number of connections for each locus in the genome and add it to the data
degrees. <- do.call("c", lapply(architecture$networks, function(network) table(with(network, c(edges0, edges1)))))
names(degrees.) <- as.character(as.numeric(gsub("^.*\\.", "", names(degrees.))) + 1)
degrees <- rep(0, nloci)
degrees[as.numeric(names(degrees.))] <- degrees.
data$degree <- rep(degrees, ntimes)

# Does Fst correlate with dditive variance?
variances <- read_loci(simulation, "genome_varA")
variances <- do.call("c", lapply(variances, function(variances) variances[seq_len(nloci) * 3]))
data$varA <- variances

# Interaction variance?
variances <- read_loci(simulation, "genome_varN")
variances <- do.call("c", lapply(variances, function(variances) variances[seq_len(nloci) * 3]))
data$varN <- variances

data$Qst <- do.call("c", read_loci(simulation, "genome_Qst"))
data$Cst <- do.call("c", read_loci(simulation, "genome_Cst"))

# All together?
ggpairs(data %>% filter(time == 19900), aes(color = trait),
        columns = c("Fst", "varA", "varN", "effect", "degree", "Qst", "Cst"))

# Multiple scans
pFst <- ggplot(data %>% filter(time == 19900), aes(x = locus, y = Fst, color = trait)) +
  geom_point()

pvarA <- ggplot(data %>% filter(time == 19900), aes(x = locus, y = varA, color = trait)) +
  geom_point()

pvarN <- ggplot(data %>% filter(time == 19900), aes(x = locus, y = varN, color = trait)) +
  geom_point()

pQst <- ggplot(data %>% filter(time == 19900), aes(x = locus, y = Qst, color = trait)) +
  geom_point()

pCst <- ggplot(data %>% filter(time == 19900), aes(x = locus, y = Cst, color = trait)) +
  geom_point()

pFst. <- ggplot(data %>% filter(time == 19900), aes(x = trait, y = Fst, color = trait)) +
  geom_violin() +
  geom_jitter()

pvarA. <- ggplot(data %>% filter(time == 19900), aes(x = trait, y = varA, color = trait)) +
  geom_violin() +
  geom_jitter()

pvarN. <- ggplot(data %>% filter(time == 19900), aes(x = trait, y = varN, color = trait)) +
  geom_violin() +
  geom_jitter()

pQst. <- ggplot(data %>% filter(time == 19900), aes(x = trait, y = Qst, color = trait)) +
  geom_violin() +
  geom_jitter()

pCst. <- ggplot(data %>% filter(time == 19900), aes(x = trait, y = Cst, color = trait)) +
  geom_violin() +
  geom_jitter()

plot_grid(pFst, pFst., pQst, pQst., pCst, pCst., pvarA, pvarA., pvarN, pvarN., ncol = 2, nrow = 5, rel_widths = c(2, 1))
