# Speciation trajectories biphase plots, habitat symmetry values flattened together

rm(list = ls())

library(EGS)
library(pbapply)
library(tidyverse)
library(cowplot)

variables <- c("EI", "SI")

ylabel <- "Spatial isolation"
xlabel <- "Ecological divergence"

flatten <- "hsymmetry"
legendtitle <- "Habitat symmetry"

imgname <- "figures/cotrajectories/flat/cotrajectories_ESI_flat_sim6.png"

# Root directory of the different batches of simulations
root <- "/media/raphael/bigass/simulations/EGS/EGS_sim6"

# Simulation folders
simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

# Keep only non-extinct simulations
extincts <- find_extinct(root)
simulations <- simulations[!simulations %in% extincts]

# How many simulations?
nsims <- length(simulations)

# Extract some statistics of interest
data <- pblapply(simulations, function(simulation) sapply(variables, function(variable) read_population(simulation, variable)))
data <- data.frame(do.call("rbind", data) %>% as.data.frame())
colnames(data) <- variables

# Add time
times <- read_time(simulations[1])
data$time <- rep(times, nsims)

# How many time points?
ntimes <- length(times)

# Extract some corresponding parameters and arrange them in a table
parameters <- pblapply(simulations, read_parameters, c("hsymmetry", "ecosel"))
parameters <- do.call("rbind", lapply(parameters, function(parameters) sapply(parameters, function(parameter) as.numeric(rep(parameter, ntimes)))))
parameters <- as.data.frame(parameters)

# Assemble the data and the parameters together
data <- cbind(parameters, data)

# Add simulation identifiers
data <- data %>% group_by(ecosel, time) %>% mutate(replicate = seq_len(n()))

data$hsymmetry <- factor(data$hsymmetry)
s_labels <- gsub("ecosel", "s", make_facet_labels(data, "ecosel"))

p <- ggplot(data, aes(x = get(variables[1]), y = get(variables[2]), color = get(flatten), alpha = factor(replicate))) +
  geom_line() +
  facet_wrap(. ~ ecosel, labeller = labeller(ecosel = s_labels)) +
  theme_bw() +
  ylab(ylabel) +
  xlab(xlabel) +
  labs(color = "Habitat symmetry") +
  guides(alpha = FALSE) +
  theme(legend.position = "top") +
  scale_color_manual(values = sapply(seq_along(levels(data$hsymmetry)), function(i) rgb((i - 1) * 0.15, (i - 1) * 0.15, (i - 1) * 0.15))) +
  scale_alpha_manual(values = runif(500, min = 0.49, max = 0.51))
p

ggsave(imgname, p, height = 5, width = 5, dpi = 300)
