# Make multiple heatmaps across parameter space of a population-variable
# The axes of the heatmaps are habitat symmetry and ecological selection
# The facets correspond to whatever parameters are different between different
# batches of simulations (here dispersal and degree of epistasis), but you need
# to make sure those are correctly appended to their batches of simulations at
# the end of the for-loop

rm(list = ls())

library(EGS)
library(tidyverse)
library(pbapply)
library(cowplot)

# What variable to plot?
variable <- "EI"

# What legend title?
legendtitle <- "Ecological divergence"

# Color?
highcolor <- "lightgreen"

# File name of the figure?
imgname <- "figures/heatmaps/heatmaps_ecological_divergence.png"

# Root directories of the different batches of simulations
roots <- list.files("/media/raphael/bigass/simulations/EGS", pattern = "EGS_", full.names = TRUE)

# Collect simulation data from multiple batches
data <- lapply(roots, function(root) {

  # Simulation folders
  simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

  # Keep only non-extinct simulations
  extincts <- find_extinct(root)
  simulations <- simulations[!simulations %in% extincts]

  # How many simulations?
  nsims <- length(simulations)

  # Extract some statistics of interest
  data <- pblapply(simulations, read_population, variable)
  data <- do.call("rbind", data) %>% as.data.frame()

  # Add time
  times <- read_time(simulations[1])
  data$time <- rep(times, nsims)

  # How many time points?
  ntimes <- length(times)

  # Add simulation identifiers
  data$id <- rep(seq_len(nsims), each = ntimes)

  # Extract some corresponding parameters and arrange them in a table
  parameters <- pblapply(simulations, read_parameters, c("hsymmetry", "ecosel"))
  parameters <- do.call("rbind", lapply(parameters, function(parameters) sapply(parameters, function(parameter) as.numeric(rep(parameter, ntimes)))))
  parameters <- as.data.frame(parameters)

  # Assemble the data and the parameters together
  data <- cbind(parameters, data)

  # Rearrange the data for plotting
  data <- data %>% gather_("variable", "value", variable)

  # Add extra parameters (may have to change here)
  extras <- read_parameters(simulations[1], c("scaleA", "scaleI", "dispersal"))
  data$gpmap <- sum(as.numeric(unlist(extras[-1])) * 2 ^ (seq_len(6)-1)) # binary code
  data$dispersal <- as.numeric(extras$dispersal)

  # Summarize the data by computing the average statistic over the 100 last time generations
  smr <- data %>%
    filter(time > 19000) %>%
    group_by(hsymmetry, ecosel, gpmap, dispersal, id) %>%
    summarize(X = mean(X)) %>%
    ungroup() %>%
    group_by(hsymmetry, ecosel, gpmap, dispersal) %>%
    summarize(X = mean(X))

  return (smr)

})

data <- do.call("rbind", data)

m_labels <- gsub("dispersal", "m", make_facet_labels(data, "dispersal"))
data$gp <- factor(data$gp)
levels(data$gp) <- c("Additive genetics", "Epistatic genetics")
gp_labels <- gsub("gp = ", "", make_facet_labels(data, "gp"))

p <- ggplot(data, aes(x = hsymmetry, y = ecosel, fill = X)) +
  geom_tile() +
  xlab("Habitat symmetry") +
  ylab("Ecological divergent selection") +
  labs(fill = legendtitle) +
  facet_grid(dispersal ~ gp, labeller = labeller(dispersal = m_labels, gp = gp_labels)) +
  scale_fill_gradient(low = "black", high = highcolor) +
  theme_bw() +
  theme(legend.position = "bottom")
p

ggsave(imgname, p, height = 4.5, width = 3, dpi = 300)
saveRDS(p, gsub(".png", ".rds", imgname))
