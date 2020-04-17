# Speciation trajectories biphase plots

rm(list = ls())

library(EGS)
library(pbapply)
library(tidyverse)
library(cowplot)

variables <- c("EI", "RI")

ylabel <- "Reproductive isolation"
xlabel <- "Ecological divergence"

imgname_template <- "figures/cotrajectories/cotrajectories_%s_%s_sim%s.png"

# Root directory of the different batches of simulations
root_template <- "/media/raphael/bigass/simulations/EGS/EGS_sim%s"

for (i in 1) {

  imgname <- sprintf(imgname_template, variables[1], variables[2], i)
  root <- sprintf(root_template, i)

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
  data <- data %>% group_by(hsymmetry, ecosel, time) %>% mutate(replicate = seq_len(n()))

  # Add final value
  data <- data %>% group_by(hsymmetry, ecosel, replicate) %>% mutate(final = last(get(variables[1])))

  data$ecosel <- factor(data$ecosel, levels = rev(levels(factor(data$ecosel))))
  h_labels <- gsub("hsymmetry", "h", make_facet_labels(data, "hsymmetry"))
  s_labels <- gsub("ecosel", "s", make_facet_labels(data, "ecosel"))

  p <- ggplot(data, aes(x = get(variables[1]), y = get(variables[2]), color = final, alpha = factor(replicate))) +
    geom_line() +
    facet_grid(ecosel ~ hsymmetry, labeller = labeller(ecosel = s_labels, hsymmetry = h_labels)) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(ylabel) +
    xlab(xlabel) +
    scale_color_gradient(low = "lightgray", high = "black") +
    scale_alpha_manual(values = runif(100, min = 0.49, max = 0.51))
  p

  ggsave(imgname, p, height = 6, width = 5, dpi = 300)


}
