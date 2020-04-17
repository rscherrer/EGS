# Speciation trajectories through time

rm(list = ls())

library(EGS)
library(pbapply)
library(tidyverse)
library(cowplot)

variable <-"varN_z"
highcolor <- "black"
ylabel <- "Interaction variance of the mating trait"

imgname_template <- "figures/trajectories/trajectories_%s_sim%s.png"

# Root directory of the different batches of simulations
root_template <- "/media/raphael/bigass/simulations/EGS/EGS_sim%s"

# Go through all simulation batches
for (i in 1) {

  imgname <- sprintf(imgname_template, variable, i)
  root <- sprintf(root_template, i)

  # Simulation folders
  simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

  # Keep only non-extinct simulations
  extincts <- find_extinct(root)
  simulations <- simulations[!simulations %in% extincts]

  # How many simulations?
  nsims <- length(simulations)

  # Extract some statistics of interest
  data <- pblapply(simulations, read_population, variable)
  data <- data.frame(do.call("c", data) %>% as.data.frame())
  colnames(data) <- variable

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
  data <- data %>% group_by(hsymmetry, ecosel, replicate) %>% mutate(final = last(get(variable)))

  data$ecosel <- factor(data$ecosel, levels = rev(levels(factor(data$ecosel))))
  h_labels <- gsub("hsymmetry", "h", make_facet_labels(data, "hsymmetry"))
  s_labels <- gsub("ecosel", "s", make_facet_labels(data, "ecosel"))

  p <- ggplot(data, aes(x = time / 1000, y = get(variable), color = final, alpha = factor(replicate))) +
    geom_line() +
    facet_grid(ecosel ~ hsymmetry, labeller = labeller(ecosel = s_labels, hsymmetry = h_labels)) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(ylabel) +
    xlab("Time (\U00D7 1000 generations)") +
    scale_color_gradient(low = "lightgray", high = highcolor) +
    scale_alpha_manual(values = runif(100, min = 0.49, max = 0.51))
  p

  ggsave(imgname, p, height = 6, width = 5, dpi = 300)

}

