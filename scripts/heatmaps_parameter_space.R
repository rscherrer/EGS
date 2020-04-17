# Run this script to produce a heatmap of simulation outcomes throughout
# parameter space

rm(list = ls())

library(EGS)
library(tidyverse)
library(pbapply)

variable <- "RI"
imgname_template <- "figures/heatmaps/mean/heatmap_%s_sim%s.png"
legendtitle <- "Reproductive isolation"
highcolor <- "lightblue"
summary <- "mean"

root_template <- "/media/raphael/bigass/simulations/EGS/EGS_sim%s"

for (i in 1) {

  root <- sprintf(root_template, i)
  imgname <- sprintf(imgname_template, variable, i)

  # Simulation folders
  simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

  # Keep only non-extinct simulations
  extincts <- find_extinct(root)
  simulations <- simulations[!simulations %in% extincts]

  # How many simulations?
  nsims <- length(simulations)

  # Extract some statistics of interest
  data <- pblapply(simulations, read_population, variable)
  data <- data.frame(do.call("c", data))
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

  # Rearrange the data for plotting
  data <- data %>% gather_("variable", "value", variable)

  if (summary == "mean") {

    # Summarize the data by computing the average statistic over the 100 last time generations
    smr <- data %>%
      filter(time > 19000) %>%
      group_by(hsymmetry, ecosel, replicate) %>%
      summarize(X = mean(value)) %>%
      ungroup() %>%
      group_by(hsymmetry, ecosel) %>%
      summarize(X = mean(X))

  } else if (summary == "number") {

    # Summarize the number of final values above a certain threshold
    smr <- data %>%
      group_by(hsymmetry, ecosel, replicate) %>%
      summarize(X = last(value)) %>%
      ungroup() %>%
      group_by(hsymmetry, ecosel) %>%
      summarize(X = length(which(X > 0.9)))

  }

  # Plot the heatmap
  p <- ggplot(smr, aes(x = hsymmetry, y = ecosel, fill = X)) +
    geom_tile() +
    theme_bw() +
    xlab("Habitat symmetry") +
    ylab("Ecological divergent selection") +
    labs(fill = legendtitle) +
    theme(legend.position = "top", legend.key.size = unit(5, "mm")) +
    scale_fill_gradient(low = "black", high = highcolor)
  p

  # Save it
  ggsave(imgname, p, width = 3, height = 3.6, dpi = 300)

  # Save the summary if needed (e.g. to combine heatmaps together)
  # saveRDS(smr, gsub("heatmaps", "heatmaps/rds", gsub(".png", ".rds", imgname)))

}
