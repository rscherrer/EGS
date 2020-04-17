# Compare the rates of completion between several batches of simulations

rm(list = ls())

library(EGS)
library(pbapply)
library(tidyverse)

# File name of the figure to save
imgname_template <- "figures/boxplots/boxplots_rates_EI_h_%s_s_%s_batches_%s_%s.png"

h_values <- c(0, 0.25, 0.5, 0.75, 1)
s_values <- rep(1.2, 5)

# What variable to analyze?
variable <- "EI"
ylabel <- "Rate of eological divergence (unit per generation)"

# What batches of simulations to compare?
batches <- c(1, 2)
xlevels <- c("Additive", "Epistatic")
xlabel <- "Genotype-phenotype map"
xcolors <- c("goldenrod", "seagreen")

# For each set of parameters
for (k in seq_along(h_values)) {

  # What are the parameters of interest?
  interest <- list(hsymmetry = h_values[k], ecosel = s_values[k])

  imgname <- sprintf(imgname_template, interest[[1]], interest[[2]], batches[1], batches[2])

  # Template to find batches of simulations
  root_template <- "/media/raphael/bigass/simulations/EGS/EGS_sim%s"

  # For each batch of simulations
  data <- lapply(batches, function(i) {

    # Focus on the current batch of simulations
    root <- sprintf(root_template, i)

    # Simulation folders
    simulations <- list.files(root, pattern = "^sim_", full.names = TRUE)

    # Keep only non-extinct simulations
    extincts <- find_extinct(root)
    simulations <- simulations[!simulations %in% extincts]

    # Keep only the parameter set of interest
    parameters <- pblapply(simulations, read_parameters, names(interest))
    ofinterest <- sapply(parameters, function(parameters) all(sapply(seq_along(parameters), function(j) parameters[[names(interest)[j]]] == interest[[j]])))
    simulations <- simulations[ofinterest]

    # Read the simulation data
    data <- do.call("c", lapply(simulations, read_population, variable))

    # Read time
    times <- lapply(simulations, read_time)

    # How many simulations?
    nsims <- length(times)

    # How many time points?
    ntimes <- length(times[[1]])

    # Assemble the data
    data <- data.frame(X = data, time = do.call("c", times))

    # Add replicate identifier
    data$replicate <- rep(seq_len(nsims), each = ntimes)

    # Add simulation batch identifier
    data$batch <- batches[i]

    return (data)

  })

  # Assemble the batches
  data <- do.call("rbind", data)

  # Summarize the time to speciation completion
  smr <- data %>%
    group_by(batch, replicate) %>%
    summarize(X = ifelse(any(X > 0.9), 1 / time[min(which(X > 0.9))], 0))

  smr$batch <- factor(smr$batch)
  levels(smr$batch) <- xlevels

  # Boxplots
  p <- ggplot(smr, aes(x = batch, y = X, color = batch)) +
    geom_boxplot() +
    geom_jitter() +
    theme_bw() +
    ylab(ylabel) +
    xlab(xlabel) +
    theme(legend.position = "none") +
    scale_color_manual(values = xcolors)
  p

  ggsave(imgname, p, width = 2, height = 3, dpi = 300)

}


