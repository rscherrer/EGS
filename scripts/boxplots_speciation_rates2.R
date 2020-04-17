# Compare the rates of completion between several batches of simulations

rm(list = ls())

library(EGS)
library(pbapply)
library(tidyverse)

# What variable to analyze?
variable <- "EI"
ylabel <- "Time to ecological divergence (\U00D7 1000 generations)"

# What batches of simulations to compare?
batches <- c(5, 6)
xlevels <- c("Additive", "Epistatic")
xlabel <- "Genotype-phenotype map"
xcolors <- c("goldenrod", "seagreen")

# File name of the figure to save
imgname_template <- "figures/boxplots/boxplots_rates_%s_batches_%s_%s.png"
imgname <- sprintf(imgname_template, variable, batches[1], batches[2])

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

  # Add simulation batch identifier
  data$batch <- i

  return (data)

})

# Assemble the batches
data <- do.call("rbind", data)

# Summarize the time to speciation completion
smr <- data %>%
  group_by(batch, hsymmetry, ecosel, replicate) %>%
  summarize(X = ifelse(any(get(variable) > 0.9), time[min(which(get(variable) > 0.9))], 20000))

smr$batch <- factor(smr$batch)
levels(smr$batch) <- xlevels

smr$ecosel <- factor(smr$ecosel, levels = rev(levels(factor(smr$ecosel))))
h_labels <- gsub("hsymmetry", "h", make_facet_labels(smr, "hsymmetry"))
s_labels <- gsub("ecosel", "s", make_facet_labels(smr, "ecosel"))

# Boxplots
p <- ggplot(smr %>% filter(X > 0), aes(x = batch, y = X / 1000, color = batch)) +
  #geom_boxplot() +
  geom_violin() +
  geom_jitter() +
  coord_flip() +
  theme_bw() +
  ylab(ylabel) +
  xlab(xlabel) +
  theme(legend.position = "none") +
  scale_color_manual(values = xcolors) +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  facet_grid(ecosel ~ hsymmetry, labeller = labeller(hsymmetry = h_labels, ecosel = s_labels))
p

ggsave(imgname, p, width = 5, height = 6, dpi = 300)


