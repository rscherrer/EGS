# Run this script to extract simulations that went extinct
# and place them in a common folder for re-launch

rm(list = ls())

library(EGS)

# Where should we look for?
root <- "/media/raphael/bigass/simulations/EGS/EGS_sim1"

# Where should the simulations go?
target <- "/media/raphael/bigass/simulations/EGS/rerun"

# Find simulations that went extinct
extincts <- find_extinct(root)

# Create copy of the extinct folders in the target directory
sapply(extincts, function(folder) dir.create(paste0(target, gsub(root, "", folder))))

# For each new simulation folder...
sapply(extincts, function(folder) {

  # Copy the parameter file there
  file.copy(paste0(folder, "/parameters.txt"), paste0(target, gsub(root, "", folder)))

})

# Put there a file with the names of the folders to re-launch (that's for the cluster)
targetfile <- file(paste0(target, "/extincts.txt"))
writeLines(sapply(extincts, function(folder) paste0(".", gsub(root, "", folder))), targetfile)
close(targetfile)

# Done!
