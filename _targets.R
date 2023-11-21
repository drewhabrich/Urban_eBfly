library(targets)
library(tarchetypes)

#run tar_make() to run the pipeline
#run tar_manifest() to see the steps in the pipeline
#run tar_visnetwork() to see the visual input-output steps for the pipeline
#tar_read() to view the results for each target.

# Path to the custom functions file ("./R/functions"))
tar_source("R/")

# Set target-specific options such as packages that are required:
tar_option_set(packages = c("tidyverse", "dplyr", "ggplot2", "ggpubr","viridis",
                            "rnaturalearth", "terra", "sf", "vegan", "rgbif"))

## target list
list(
  
)

