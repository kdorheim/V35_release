# TODO this should be replaced with renv but for now 
# set up the constants/environment for this project. 

# 0. Packages ------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(hector) 
library(ggplot2)

# The root directory for the project
BASE_DIR <- here::here()



# 1. Visual Settings -----------------------------------------------------------

theme_set(theme_bw())

DEFAULT_HEIGHT <- 4
DEFAULT_WIDTH <- 4


COLOR_SCHEME <- c("black", "orange")
names(COLOR_SCHEME) <- c("3.2.0", "3.5.0")

my_ggsave <- function(PLOT, name, type = ".png", 
                      WIDTH = DEFAULT_WIDTH, 
                      HEIGHT = DEFAULT_HEIGHT){
  
  # Helper function that saves my plots in the figs folder of this project
  # Args 
  #   PLOT: ggplot object 
  #   name: str of the file name 
  #   type: default set to ".png" controls what type of file is save 
  #   WIDTH: DEFAULT_WIDTH, controls the width of the figure written out 
  #   HEIGHT: DEFAULT_HEIGHT, controls the width of the figure written out 
  # Returns: file written to disk 
  
    
  fout <- file.path(BASEIR, "figs", paste0(name, type))
  ggsave(filename = fout, plot = PLOT, width = WIDTH, height = HEIGHT)
  return(fout)
  
}

