path = getwd()
setwd(path)
path2output = file.path(path, "Output")

# Load packages
library("modiscloud")
library("matrixcalc")
library("tawny")
library("mvtnorm")
library("parallel")
library("doParallel")
library("scatterplot3d")
library("RColorBrewer")
library("plotly")
library("colourlovers")
library("colorspace")
library("grid")
library("webshot") # for showing plotly graphs into pdf's using knitr
library("extrafont")
packages = c("modiscloud","matrixcalc","tawny","mvtnorm","parallel","doParallel","snow","foreach")

# Remove scientific notation when below 15 decimals
options(scipen = 15)

# Install packages (run only once, then recomment)

# install.packages("modiscloud")
# install.packages("matrixcalc")
# install.packages("tawny")
# install.packages("mvtnorm")
# install.packages("parallel")
# install.packages("doParallel")
# install.packages("scatterplot3d")
# install.packages("RColorBrewer")
# install.packages("plotly")
# install.packages("colourlovers")
# install.packages("webshot")
# install.packages("extrafont")

# font_import()  # call after having installed Latin Modern Roman 10 Regular. Do only once, as it might take a few minutes.
# fonts() # See installed fonts.
# fonttable() # See installed fonts.

# Load modules of the code
source("compute_matrices.R")
source("get_data.R")
source("abadir.R")
source("useful.R")
source("produce_results.R")
source("plotting.R")
source("optimalm.R")
