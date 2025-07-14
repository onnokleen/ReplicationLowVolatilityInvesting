rm(list = ls())
# Load packages
library(dplyr)
library(scales)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(lubridate)
library(foreach)
library(grid)
library(gridExtra)
library(xtable)
library(doParallel)
library(sandwich)
library(lmtest)
library(broom)
library(purrr)
library(mfGARCH)

# registerDoParallel(2)

# Global tikzDevice settings
library(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[tikz,crop=true]{standalone}")
options(tikzLatexPackages = 
          c("\\usepackage{amsmath}",
            "\\usetikzlibrary{calc}",
            "\\usepackage{dsfont}",
            "\\usepackage{luatex85}"))

# Helper functions, e.g. grid_arrange_shared_legend
source("functions/helpers.R")

