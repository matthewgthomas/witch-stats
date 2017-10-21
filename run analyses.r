##
## Analyses for "Population structured by witchcraft beliefs" by Mace et al.
##
## Code by Matthew Gwynfryn Thomas (@matthewgthomas)
##
## Packages required:
## - tidyverse
## - dplyr
## - broom
## - readr
## - data.table
## - ggplot2
## - wesanderson
## - igraph
## - MuMIn
## - AICcmodavg
## - lme4
## - geepack
## - survival
## - pscl
## - arm
## - spdep
## - pander
##
source("init.r")

source("assortment and clustering.r")    # Figures 1 and 2
source("ferility of household heads.r")  # Figure 3, Supplementary Figure 3, and Supplementary Tables 6 and 7
source("public goods game analysis.r")   # Table 1

# supplementary materials
source("fig s1.r")

source("ZIP predicting in-degree.r")  # Supplementary Table 1
source("gee.r")                       # Supplementary Table 2
source("contingency table.r")         # Supplementary Table 3 (this will need to be stitched together manually)
source("logistic regression predicting zhu houses.r")  # Supplementary Table 4 and Supplementary Figure 2
rmarkdown::render("zhu_dispersal.Rmd", "word_document", output_dir=results.dir)  # Supplementary Table 5
source("age at first birth.r")        # Supplementary Table 8

# produce the in-text statistics
rmarkdown::render("main text and SI text.r", "word_document", output_dir=results.dir)
