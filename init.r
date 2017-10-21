##
## load Mosuo data
##
library(tidyverse)

source("init plots.r")

# where stuff is
data.dir = "data"
plots.dir = "plots"
models.dir = "models"
results.dir = "results"

# create folders if needed (except data folder: if that's missing, you can't do much more here)
if (!dir.exists(plots.dir))    # plots
  dir.create(plots.dir)
if (!dir.exists(models.dir))   # models
  dir.create(models.dir)
if (!dir.exists(results.dir))  # results
  dir.create(results.dir)

load(file.path(data.dir, "mosuo.rdata"))

# split people into kids and adults
adult.age = 15
mosuo.children = subset(mosuo.people, Age < adult.age)
mosuo.adults   = subset(mosuo.people, Age >= adult.age)

