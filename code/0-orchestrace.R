# postupné vytočení všech souborů - nic víc, nic míň...
library(tictoc)


plocha <- 5e6 # plocha mřížky (v metrech čtverečných)

tic()
source("./code/1-init+auxillary.R")
source("./code/2-bars-overview.R")
source("./code/3-administrative-area-inputs.R")
source("./code/3-point-inputs.R")
source("./code/3-raster-inputs.R")
source("./code/4-regression.R")
toc()