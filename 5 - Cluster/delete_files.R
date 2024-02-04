library(plyr)
library(tidyverse)
library(doMC)

# set working directory
wd = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/Output"
setwd(wd)

# make binding function
delete_em = function(path){

  # set working directory
  # setwd(paste0(wd, path))

  # bind files
  sapply(list.files(), function(a) {if(grepl("results", a)){unlink(a)}})

}

delete_em(wd)

# set folders
# folders = list.files()[grepl("1_Dec", list.files())]
#folders = c("ES_29_Sep_0", "ES_29_Sep_4", "ES_29_Sep_2", "ES_29_Sep_1", "ES_29_Sep_3")
# print(folders)

# set up parallelization
# doMC::registerDoMC(cores = 10)
# foreach::getDoParWorkers()

# run parallelized loop
# foreach(i=1:length(folders)) %dopar% delete_em(folders[i])
