# libraries
library(plyr)
library(tidyverse)
library(doMC)

# set working directory
wd = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/Output"
setwd(wd)

# make binding function
bind_em = function(path){
  
  # set working directory
  # setwd(paste0(wd, path))
  
  # bind files
  out = ldply(list.files(), function(a){if(grepl("results", a)){load(a); return(out)}})
  print(out)
  
  # reset working directory and save files
  setwd("/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster")
  save(out, file = "/gpfs/home/sdong12/nursing.home/nursing-home/5 - Cluster/output1.RData")
  print(path)
}

bind_em(wd)

# set folders
# folders = list.files()[grepl("2022", list.files())]
# #folders = c("ES_29_Sep_3", "ES_29_Sep_4", "ES_29_Sep_2", "ES_29_Sep_1", "ES_29_Sep_0") 
# print(folders)
# 
# # set up parallelization
# doMC::registerDoMC(cores = 10)
# foreach::getDoParWorkers()
# 
# # run parallelized loop
# foreach(i=1:length(folders)) %dopar% bind_em(folders[i])
