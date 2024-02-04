#!/bin/bash                                                                                                    
#SBATCH -J TestCalib # A single job name for the array                                                         
#SBATCH -n 10 # Number of cores                                                                                
#SBATCH -N 1 # All cores on one machine 
                                                                       
#SBATCH -p batch # Default                                                                          
#SBATCH --mem-per-cpu=4G # Default is KB, not MB                                              
#SBATCH -t 0-3:00 # (D-HH:MM) 8 for model setting                  
                                                                  
module load r/4.2.2

R CMD BATCH --quiet --no-restore --no-save cleaning_script.R test.out
