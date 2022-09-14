#!/bin/bash                                                                                                    
#SBATCH -J TestCalib # A single job name for the array                                                         
#SBATCH -n 30 # Number of cores                                                                                
#SBATCH -N 1 # All cores on one machine 
                                                                       
#SBATCH -p batch # Default                                                                          
#SBATCH --mem-per-cpu=8G # Default is KB, not MB                                              
#SBATCH -t 0-12:00 # (D-HH:MM) 8 for model setting                  
                                                                                                
#SBATCH --mail-type=END                                                                                        
#SBATCH --mail-user=shirley_dong@brown.edu                                                                   

module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018 R/3.4.3

R CMD BATCH --quiet --no-restore --no-save base_case.R test.out
