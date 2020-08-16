setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
for(tt in 1:11){
  system(paste('qsub -V -pe omp 8 -l h_rt=01:00:00 /usr3/graduate/mkmoon/GitHub/PlanetLSP/run_script.sh ',tt,sep=''))  
}

