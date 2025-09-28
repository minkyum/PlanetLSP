# Get HLS time series
setwd('/projectnb/modislc/users/mkmoon/Planet/glasslands/runLogs/')
for(tt in 1:7){
  system(paste('qsub -V -pe omp 8 -l h_rt=04:00:00 /usr3/graduate/mkmoon/GitHub/PlanetLSP/glass/run_script.sh ',tt,sep=''))  
}
