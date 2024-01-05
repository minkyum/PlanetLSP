vgt <- c('DB','MF','EN','AG','GR','SH','CR')
for(ss in 7){
  setwd('/projectnb/modislc/users/mkmoon/Planet/phe/2017/')
  for(cc in 1:20){
    system(paste('qsub -V -pe omp 8 -l h_rt=04:00:00 /usr3/graduate/mkmoon/GitHub/PlanetLSP/run_script.sh ',ss,cc,sep=''))  
  }
}


###############################
setwd('/projectnb/modislc/users/mkmoon/Planet/phe/2019/CR/')
for(cc in 1:20){
  system(paste('qsub -V -pe omp 4 -l h_rt=03:00:00 /usr3/graduate/mkmoon/GitHub/PlanetLSP/run_forBen.sh ',cc,sep=''))  
}
