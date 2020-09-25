vgt <- c('DB','MF','EN','AG','GR','SH')
for(ss in 1:6){
  setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',vgt[ss]))
  for(cc in 1:20){
    system(paste('qsub -V -pe omp 8 -l h_rt=04:00:00 /usr3/graduate/mkmoon/GitHub/PlanetLSP/run_script.sh ',ss,cc,sep=''))  
  }
}

