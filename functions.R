#functions
library(tidyverse)
library(sf)
library(parallel)





















sectorpairs_distancezscores <- function(famedata,sectortype,sectorname1,sectorname2, permutenumber = 1000){
  
  sectortype <- enquo(sector_name)
  
  #Calculate pairwise distances between firms in sector x and sector y
  sector_x <- famedata %>% filter(grepl(sectorname1,!!sectortype))
  sector_y <- famedata %>% filter(grepl(sectorname2,!!sectortype))
  
  #Get distances between all firms in both of the sector pairs
  dist_matrix <- st_distance(sector_x, sector_y)
  
  
  #For the null, get equivalent number of distances between random firms
  #Repeat to get distributions of means and density peaks
  
  #Faster parallel
  num_cores <- detectCores()
  
  #~3x time savings for higher iterations, lower for lower iterations
  matrices_list <- mclapply(1:permutenumber, function(x) repeatrandomfirms(famedata,sector_x,sector_y), mc.cores = num_cores)
  
  # Convert each matrix to a vector and combine them into a single vector
  # combined_vector <- do.call(c, lapply(matrices_list, as.vector))
  
  #Extract means for each of the iterations to get spread for z score / t value
  permuted_means <- sapply(matrices_list, mean)
  
  #Get z score for means
  zscore_means <- (mean(as.numeric(dist_matrix)) - mean(permuted_means))/sd(permuted_means)
  
  
  
  
  #Get density max point for pair
  max_density_value <- getmaxdensity(dist_matrix)
  
  #Repeat for permuted random firms
  permuted_max_density_vals <- sapply(matrices_list, getmaxdensity)

  
  
  
}




repeatrandomfirms <- function(famedata,sector_x,sector_y){
  
  sector_x_randomfirms <- famedata[sample(nrow(famedata),nrow(sector_x)),]
  sector_y_randomfirms <- famedata[sample(nrow(famedata),nrow(sector_y)),]
  
  return(st_distance(sector_x_randomfirms, sector_y_randomfirms))
  
}


getmaxdensity <- function(values){
  
  density_est <- density(values)
  
  # Find the value at which the density is maximized
  density_est$x[which.max(density_est$y)]
  
}



