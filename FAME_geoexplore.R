#Exploring FAME sector / geography
#Fame South Yorkshire businesses
library(tidyverse)
library(sf)
library(tmap)
library(lubridate)
library(pryr)
library(plotly)
library(spdep)
library(parallel)
source("functions.R")

fame.geo <- readRDS('localdata/fame_sy_processed_geo.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTING MORAN'S I FOR SECTOR SPATIAL DEPENDENCE DETECTION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Doing a single run before repeating for all 5 digits sector pairs (which is going to be... 662^2 = 438244 pairs, hmmmmm!)
#Maybe start with higher level SIC codes e.g. 3 digit has 260 sectors...
#fair chance correlation wouldn't show up at 5 digit anyway, but will try and search that space

#Check on counts in each 3 digit sector. Those below seem low!
#62086 do have values, 2186 NA
table(is.na(fame.geo$SIC_3DIGIT_NAME))
table(fame.geo$SIC_3DIGIT_NAME)

#2 digit might be better?
table(is.na(fame.geo$SIC_2DIGIT_NAME))#Same NA count
table(fame.geo$SIC_2DIGIT_NAME)

#Urgh, that's hard to view
fame.geo %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_2DIGIT_NAME) %>% 
  summarise(count = n()) %>% 
  View

#And SIC section
fame.geo %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  summarise(count = n()) %>% 
  View

#Just checking but summing employee numbers
fame.geo %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  summarise(employee_count = sum(employees, na.rm = T)) %>% 
  View


#Calculate pairwise distances between firms in sector x and sector y
sector_x <- fame.geo %>% filter(grepl('Manufacture of other general-purpose machinery',SIC_3DIGIT_NAME))
# sector_y <- fame.geo %>% filter(grepl('Manufacture of basic iron and steel',SIC_3DIGIT_NAME))
sector_y <- fame.geo %>% filter(grepl('information',SIC_3DIGIT_NAME))#a bunch of information related sectors, in fact...


dist_matrix <- st_distance(sector_x, sector_y)

#Actually, there's a quick way right there to look for colocation
#This is a sorta-variogram
# plot(density(dist_matrix))
hist(dist_matrix, breaks= 30)


#Set up a permutation test to see how that differs from random distances between those sectors
#Only need to randomise one of them to randomise all distances
#Ah no - that just ends up with all the same list of distance pairs, of course. Order won't matter.

# sector_y_permute <- sector_y[sample(nrow(sector_y)),]#replace = F by default
# dist_matrix <- st_distance(sector_x, sector_y_permute)
# hist(dist_matrix, breaks= 30)

#The comparison FOR ALL SECTOR PAIRS should just be a sample of random businesses of the same number
#Repeated
# sector_x_randomfirms <- fame.geo[sample(nrow(fame.geo),nrow(sector_x)),]
# sector_y_randomfirms <- fame.geo[sample(nrow(fame.geo),nrow(sector_y)),]
# 
# dist_matrix <- st_distance(sector_x_randomfirms, sector_y_randomfirms)
# hist(dist_matrix, breaks= 30)


#Can repeat that multiple times to get plenty of values for a stable distribution...
repeatrandomfirms <- function(){
  
  sector_x_randomfirms <- fame.geo[sample(nrow(fame.geo),nrow(sector_x)),]
  sector_y_randomfirms <- fame.geo[sample(nrow(fame.geo),nrow(sector_y)),]
  
  return(st_distance(sector_x_randomfirms, sector_y_randomfirms))
  
}

# Run the function 5 times and store the matrices
#Takes about 1000 to get something close to stability
# matrices_list <- replicate(1000, repeatrandomfirms())

#Faster parallel
num_cores <- detectCores()

# x <- Sys.time()
# matrices_list <- replicate(1000, repeatrandomfirms())
# Sys.time() - x

#Savings increase for higher iterations. 3.2 seconds here vs 10 for non-parallel
x <- Sys.time()
matrices_list <- mclapply(1:1000, function(x) repeatrandomfirms(), mc.cores = num_cores)
Sys.time() - x

# Convert each matrix to a vector and combine them into a single vector
combined_vector <- do.call(c, lapply(matrices_list, as.vector))

hist(combined_vector, breaks= 30)


#Extract means for each of the iterations to get conf intervals
means <- sapply(matrices_list, mean)
# plot(density(means))
hist(means, breaks = 30)
abline(v=mean(means), lwd = 4, col = 'red')
abline(v=mean(dist_matrix), col="blue")

#OK, get a lovely one sided p value from that!
#Even for very uncorrelated, high sig value here, will need to mull
mean(means < as.numeric(mean(dist_matrix)))

#Get z score
(mean(as.numeric(dist_matrix)) - mean(means))/sd(means)


#Do same for max density value - the highest peak of distances between firms
#(May be several peaks...)

#Get density max point for pair
max_density_value <- getmaxdensity(dist_matrix)

#Repeat for permuted random firms
permuted_max_density_vals <- sapply(matrices_list, getmaxdensity)

hist(permuted_max_density_vals, breaks = 30)
abline(v=mean(permuted_max_density_vals), lwd = 4, col = 'red')
abline(v=mean(max_density_value), col="blue")

#Get z score for max density point
(mean(as.numeric(dist_matrix)) - mean(means))/sd(means)




#OK... what we could do with then is, for the same bins, seeing the difference to the original
orig <- as.vector(dist_matrix)

#Get density max point
density_est <- density(dist_matrix)
# density_est <- density(orig)
plot(density_est)

# Find the value at which the density is maximized
max_density_value <- density_est$x[which.max(density_est$y)]



#Stick em together, check density difference
both <- data.frame(vals = c(orig,combined_vector), which = c(rep('orig', length(orig)),rep('random', length(combined_vector)))  )

ggplot(both, aes(x = vals, fill = which, colour = which)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = both %>% filter(which == 'orig') %>% summarise(mean(vals)) %>% pull, colour = 'green') +
  geom_vline(xintercept = max_density_value, colour = 'blue') +
    geom_vline(xintercept = both %>% filter(which == 'random') %>% summarise(mean(vals)) %>% pull) 

# ggplot(both, aes(x = vals, fill = which)) +
# # ggplot(both %>% filter(which == 'orig'), aes(x = vals, fill = which)) +
#   geom_histogram( aes(y=..density..) )




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DISTANCE MATRIX COMPARISON FOR ALL SECTOR PAIRS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Continuing with approach above, let’s run for all pairs (testing for different SIC levels):
# • distance matrix between pairs of sectors
# • permuted distance matrices (1000 does a good job) from randomly selected firms of same number as the current pair
# • Z scores of the permute mean spread vs mean of sector pair distances
# 
# There’s also then a chance to pull out the density max peak from the pairs – I should keep and store that to look for patterns.



























