#Exploring FAME sector / geography
#Fame South Yorkshire businesses
library(tidyverse)
library(sf)
library(dbmss)
library(tmap)
library(lubridate)
library(pryr)
library(plotly)
library(spdep)
library(parallel)
source("functions.R")

fame.geo <- readRDS('localdata/fame_sy_processed_geo.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTING DISTANCE METRICS FOR SECTOR SPATIAL DEPENDENCE DETECTION----
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





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTING LOCAL SAMPLING OF SECTOR PAIRS AND COMPARING TO SAME PAIR RANDOM DISTRIBUTION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Testing this approach:
# 1. Pick n neighbouring firms from a pool of two sectors.
# 2. For the null, pick the same number of firms randomly from across the region of interest.
#This "assumes that, if there were no spatial clustering or co-location patterns, the local pool would reflect the overall regional sectoral mix" - 
#get the probability of sector x from that and then repeat multiple times to get a spread of probabilities. That can be used as the distribution for step 
# 3. Compare where the probability of firms from step 1 fits on the null distribution from step 2, effectively a p value.

#Again, test with two sectors
sector_x <- fame.geo %>% filter(grepl('Manufacture of other general-purpose machinery',SIC_3DIGIT_NAME))
# sector_y <- fame.geo %>% filter(grepl('Manufacture of basic iron and steel',SIC_3DIGIT_NAME))
sector_y <- fame.geo %>% filter(grepl('information',SIC_3DIGIT_NAME))#a bunch of information related sectors, in fact...

#Can use this for easy K nearest neighbours
#Order should match row order in the dfs above, so we can extract firm info
dist_matrix <- st_distance(sector_x, sector_y)

#Start with 20 nearest neigbours
#Our null will be sampling from the same two sectors randomly
#20 at a time
#And keeping the proportion sector split
#The mean is just going to be the proportion count of both
#We're after the spread from doing this




#~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTING BDMSS PACKAGE----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#https://ericmarcon.github.io/dbmss/
#Vignette: https://ericmarcon.github.io/dbmss/articles/dbmss.html
#reference: https://ericmarcon.github.io/dbmss/articles/reference.html

#With measures from two key papers:
#Duranton, G., Overman, H.G., 2005. Testing for Localization Using Micro-Geographic Data. Review of Economic Studies 72, 1077–1106. https://doi.org/10.1111/0034-6527.00362
#Marcon, E., Puech, F., 2010. Measures of the geographic concentration of industries: improving distance-based methods. Journal of Economic Geography 10, 745–762. https://doi.org/10.1093/jeg/lbp056

#That are both doing exactly the two-category point pattern analysis I want to do
#With weights too so e.g. we can include employee number 
#Though I'd like to understand what role that's playing - want to be able to say "how much more likely cluster due to larger firms?
#Tho easy test there would be dividing into two firm sizes and comparing


#1. Create a wmppp object with Columns named "X", "Y", "PointType", "PointWeight"
#So let's pull out two sectors and do that
# sectorx = "282 : Manufacture of other general-purpose machinery"
sectorx = "261 : Manufacture of electronic components and boards"
# sectory = "241 : Manufacture of basic iron and steel and of ferro-alloys"
sectory = "263 : Manufacture of communication equipment"  

#2 digits
unique(fame.geo$SIC_2DIGIT_NAME)

sectorx = "24 : Manufacture of basic metals"
sectory = "25 : Manufacture of fabricated metal products, except machinery and equipment"

# sectorx = "61 : Telecommunications"
sectorx = "62 : Computer programming, consultancy and related activities"
sectory = "63 : Information service activities"

#Sections
unique(fame.geo$SIC_SECTION_NAME)

sectorx = "Manufacturing"
sectory = "Information and communication"

df <- rbind(
  fame.geo %>% filter(SIC_SECTION_NAME==sectorx),
  fame.geo %>% filter(SIC_SECTION_NAME==sectory)
) %>% 
  filter(!is.na(employees)) %>% 
  cbind(st_coordinates(.)) %>% 
  st_set_geometry(NULL) %>% 
  select(
    PointType = SIC_SECTION_NAME,
    PointWeight = employees,
    X,Y
  )

#point pattern object
#Can supply 'window' of shape of SY but let's come back to that
pp <- wmppp(df)
summary(pp)

#Recreating the two-tree example from here:
#https://ericmarcon.github.io/dbmss/articles/reference.html


#With two sectors instead...
autoplot(
  pp,
  labelSize = expression("employees"), 
  labelColor = "Sector"
)



#Marcon et al M
#From the paper p.751:
#"The meaning of the co-location M functions is thus simple: they test whether the relative density of employees from one
#sector around establishments of another sector is on average greater or lesser than in the whole area."
#Which would work to detect clustering, up to a point (greater no of employees = higher clustering chance)

# "The value of Equation (2) shows whether the relative density of plants S2 located
# around those of sector S1 is greater (MS1 ,S2 ðrÞ > 1) or lesser (MS1 ,S2 ðrÞ < 1) than in the
# entire area A."

#Note, it's not symmeterical, which I would have thought it would be...
#But no, it comparing to whole area?

#Test with even weights
pp$marks$PointWeight <- 1

x <- Sys.time()

Envelope <- MEnvelope(
  pp, 
  r = seq(0, 4000, 50),#r between 0 and 2000 metres with 100 metre steps 
  NumberOfSimulations = 1000,
  Alpha = 0.05, 
  ReferenceType = sectorx, 
  NeighborType = sectory, #"equal to the reference type by default to caculate univariate M"
  SimulationType = "RandomLabeling", 
  Global = TRUE
)

Sys.time() -x

# autoplot(Envelope, main = "", ylim = c(0, 20))
autoplot(Envelope, main = "")



#Duranton Overman K (oh I think only for one sector...)
Envelope <- KEnvelope(
  pp, 
  r = seq(0, 4000, 50),#r between 0 and 2000 metres with 100 metre steps 
  NumberOfSimulations = 1000,
  Alpha = 0.05, 
  ReferenceType = sectorx, 
  NeighborType = sectory, 
  SimulationType = "RandomLabeling", 
  Global = TRUE
)


autoplot(Envelope, main = "", xlim = c(0,1000))












