#ONS business demography dataset processing / analysis
#Excel data grabbed from here, stored in localdata (gitignored folder)
#https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyreferencetable

library(tidyverse)
# library(stringdist)

#USE ENTERPRISE BIRTHS TO FIGURE OUT HOW TO HARMONISE LOCAL AUTHORITY GEOGRAPHIES FROM 2017 TO 2022----

#Enterprise births
#Different sheets have different geographies
#What level of commonality can we find?

#Note: ONS official analysis only goes down to region
#https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/bulletins/businessdemography/latest

#Have to load individually to compare geographies
#Many place names have asterisks, not helpful for matching! Also drop commas, those aren't consistent across time (sigh)
births1718 <- read_csv('localdata/businessdemographyONSbirths_2017_18.csv') %>% mutate(region = gsub("\\*|,", "", region))
births19 <- read_csv('localdata/businessdemographyONSbirths_2019.csv') %>% mutate(region = gsub("\\*|,", "", region))
births20 <- read_csv('localdata/businessdemographyONSbirths_2020.csv') %>% mutate(region = gsub("\\*|,", "", region))
births2122 <- read_csv('localdata/businessdemographyONSbirths_2021_22.csv') %>% mutate(region = gsub("\\*|,", "", region))

lapply(list(births1718,births19,births20,births2122), nrow)

#So they've kept consistent where they can (17-18 and 21-22)
#Rows drop every time period

#Changes from first to last period can give the entirety of geog change
#It's all new combinations of previous district LAs to unitary etc
#Let's piece together how to harmonise

#Which regions are in the first timepoint that are NOT in the last?
#These ones got combined - hopefully no borders were changed!
in1stNOTlast <- births1718 %>% select(region) %>% filter(!region %in% births2122$region) %>% arrange(region) %>%  pull

#Check on rough word match
#E.g. bournemouth from 1st timepoint is now in "Bournemouth Christchurch and Poole" in last
#Check those in first to find string distance in 2nd
# dist_matrix <- stringdistmatrix(in1stNOTlast, births2122$region, method = "jw")
# # dist_matrix <- stringdistmatrix(in1stNOTlast, births2122$region, method = "lv") 
# print(dist_matrix)
# 
# closest_match_indices <- apply(dist_matrix, 1, which.min)
# closest_matches <- births2122$region[closest_match_indices]
# closest_matches
# 
# dist_df <- data.frame(dist_matrix)
# rownames(dist_df) <- in1stNOTlast
# colnames(dist_df) <- births2122$region

#That didn't work. Try this instead! Just use fragments and look for those
#Seven or eight letters at start should do

# Function to extract the first 8 characters and search in vector_b
match_substring <- function(a, b_vector, n = 8) {

    # Get the first 8 characters from the string
  substring_a <- substr(a, 1, min(n, nchar(a)))
  
  # Look for the substring in each element of vector_b
  matches <- sapply(b_vector, function(b) {
    grepl(substring_a, b, ignore.case = TRUE)
  })
  
  # Return elements from vector_b where a match was found
  b_vector[matches]
}

# Apply the matching for each element of vector_a
match_results <- lapply(in1stNOTlast, match_substring, b_vector = births2122$region)

# View results
names(match_results) <- in1stNOTlast
match_results




#That kind of works, but I think I may have to do this manually
#Given the various different ways everything is aggregated

#Another easy way might be:
#Get latest local authority details (or from 2022)
#Check match against latest year
#Pull those, aggregate others as necessary
#Dropping various higher level categories, which I can then sum myself

#LA districts dec 2022
#https://geoportal.statistics.gov.uk/datasets/a2128b32c7fb4205ba99e6344fcbb2be_0/explore
la22 <- read_csv('data/Local_Authority_Districts_December_2022_UK.csv') %>% mutate(LAD22NM = gsub(",", "", LAD22NM))

#Got all of them
table(la22$LAD22CD %in% births2122$code)
table(la22$LAD22CD %in% births1718$code)

#Name match? Yes, full, with commas removed
table(la22$LAD22NM %in% births2122$region)


#So can find non-matching names in first timepoint (codes aren't same)
#These are: LA names from 2022 NOT present in the 17-18 data
#Meaning that they will have been replaced with larger entities, mostly
#And that, in theory, as long as no boundaries actually changed, all previous timepoints can be aggregated to common geographies...
#Ah a nice short list!
x <- la22$LAD22NM[!la22$LAD22NM %in% births1718$region]
x[order(x)]
# "Bournemouth Christchurch and Poole"
# "Buckinghamshire"
# "Dorset"
# "East Suffolk"
# "West Suffolk"
# "Folkestone and Hythe"
# "Herefordshire County of"
# "North Northamptonshire"
# "Somerset West and Taunton"
# "West Northamptonshire"


#Working through them to work out what's happened to each.
#See also - good ol' wikipedia, change maps and everything:
#https://en.wikipedia.org/wiki/2019%E2%80%932023_structural_changes_to_local_government_in_England

# "Bournemouth Christchurch and Poole" --> each their own separate LA in 2017

# "Buckinghamshire" --> was Buckinghamshire County in 2017, by 2022 we have a combined Aylesbury Vale / Chiltern / South Bucks / Wycombe
#Or should do i.e. those four shouldn't exist in 2022...
births1718$region[grepl('Aylesbur|Chiltern|South Bu|Wycombe', births1718$region, ignore.case = T)]#sanity check that this actually finds them
births2122$region[grepl('Aylesbur|Chiltern|South Bu|Wycombe', births2122$region, ignore.case = T)]#And that they're not present in 2022

# "Dorset"
#Again, was Dorset county. Now all combined? 
#No - Christchurch went to the above
#Which leaves:
# East Dorset
# North Dorset
# Purbeck
# West Dorset
# Weymouth and Portland

#Check again
births1718$region[grepl('East Dorset|North Dorset|Purbeck|West Dorset|Weymouth and Portland', births1718$region, ignore.case = T)]#sanity check that this actually finds them
births2122$region[grepl('East Dorset|North Dorset|Purbeck|West Dorset|Weymouth and Portland', births2122$region, ignore.case = T)]#And that they're not present in 2022
births2122$region[grepl('East Dor|North Dor|Purbeck|West Dor|Weymouth', births2122$region, ignore.case = T)]#And that they're not present in 2022

# "East Suffolk"
#Wikipedia: "On 1 April 2019, the number of districts in Suffolk was reduced from seven to five."
#Here's the seven from 2017:

#East suffolk from:
# Suffolk Coastal
# Waveney

# "West Suffolk" from:
# St Edmundsbury
# Forest Heath

#Which would leave these three as is? Correct
# Babergh
# Ipswich
# Mid Suffolk

#Confirm all present in 2022... tick
births2122$region[grepl('Babergh|Ipswich|Mid Suffolk|West Suffolk|East Suffolk', births2122$region, ignore.case = T)]

#These are the only two just-name-changes
# "Folkestone and Hythe" --> purely a rename from Shepway in earlier time points
# "Herefordshire County of" --> just named differently, "Herefordshire" in earlier data.

# "North Northamptonshire"
#This was a 2021 change
#Wikipedia: "On 1 April 2021, the non-metropolitan county of Northamptonshire and its seven districts were abolished, and two new unitary authorities were created:[7]
#North Northamptonshire, consisting of the old non-metropolitan districts of Corby, East Northamptonshire, Kettering, and Wellingborough
#West Northamptonshire, consisting of the old non-metropolitan districts of Daventry, Northampton, and South Northamptonshire"

#So yes, those seven districts, still present in 2020:

#North Northamptonshire:
# Corby
# East Northamptonshire
# Kettering
# Wellingborough

#AND:
#"West Northamptonshire" in 2022, was these three prior to 2021:
# Daventry
# Northampton
# South Northamptonshire

# "Somerset West and Taunton"
#2019 for this one.
#"On 1 April 2019, the number of districts in the non-metropolitan county of Somerset was reduced from five to four
#when Taunton Deane and West Somerset were merged as Somerset West and Taunton."

#The original five in "somerset county" in 2017:
# Mendip
# Sedgemoor
# South Somerset
# Taunton Deane <-- these two merged
# West Somerset <-- these two merged

#To confirm, these 3 still present in 2022? TICK
births2122$region[grepl('Mendip|Sedgemoor|South Somerset', births2122$region, ignore.case = T)]




#Can use codes for LAs - only 10 different from earlier period, this helps fixes where JUST names are different, boundaries same
#I.e. where in 2021-22 these all match, only these don't in 17-18
table(la22$LAD22CD %in% births1718$code)

#Which are these places
la22$LAD22NM[!la22$LAD22CD %in% births1718$code]

#Ones where code stayed same but ONLY names changed... just those two already identified
#These... in 2022
la22$LAD22NM[la22$LAD22CD %in% births1718$code & !la22$LAD22NM %in% births1718$region]
#used to be these in 2017
births1718$region[births1718$code %in% la22$LAD22CD & !births1718$region %in% la22$LAD22NM]





#DO THE HARMONISING USING ENTERPRISE BIRTH DATA AS TEST----
#(Presuming/hoping that other demography features share the same geography issue and can be fixed in the same way)

#The logic of what we want to do here:
#Manually aggregate counts to the larger geographies where those appear

#TEST FUNCTION
chk <- harmonise_localauthorities(births1718)

#Check LA match with 2022... tick, all present
table(la22$LAD22NM %in% chk$region)

#filter down to the right number by keeping only 2022 LAs
chk <- chk %>% filter(region %in% la22$LAD22NM)


#OK, trying for all of em
result_list <- purrr::map(list(births1718,births19,births20, births2122), harmonise_localauthorities)

#All good
table(la22$LAD22NM %in% result_list[[4]]$region)
table(la22$LAD22NM %in% births2122$region)
#la22$LAD22NM[!la22$LAD22NM %in% result_list[[4]]$region]
#la22$LAD22NM[!la22$LAD22NM %in% births2122$region]

#Now just need to merge on region / possibly add LA codes back in
births <- purrr::reduce(result_list, left_join, by = 'region')

#Add LA code back in
births <- births %>% 
  left_join(
    la22 %>% select(code = LAD22CD, region = LAD22NM)
  )


#TEST READING DIRECTLY USING READXL----

#Looking at the data, where are the cells we want for e.g. this sheet?
#They'll vary but...
#Testing with D500 what happens with extra empty rows... just leaves NAs, perfect
chk <- readxl::read_excel('localdata/businessdemographyexceltables2022.xlsx', range = "Table 2.1a!A4:D500") %>% 
  rename(code = ...1, region = ...2)

#We can get all sheet names...
# readxl::excel_sheets('localdata/businessdemographyexceltables2022.xlsx')

#Contents sheet explains which is which
contents <- readxl::read_excel('localdata/businessdemographyexceltables2022.xlsx', 
                               range = "Contents!A5:B45", 
                               col_names = c('table','contents'))

#May want to do SIC codes later (at UK level only)
#But let's filter out everything we don't need for regional analysis
contents <- contents %>% 
  filter(!grepl('SIC2007', contents, ignore.case  = T))

#NOTES ON FEATURES TO KEEP NOW / THINK ABOUT LATER:
#Survival of new born firms over time - interesting but only more distant years of interest
#Can examine those directly without too much effort?

#Why is there a separate combined births/deaths/active 2022 sheet? Just convenience?

#OK, so the things we're grabbing are:
#births (already got above)
#deaths
#active
#count of high growth enterprises
#active enterprises 10+ employees (very useful for working out smaller ones too)

#Def of 'high growth' used by ONS here:
# There are several different methods of measuring high growth. The following definition has been used for this analysis:
# All enterprises with average annualised growth greater than 20% per annum, over a three year period. Growth can be measured by the number of employees or by turnover. For this analysis growth has been measured using employment.
# It is also recommended that a meaningful size threshold be set to avoid the growth of small businesses distorting any results. Eurostat have provisionally set a starting threshold of 10 employees.
# In order to calculate the growth of units, it is not necessary to check the change in employee numbers or turnover from one year to the next over a three year period. Instead it is sufficient to compare the population of active enterprises in year xx-3 with those in year xx.
# In practice, average annualised growth of 20% per annum over three years would be equal to 72.8% growth from xx-3 to year xx.
# We are unable to isolate and remove all cases where data has grown due to a merger or a takeover. These cases are isolated and we do not expect them to have a big impact on the data.


#little function to keep tidy
deathsheets <- contents %>% 
  filter(grepl('Deaths Of Enterprises', contents)) %>% 
  select(table) %>% 
  pull() %>% 
  as.list

# debugonce(firm_read)
list_of_sheets <- map(deathsheets, firm_read)

#Drop straight into harmonising function...
death_sheet_list <- purrr::map(list_of_sheets, harmonise_localauthorities)

#All good?
print('Local authority matches?')
print(table(la22$LAD22NM %in% death_sheet_list[[1]]$region))
print(table(la22$LAD22NM %in% death_sheet_list[[2]]$region))
print(table(la22$LAD22NM %in% death_sheet_list[[3]]$region))
print(table(la22$LAD22NM %in% death_sheet_list[[4]]$region))

#Now just need to merge on region / possibly add LA codes back in
deaths <- purrr::reduce(death_sheet_list, left_join, by = 'region')

#Add LA code back in
deaths <- deaths %>% 
  left_join(
    la22 %>% select(code = LAD22CD, region = LAD22NM)
  ) %>% 
  relocate(code)



#Issue with active 10+ employees, let's try to find out what
#At the harmonising stage
deathsheets <- contents %>% 
  filter(grepl('10\\+ Employees', contents)) %>% 
  select(table) %>% 
  pull() %>% 
  as.list

# debugonce(firm_read)
list_of_sheets <- map(deathsheets, firm_read)

#Sheet 2 not loading in, Table 7.3b
#Probably name doesn't match does it?
#Seems to...
debugonce(firm_read)
firm_read(deathsheets[[2]])

#Drop straight into harmonising function...
death_sheet_list <- purrr::map(list_of_sheets, harmonise_localauthorities)





#FINAL FIRM DEMOGRAPHY PROCESSING, SAVE TO PUBLIC DATA FOLDER----


#All the above into a function
# debugonce(get_all_firm_demography_data)
births <- get_all_firm_demography_data('Births Of New Enterprises')
deaths <- get_all_firm_demography_data('Deaths Of Enterprises')
active <- get_all_firm_demography_data('Count Of Active Enterprises For')
active10plus <- get_all_firm_demography_data('10\\+ Employees')
highgrowth <- get_all_firm_demography_data('Count Of High Growth Enterprises')


#Save a couple of versions
saveRDS(
  list(births = births, deaths = deaths, active = active, active10plus = active10plus, highgrowth = highgrowth),
  'data/firm_demography_dataframe_list2022.rds'
  )

#Save also as separate CSVs for others
write_csv(births, 'data/births_firmdemography_to_2022.csv')
write_csv(deaths, 'data/deaths_firmdemography_to_2022.csv')
write_csv(active, 'data/active_firmdemography_to_2022.csv')
write_csv(active10plus, 'data/active10plus_firmdemography_to_2022.csv')
write_csv(highgrowth, 'data/highgrowth_firmdemography_to_2022.csv')



