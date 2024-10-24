#ONS business demography dataset processing / analysis
#Excel data grabbed from here, stored in localdata (gitignored folder)
#https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyreferencetable
library(tidyverse)
library(zoo)
library(nomisr)
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

#While we're here, do SY summed active values look about right?
#Little bit low compared to FAME but right ballpark 
active %>% 
  filter(grepl('sheffield|barnsl|donca|rother',region,ignore.case = T)) %>% 
  summarise(across(`2017`:`2022`, sum))

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



#LONG VERSIONS - year in long form
births <- get_all_firm_demography_data('Births Of New Enterprises', returnlong = T)
deaths <- get_all_firm_demography_data('Deaths Of Enterprises', returnlong = T)
active <- get_all_firm_demography_data('Count Of Active Enterprises For', returnlong = T)
active10plus <- get_all_firm_demography_data('10\\+ Employees', returnlong = T)
highgrowth <- get_all_firm_demography_data('Count Of High Growth Enterprises', returnlong = T)


#Save a couple of versions
saveRDS(
  list(births = births, deaths = deaths, active = active, active10plus = active10plus, highgrowth = highgrowth),
  'data/firm_demography_dataframe_list2022_LONG.rds'
  )

#Save also as separate CSVs for others
write_csv(births, 'data/births_firmdemography_to_2022_LONG.csv')
write_csv(deaths, 'data/deaths_firmdemography_to_2022_LONG.csv')
write_csv(active, 'data/active_firmdemography_to_2022_LONG.csv')
write_csv(active10plus, 'data/active10plus_firmdemography_to_2022_LONG.csv')
write_csv(highgrowth, 'data/highgrowth_firmdemography_to_2022_LONG.csv')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EXAMINE FIRM DEMOG DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Flag MCAs and core cities----

#Work with all of em in the one list
d <- readRDS('data/firm_demography_dataframe_list2022_LONG.rds')

#local authority 2024 to MCA lookup
#https://geoportal.statistics.gov.uk/datasets/64c740b84a02419fb7a561555337d931_0/explore
#REMEMBER THESE ARE ONLY ENGLAND
mcalookup <- read_csv('data/Local_Authority_District_to_Combined_Authority_(May_2024)_Lookup_in_EN.csv') 

#One non match
table(mcalookup$LAD24CD %in% d$births$code)
mcalookup %>% filter(!mcalookup$LAD24CD  %in% d$births$code)

#It's North Yorkshire.
#From same amazingly useful wikipedia page
#A new unitary authority, North Yorkshire Council, replaced North Yorkshire County Council and the non-metropolitan districts of 
#Craven, Hambleton, Harrogate, Richmondshire, Ryedale, Scarborough and Selby on 1 April 2023. 

#We want to keep the YNY MCA so want to use this most recent data
#But need to code these manually to be up to date

#So match which ones we can then do YNY manually
d <- d %>% purrr::map(left_join, mcalookup %>% select(LAD24CD,CAUTH24CD,CAUTH24NM), by = c('code' = 'LAD24CD'))



#then fix YNY issue
#Check match... tick
table(c('Craven', 'Hambleton', 'Harrogate', 'Richmondshire', 'Ryedale', 'Scarborough', 'Selby') %in% d$births$region)

#function to keep tidy
mutate_yny <- function(df){
  
  df %>% mutate(
    CAUTH24CD = case_when(
      region %in% c('Craven', 'Hambleton', 'Harrogate', 'Richmondshire', 'Ryedale', 'Scarborough', 'Selby') ~ 'E47000012',
      .default = CAUTH24CD
      )
    ,
    CAUTH24NM = case_when(
      region %in% c('Craven', 'Hambleton', 'Harrogate', 'Richmondshire', 'Ryedale', 'Scarborough', 'Selby') ~ 'York and North Yorkshire',
      .default = CAUTH24NM
    )
  )
  
}

d <- d %>% purrr::map(mutate_yny)



#CORE CITIES
#Check match in LAs... tick
d$births$region[grepl(x = d$births$region, pattern = 'sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Newcastle upon|Nottingham', ignore.case = T)] %>% unique

#Label all the demog LAs
d <- d %>% purrr::map(
  ~ dplyr::mutate(., corecity = ifelse(
    grepl('sheffield|Belfast|Birmingham|Bristol|Cardiff|Glasgow|Leeds|Liverpool|Manchester|Newcastle upon|Nottingham', region, ignore.case = T),
    'Core city','Other'
  ))
)



## Examine high growth firm raw numbers----

#Just to start digging into this. Knowing actual numbers useful
#Compare just MCAs and core cities to start with
mcas.hg <- d$highgrowth %>%
  filter(!is.na(CAUTH24NM)) %>% 
  group_by(CAUTH24NM,year) %>%
  summarise(count = sum(count)) %>% 
  rename(MCA = CAUTH24NM)


ggplot(mcas.hg, aes(x = year, y = count, colour = fct_reorder(MCA,-count))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired')



#Let's do same for core cities... each is already separate LA, don't need to summarise
core.hg <- d$highgrowth %>%
  filter(corecity == 'Core city') %>% 
  rename(MCA = CAUTH24NM)

ggplot(core.hg, aes(x = year, y = count, colour = fct_reorder(region,-count))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired')


#OK, now to repeat that as PROPORTION OF ACTIVE FIRMS
#Note, high growth only calculated for firms of 10+ employees
#So use that as denom

#Merge as new column to make division easy
hg_propof10plusfirms <- d$highgrowth %>% 
  rename(count_highgrowth = count) %>% 
  left_join(
    d$active10plus %>% select(region,year,count_active10plus = count), by = c('region','year')
  ) 

#Repeat plots
#Have to get percent here so it's after summing counts
mcas.hg.prop <- hg_propof10plusfirms %>%
  filter(!is.na(CAUTH24NM)) %>% 
  group_by(CAUTH24NM,year) %>%
  summarise(
    count_highgrowth = sum(count_highgrowth),
    count_active10plus = sum(count_active10plus)
    ) %>% 
  rename(MCA = CAUTH24NM) %>% 
  mutate(highgrowthfirms_aspercentof_firms10plusemployees = (count_highgrowth/count_active10plus) * 100 )


ggplot(mcas.hg.prop, aes(x = year, y = highgrowthfirms_aspercentof_firms10plusemployees, colour = fct_reorder(MCA,-highgrowthfirms_aspercentof_firms10plusemployees))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired')



#Would be good to see a plot of those against each other over time - is it active firm numbers increasing or high growth firms dropping?
#Quick and dirty version
mcas.hg.prop <- mcas.hg.prop %>% 
  group_by(MCA) %>% 
  mutate(
    count_highgrowth_movingav = rollapply(count_highgrowth, 3, mean, align = 'center', fill = NA),
    count_active10plus_movingav = rollapply(count_active10plus, 3, mean, align = 'center', fill = NA)
    ) %>% 
  ungroup()

#Use moving av centrepoints
ggplot(
  mcas.hg.prop %>% filter(year %in% c(2018,2021)),
  aes(x = count_highgrowth_movingav, y = count_active10plus_movingav, shape = factor(year), group = MCA, colour = fct_reorder(MCA,-count_active10plus_movingav))
) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_brewer(palette = 'Paired') +
  scale_y_log10() +
  scale_x_log10() 




#Let's do same for core cities... each is already separate LA, don't need to summarise
#THIS LOOKS TOO VOLATILE, PROB NOT ENOUGH DATA
#Use moving av again
core.hg.prop <- hg_propof10plusfirms %>%
  filter(corecity == 'Core city') %>% 
  mutate(highgrowthfirms_aspercentof_firms10plusemployees = (count_highgrowth/count_active10plus) * 100 ) %>% 
  group_by(region) %>% 
  mutate(
    count_highgrowth_movingav = rollapply(count_highgrowth, 3, mean, align = 'center', fill = NA),
    count_active10plus_movingav = rollapply(count_active10plus, 3, mean, align = 'center', fill = NA),
    highgrowthfirms_aspercentof_firms10plusemployees_movingav = rollapply(highgrowthfirms_aspercentof_firms10plusemployees, 3, mean, align = 'center', fill = NA)
  ) %>% 
  ungroup()
  
#Volatile, use moving av
ggplot(core.hg.prop, aes(x = year, y = highgrowthfirms_aspercentof_firms10plusemployees_movingav, colour = fct_reorder(region,-highgrowthfirms_aspercentof_firms10plusemployees_movingav))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired') +
  coord_cartesian(xlim = c(2018,2021)) +
  labs(colour = 'core city')


#repeat check of which of active vs high growth is changing
#Use moving av centrepoints
ggplot(
  core.hg.prop %>% filter(year %in% c(2018,2021)),
  aes(x = count_highgrowth_movingav, y = count_active10plus_movingav, shape = factor(year), group = region, colour = fct_reorder(region,-count_active10plus_movingav))
) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_brewer(palette = 'Paired') +
  scale_y_log10() +
  scale_x_log10() 



#Firm efficiency and turnover----

#equivalent to migration efficiency
#See “variouschecks.xlsx” in localdata folder for a reminder: -1 = “all deaths no births”, 1 is all births no deaths, 0 is balance
#And also https://journals.sagepub.com/doi/full/10.1177/23998083231173696#bibr8-23998083231173696
#"We use two flow measures: migration efficiency and turnover (see e.g. Dennett and Stillwell, 2008). 
#Migration efficiency measures the polarity of flows into and out of a zone. 
#Minus 1 indicates all flows were out, none came in; +1 indicates all flows were in, none left. 
#Zero indicates a balance of inward and outward flows. 
#Migration efficiency provides a measure of polarity but not scale; 
#so we use turnover for the latter: the sum of inward and outward flows divided by total zone population."

#Combine births and deaths
#And also active firms for turnover
bd <- d$births %>% 
  rename(count_births = count) %>% 
  left_join(
    d$deaths %>% select(region,year,count_deaths = count), by = c('region','year')
  ) %>% 
  left_join(
    d$active %>% select(region,year,count_active = count), by = c('region','year')
  ) %>%
  relocate(count_deaths, .after = count_births) %>% 
  relocate(count_active, .after = count_deaths)


#Aggregate to MCAs and find firm growth efficiency for those summed numbers
#Then there's turnover, which can measure scale where efficiency can't
#To give a sense of the level of creative destruction
bd.mca <- bd %>%
  filter(!is.na(CAUTH24NM)) %>% 
  group_by(CAUTH24NM,year) %>%
  summarise(
    count_births = sum(count_births),
    count_deaths = sum(count_deaths),
    count_active = sum(count_active)
    ) %>% 
  rename(MCA = CAUTH24NM) %>% 
  mutate(
    firmefficency = (count_births - count_deaths)/(count_births + count_deaths) * 100,#diffs here quite small so scale to 100
    turnover = (count_births + count_deaths)/(count_active),
    births_over_active_percent = ((count_births)/(count_active))*100,
    deaths_over_active_percent = ((count_deaths)/(count_active))*100
  ) %>% 
  group_by(MCA) %>% 
  mutate(
    firmefficency_movingav = rollapply(firmefficency, 3, mean, align = 'center', fill = NA),
    turnover_movingav = rollapply(turnover, 3, mean, align = 'center', fill = NA),
    births_over_active_percent_movingav = rollapply(births_over_active_percent, 3, mean, align = 'center', fill = NA),
    deaths_over_active_percent_movingav = rollapply(deaths_over_active_percent, 3, mean, align = 'center', fill = NA)
  ) %>% 
  ungroup()
  


ggplot(bd.mca, aes(x = year, y = firmefficency, colour = fct_reorder(MCA,-firmefficency))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0)


#Try smoothed version. Shouldn't matter whether smoothing underlying values or this result, I don't think
#Though will doublecheck
ggplot(bd.mca, aes(x = year, y = firmefficency_movingav, colour = fct_reorder(MCA,-firmefficency_movingav))) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(2018,2021))

#Plot turnover values for MCAs 
ggplot(bd.mca, aes(x = year, y = turnover, colour = fct_reorder(MCA,-turnover))) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Paired') 

ggplot(bd.mca, aes(x = year, y = turnover_movingav, colour = fct_reorder(MCA,-turnover_movingav))) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Paired') +
  coord_cartesian(xlim = c(2018,2021))


#Births and deaths as prop of active plotted separately
#Time plot of smoothed
ggplot(
  bd.mca %>% filter(year %in% c(2018,2021)),
  aes(x = births_over_active_percent_movingav, y = deaths_over_active_percent_movingav, shape = factor(year), group = MCA, colour = fct_reorder(MCA,-turnover_movingav))
) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_brewer(palette = 'Paired') +
  geom_abline(slope = 1, intercept = 0) +
  scale_y_log10() +
  scale_x_log10() +
  labs(colour = 'MCAs', shape = 'Year (3 yr av)')







#And for core cities?
bd.core <- bd %>%
  # filter(corecity == 'Core city') %>% 
  filter(corecity == 'Core city' | grepl('barns|doncaster|rotherh',region,ignore.case=T)) %>% #version that includes other places in SY
  mutate(
    firmefficency = (count_births - count_deaths)/(count_births + count_deaths) * 100,#diffs here quite small so scale to 100
    turnover = (count_births + count_deaths)/(count_active),
    births_over_active_percent = ((count_births)/(count_active))*100,
    deaths_over_active_percent = ((count_deaths)/(count_active))*100
  ) %>% 
  group_by(region) %>% 
  mutate(
    firmefficency_movingav = rollapply(firmefficency, 3, mean, align = 'center', fill = NA),
    turnover_movingav = rollapply(turnover, 3, mean, align = 'center', fill = NA),
    births_over_active_percent_movingav = rollapply(births_over_active_percent, 3, mean, align = 'center', fill = NA),
    deaths_over_active_percent_movingav = rollapply(deaths_over_active_percent, 3, mean, align = 'center', fill = NA)
  ) %>% 
  ungroup()

ggplot(bd.core, aes(x = year, y = firmefficency_movingav, colour = fct_reorder(region,-firmefficency_movingav))) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(2018,2021)) +
  labs(colour = 'Core city')



#Two var plot
ggplot(
  bd.core %>% filter(year %in% c(2018,2021), region!='Belfast'),
  aes(x = births_over_active_percent_movingav, y = deaths_over_active_percent_movingav, shape = factor(year), group = region, colour = fct_reorder(region,-turnover_movingav))
) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_brewer(palette = 'Paired') +
  geom_abline(slope = 1, intercept = 0) +
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed() +
  labs(colour = 'MCAs', shape = 'Year (3 yr av)')


#Version with arrows via geom_segment?
#Just two timepoints
bd.core.wide <- bd.core %>% 
  filter(year %in% c(2018,2021)) %>% 
  select(region,year,firmefficency:deaths_over_active_percent_movingav) %>% 
  pivot_wider(names_from = year, values_from = firmefficency:deaths_over_active_percent_movingav)

ggplot(
  bd.core.wide %>% filter(region!='Belfast'),
  aes(colour = fct_reorder(region,-turnover_movingav_2021))
) +
  # geom_line() +
  # geom_point(size = 3) +
  geom_segment(aes(x = births_over_active_percent_movingav_2018, y = deaths_over_active_percent_movingav_2018 ,
                                     xend = births_over_active_percent_movingav_2021, yend = deaths_over_active_percent_movingav_2021),
               arrow = arrow(length = unit(0.5, "cm")),
               size = 1) +
  scale_color_brewer(palette = 'Paired') +
  geom_abline(slope = 1, intercept = 0) +
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed() +
  labs(colour = 'MCAs')






#Note for report: could really do with some better ways to examine 10+ employee size firms
#Given what proportion are <10 employees or a single employee


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get population denominator(s)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Can we use 16+ in employment as denom?
#Already got most of the way there to get this but we'll need a different geography
#See https://github.com/DanOlner/ukcompare/blob/0a07b463cf96cbbf61920b10914d55c30e9831f6/explore_code/sic_soc_explore.R#L161

#Check what data we want
cell <- nomis_get_metadata(id = "NM_17_1", concept = "CELL")


#Test geographies...
#Did data get backcast or are we going to have to do any geog harmonising again?
nomis_get_metadata(id = "NM_17_1", concept = "GEOGRAPHY", type = "type") %>% print(n=60)

#Trying TYPE432 "local authorities: district / unitary (as of April 2021"

#Leave time out, get all times
in_employment <- nomis_get_data(id = "NM_17_1", geography = "TYPE432", cell = '402719489') %>% 
# in_employment <- nomis_get_data(id = "NM_17_1", time = "latest", geography = "TYPE432", cell = '402719489') %>% 
  select(DATE_NAME,GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,MEASURES_NAME,OBS_VALUE,OBS_STATUS,OBS_STATUS_NAME) %>% 
  rename(ALL_IN_EMPLOYMENT_16PLUS = OBS_VALUE)

#Rolling time period, this many in total - plenty for here
unique(in_employment$DATE_NAME)[grepl('Dec',unique(in_employment$DATE_NAME))]

#Check LA match...
table(la22$LAD22CD %in% in_employment$GEOGRAPHY_CODE)

#Falses are...? Ah I that's GB data isn't it? Haven't got employment numbers for NI
la22$LAD22NM[!la22$LAD22CD %in% in_employment$GEOGRAPHY_CODE]



#So will need to just use GB with this pop data

#Start with a basic question then:
#What's the proportion of high growth firms over the "all in employment 16+" figure?


#For core cities? In which I'm also include BDR for comparison to those...
#Add count values into here
bd.core.emp <- bd.core %>%
  right_join(
    in_employment %>% 
      select(code = GEOGRAPHY_CODE, DATE_NAME, ALL_IN_EMPLOYMENT_16PLUS, MEASURES_NAME) %>% 
      pivot_wider(names_from = MEASURES_NAME, values_from = ALL_IN_EMPLOYMENT_16PLUS)
  )
  

#NEXT: pick the timepoints from the DATE_NAME column in employment values
#And recode so it'll match timepoints in the firm demog data
#Then use that to join








