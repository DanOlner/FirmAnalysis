#Companies house bulk download (~3gb)
#Monthly updates from here:
#https://www.gov.uk/guidance/companies-house-data-products
library(tidyverse)
library(sf)
library(pryr)
#library(data.table)

#LOAD----

# ch <- read_csv('localdata/BasicCompanyDataAsOneFile-2024-11-01.csv')
# 
# #check match against SY place names... this is more than we get against a postcode match below. Keep town name.
# table(grepl('sheffield|rotherham|doncaster|barnsley',ch$RegAddress.PostTown,ignore.case = T))
# 
# #reduce to only the columns we need for now and resave for ease of loading
# ch <- ch %>% 
#   select(CompanyName,CompanyNumber,RegAddress.PostCode,RegAddress.PostTown,CompanyCategory,CompanyStatus,CountryOfOrigin,IncorporationDate,SICCode.SicText_1:SICCode.SicText_4,URI)
# 
# #3.8gb in memory before col select, 2gb after
# object_size(ch)
# 
# saveRDS(ch,'localdata/BasicCompanyDataAsOneFile-2024-11-01_reducedcols.rds')

ch <- readRDS('localdata/BasicCompanyDataAsOneFile-2024-11-01_reducedcols.rds')

#Does every business have postcodes? Not quite
table(!is.na(ch$RegAddress.PostCode))

#Check on some that don't...
ch %>% filter(is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% View

#Check on those that do!
ch %>% filter(!is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% View



#USE POSTCODE LOOKUP TO FILTER DOWN TO SOUTH YORKSHIRE----

#NOTE: USING LATEST CODEPOINT OPEN INSTEAD, SEE BELOW
#BETTER MATCH
#PROCESSED HERE: 
#https://github.com/DanOlner/utilities/blob/master/postcodes_to_localauthorities_makelookup.R

#"Postcode (May 2021) to OA (2021) to LSOA to MSOA to LTLA to UTLA to RGN to CTRY (May 2021) Best Fit Lookup in EW"
#"Postcodes are best-fitted by plotting the location of the postcode's mean address into the areas of the output geographies."
#Yep that'll do.
#https://geoportal.statistics.gov.uk/datasets/ons::postcode-may-2021-to-oa-2021-to-lsoa-to-msoa-to-ltla-to-utla-to-rgn-to-ctry-may-2021-best-fit-lookup-in-ew/about
# pclookup <- read_csv("localdata/pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu/pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu_v2.csv")

#Local/unitary authorities
#unique(pclookup$ltla22nm)
#unique(pclookup$utla22nm)

#The four LAs in South Yorkshire then... tick
# unique(pclookup$ltla22nm)[grepl('sheffield|rotherham|doncaster|barnsley',unique(pclookup$ltla22nm),ignore.case = T)]

#Use those to match again CH postcodes to extract just South Yorkshire businesses
#47K postcodes
# sy.pclookup <- pclookup %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',ltla22nm,ignore.case = T))

#Both have same capital / space format, should have good match numbers...
# table(sy.pclookup$pcd %in% ch$RegAddress.PostCode)

#Check whether postcode match changes if formats matched




#Try with actual codepoint open latest file - the above is 2021, it's missing some latest
#Though some others might drop out, so let's see...
#257mb
pc <- read_csv('https://www.dropbox.com/scl/fi/0873bsx2ka1d3clhs8e07/postcode_localauthority_lookup_2024-11-20.csv?rlkey=rtda1srm2nlzmqxc2ms6fllno&dl=1')

#check LA match here... tick
pc$localauthority_name[grepl('sheffield|rotherham|doncaster|barnsley',pc$localauthority_name,ignore.case = T)] %>% unique

#That's fewer postcodes than the one above...
sy.pc <- pc %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',localauthority_name,ignore.case = T))


#How many SY business matches do we get? 53217. Fewer than I'm getting in FAME.
# table(ch$RegAddress.PostCode %in% sy.pclookup$pcd)

#How many from latest codepoint open, assuming the LA matches above are actually getting all of SY?
#75740 - BETTER!! OK then.
table(ch$RegAddress.PostCode %in% sy.pc$Postcode)



#Reasonably close match on the number here, but might be different firms
table(grepl('sheffield|rotherham|doncaster|barnsley',ch$RegAddress.PostTown,ignore.case = T))

#Are those overlapping firms though? Do we get either:
#SY postcodes NOT in the place name search?
#Place names NOT in the SY postcode search?

#Get both subsets and check
sy.viapostcode <- ch %>% filter(RegAddress.PostCode %in% sy.pc$Postcode)
sy.viaplacename <- ch %>% filter(grepl('sheffield|rotherham|doncaster|barnsley',RegAddress.PostTown,ignore.case = T))

#Check on matching company name
#How many via postcode in via placename? (Same number of matches will of course come up in both directions)
#Actually, different firms- about 4000 don't match
table(sy.viapostcode$CompanyName %in% sy.viaplacename$CompanyName)

#So that's 49892 common via both methods
#Leaving a good chunk of falses in both that don't match

#Let's look at those. 4200 of these
#This is MATCHES ON POSTCODE WHERE WE COULDN'T GET LOCAL AUTHORITY NAME MATCH
#That's a lot of named places in SY (including many "South Yorkshire")
#And typos e.g. SHEFFILED
#So let's trust the postcodes...
sy.viapostcode[!sy.viapostcode$CompanyName %in% sy.viaplacename$CompanyName,] %>% View

#Then... what about place names we have that postcode matching didn't get?
#First one checked - S4 7AA - is new (postcode lookup is 2021 I think)
#Some others are beyond SY boundaries e.g. Eckington
sy.viaplacename[!sy.viaplacename$CompanyName %in% sy.viapostcode$CompanyName,] %>% View

#Conclusion: non postcode matches are probably going to be beyond SY, mostly


#KEEP ONLY SY COMPANIES HOUSE POSTCODE MATCHES
#AND GEOCODE THEM
ch.geo <- ch %>%
  rename(Postcode = RegAddress.PostCode) %>% 
  left_join(
    sy.pc %>% select(Postcode,localauthority_code,localauthority_name,Eastings,Northings),
    by = 'Postcode'
  ) %>% 
  filter(!is.na(Eastings)) %>% #will be some missing due to rogue postcodes
  st_as_sf(coords = c("Eastings", "Northings"), crs = 27700)

#Save to look at in QGIS... yep all looking great
st_write(ch.geo, 'localdata/QGIS/companieshouse_southyorkshire_geo.shp')

#Save as RDS for use elsewhere via github
saveRDS(ch.geo,'data/companieshouse_southyorkshire_geopoints.rds')
















