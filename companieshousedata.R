#Companies house bulk download (~3gb)
#Monthly updates from here:
#https://www.gov.uk/guidance/companies-house-data-products
library(tidyverse)
library(sf)
library(pryr)
library(xml2)
source('functions.R')
#library(data.table)

# 1. COMPANIES HOUSE FULL UK COMPANIES LIST

## LOAD----

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



## USE POSTCODE LOOKUP TO FILTER DOWN TO SOUTH YORKSHIRE----

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

ch.geo <- readRDS('data/companieshouse_southyorkshire_geopoints.rds')




# 2. EXTRACTING FROM COMPANIES HOUSE ACCOUNTS FILES----

## Start by testing with a single one. Using iXBRL format----

#Downloaded entire accounts files, cos scraping would take longer (though that might be an option too)
#Via here: https://download.companieshouse.gov.uk/en_accountsdata.html
#Each file is monthly accounts
#"Data is only available for electronically filed accounts, which currently stands at about 75% of the 2.2 million accounts we expect to be filed each year."

# Load a file I know has got employee counts in
# doc <- read_xml("localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_00056710_20240430.html")
# 
# doc <- read_xml("localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_14688350_20240331.html")
# 
# # Extract company name
# company_name <- xml_text(xml_find_first(doc, "//ix:nonNumeric[@name='bus:EntityCurrentLegalOrRegisteredName']"))
# company_name <- xml_text(xml_find_first(doc, "//ix:nonNumeric[@name='frs-bus:EntityCurrentLegalOrRegisteredName']"))
# 
# #Testing contains method for matching various
# company_name <- xml_text(
#   xml_find_first(
#     doc,
#     "//ix:nonNumeric[contains(@name, 'EntityCurrentLegalOrRegisteredName')]",
#     ns = c(ix = "http://www.xbrl.org/2013/inlineXBRL")
#   )
# )
# 
# 
# # Extract employee numbers for 2024 and 2023
# employees_2024 <- xml_text(xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D0']"))
# employees_2023 <- xml_text(xml_find_first(doc, "//ix:nonFraction[@name='core:AverageNumberEmployeesDuringPeriod' and @contextRef='D11']"))
# 
# print(paste("Company:", company_name, "| Employees (2024):", employees_2024, "| Employees (2023):", employees_2023))
# 
# 
# #Test function version... tick
# debugonce(get_accounts_employeenumber)
# get_accounts_employeenumber('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_00056710_20240430.html')
# 
# get_accounts_employeenumber('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_14688350_20240331.html')
# 
# get_accounts_employeenumber('localdata/Accounts_Bulk_Data-2024-11-19/Prod223_3832_02641794_20240630.html')



  #OK, now check match of accounts versus SY businesses
#(Can check against all UK businesses later)

#File names have company number and then filing date
#Can separate thus
#This is the latest, only 16K account filings
accounts <- list.files("~/localdata/Accounts_Bulk_Data-2024-11-19",'*.html', full.names = T) %>%
  as_tibble() %>%
  rename(filelocation = value)

#Add on shorter filename for ease of processing, extract company number
accounts <- accounts %>% 
  mutate(
    value = basename(filelocation),#pull out just short final name
    companynumber = str_split(value, "_", simplify = TRUE)[, 3]
  ) %>% 
  select(-value)


#OK, check if any accounts matches this month in SY
table(accounts$companynumber %in% ch.geo$CompanyNumber)

#Yes - let's just view those
ch.geo %>% filter(CompanyNumber %in% accounts$companynumber) %>% View



#Test how many of these we can extract employee number from. Function that up.
#Just for South Yorkshire for now
employee.numbers <- map(accounts$filelocation[accounts$companynumber %in% ch.geo$CompanyNumber],get_accounts_employeenumber, .progress = T) %>% bind_rows

#Add back in company numbers
employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]


#Check on those not being picked up - what's happening with those accounts?
#Example: 02641794
#OK, fixed that - we now have all firm names being extracted
#Some still with NA employee number - let's just check the originals don't have them
#E.g. 02309294 "LJT Motors Limited"... yep no employee values
#Or 14771941 "ARCHERS INVESTMENT COMPANY LIMITED"... "Dormant accounts"
#For which there's an xml field, so let's try and get that... ah there's a lot of dormant companies, OK
#https://www.yourcompanyformations.co.uk/blog/dormant-company-explained/

#remaining ones without employee counts, the script has been scrambled and is unreadable 
#But note: we're getting MUCH higher positive values than FAME has (which is about 50%)
#Look:
table(!is.na(employee.numbers$Employees_thisyear[employee.numbers$dormantstatus=='false'])) %>% prop.table


#The issue might be the ~25% who don't file electronic accounts, among other things

#Just check with a few samples that the order of employee year is correct
#Pick to look at...
#OK LOOKING GOOD
View(employee.numbers %>% filter(dormantstatus == 'false'))
#10634903 UK200: tick
#09038383 Quando: tick
#09735145 pet repair: tick
#05880174 H & E Electrical Services Limited: tick
#08421033 Mexborough mini market: tick



#REPEAT FOR SEPTEMBER 2024 MONTHLY DATA
#To check computational demand etc
#192K entries in a month ... err now somehow gone up to 359K on a second run
#Think OS must still have been indexing?
#Stored locally only
accounts <- list.files("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024",'*.html', full.names = T) %>%
  as_tibble() %>%
  rename(filelocation = value)

#Add on shorter filename for ease of processing, extract company number
accounts <- accounts %>% 
      mutate(
        value = basename(filelocation),#pull out just short final name
        companynumber = str_split(value, "_", simplify = TRUE)[, 3]
      ) %>% 
  select(-value)


#OK, check if any accounts matches this month in SY - yes, 4841
table(accounts$companynumber %in% ch.geo$CompanyNumber)

#Yes - let's just view those
# ch.geo %>% filter(CompanyNumber %in% accounts$companynumber) %>% View

#Extract employee numbers from accounts files
employee.numbers <- map(accounts$filelocation[accounts$companynumber %in% ch.geo$CompanyNumber],get_accounts_employeenumber, .progress = T) %>% bind_rows

#Add back in company numbers
employee.numbers$companynumber <- accounts$companynumber[accounts$companynumber %in% ch.geo$CompanyNumber]

#Check what proportion of firms we got employees for here... 99%, bonza
table(!is.na(employee.numbers$Employees_thisyear[employee.numbers$dormantstatus=='false'])) %>% prop.table

employee.numbers %>% filter(dormantstatus == 'false') %>% View

#Note: ENTRY MISTAKES E.G. THE TOP LISTED EMPLOYEE COUNT IS ACTUALLY THE INCOME - 147K.
#Watch out for those.








## Work on bulk account downloading for processing all accounts for the last year----

#Which in theory should give us all live accounts given yearly submission necessary, but let's see

# Load the HTML file
doc <- read_html("https://download.companieshouse.gov.uk/en_monthlyaccountsdata.html")

# Extract all <a> tags with hrefs containing "Accounts_Monthly_Data"
zip_links <- xml_attr(
  xml_find_all(doc, "//a[contains(@href, 'Accounts_Monthly_Data') and contains(@href, '.zip')]"),
  "href"
)

#CHECK IF ANY EXISTING FOLDERS ALREADY DOWNLOADED, ONLY GET NEW ONES
existing_folders <- list.dirs('~/localdata/monthly_companieshouse_accounts', full.names = F)

#These folders are already present and unzipped
# zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

#These are the ones to keep and download
# zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

zip_links <- zip_links[!gsub('.zip','',zip_links) %in% existing_folders]

#Append those to the web URL for download
zip_links_urls <- paste0('https://download.companieshouse.gov.uk/', zip_links)

# Define the folder to save the ZIP files
output_folder <- "~/localdata/monthly_companieshouse_accounts"

# Set the full path to save the file
file_path <- file.path(output_folder, zip_links)




#Download before unzipping
#THIS IS GOING TO TAKE SOME HOURS ON FIRST RUN!
for(i in 1:length(zip_links)){
  
  cat('Starting',i,'out of',length(zip_links),', file:',file_path[i],'\n')

  x <- Sys.time()
  
  #Download a single file - several GB
  tryCatch({
    download.file(zip_links_urls[i], file_path[i], mode = "wb")  # Binary mode for ZIP files
    message(paste("Downloaded"))
  }, error = function(e) {
    message(paste("Failed to download", "Error:", e$message))
  })
  
  print(Sys.time()-x)
  
  print("Unzipping...")
  
  unzip(paste0(output_folder,'/',zip_links[i]),exdir=paste0(output_folder,'/',gsub('.zip','',zip_links[i])))
  
  cat('Finished everything for',i,'out of',length(zip_links),', file:',file_path[i],'\n')
  print(Sys.time()-x)

}


#Test download with smaller daily file
# tryCatch({
#   download.file("https://download.companieshouse.gov.uk/Accounts_Bulk_Data-2024-11-16.zip", paste0(output_folder,'/test.zip'), mode = "wb")  # Binary mode for ZIP files
#   message(paste("Downloaded:", file_name))
# }, error = function(e) {
#   message(paste("Failed to download:", file_name, "Error:", e$message))
# })
# 
# unzip(paste0(output_folder,'/test.zip'),exdir=paste0(output_folder,'/test'))








## Examine all iXBRL tags in a particular doc to see what we've got----

#Gripple as example
doc <- read_xml("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024/Prod224_2476_01772901_20231231.html")

# Define namespaces dynamically
ns <- xml_ns(doc)

# Extract all <ix:nonNumeric> and <ix:nonFraction> tags
non_numeric_nodes <- xml_find_all(doc, "//ix:nonNumeric", ns = ns)
non_fraction_nodes <- xml_find_all(doc, "//ix:nonFraction", ns = ns)

# Combine the text content of these nodes into a single vector
all_tags <- c(
  xml_text(non_numeric_nodes),
  xml_text(non_fraction_nodes)
)

# Print the resulting vector
print(all_tags)

# Optional: To inspect the corresponding tag names and attributes
all_tags_with_names <- c(
  paste("Tag Name:", xml_name(non_numeric_nodes), "Value:", xml_text(non_numeric_nodes)),
  paste("Tag Name:", xml_name(non_fraction_nodes), "Value:", xml_text(non_fraction_nodes))
)

print(all_tags_with_names)



#See what the names of the tags are, matched against values
#TODO: search several accounts, look for what frequencies we get for different iXBRL tags
doc <- read_xml("~/localdata/monthly_companieshouse_accounts/Accounts_Monthly_Data-September2024/Prod224_2476_01772901_20231231.html")

# Define namespaces dynamically
ns <- xml_ns(doc)

# Extract all <ix:nonNumeric> and <ix:nonFraction> nodes
non_numeric_nodes <- xml_find_all(doc, "//ix:nonNumeric", ns = ns)
non_fraction_nodes <- xml_find_all(doc, "//ix:nonFraction", ns = ns)

# Extract the 'name' attribute from these nodes
non_numeric_names <- xml_attr(non_numeric_nodes, "name")
non_fraction_names <- xml_attr(non_fraction_nodes, "name")

# Combine into a single vector
all_names <- c(non_numeric_names, non_fraction_names)

# Remove any NA values (nodes without a 'name' attribute)
all_names <- all_names[!is.na(all_names)]

# Print the resulting vector of names
print(all_names)

# Optional: Combine names with their text content for detailed inspection
all_details <- c(
  paste("Name:", non_numeric_names, "Value:", xml_text(non_numeric_nodes)),
  paste("Name:", non_fraction_names, "Value:", xml_text(non_fraction_nodes))
)

# Remove entries where the name is NA
all_details <- all_details[!is.na(all_names)]

print(all_details)

all_details[grepl('profit',all_details,ignore.case = T)]








