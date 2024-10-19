library(httr)
library(jsonlite)


#Test google places API postcode extraction 
debugonce(get_fulladdress_from_osm)
chk <- get_fulladdress_from_osm("Sheffield University", "Sheffield", quotebusiness = T)

#Via https://stackoverflow.com/questions/51828712/r-regular-expression-for-extracting-uk-postcode-from-an-address-is-not-ordered
address_to_postcode(chk$display_name)


#use nopc df from fame_processing, ordered by employee number, to try to get postcodes
nopc <- nopc %>% arrange(-employees)

#Use only those with employee counts
nopc_employeecount <- nopc %>% filter(!is.na(employees))



#OPENSTREETMAP POSTCODE FINDER (HIT AND MISS BUT DOES OK)----
postcodes_n_addresses <- list()

#leave it 2 seconds between requests, though limit is > 1
for(name in nopc_employeecount$`Company name`[1:10]){
  
  result <- get_fulladdress_from_osm(name, "South Yorkshire", quotebusiness = T)
  
  if(length(result) > 0){
  
    print(result$display_name)
    
    result = result$display_name
    
    postcode <- address_to_postcode(result)
    
    print(postcode)
  
  } else {
    
    result = NA
    postcode = NA
    
  }
  
  #Via https://stackoverflow.com/questions/51828712/r-regular-expression-for-extracting-uk-postcode-from-an-address-is-not-ordered
  #tested on one, let's see for several
  postcodes_n_addresses[[length(postcodes_n_addresses)+1]] <- c(companyname = name, address = result, postcode = postcode)

  print("--")

  Sys.sleep(2)
  
}

postcodes_n_addresses.df <- bind_rows(postcodes_n_addresses)

#Manually fix a few large employers that the business search above didn't capture




#GOOGLE PLACES NEW POSTCODE FINDER (FASTER / MORE ACCURATE / ONLY FREE ALLOWANCE)----

#This has $200 free a month, and 1000 searches is $2.83, so should be OK?
#Ah except it'll want to connect to a billing account...

# debugonce(get_uk_postcode_google_places_new)
# get_uk_postcode_google_places_new("business doncaster", "South Yorkshire", api_key)

api_key <- scan('localdata/googleapikey.txt', character(), quote = "")

#Repeat for google places api
postcodes_n_addresses <- list()

#Note: all uni of sheffield results are same firm
#But let's store employee number as another key for disambiguation just in case
# for(name in nopc_employeecount$`Company name`[1:3]){
for(i in 1:nrow(nopc_employeecount)){
  
  # debugonce(get_uk_postcode_google_places_new)
  result <- get_uk_postcode_google_places_new(nopc_employeecount$`Company name`[i], "South Yorkshire", api_key)
  
  if(length(result) > 0){
    
    print(result)
    
  } else {
    
    result = NA
    
  }
  
  #Via https://stackoverflow.com/questions/51828712/r-regular-expression-for-extracting-uk-postcode-from-an-address-is-not-ordered
  #tested on one, let's see for several
  postcodes_n_addresses[[length(postcodes_n_addresses)+1]] <- c(
    companyname = nopc_employeecount$`Company name`[i], 
    employees = nopc_employeecount$employees[i], 
    postcode = result)
  
  print("--")
  
  #https://developers.google.com/maps/documentation/places/web-service/usage-and-billing
  #"Rate limit is 600 QPM (requests per minute) per API method per project."
  Sys.sleep(0.3)
  
}

#save temporarily to make sure not lost!
saveRDS(postcodes_n_addresses,'localdata/googleplacesnew_postcodefixing.rds')

postcodes_n_addresses.df <- bind_rows(postcodes_n_addresses)

saveRDS(postcodes_n_addresses.df,'localdata/googleplacesnew_postcodefixing_dataframe.rds')

#How'd that do? Pretty good
table(is.na(postcodes_n_addresses.df$postcode))


