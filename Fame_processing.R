#Fame South Yorkshire businesses
library(tidyverse)
library(sf)
library(tmap)
library(lubridate)
library(pryr)
library(plotly)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get postcode location lookup----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We're looking at South Yorkshire - can check this against that map
#But shouldn't have to load in entire of codepoint open to get SY matches
#Just find correct postcode areas
#https://en.wikipedia.org/wiki/List_of_postcode_areas_in_the_United_Kingdom

#files <- list.files('../../../MapPolygons/lookups/codepointopen/codepo_gb/Data/CSV', pattern = 's.csv|dn.csv|wf.csv|ng.csv', full.names = T)

files <- paste0('../../../MapPolygons/lookups/codepointopen/codepo_gb/Data/CSV/', paste0(c('s.csv','dn.csv','wf.csv','ng.csv'), sep = ''))

#Combine into one (no header in origs)
pc <- files %>% 
  lapply(read_csv, col_names = F) %>% 
  bind_rows

#From downloaded metadata in codepoint open folder
names(pc) <- c('Postcode','Positional_quality_indicator','Eastings','Northings','Country_code','NHS_regional_HA_code','NHS_HA_code','Admin_county_code','Admin_district_code','Admin_ward_code')

#Remove spaces
pc$postcode_nospaces <- gsub(' ','',pc$Postcode)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get Fame data for South Yorkshire (downloaded in two tranches)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fame <- c('localdata/FAME_SHEFFIELD_NUTS2_12_9_24.csv','localdata/FAME_BDR_NUTS2_12_9_24.csv') %>% 
  lapply(read_csv) %>% 
  bind_rows %>% 
  rename(
    employees = `Number of employees\nLast avail. yr`,
    turnover = `Operating revenue (Turnover)\nth GBP Last avail. yr`
    )

fame$postcode_nospaces <- gsub(' ','',fame$`R/O Full Postcode`)


#Employee column needs some processing to get to numeric
# fame <- fame %>% 
#   mutate(
#     employees2 = case_when(
#       employees == 'n.a' ~ NA,
#       
#     )
#   )

fame$employees[fame$employees == 'n.a.'] <- NA
fame$employees <- gsub(',','',fame$employees)
#Can now convert to numeric
fame$employees <- as.numeric(fame$employees)

#Repeat for turnover
fame$turnover[fame$turnover == 'n.a.'] <- NA
fame$turnover <- gsub(',','',fame$turnover)
#Can now convert to numeric
fame$turnover <- as.numeric(fame$turnover)

#DATES
#How many NA? A lot...
table(is.na(fame$`Date of incorporation`))

#Overlap with NA no location? Most but not quite all
table(is.na(fame$`Date of incorporation`), is.na(fame$postcode_nospaces))

#Check dates have consistent formatting... looks so.
fame$`Date of incorporation`[sample(1:nrow(fame),100)]


#FORMAT AS DATE
fame$date_of_incorporation <- lubridate::dmy(fame$`Date of incorporation`)


#~~~~~~~~~~~~~~~~~~~~~~
#LINK TO SIC LOOKUP----
#~~~~~~~~~~~~~~~~~~~~~~

siclookup = read_csv('https://github.com/DanOlner/ukcompare/raw/master/data/SIClookup.csv')

#check match
table(unique(siclookup$SIC_5DIGIT_CODE) %in% unique(fame$`Primary UK SIC (2007) code`))

#Which ones IN LOOKUP NOT IN FAME DATA
unique(siclookup$SIC_5DIGIT_CODE)[!unique(siclookup$SIC_5DIGIT_CODE) %in% unique(fame$`Primary UK SIC (2007) code`)]

#This might just be because not every sector has a firm in SY. Check...
#Ah nearly - just four in the FAME data with no match
table(unique(fame$`Primary UK SIC (2007) code`) %in% unique(siclookup$SIC_5DIGIT_CODE))

#What are those 4 in the FAME data?
#One is NA...
unique(fame$`Primary UK SIC (2007) code`)[!unique(fame$`Primary UK SIC (2007) code`) %in% unique(siclookup$SIC_5DIGIT_CODE)]

#78100 wouldn't have its own category, only sub ones
#Which firms?
#Yes, those should be 78109 " Activities of employment placement agencies (other than motion picture, television and other theatrical casting)"
View(fame %>% filter(`Primary UK SIC (2007) code` == '78100'))

#Then 62010...
#Those can go in 62012 "business / domestic software development"
View(fame %>% filter(`Primary UK SIC (2007) code` == '62010'))

#Then 98000...
#Which is supposed to be households producing goods for own use, which clearly a lot of these are not
#("Households should be classified here only if it is impossible to identify a primary activity for the subsistence activities
#of the household. If the household engages in market activities, it should be classified according to the 
#primary market activity carried out/")
#https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html
View(fame %>% filter(`Primary UK SIC (2007) code` == '98000'))

#Which should mean they're not market active, so I'll remove those...?
#Well let's just keep, but no need to.

#Process -->
fame$`Primary UK SIC (2007) code`[fame$`Primary UK SIC (2007) code` == '78100'] <- '78109'
fame$`Primary UK SIC (2007) code`[fame$`Primary UK SIC (2007) code` == '62010'] <- '62012'
fame$`Primary UK SIC (2007) code`[fame$`Primary UK SIC (2007) code` == '98000'] <- '98200'#undiffd non market services

#Leave NAs
unique(fame$`Primary UK SIC (2007) code`)[!unique(fame$`Primary UK SIC (2007) code`) %in% unique(siclookup$SIC_5DIGIT_CODE)]

#Merge in SIC lookup
fame <- fame %>% 
  left_join(
    siclookup,
    by = c('Primary UK SIC (2007) code' = 'SIC_5DIGIT_CODE')
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LINK TO POSTCODE LOCATION DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Check link to postcode lookup
# table(fame$`R/O Full Postcode` %in% pc$Postcode)
table(fame$postcode_nospaces %in% pc$postcode_nospaces)

#Check falses... just missing postcodes
#Vast majority are NA - 13167 out of 13332. Rest likely typos or defunct postcodes
table(fame$postcode_nospaces[!fame$postcode_nospaces %in% pc$postcode_nospaces], useNA = 'always')

#Just look at rows with no postcode
#View(fame %>% filter(is.na(postcode_nospaces)))
nopc <- fame %>% filter(is.na(postcode_nospaces))

range(nopc$employees, na.rm = T)

#It's a lot of sometimes very large employees and/or third sector orgs
#Plus others
#With no addresses
#Those could do with having postcodes added / would need a bit of processing

#Check others WITH postcodes
fame %>% filter(!is.na(postcode_nospaces)) %>% View

#Some things to note:
#Employee number is definitely WHOLE COMPANY not localised (or not necessarily)
#E.g. stoneacre, near the top of those private firms - 
#Around 3000 employees, presumably spread across all its acquisitions, not just in Doncaster
#https://en.wikipedia.org/wiki/Stoneacre_Motor_Group
#Also, it's the same ownership as the one below it, Decidebloom - bit of an issue there for summing things


#Postcode location matching
fame.geo <- fame %>% 
  filter(!is.na(postcode_nospaces)) %>%
  left_join(
    pc %>% select(postcode_nospaces,Eastings,Northings)
  ) %>% 
  filter(!is.na(Eastings)) %>% #will be some missing due to rogue postcodes found above
  st_as_sf(coords = c("Eastings", "Northings"), crs = 27700)

#Save to look at in QGIS
st_write(fame.geo, 'localdata/QGIS/fame_geo.shp')


#SAVE!!
saveRDS(fame.geo,'localdata/fame_sy_processed_geo.rds')



#~~~~~~~~~~~~~~~~~~~
#CHECK GEOGRAPHY----
#~~~~~~~~~~~~~~~~~~~

#Check the geographical reach - want SY ITL2 zone covered, may need more Fame or more postcodes!
la <- st_read('../../../MapPolygons/GreatBritain/2016/Local_Authority_Districts_December_2016_Generalised_Clipped_Boundaries_in_Great_Britain/') %>% 
  st_transform(crs = 27700)

sy <- la %>% filter(grepl('sheff|barnsley|doncas|rotherham',lad16nm, ignore.case = T))

st_write(sy, 'localdata/QGIS/sy_localauthorityboundaries.shp')

plot(st_geometry(sy))
plot(fame.geo[sample(nrow(fame.geo),10000),] %>% st_geometry, add = T)


#Check points against map...
tmap_mode('view')

tm_shape(sy) +
  tm_borders() +
tm_shape(fame.geo[sample(nrow(fame.geo),1000),]) +
  tm_dots()

#NOTE: MERGING SIC LOOKUP IN IS BREAKING SOMETHING, DON'T KNOW WHY





#~~~~~~~~~~~~~~~~~~~~~~
#CHECK OTHER FIELDS----
#~~~~~~~~~~~~~~~~~~~~~~

#How many unique 5 digit SICs?
#Are they all being used? Most of them, yes. 662 out of 723?
length(unique(fame.geo$`Primary UK SIC (2007) code`))


#How many just within manufacturing?
#A third of them??
(unique(fame.geo$SIC_2DIGIT_NAME[ grepl('manufactur',fame.geo$SIC_2DIGIT_NAME,ignore.case = T) ]))
length(unique(fame.geo$`Primary UK SIC (2007) code`[ grepl('manufactur',fame.geo$SIC_2DIGIT_NAME,ignore.case = T) ]))

#Let's save those manufacturing 5-digit SICS and see where in SY...
#Issue on saving is that truncated field names are duplicates. We don't need them all...
st_write(
  fame.geo %>% select(-c(SIC_2DIGIT_CODE,SIC_3DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_SECTION_LETTER,SIC_SECTION_CODE)) %>%  
    filter(grepl('manufactur',SIC_2DIGIT_NAME,ignore.case = T)),
  'localdata/QGIS/fame_geo_manufacturing.shp')


#Let's look at some simple stats for manuf
fame.geo.manuf <- fame.geo %>% select(-c(SIC_2DIGIT_CODE,SIC_3DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_SECTION_LETTER,SIC_SECTION_CODE)) %>%  
  filter(grepl('manufactur',SIC_2DIGIT_NAME,ignore.case = T))



ggplot(fame.geo.manuf, aes(x = SIC_2DIGIT_NAME, y = employees)) +
  geom_jitter() +
  coord_flip() +
  scale_y_log10()


#Turnover per employee vs actual firm size by turnover
fame.geo <- fame.geo %>% 
  mutate(turnoverperemployee = turnover/employees)

#How many do we not have turnover or employee count info for?
#266 with turnover data, 64006 without. Oh dear.
#0.41% or a bit less than 1 in 200 firms?
table(is.na(fame.geo$turnover))

#Only larger firms need to report turnover... tho where then does the employee data come from
#When turnover is missing?



#Better - about 50/50, slightly more with
table(is.na(fame.geo$employees))

#Any bias for which broad sectors we have job counts for?
#Broadly similar, nothing too stand out, though some interesting differences
ggplot(fame.geo, aes(x = SIC_SECTION_NAME, fill = is.na(fame.geo$employees))) +
  geom_bar(position = position_dodge(width = 0.5)) +
  coord_flip()


#Of those we do have data for turnover AND employees...
p <- ggplot(fame.geo, aes(x = turnoverperemployee, y = turnover, colour = SIC_SECTION_NAME, group = 1)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm') +
  scale_x_log10() +
  scale_y_log10() 

ggplotly(p)


#... relationship is pretty economies-of-scale-strong
cor(fame.geo$turnoverperemployee,fame.geo$turnover, use = 'pairwise.complete.obs', method = 'spearman')


#And vs employee number... hmmph
ggplot(fame.geo, aes(x = turnoverperemployee, y = employees, colour = SIC_SECTION_NAME, group = 1)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = 'lm')

#And just turnover vs employee!
ggplot(fame.geo, aes(x = turnover, y = employees, colour = SIC_SECTION_NAME, group = 1)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm') +
  scale_x_log10() +
  scale_y_log10() 


#Breakdown of av turnover per employee per SIC section
#Suspect a pattern
p <- ggplot(fame.geo, aes(x = SIC_SECTION_NAME, y = turnoverperemployee)) +
  geom_violin() +
  geom_jitter() +
  scale_y_log10() +
  coord_flip()

ggplotly(p)


#Looking directly at a couple of sectors
fame.geo %>% filter(grepl('manuf',SIC_SECTION_NAME, ignore.case = T)) %>% View
fame.geo %>% filter(grepl('wholesale',SIC_SECTION_NAME, ignore.case = T)) %>% View
fame.geo %>% filter(grepl('information',SIC_SECTION_NAME, ignore.case = T)) %>% View


#~~~~~~~~~~~~~~~~~~
#CHECK ON DATES----
#~~~~~~~~~~~~~~~~~~

#Keep only businesses in the last ten years
fameten <- fame %>% filter(date_of_incorporation > dmy('01/09/2013'))

#bin by year / keep only year
fameten$year_of_incorporation = year(fameten$date_of_incorporation)


#plot incorp dates for different sectors...
#Currently for any size

#Huh. Is that a data artifact or did this actually happen?
ggplot(fameten, aes(x = factor(year_of_incorporation))) +
  geom_bar() 


ggplot(
  # fameten, 
  # fameten %>% filter(employees < 10 & employees > 1), 
  # fameten %>% filter(employees > 10),
  fameten %>% filter(employees == 1),
       aes(x = factor(year_of_incorporation))) +
  geom_bar() +
  facet_wrap(~SIC_SECTION_NAME)



ggplot(
  # fameten, 
  # fameten %>% filter(employees < 10 & employees > 1), 
  fameten %>% filter(employees == 1),
  # fameten %>% filter(employees == 1), 
       aes(x = factor(year_of_incorporation))) +
  geom_bar() +
  facet_wrap(~SIC_2DIGIT_NAME)




#Number of employees in full dataset...
# ggplot(fame.geo %>% filter(employees < 26), aes(x = factor(employees))) +
#   geom_bar()


#~~~~~~~~~~~~~~~~~
#SOME BASIC QUESTIONS----
#~~~~~~~~~~~~~~~~~

#For firms we have emlployment numbers for, what % of firms have 50% of employees?
table(is.na(fame.geo$employees))

fame.emp <- fame.geo %>% filter(!is.na(employees)) %>% st_set_geometry(NULL)

#Order by employee number, smallest to largest
#Cumulative count
#Find cutoff of 50%
fame.emp <- fame.emp %>% 
  arrange(employees) %>% 
  mutate(cumul_employeecount = cumsum(employees))

selectedrow <- which.min(abs(fame.emp$cumul_employeecount - sum(fame.emp$employees)/2))

#Meaning that 328 businesses out of 33747 have 50% of employees
#(And note I think this dataset is missing the universities currently)
nrow(fame.emp) - selectedrow

#That's this % of businesses (in this DF) ... a bit less than 1%
((nrow(fame.emp) - selectedrow)/nrow(fame.emp)) * 100

#Which we can test... tick
sum(fame.emp$employees[1:33419])
sum(fame.emp$employees[33420:nrow(fame.emp)])

#plot the cumul of employees...
plot(fame.emp$cumul_employeecount)


#But trying to understand what's going on with employee counts... urgh
#E.g.
#https://dunamishv.co.uk/about/who-we-are/
#DUMANIS GROUP LIMITED has one employee
#DUMANIS INFRASTRUCTURE SERVICES LIMITED has 35, which is the right number judging by the website
#But the website's different for the firm with larger numbers
#This crops up a lot
#Firm count numbers are not a great guide to anything...?



#How many firms in each of the 5 digit categories / how many if firms with employee count?
table(fame.geo$SIC_5DIGIT_NAME) %>% View



#~~~~~~~~~~~~~~~~~
#Random things----
#~~~~~~~~~~~~~~~~~

#est turnover of SY clean tech sector from DSIT / data city clusters project
#19,562,646,000
t <- 19562646000

#Total turnover via Fame (can use non geo version)
#I think thousands, from the excel sheet... 
#And confirmed via a filter check on turnover above a million in FAME
sum(fame$turnover, na.rm = T)

#12,427,671,000
ft <- sum(fame$turnover, na.rm = T) * 1000

(t/ft)*100





