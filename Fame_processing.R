#Fame South Yorkshire businesses
library(tidyverse)
library(sf)
library(tmap)
library(lubridate)
library(pryr)
library(plotly)
library(plotme)


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

#How many of those have employee counts? Most of them
table(is.na(nopc$employees))

#How many are small numbers?
table(nopc$employees)

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


#Some sectors...
tmap_mode('view')

#
#Arts entertainment and recreation doesn't get any more granular beyond 2 digit
fame.geo$SIC_2DIGIT_NAME[grepl('entertainment',fame.geo$SIC_2DIGIT_NAME,ignore.case=T)] %>% unique
fame.geo$SIC_3DIGIT_NAME[grepl('entertainment',fame.geo$SIC_3DIGIT_NAME,ignore.case=T)] %>% unique

#Performing arts...
fame.geo$SIC_5DIGIT_NAME[grepl('performing',fame.geo$SIC_5DIGIT_NAME,ignore.case=T)] %>% unique

#Get all 5 digit from 3 digit "entertainment" down
fame.geo$SIC_5DIGIT_NAME[grepl('entertainment',fame.geo$SIC_3DIGIT_NAME,ignore.case=T)] %>% unique


tm_shape(sy) +
  tm_borders() +
  tm_shape(fame.geo %>% filter(grepl('entertainment',SIC_3DIGIT_NAME,ignore.case=T))) +
  tm_dots()

#View em
fame.geo %>% filter(grepl('entertainment',SIC_3DIGIT_NAME,ignore.case=T)) %>% View



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


#For those with employment numbers
#What proportion of firms are just single employee?
table(fame.emp$employees == 1)

#Counts for different bands? Cf. ONS data on this!
table(fame.emp$employees[fame.emp$employees < 50]) %>% plot

#Deciles? Hmm nope, too many 1s, obv!
# fame.emp <- fame.emp %>%
#   mutate(
#     employeecountdeciles = as.numeric(cut_number(employees, 10))
#   )






#How many firms in each of the 5 digit categories / how many if firms with employee count?
table(fame.geo$SIC_5DIGIT_NAME) %>% View

#Visualise nesting of SICs
#Count firms first (with employee counts? Let's compare)
firmcountSIC <- fame.geo %>% 
  st_set_geometry(NULL) %>% 
  group_by(SIC_5DIGIT_NAME) %>% 
  summarise(
    count = n(),
    SIC_2DIGIT_CODE = max(SIC_2DIGIT_CODE)
    ) 


#UTF8 error, try this
#https://github.com/r-spatial/mapview/issues/194#issuecomment-435455450
firmcountSIC <- firmcountSIC %>% mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)})

firmcountSIC <- firmcountSIC %>% 
  mutate(SIC_5DIGIT_CODE = substr(SIC_5DIGIT_NAME,1,5))


#Sunburst awkward parent child rel
parent_data <- firmcountSIC %>%
  filter(!is.na(SIC_2DIGIT_CODE)) %>% 
  group_by(SIC_2DIGIT_CODE) %>%
  summarise(count = sum(count)) %>%
  mutate(SIC_5DIGIT_CODE = SIC_2DIGIT_CODE, SIC_2DIGIT_CODE = "")  # Set parent to empty for top-level

# Combine parent and child data into one frame
sunburst_data <- bind_rows(firmcountSIC, parent_data)

plot_ly(
  sunburst_data,
  labels = ~SIC_5DIGIT_CODE,
  parents = ~SIC_2DIGIT_CODE,
  values = ~count,
  type = 'sunburst',
  branchvalues = 'total'
) %>%
  layout(title = "Sunburst Plot of Industrial Categories")




#Let's see if this works - helper for easily converting dataframe to sunburst/treemap friendly hierarchy
#https://github.com/yogevherz/plotme
library(plotme)

#Success!
fame.geo %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(SIC_SECTION_CODE)) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  mutate(SIC_5DIGIT_CODE = substr(SIC_5DIGIT_NAME,1,5)) %>% 
  count(SIC_SECTION_CODE,SIC_3DIGIT_CODE,SIC_5DIGIT_CODE) %>% 
  count_to_sunburst()

#Will have to abbreviate names, but what happens if try to use?
fame.geo %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(SIC_SECTION_NAME), employees > 500) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  mutate(SIC_5DIGIT_CODE = substr(SIC_5DIGIT_NAME,1,5)) %>% 
  count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_sunburst(sort_by_n = T)


#Vesion that counts employees
#Check firm count logic, translate to employee count for each of those
fame.geo %>%
  st_set_geometry(NULL) %>%
  count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  View

#Employee count version will just be 
#This isn't working - it's counting sublayers based on row count, not the actual sum totals
#(inner and middle layer don't change counts when employee number changed)
#UPDATE: yes, think probably it is working... it's just changes are minimal
#E.g. construction is top if just counting firms with any number of employees
fame.geo %>%
  st_set_geometry(NULL) %>%
  filter(employees > 1000) %>% 
  # filter(!is.na(employees)) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  group_by(SIC_5DIGIT_NAME) %>% 
  summarise(
    n = sum(employees),
    SIC_3DIGIT_NAME = max(SIC_3DIGIT_NAME),SIC_SECTION_NAME = max(SIC_SECTION_NAME)
    ) %>% 
  ungroup() %>% 
  select(c(4,3,1,2)) %>% #need cols ordering correctly
  # View
  count_to_sunburst(sort_by_n = T)


#Oh actually, in theory it already has a weight function for that in count. Does it work?
#Same problem - inner layer counts don't change regardless of changing employee filter
fame.geo %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(SIC_SECTION_NAME), employees > 0) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  mutate(SIC_5DIGIT_CODE = substr(SIC_5DIGIT_NAME,1,5)) %>% 
  # count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME, wt = employees) %>% 
  count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME) %>% 
  count_to_sunburst(sort_by_n = T)





#How many firms is that, can see plz?
fame.geo %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(SIC_SECTION_NAME), employees > 1000) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  mutate(SIC_5DIGIT_CODE = substr(SIC_5DIGIT_NAME,1,5)) %>% 
  count(SIC_SECTION_NAME,SIC_3DIGIT_NAME,SIC_5DIGIT_NAME, wt = employees) %>% 
  View


#Actually, would be nice to see how SIC section composition changes as employee number goes up
#Looks quite stable-ish
#And probably more informative

#What I'm after here:
#For a series of employee count bins (deciles not doable cos of spread, but let's look)
#Get the proportion of employment by sector
#Err... though don't we have that better from ONS data?
#Probably, but comparison could be useful
#And can do more granular here



#~~~~~~~~~~~~~~~~~
#Sunburst plot testing----
#~~~~~~~~~~~~~~~~~

#It's a pain to figure out the value hierarchy
#The plotme plugin above doesn't seem to get it right

#So - work out exactly what's going on with it (that the documentation utterly fails to do)
#So we can make a proper one

#With some small sample data
fame.s <- fame.geo %>% 
  st_set_geometry(NULL) %>% 
  select(employees,contains('_NAME')) %>% 
  filter(!is.na(employees),!is.na(SIC_SECTION_NAME)) %>% 
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)})#needed cos formating breaks sunburst plot

#Pick some random sectors
fame.s <- fame.s %>% 
  filter(grepl('manuf|educ|constru',SIC_SECTION_NAME,ignore.case = T))



#TESTING WITH BASIC EXAMPLE
#So that's a basic count
#Let's see about transforming

#Test just two layers first
#labels are ALL labels regardless of position; labels shouldn't be duplicated (they won't be summed)
#Parents are the parents of labels; blank text indicates root categories at the centre
twolayer <- data.frame(
  labels = c('manuf','blue steel','green steel','advanced','ict','games programming','admin programming','telecoms'),
  parents = c('','manuf','manuf','manuf','','ict','ict','ict'),
  n = c(45,10,15,20,61,6,30,25)
)

  
#then we define labels and those labels' parents?
#plot_ly(twolayer, labels = ~labels, parents = ~parents, values =~n, type = 'sunburst')
plot_ly(twolayer, labels = ~labels, parents = ~parents, values =~n, type = 'sunburst', branchvalues = 'total')



#So how to add third layer?
#Looking at structure of example from R plotly
#Ah OK, so that example is using IDs to unique-ify e.g. football in different places, where the label 'football' is repeated
d <- data.frame(
  ids = c(
        "North America", "Europe", "Australia", "North America - Football", "Soccer",
        "North America - Rugby", "Europe - Football", "Rugby",
        "Europe - American Football","Australia - Football", "Association",
        "Australian Rules", "Autstralia - American Football", "Australia - Rugby",
        "Rugby League", "Rugby Union"
      ),
  labels = c(
        "North<br>America", "Europe", "Australia", "Football", "Soccer", "Rugby",
        "Football", "Rugby", "American<br>Football", "Football", "Association",
        "Australian<br>Rules", "American<br>Football", "Rugby", "Rugby<br>League",
        "Rugby<br>Union"
      ),
  parents = c(
        "", "", "", "North America", "North America", "North America", "Europe",
        "Europe", "Europe","Australia", "Australia - Football", "Australia - Football",
        "Australia - Football", "Australia - Football", "Australia - Rugby",
        "Australia - Rugby"
    
  ),
  stringsAsFactors = FALSE
)


#So returning to the three layers, with no repeated labels (or shouldn't be..)
#Two root nodes: manuf and ict
#labels should include those, and also if we want a middle layer, those too. Let's test...
#Parent: Normal steel >> blue and green, advanced steel >> space steel
#Parent: programming >> the two types of programming, other ict >> telecoms
threelayer <- data.frame(
  labels = c('manuf','blue steel','green steel','space steel','normal steel','advanced steel',   'ict','games programming','admin programming','telecoms','programming','other ict'),
  parents = c('','normal steel','normal steel','advanced steel','manuf','manuf',    '','programming','programming','other ict','ict','ict'),
  n = c(45,10,15,20,25,20,   61,6,30,25,36,25)
)

plot_ly(threelayer, labels = ~labels, parents = ~parents, values =~n, type = 'sunburst', branchvalues = 'total')

#OK! Think I've got that.
#The next question becomes - how to recreate that structure from a straightfoward count dataframe?
#Note, count can be weighted by e.g. employees, so we can get the right numbers from that (I think)
#("computes sum(wt) for each group")




#So let's look at the values we get, to try and work out why the plotme function is failing
#Shortened column list but all data
#Include an employee count filter per firm to test, it should now change root layer sizes
fame.s <- fame.geo %>% 
  st_set_geometry(NULL) %>% 
  select(employees,contains('_NAME')) %>% 
  filter(!is.na(employees),!is.na(SIC_SECTION_NAME)) %>% 
  filter(employees > 0) %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)})#needed cos formating breaks sunburst plot



#Then need to work out rest
fame.count <- fame.s %>% 
  count(SIC_5DIGIT_NAME,SIC_2DIGIT_NAME,SIC_SECTION_NAME, wt = employees)

#Now -
#Each unique SIC text value here needs its own unique entry in the 'labels' column
#The parent-child relationships are then entirely present in these columns
#Just need to work out how to format them into those two labels and parents columns
#And sum any values correctly

#Let's do it bit by bit for this example before generalising
#This is all the labels
#Do section name first - those are all the roots and have no parents, so we can just repeat a blank or NA for those in the parent vector
#Do each sublayer in order in sequence next
#So the second lot of vector values will be 2 digit SIC names, and we can match with SECTION parents, etc...
#Order alphabetically so we can guarantee the same order when counting
labels <- c (
  unique(fame.count$SIC_SECTION_NAME)[order(unique(fame.count$SIC_SECTION_NAME))],
  unique(fame.count$SIC_2DIGIT_NAME)[order(unique(fame.count$SIC_2DIGIT_NAME))],
  unique(fame.count$SIC_5DIGIT_NAME[order(unique(fame.count$SIC_5DIGIT_NAME))])
)

#Make each parent list in chunks
#Sections won't have parents, they're roots, so leave blank
roots = rep('',length(unique(fame.count$SIC_SECTION_NAME)))

#Section values will be summed employee values (ordered correctly)
rootsvalues = fame.count %>% group_by(SIC_SECTION_NAME) %>% summarise(n = sum(n)) %>% arrange(SIC_SECTION_NAME) %>% pull(n)



#2 digits' parents will be all be matching SIC section names
#Get distinct, pull matching SIC sections, use those
twodigit_parentnames <- fame.count %>% distinct(SIC_2DIGIT_NAME, .keep_all = T) %>% arrange(SIC_2DIGIT_NAME) %>% pull(SIC_SECTION_NAME)

#get their employee values too
twodigitvalues = fame.count %>% group_by(SIC_2DIGIT_NAME) %>% summarise(n = sum(n)) %>% arrange(SIC_2DIGIT_NAME) %>% pull(n)

#Check that looks right... tick
data.frame(
  twodigit = unique(fame.count$SIC_2DIGIT_NAME)[order(unique(fame.count$SIC_2DIGIT_NAME))],
  twodigit_parentnames,
  twodigitvalues
) %>% View


#then the outer branch of the hierarchy for 5 digit
#Find their 2 digit parents in the same way
fivedigit_parentnames <- fame.count %>% distinct(SIC_5DIGIT_NAME, .keep_all = T) %>% arrange(SIC_5DIGIT_NAME) %>% pull(SIC_2DIGIT_NAME)

#And values we already have, just get right order
fivedigitvalues <- fame.count %>% arrange(SIC_5DIGIT_NAME) %>% pull(n)

#Check... tick!
data.frame(
  fivedigit = unique(fame.count$SIC_5DIGIT_NAME)[order(unique(fame.count$SIC_5DIGIT_NAME))],
  fivedigit_parentnames,
  fivedigitvalues
) %>% View



#All pieces ready, assemble into parent and values
parents = c(roots,twodigit_parentnames,fivedigit_parentnames)

values = c(rootsvalues,twodigitvalues,fivedigitvalues)



#In theory, could put that directly in now? YEP!
plot_ly(labels = labels, parents = parents, values = values, type = 'sunburst', branchvalues = 'total')







#Cumulative plot for key sectors... gini plot should do it no?
#Arrange data by firm size (employees) in descending order
firm_data <- fame.geo %>%
  st_set_geometry(NULL) %>% 
  filter(SIC_SECTION_NAME == 'Manufacturing',!is.na(employees)) %>% 
  arrange(desc(employees)) %>% 
  select(employees)

#Calculate cumulative employee count
firm_data <- firm_data %>%
  mutate(
    cumulative_employees = cumsum(employees),
    cumulative_employees_percent = (cumulative_employees / sum(employees)) * 100
    )

#Create the cumulative function plot
ggplot(firm_data, aes(x = employees, y = cumulative_employees_percent)) +
  geom_step(direction = "hv") + # step plot to represent cumulative function
  labs(
    title = "Cumulative Employee Count by Firm Size",
    x = "Firm Size (Number of Employees)",
    y = "Cumulative Employee Count"
  )


#Repeat for all sections
firm_data <- fame.geo %>%
  st_set_geometry(NULL) %>% 
  select(SIC_SECTION_NAME,employees) %>% 
  filter(!is.na(employees)) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  arrange(desc(employees)) %>% 
  mutate(
    cumulative_employees = cumsum(employees),
    cumulative_employees_percent = (cumulative_employees / sum(employees)) * 100
  )

#Keep largest sections
#One-step approach combining everything
top_sectors <- firm_data %>%
  group_by(SIC_SECTION_NAME) %>%
  summarise(total_employees = sum(employees, na.rm = TRUE)) %>%
  arrange(desc(total_employees)) %>%
  slice_head(n = 8) %>% 
  pull(SIC_SECTION_NAME)
  
  #Way to get df in one pipeline
  # slice_head(n = 5) %>%
  # inner_join(firm_data, by = "SIC_SECTION_NAME") # Keep only firms in the top 5 sectors


# p <- ggplot(
ggplot(
  firm_data %>% filter(SIC_SECTION_NAME %in% top_sectors), 
  aes(x = employees, y = cumulative_employees_percent, colour = SIC_SECTION_NAME)
  ) +
  geom_step(direction = "hv") + # step plot to represent cumulative function
  # geom_point(shape = 18, size = 1) + # step plot to represent cumulative function
  labs(
    title = "Cumulative Employee Percent by Firm Size",
    x = "Firm Size (to the right, firms with x number of employees or higher)",
    y = "Cumulative Employee Percent in Sector"
  ) +
  scale_colour_brewer(palette = 'Paired') +
  # coord_cartesian(xlim = c(0,1000)) +
  coord_flip(xlim = c(0,1000))
  # coord_flip()
  # scale_x_log10()
  
plotly(p, tooltip = c('SIC_SECTION_NAME','cumulative_employees_percent'))


#Label by 1 to 10, 11 to 100 etc
firm_data <- firm_data %>% 
  mutate(
    firmsizegroup = case_when(
      employees > 0 & employees < 10 ~ '1 to 9',
      employees > 9 & employees < 100 ~ '10 to 99',
      employees > 99 & employees < 1000 ~ '100 to 999',
      employees > 999 & employees < 10000 ~ '1000 to 9999',
    )
  )

#BLEURGH!!!
ggplot(
  firm_data %>% filter(SIC_SECTION_NAME %in% top_sectors), 
  aes(x = employees, y = cumulative_employees_percent, colour = SIC_SECTION_NAME)
) +
  geom_step(direction = "hv") + # step plot to represent cumulative function
  # geom_point(shape = 18, size = 1) + # step plot to represent cumulative function
  labs(
    title = "Cumulative Employee Percent by Firm Size",
    x = "Firm Size (to the right, firms with x number of employees or higher)",
    y = "Cumulative Employee Percent in Sector"
  ) +
  scale_colour_brewer(palette = 'Paired') +
  # coord_cartesian(xlim = c(0,1000)) +
  # coord_flip(xlim = c(0,1000))
  coord_flip() +
  facet_wrap(~firmsizegroup, scales = 'free_y', ncol = 1)

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





