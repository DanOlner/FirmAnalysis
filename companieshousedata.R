#Companies house bulk download (~3gb)
library(tidyverse)
library(pryr)

ch <- read_csv('localdata/BasicCompanyDataAsOneFile-2024-10-01.csv')

#3.8gb in memory
object_size(ch)

#Does every business have postcodes? Not quite
table(!is.na(ch$RegAddress.PostCode))

#Check on some that don't...
ch %>% filter(is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% View

#Check on those that do!
ch %>% filter(!is.na(RegAddress.PostCode)) %>% slice_sample(n = 100) %>% View

