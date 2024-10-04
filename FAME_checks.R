#FAME checks
library(tidyverse)
library(sf)
library(tmap)
library(lubridate)
library(pryr)
library(plotly)

#Getting a bunch of column fields to see what differences are

chk <- read_csv('localdata/FAME_fieldtest.csv')

