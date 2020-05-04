### 2020 Texas Congressional District Review
# Date: 8/11/2019
# Topic: Not Sure Yet
# Author: Alex Caple

# Library Load! -----------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)

# Mapping
library(ggplot2)
library(ggmap)
library(ggrepel)
library(sf)
library(reshape2)

# Graphic Improvement
library(Cairo)
library(extrafont)
font_import(pattern = "calibri", prompt = FALSE)

# Dealing with lists from websites
library(purrr)
library(repurrrsive)

# Scraping Library
library(xml2)
library(rvest)
library(magrittr) # grabs iframes
library(RCurl)

# Here -----------------------------------------------------------------
library(here)

# Notes and Websites ------------------------------------------------------

#  * Texas Redistricting? -------------------------------------------------
# This is where the shapefile data was taken from - we are still missing the precint data; we need to get that! 
# https://tlc.texas.gov/redist/districts/congress.html


# * Texas Tribune ---------------------------------------------------------
# Notes on Retiring Members of Congress
# tt.url <-  "https://www.texastribune.org/2019/08/06/republican-texas-congressional-retirements-energize-democrats-and-gop/"

### Retirements from Texas Tribune - Article Change is Odd
# https://www.texastribune.org/2019/08/06/republican-texas-congressional-retirements-energize-democrats-and-gop/"
# Graphic In the Article

# Source ------------------------------------------------------------------
# None at this time.

# Functions ---------------------------------------------------------------
# None at this time.

# Data Load ---------------------------------------------------------------

# * * Texas Tribune : Retirements -----------------------------------------

tt.retire.url <- "https://graphics.texastribune.org/graphics/congressional-retirements-2019-08/index_v3/"

tt.retire <- tt.retire.url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  extract(1) %>% 
  html_table(fill=TRUE) %>% 
  map_df(., extract, c(2, 3, 4, 5)) 

colnames(tt.retire) <- make.names(colnames(tt.retire), unique=TRUE)

tt.retire <- tt.retire %>% 
  rename(District = X) %>% 
  mutate(District.Numeric = gsub("Dist.", "", District),
         District.Numeric = as.numeric(gsub(",.*", "", District.Numeric)),
         Retirement = c("Yes"))

tt.retire

#  * * Wikipedia : Texas US House Delegation ------------------------------
wiki.url <- "https://en.wikipedia.org/wiki/United_States_congressional_delegations_from_Texas"

tx.congress.members <- wiki.url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  extract(1) %>% 
  html_table(fill=TRUE) %>% 
  map_df(., extract, c(1, 2, 3, 4, 5)) %>% 
  set_colnames(., .[1, ]) %>%    # . = current tibble, .[.] Means first row! 
  .[-1, ] # Drop first row - I don't like this but can't find anything else thats different
  
# learn to clean better! 
# ROW NAMES BROKEN, BUT MOVE ALONG - DONT BE PERFECT

tx.congress.members <- tx.congress.members %>% 
  mutate(District.Numeric = as.numeric(gsub("[A-z]", "", District)),
         Member = gsub("\\([^()]*\\)", "", `Member(Residence)`),
         Residence = gsub(".*\\(", "", `Member(Residence)`),
            Residence = gsub("\\)", "", Residence),
         First.Elected = as.numeric(gsub(".*\\s+", "", Incumbency)),
         Years.in.Office = as.numeric(gsub("-.*", "", Sys.Date())) - First.Elected,
         CPVI.R.Pos = as.numeric(gsub(".*\\+", "", CPVI)),
            CPVI.R.Pos = ifelse(substr(CPVI, 1, 1) == "R", CPVI.R.Pos, CPVI.R.Pos*-1))

#  * * Precint ShapeFile? -------------------------------------------------

### NEED TO FIND PRECINT DATA #########

#  * * Shapefiles ---------------------------------------------------------
# Texas Counties
sf.tex.counties <- read_sf(here("Shapefiles",
                                "Texas.Counties",
                                "Texas_County_Boundaries.shp"))

sf.tex.state <- sf.tex.counties %>% summarize(Total = sum(OBJECTID))

# 2018 Precincts for Texas - where are the 2016 ones?
# ftp://ftpgis1.tlc.state.tx.us/2011_Redistricting_Data/Precincts/Geography/

sf.tex.p18 <- read_sf(here("Shapefiles",
                           "Precincts.2018",
                           "Precincts.shp"))

tex.p18.data <- read_xlsx(here("Shapefiles",
                              "Precincts.2018",
                              "Precinct_Districts.xlsx"))

sf.tex.p18 <- merge(sf.tex.p18,
                    tex.p18.data,
                    by="PCTKEY",
                    all.x=TRUE)

# Texas 2013 Congressional Districts

sf.tex.congress <- read_sf(here("Shapefiles",
                                "Texas.Congress.2013",
                                "PLANC235.shp"))

sf.tex.congress <- st_transform(sf.tex.congress, crs=st_crs(sf.tex.p18))



# Data Merges -------------------------------------------------------------

tx.congress.members <- merge(tx.congress.members,
                             tt.retire %>% 
                               select(District.Numeric, Retirement),
                             by="District.Numeric",
                             all.x=TRUE)

tx.congress.members <- tx.congress.members %>% 
  mutate(Retirement = replace_na(Retirement, "No"))


sf.tex.congress <- merge(sf.tex.congress,
                         tx.congress.members,
                         by.x="District",
                         by.y="District.Numeric",
                         all.x=TRUE) 

ggplot() + geom_sf(data=sf.tex.congress,
                   aes(fill=Retirement),
                   color="grey")

# Analysis Questions ------------------------------------------------------


