### Texas's 21st Congressional District
# ie Wendy Davis vs Chip Roy
# Date: 5/3/2020
# Topic: Analysing Chip Roy's and Wendy Davis's Chances This Fall
# Author: Alex Caple, and as always StackOverflow

### NEEDS MAJOR CLEANING

# This File Assumes that the "20190811.Texas.Congressional.District.r" Data & Packages are already loaded.
# source(here("2020_Races", "20190811.Texas.Congressional.Districts.r"))


#  Data -------------------------------------------------------------------
#  * Shapefiles -----------------------------------------------------------
# * * District 21 Shapes---------------------------------------------------
# Single Shape of D21
sf.d21 <- sf.tex.congress %>% filter(District == 21)

# Change CRS To match Precincts
sf.tex.congress <- st_transform(sf.tex.congress, crs=st_crs(sf.tex.p18))

# Pull Out D21 Precints; should ask on Twitter if these are Correct
sf.d21.p <- st_intersection(sf.tex.p18,
                            sf.tex.congress %>%
                              filter(District == 21))

# Pull out D21 Counties
# Change CRS to match counties - shouldn't they just all match now? This is wrong, fix.
sf.tex.congress <- st_transform(sf.tex.congress, crs=st_crs(sf.tex.counties))

# Highlights Counties of Texas That D21 Touches
sf.d21.state <- sf.tex.counties %>%
  mutate(Dist.21 = ifelse(CNTY_NM %in% c(st_intersection(sf.tex.counties,
                                                         sf.tex.congress %>%
                                                           filter(District == 21))$CNTY_NM), 1, 0))

# BBOX Creation to Highlight Area - Consider if too large? 
sf.d21.c.bbox <- st_as_sfc(st_bbox(sf.d21.state %>% filter(Dist.21 == 1)))


#subset state to D21 Counties in box
sf.d21.c <- st_intersection(sf.d21.state,
                            sf.d21.c.bbox)

# Test Plot to View all Current Pulled Shapes
ggplot() + geom_sf(data=sf.d21.state,
                   fill= ifelse(sf.d21.state$Dist.21 == 1, "light grey", "white"),
                   color="grey10")+
  geom_sf(data=sf.d21.c.bbox,
          fill=NA,
          color="grey10",
          size=1.3) +
 geom_sf(data=sf.d21,
          fill=NA,
          color="black",
         size=2) +
 geom_sf(data=sf.d21.p,
          fill=NA,
          color="pink")


#  * Voting Data for D21 --------------------------------------------------

### Open Elections Data Texas Precint Data - how do we cite and source? Look up!
git.url <- "https://github.com/openelections/openelections-data-tx/raw/master/2016/20161108__tx__general__precinct.csv"
temp <- tempfile()
download.file(git.url, temp)
tex.p16.full <- read.csv(temp, sep=",")
head(tex.p16.full)
unlink(temp)



##### HERE ########
### NEED TO GO BACK THROUGH THE 2016 PRECINCTS AND YOU'VE CAUGHT UP - 1 HOUR WORK LOST FOR NOT SAVING, THANK GOD FOR HISTORY
### YEP - NEED TO GO THROUGH AND RECHECK CONNECTION - 453 total precints, are they all here?

table(sf.d21.p$COUNTY)
### DATA - SUBSET D21 DATA
cname = "UVALDE"
tex.p16.full %>% filter(county == cname) %>% group_by(precinct) %>% summarize(n()) %>% as.data.frame()
tex.p16.full %>% filter(district == 21)
sf.tex.p18 %>% filter(COUNTY == toupper(cname)) %>% group_by(PCTKEY) %>% summarize(n()) %>% as.data.frame()
tex.p16.full %>% filter(county == cname) %>% View()

# FINEs still need to be match checked
# all codes are 4 digits

# BANDERA - fine
# BEXAR - fine
# BLANCO - fine
# BURNET - need to combine the areas here
# COMAL  - fine
# EDWARDS - fine
# GILLESPIE - check 14? - may need to check this seperatly
# HAYS - fine
# KENDALL - fine
# KERR - fine
# KIMBLE - fine
# LLANO - check the city thing
# MASON - check the early thing
# MEDINA - check utopia
# REAL - 4 & 7 combined? Check on that
# TRAVIS - check A B part- I think its likely too stations same area ut check
# UVALDE -  Fine - wait, Null value ... ? check

#### wORK SPACE #####

ggplot() + geom_sf(data=sf.d21.p,
          fill=NA,
          color="purple")

# shitty merge process clean and change please

tex.p16.full <- tex.p16.full %>% mutate(county = toupper(county))

test <- sf.tex.p18 %>% 
  select(COUNTY, CNTY) %>% 
  distinct(COUNTY, .keep_all = TRUE) %>% 
  as.data.frame()
test$geometry <- NULL
test <- test %>% distinct(COUNTY, .keep_all = TRUE)

# This brings the County key into the Precinct voting file
tex.p16.full <- merge(tex.p16.full,
                      test,
                      by.x="county",
                      by.y="COUNTY",
                      all.x=TRUE)

### Must be numeric will need to clean county key 
t2 <- tex.p16.full %>% mutate(Pkey = as.numeric(gsub("[^0-9.-]", "", precinct)),
                        PT = formatC(Pkey, width=4, format ="d", flag =0))

t2$PKEYT <- paste0(t2$CNTY, t2$PT)

# Ugh its LONG FORM PREC VOTING DATA
sf.d21.data.p <- merge(sf.d21.p,
                       t2 %>% 
                         filter(office == "U.S. House") %>% 
                         select(PKEYT, votes),
                       by.x="PCTKEY",
                       by.y="PKEYT",
                       all.x=TRUE)

sf.d21.data.p <- sf.d21.data.p %>% 
  mutate(votes = as.numeric(votes)) %>% 
  replace(., is.na(.), 0)

ggplot() + geom_sf(data=sf.d21.data.p,
                   aes(fill=votes),
                   color="black")

sf.d21.data.p %>% 
  mutate(Check = ifelse(is.na(votes), 1, 0)) %>% 
  group_by(Check) %>% 
  summarize(n())

# ok its bad - needs a lot of cleaning! but we did it! 


