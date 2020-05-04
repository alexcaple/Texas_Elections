### Function to Scrape Data from SOS Website - Currently in Loop
# Date: 5/3/2020
# Author: Alex Caple, and as always StackOverflow

# Library Load! -----------------------------------------------------------
# This File Assumes That the "20190811.Texas.Congressional.District.r" Data & Packages are already loaded.

# Here -----------------------------------------------------------------
library(here)

# Source ------------------------------------------------------------------
# source(here("2020_Races", "20190811.Texas.Congressional.Districts.r"))

# Functions ---------------------------------------------------------------
# Need to Build Into Function
# total.county <-data.frame()
# 
# for(i in 1:254){
#   sos.url <- paste0("https://elections.sos.state.tx.us/elchist233_county", i, ".htm")
#   
#   county.data <-  sos.url %>% 
#     read_html() %>% 
#     html_nodes("table") %>% 
#     extract(1) %>% 
#     html_table(fill=TRUE) %>% 
#     map_df(., extract, c(1, 2, 4, 5)) %>% 
#     set_colnames(., .[1, ])
# 
#     colnames(county.data)[1] <- c("Filter") 
#     colnames(county.data)[2] <- c("Candidate") 
#     colnames(county.data)[3] <- c("Votes") 
#     colnames(county.data)[4] <- c("P.Vote") 
# 
#     county.data[county.data==""]<-NA
# 
#     county.data <- county.data %>% 
#       fill(Filter, .direction="down")
# 
#     county.data <- county.data %>% 
#       filter(Filter == "President/Vice-President",
#          Candidate %in% c("Hillary Clinton", "Bernie Sanders")) %>% 
#       mutate(County = i)
#     
#     total.county <- rbind(total.county, county.data)
#     Sys.sleep(2)
# }

# Data --------------------------------------------------------------------

total.county <- read.csv(here("Data", "2016 Bernie Hillary County Results.csv"), sep=",")

#  * * Shapefiles ----------------------------------------
# Texas Counties
sf.tex.counties <- read_sf(here("Shapefiles",
                                "Texas.Counties",
                                "Texas_County_Boundaries.shp"))

tex.county.names <- st_drop_geometry(sf.tex.counties)
tex.county.names <- tex.county.names %>% 
  select(OBJECTID, CNTY_NM) %>% 
  arrange(CNTY_NM) %>% 
  mutate(Connect = 1:254)

tot.county <- total.county %>% 
  select(Candidate, County, Votes) %>% 
  group_by(Candidate) %>% 
  spread(Candidate, Votes) %>% 
  mutate(`Bernie Sanders` = as.numeric(gsub(",","",`Bernie Sanders`)),
         `Hillary Clinton` = as.numeric(gsub(",","",`Hillary Clinton`)),
         Total = `Bernie Sanders` + `Hillary Clinton`,
         P.HC.Total = `Hillary Clinton`/Total,
         P.Diff = .5 - P.HC.Total,
         P.Can = ifelse(P.Diff < 0.5, "Hillary Clinton", 
                        ifelse(P.Diff >0.5, "Bernie Sanders", "err")))


tex.county.names <- merge(tex.county.names,
                          tot.county,
                          by.x="Connect",
                          by.y="County",
                          all.x=TRUE)

sf.tex.counties <- merge(sf.tex.counties,
                         tex.county.names %>% 
                           select(OBJECTID, P.Diff),
                         by="OBJECTID",
                         all.x=TRUE)


# Map ---------------------------------------------------------------------

ggplot()+
  geom_sf(data=sf.tex.counties,
          aes(fill=P.Diff),
          color="grey10",
          size=.5) +
  scale_fill_gradient2()+
  theme(legend.position="none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.grid.major.y=element_blank(),
        panel.background = element_rect(fill ="transparent",colour = "white"), 
        text=element_text(family="Calibri", size=12)) + 
        coord_sf()
