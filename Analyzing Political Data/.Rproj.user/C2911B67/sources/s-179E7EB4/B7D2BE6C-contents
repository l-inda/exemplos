# wrangle presidential eleciton data
library(dplyr)
#install.packages("politicaldata")
library(politicaldata)

pres <- politicaldata::pres_results

pres <- pres %>% 
  filter(year%in% 2008:2016)

pres$state_name = tolower(state.name[match(pres$state,state.abb)]) # we need the full name of the state to join with the map

# get map data
library(maps)
library(sf)
pres_map <- maps::map("state", plot = FALSE, fill = TRUE) # acquirig the data frame

pres_map <- sf::st_as_sf(pres_map) # changing the lat-long format to a simple feature object

names(pres_map) <- c("geometry","state_name")

pres_map <- pres_map %>% filter(state_name != 'district of columbia')

# combine with election data
pres_map <- pres_map %>% 
  left_join(pres,by = 'state_name') # joining the map with the political data

# plotting with ggplot2
library(ggplot2)
gg <- ggplot(pres_map,
             aes(fill=dem-rep>0),
             col='black') +
  geom_sf(aes(alpha=abs(dem-rep))) +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100",ndiscr = 0) + # change the projection from mercator (blegh)
  scale_fill_manual(values=c("TRUE"="blue","FALSE"="red")) +
  scale_alpha(range=c(0.1,1)) +
  facet_wrap(~year) +
  theme_void() +
  theme(legend.position='none')

ggsave(plot = gg,filename =paste0(getwd(),'/data/figures/us_election_2008_2012_2016.png'),width=12,height=3,units='in')

# get rid of 2012, compute change, then filter to 2016
pres_map_change <- pres_map %>% 
  filter(year!=2012) %>%
  group_by(state) %>%
  mutate(dem_gain = dem-rep - (lag(dem)-lag(rep))) %>%
  filter(year==2016)

# plotting with ggplot2
library(ggplot2)
gg <- ggplot(pres_map_change,
             aes(fill=dem_gain*100),
             col='black') +
  geom_sf() +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100",ndiscr = 0) + # change the projection from mercator (blegh)
  scale_fill_gradient2("Change in Dem. Margin",
                       high='blue',low='red',mid='white',midpoint=0) +
  theme_void() +
  guides('alpha'='none') +
  theme(legend.position='bottom')

ggsave(plot = gg,filename =paste0(getwd(),'/data/figures/us_election_swing_2008_2016.png'), width=6,height=4,units='in')

# import census estiamtes from the American community survey
state_hispanic <- read.csv(paste0(getwd(),"/data/acs_hispanic_pct.csv"))

state_hispanic$state_name <- tolower(state_hispanic$state_name)

library(knitr)
# take a look with the kable() function for making nice rmarkdown tables
kable(state_hispanic[1:5,], caption = 'Percent Hispanic/Latino, by state.') 

# combine with presidential election results and get rid of mapping data
pres_hispanic <- pres_map_change %>% 
  left_join(state_hispanic,by='state_name')

# plotting with ggplot2
library(ggplot2)
gg <- ggplot(pres_hispanic,
             aes(fill=hispanic.pct*100),
             col='black') +
  geom_sf() +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100",ndiscr = 0) + # change the projection from mercator (blegh)
  scale_fill_gradient("Percent of state population that is\nHispanic and/or Latino",
                      high='orange',low='gray80') +
  theme_void()  +
  theme(legend.position='bottom')

ggsave(plot = gg,filename = paste0(getwd(),'/data/figures/hispanic_share_pop.png'), width=6,height=4,units='in')

# plot a scatter plot with a GAM smooth, y ~ s(x)
gg <- ggplot(pres_hispanic,aes(x=hispanic.pct,y=dem_gain*100)) +
  geom_hline(yintercept=0,linetype=2) +
  geom_point(aes(col=dem_gain),size=2) +
  geom_smooth(method='gam',formula = y~s(x),col='gray40',alpha=0.3) +
  scale_x_continuous(labels=scales::percent_format(2)) +
  scale_color_gradient2("Change in Dem. Margin",
                        high='blue',low='red',mid='gray90',midpoint=0) +
  theme_minimal() +
  labs(title="Whiter States Have Trended Toward Republicans",
       x='Percent of Population That is Hispanic/Latino',
       y='Change in Dem. Margin 2008 to 2016') +
  theme(legend.position = 'none')

ggsave(plot = gg,filename =paste0(getwd(), '/data/figures/hispanics_dem_diff_2008_2016.png'), width=6,height=5,units='in')

# import tidycensus and API key
#install.packages("tidycensus")
library(tidycensus)
library(tidyverse)

#First time:
#census_api_key("3c22b4128c886266b1bddb7578d0a1cc85b070e6",install = TRUE)
census_api_key("3c22b4128c886266b1bddb7578d0a1cc85b070e6")
readRenviron("~/.Renviron")

invisible(Sys.getenv("CENSUS_API_KEY"))

# make a table of race codes 
race_table <- data.frame(
  code = c('A', 'B', 'C', 'D', 'E',
           'F', 'G', 'H', 'I'),
  race= c('WHITE ALONE', 'BLACK OR AFRICAN AMERICAN ALONE',
          'AMERICAN INDIAN OR ALASKAN NATIVE ALONE',
          'ASIAN ALONE', 
          'NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE', 
          'SOME OTHER RACE ALONE', 'TWO OR MORE RACES',
          'WHITE ALONE, NOT HISPANIC OR LATINO',
          'HISPANC OR LATINO'),
  stringsAsFactors=FALSE)

# get the appropriate variables from the ACS
var_list <- tidycensus::load_variables(2017, "acs5")

search_vars <- var_list[grepl('C1500', var_list$name),]

# import census estiamtes from the American community survey
acs_data.raw <- tidycensus::get_acs(geography = 'state',
                                    variables = search_vars$name,
                                    summary_var = 'B15002_001',
                                    year = 2017,
                                    survey = 'acs5')

# add variable names and filter to the educational breakdowns
acs_data <- acs_data.raw %>%
  left_join(dplyr::rename(search_vars, variable = name)) %>% # attach variable names
  filter(!grepl('Total$|Female$|Male$', label)) %>% # get rid of the totals
  mutate(gender = ifelse(grepl('Male', label), 'Male', 'Female'),
         label = gsub('^Estimate.*!!', '', label),
         code = gsub('(C[0-9]+)([A-Z])(_[0-9]+.$)', 
                     '\\2', 
                     variable))
# add race codes
acs_data <- acs_data %>%
  left_join (race_table) %>%
  select(state_name=NAME, label, gender, race, estimate:summary_moe)

# compute the percentages for each level
state_white_college <-  acs_data %>% 
  filter(race == 'WHITE ALONE, NOT HISPANIC OR LATINO') %>%
  group_by(state_name,label) %>%
  summarize(estimate = sum(estimate), 
            summary_est = mean(summary_est)) %>%
  mutate(percent = estimate / summary_est) 

# now just get the col/non-col breakown
state_white_college <- state_white_college %>%
  mutate(college = case_when(label == "Bachelor's degree or higher" ~ "college",
                             TRUE ~ "non-college")) %>%
  group_by(state_name,college) %>%
  summarise(percent = sum(percent))

# get the state abbreviation
state_white_college$state <- state.abb[match(state_white_college$state_name,state.name)] 

# filter out puerto rico and dc
state_white_college <- state_white_college %>%
  filter(!is.na(state))

# pull just the non-college numbers
state_white_college <- state_white_college %>%
  filter(college == 'non-college')

# combine with presidential election results and get rid of mapping data
pres_college <- pres_map_change %>% 
  left_join(state_white_college,by='state')

# plotting with ggplot2
library(ggplot2)
gg <- ggplot(pres_college,
             aes(fill=percent*100),
             col='black') +
  geom_sf() +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100",ndiscr = 0) + # change the projection from mercator (blegh)
  scale_fill_gradient("Percent of state population that is\nnon-college educated White",
                      high='#138D75',low='gray80') +
  theme_void()  +
  theme(legend.position='bottom')

# save the plot
ggsave(plot = gg,filename = paste0(getwd(),'/data/figures/noncollege_white_share_pop.png'), width=6,height=3,units='in')

# plot a scatter plot with a GAM smooth, y ~ s(x)
gg <- ggplot(pres_college,aes(x=percent,y=dem_gain*100)) +
  geom_hline(yintercept=0,linetype=2) +
  geom_point(aes(col=dem_gain),size=2) +
  geom_smooth(method='gam',formula = y~s(x),col='gray40',alpha=0.3) +
  scale_x_continuous(labels=scales::percent_format(2)) +
  scale_color_gradient2("Change in Dem. Margin",
                        high='blue',low='red',mid='gray90',midpoint=0) +
  theme_minimal() +
  labs(title="Whiter States Have Trended Toward Republicans",
       x='Share of Population That is Non-college Educated Whites',
       y='Change in Dem. Margin 2008 to 2016') +
  theme(legend.position = 'none')

ggsave(plot = gg,filename = paste0(getwd(),'/data/figures/noncollege_whites_dem_diff_2008_2016.png'), width=6,height=5,units='in')

#install.packages("geofacet")
library(geofacet)
pres_history <- politicaldata::pres_results

gg <- ggplot(pres_history, aes(x=year,y=(dem-rep)*100)) +
  geom_hline(yintercept=0) +
  geom_line(aes(col=dem-rep>0,group=state)) +
  geom_area(fill='gray60') +
  facet_geo(~state) + # specify that you want to facet by state, and arrange in corresponding goegraphic order
  theme_minimal() +
  scale_color_manual(values = c("TRUE"='blue',"FALSE"='red')) +
  scale_x_continuous(breaks=c(1976,1996,2016)) +
  theme(legend.position='none') +
  labs(x="",y="Dem. Margin")

ggsave(plot = gg,filename = paste0(getwd(),'/data/figures/pres_history_ggfacet.png'), width=10,height=6,units='in')

