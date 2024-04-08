#Always starts with library function of different packages
library("rerddap")
library("tidyverse")
library("tidync")
library("doParallel")
library(lubridate)
library(patchwork)
library(viridis)
library(here)
library(kableExtra)

####This tells R to use NOAA designated website and what data set to use#####
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

#####This pulls data#######
#####SCI######
sci_temp_data<-griddap(datasetx = "ncdcOisst21Agg_LonPM180", 
                   url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                   time = c("2020-01-01", "2022-12-31"), 
                   zlev = c(0,0), # this is the depth, it wants a range
                   latitude =  c(34.1,34.1),
                   longitude = c(-119.875,-119.875),
                   fields = "sst")$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  select(longitude, latitude, time, sst) %>% 
  na.omit()%>%
  mutate(month = month(time), # extract the month
         year = year(time),
         site = "Santa Cruz Island",
         season  = case_when( # make a season
           month %in% c(12,1,2) ~ "winter",
           month %in% c(3,4,5) ~ "spring",
           month %in% c(6,7,8) ~ "summer",
           month %in% c(9,10,11) ~ "fall"
         ),
         season = factor(season),
         season = fct_relevel(season, c("winter","spring","summer","fall")))


# make a simple line plot#######
ggplot(sci_temp_data, aes(x = time, y = sst))+
  geom_line()+
  geom_point(aes(color = season))

###make a box plot#####
s <- sci_temp_data %>%
  ggplot(aes(x = as.factor(year), y = sst, fill = season))+
  geom_boxplot()+
  labs(title = "Santa Cruz Island",
       fill = "Season",
       x = "Year",
       y = "Sea Surface Temperature (C)")+
  theme(plot.title = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))+
  scale_color_viridis_d(option = "D")

s

#######summarize data#####
sci_temp_data %>%
  group_by(year,season)%>% # group by year and season
  summarise(temp.mean = mean(sst, na.rm = TRUE), # average of sst
            temp.max = max(sst, na.rm = TRUE), # max sst
            temp.min = min(ssr, na.rm = TRUE)) %>% # min sst
  kbl(caption = "Temperature data at SCI")%>% #make a table
  kable_styling(full_width = F, font_size = 9)



##########################
########CNM##############
cnm_temp_data<-griddap(datasetx = "ncdcOisst21Agg_LonPM180", ##need to change name each time
                   url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                   time = c("2020-01-01", "2022-12-31"), 
                   zlev = c(0,0), # this is the depth, it wants a range
                   latitude =  c(32.67,32.67), #lat you need
                   longitude = c(-117.24,-117.24), #long you need
                   fields = "sst")$data %>% #what to look for in data
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% #setting date and removing T00:00:00z
  select(longitude, latitude, time, sst) %>% 
  na.omit()%>%
  mutate(month = month(time), # extract the month
         year = year(time), #extract the year
         site = "Cabrillo National Monument",
         season  = case_when( # make a season
           month %in% c(12,1,2) ~ "winter",
           month %in% c(3,4,5) ~ "spring",
           month %in% c(6,7,8) ~ "summer",
           month %in% c(9,10,11) ~ "fall"
         ),
         season = factor(season),
         season = fct_relevel(season, c("winter","spring","summer","fall")))

####line graph####
ggplot(cnm_temp_data, aes(x = time, y = sst))+
  geom_line()+
  geom_point(aes(color = season))

####boxplot#####
c <- cnm_temp_data %>%
  ggplot(aes(x = as.factor(year), y = sst, fill = season))+
  geom_boxplot()+
  labs(title = "Cabrillo National Monument",
       fill = "Season",
       x = "Year",
       y = "Sea Surface Temperature (C)")+
  theme(plot.title = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

c

cnm_temp_data %>%
  group_by(year,season)%>%
  summarise(temp.mean = mean(sst, na.rm = TRUE),
            temp.max = max(sst, na.rm = TRUE))

cnm_temp_data %>%
  group_by(year,season)%>% # group by year and season
  summarise(temp.mean = mean(sst, na.rm = TRUE), # average of sst
            temp.max = max(sst, na.rm = TRUE)) %>% # max sst
  kbl(caption = "Temperature data at CNM")%>% #make a table
  kable_styling(full_width = F, font_size = 9) #use this to change the font

####both boxplots####
s/c+
  plot_layout(guides = 'collect')+
  plot_annotation(title = 'Average Sea Surface Temperature per Season 2020-2022',
                  theme = theme(plot.title = element_text(size = 9)))
  ggsave(here("Output", "SST_Plot.jpg"))
