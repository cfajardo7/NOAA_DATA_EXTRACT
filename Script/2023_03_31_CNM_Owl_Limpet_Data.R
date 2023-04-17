#Always starts with library function of different packages
library("rerddap")
library("tidyverse")
library("tidync")
library("doParallel")
library(lubridate)
library(patchwork)
library(viridis)
library(here)
library(readr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

cnm_limp_dens <- read_csv(here("Data","2023_03_2023_CNM_Limpet_density_and_size_survey.csv"))

cnm_hist_size <- cnm_limp_dens %>%
  ggplot( aes
          (x=SL)) +
  geom_histogram( stat = "count", binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
cnm_hist_size

cnm_limp_dens %>%
  select(Limpet_no, SL) %>% 
  ggplot( aes(x=SL)) +
  geom_bar(fill="blue", alpha=0.8) +
  theme(plot.title = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8))+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  ggtitle("Owl Limpet Size Distribution at Cabrillo National Monument")+
  ggsave(here("Output", "OL_Size_Dist_2.jpg"))
