# Libraries
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
rerddap::info(datasetid = "noaa_psl_675a_3cea_522e", url = "https://upwell.pfeg.noaa.gov/erddap")
