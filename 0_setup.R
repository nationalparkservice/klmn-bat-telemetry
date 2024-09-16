
# User-defined variables ------------------------------------------------------

## settings (describe receiver settings, set variables/filenames, etc.) -------

CRTObin <- 15 # bin width (minutes)

# summary table interval
summary.interval<-"60 minutes"
# ****LOOK HERE! are you setting this value lower than CRTObin? CHECK. Summarizing across the receiver's count intervals may give a false representation of diurnal activity patterns
# Acceptable units:  ... 'seconds','minutes', 'hours', 'days', 'weeks', 'months', 'years', 'fortnights','quarters', 'semesters', 'decades'... (there are others, these are the reasonable ones)

# start date for summaries
start.date <- "2024-08-20"
# end date for summaries
end.date <- "2024-12-1"
# in case the master file has >1yr's data (as in 2024), can limit to current season here,
# or can produce summaries for other periods of interest e.g. only the current week

# save file name for big summary - MUST BE XLSX to preserve sheets
MySummaryFilename <- "BatDetections2024.xlsx" # can do things like add date automatically if preferred to manual naming, OR can do like master files and overwrite on output




## file configuration (less likely to change) -------------------------------

# column header to search for to identify data rows within the csv
srx1200_dataHeader1 <- "Index"
# list column names for the OLDER receiver, in the order they occur in in the raw file
SRX800_names<-c("date","time","channel","tag","ant","RSSI")
#  TODO (sarah?):  figure out a way to extract the names row from the dataset,
#                   and get rid of this messiness




# Load relational tables -------------------------------------------
tags <- readxl::read_excel("keys/Tagged Bats LABE 2024.xlsx")
receivers <- utils::read.csv("keys/Towers.csv")
down.log <- readxl::read_excel("keys/Tower offline log.xlsx")


# Load master files-------------------------------------------
masterfile_srx800 <- read.csv("masters/LBRC_D000946_master_2024.csv")
masterfile_srx1200_i <- read.csv("masters/SRX1200_individuals_master_2024.csv")
masterfile_srx1200_c <- read.csv("masters/SRX1200_crto_master_2024.csv")


# Load necessary packages -------------------------------------------------
require(tidyverse)  # TODO: replace this with individual tidyverse packages
require(stringr)
require(readr)
require(here)


# Load helper functions --------------------------------------------------------
source("under_the_hood/helper_functions.R")






