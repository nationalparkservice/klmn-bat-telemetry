
# User-defined variables -----------------------------------------------------
CRTObin <- "15" # bin width (minutes)

srx1200_dataHeader1 <- "Index" # column header to search for to identify data rows within the csv


# list column names for the OLDER receiver, in the order they occur in in the raw file
SRX800_names<-c("date","time","channel","tag","ant","RSSI")
#  TODO (sarah?):  figure out a way to extract the names row from the dataset,
#                   and get rid of this messiness


# Load relational tables -------------------------------------------
tags <- readxl::read_excel("keys/Tagged Bats LABE 2024.xlsx")
receivers <- utils::read.csv("keys/Towers.csv")


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






