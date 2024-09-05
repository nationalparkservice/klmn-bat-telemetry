
#  Converting lotek receiver data to analysis-ready format


########## IMPORTANT
# RSSI > dB eq from LOTEK
# [dBm]= 0.37 * RSSI number -145 # WITH 2-3dB error
0.37 * DYER_c$max_signal -145
# that seems to be working so I guess just normal order of operations

#  YOUR WD IS DIFFERENT FROM MINE - check this file's path ####
getwd()
setwd("N:/Working/Niziolek/Rprojs/Bats/")
#####

#  RUN THIS FIRST - clears the environment of any yesterday's files####
# rm(list=grep("_Ant",ls(),value=TRUE,invert=TRUE)) #value = T returns the elements, =F returns a list of the elements. invert = T deletes everything NOT matching the search string
rm(list=grep("_Ant",ls(),value=T)) # remove any Ant dataframes
rm(list=grep("_c",ls(),value=T)) # remove converted DFs
print0("these 'object ____ not found' errors are good - ignore them")
#####


#  Variables - USER DEFINES, but generally static  ####
# IMPORTANT: Names and RTEUnames MAY NEED CHANGED if the column headers from Lotek change

# beginning the process of moving variables to be altered between years up here.
# most of this will not be used presently, is for later improvements I want to make
#n_towers = 5    # number of towers on landscape
#n_ants = 3      # antennae per tower
TagNums <- c() # keep a list of tag numbers deployed in a season, detection record cleanup fn will exclude tags not on this list


#define col names for the raw LOTEK file (existing file imports with col names that cannot be called with $)
Names<-c("Index","Date","Time","uSec","TagID","Freq","Codeset","Antenna","Gain","RSSI") #should stay the same, unless settings are changed
# Names do not need changed unless LOTEK download looks different from 2023
rteuNames<-c("timestamp","duration","signal_freq","Name","receiver","max_signal","signal_bw",
             "freq_tag","Date","Time","TagID") #double check that these match your STAT_c headers...

#  MUST set - save file identifier  ####
dwldDATE <- as.character("WinterRoostMtgEx1")
# date of the files to be downloaded (can set e.g. 09232023_09282023 if a file spans more than one day, but all files processed should be for the same time period until/unless I fix the by-antenna save chunk)
# this identifier is never called in another script (as of 15.10.2023)
#####



# Session setup, checking ####
# packages
require(tidyverse)
require(stringr)
require(here)

dev.off() # likely not needed, but just in case

list.files() #to list only the files in wd
#list.files(recursive=T) #to list all files in wd and all subfolders




#####


# Import LOTEK data (unformatted) #####
LOTEKdfs<-dir('todaysLOTEK') #list files to pull in
LOTEKdat<-vector("list",length=length(LOTEKdfs)) # create shell list to populate

# this will still print an error about column name length and number of columns; best I can figure it doesn't like all the blank cells but does import fine
for(i in seq_along(LOTEKdfs)){
  LOTEKdat[[i]] <- read.csv(paste0('todaysLOTEK/',LOTEKdfs[i],sep=""),
                            col.names = Names)
}
print0("I think you can ignore that error - metadata rows make R unhappy")

names(LOTEKdat)<-stringr::str_extract(LOTEKdfs, "[:upper:]{4}") #all tower names are [A-Z]{4} - but just this year, or always?
list2env(LOTEKdat,globalenv()) #instead of this, can we use LOTEKdat in LOTEKtoRalph?



#####

#  Big conversion, LOTEK to Ralph format  ####

#
# # ADD TO THE PIPE which(TagID==any_of(TagNums))


LOTEKtoRalph <- function(Tower){
  name <- deparse(substitute(Tower))
  Tower[c(which(Tower[,'RSSI'] != "")),]%>%
    .[c(2:nrow(.)),]%>%
    dplyr::rename(signal_freq=Freq)%>%
    mutate(
           max_signal=as.numeric(RSSI),
           signal_bw = rep(48),
           freq_tag = as.numeric(TagID),
           duration = rep(1),
           timestamp = paste(Date,Time,sep=" "),
           timestamp = as.numeric(as.POSIXlt(timestamp,
                                             format="%m/%d/%Y %H:%M:%OS")),
           Name = rep(name,nrow(.)),
           receiver=recode(as.factor(Antenna),"A1+A2+A3+A4"="all",
                           "Antenna 1"="A1","Antenna 2"="A2","Antenna 3"="A3")
    )%>%
    select("Name","receiver","TagID","Date","Time","duration","signal_freq","max_signal",
           "signal_bw","freq_tag","timestamp")
}



SWIN_c <- LOTEKtoRalph(SWIN)
#nice
# apply to all towers
test <- lapply(LOTEKdat,LOTEKtoRalph)
# takes about 60seconds, works, but Name is x[[i]] now because of lapply...

# regular apply doesn't work on lists (needs 2 dims),
test1 <- sapply(LOTEKdat,LOTEKtoRalph)
# wow that is NOT it.



#####


#  Mass export, one file per tower #####
dfsLIST<-mget(ls(pattern="_c")) #this must happen before the following line or the list of filenames is added as a DF.
LOTEKdfs_T<-paste("rteu_",LOTEKdfs,sep="") #rteu is a placeholder identifier indicating it's formatted - can change to anything else. Alice? Allison?

#
for(i in 1:length(dfsLIST)){ # 1:length() works, seq_along() does not - seq only for lists?
  write.csv(dfsLIST[[i]],
            file=paste0('data/logger/',gsub("-","",Sys.Date()),"_rteu_",LOTEKdfs[i],sep=""),
            row.names = F)
} # save files as 20231208_rteu_TOWR_Final_LOG_MD###...(lotek serial info).csv - matches them to their pre-R state but clearly defines + facilitates sorting in dir





#####

# Prep to export to the LOTEK file system #####
#rm(list = ls()[grep("169", ls())])
BreakoutDFs<-function(DF){
  A1 <- DF[grep("Ant1",DF$receiver),] # with the comma, error: incorrect number of subscripts
  A2 <- DF[grep("Ant2",DF$receiver),] # without, error: subscript out of bounds
  A3 <- DF[grep("Ant3",DF$receiver),]
  assign(x=paste0(DF$Name[1],"_Ant1"),value = A1,envir = .GlobalEnv)
  assign(x=paste0(DF$Name[1],"_Ant2"),value = A2,envir = .GlobalEnv)
  assign(x=paste0(DF$Name[1],"_Ant3"),value = A3,envir = .GlobalEnv)
}
# this works: BreakoutDFs(DYER_c)
# lapply(dfsLIST,BreakoutDFs)# BUT it doesn't work with atomic vectors, with which I have limited familiarity. Here again, a quick and dirty solution that I will improve on later
# # is this an lapply issue, where it sarah's note on when to use lapply/sapply/apply
# sapply(dfsLIST,BreakoutDFs) # nope, same error
# #towersLIST<-c("DYER_c","KELL_c","PAUL_c","SKIH_c","SWIN_c") #not sure if we need this?
#last-ditch try?
# #for(i in seq_along(dfsLIST)){BreakoutDFs(i)} # damnit...
# fine.
#####
#BreakoutDFs(DYER_c)
BreakoutDFs(KELL_c)
#BreakoutDFs(PAUL_c)
BreakoutDFs(SKIH_c)
BreakoutDFs(SWIN_c)
#####

#  CHANGE dfsLIST to TwrLIST


# Mass export into the LOTEK file system, one file per antenna #####
antsLIST<-mget(ls(pattern="_Ant"))
for(i in 1:length(antsLIST)){
  write.csv(antsLIST[[i]],file=paste0('data/logger/',substr(names(antsLIST[i]),1,4),'/',
                                    names(antsLIST[i]),'/', # this SHOULD be the full path data/logger/DYER/DYER_Ant1
                                    dwldDATE,names(antsLIST[i]),'.csv',sep=""),# now we're naming. Need... date,
            row.names = F)
}

#####

message("Now, open app.R in the root folder and run the full script")



##  Splitting the data into different datasets according to its section in the lotek dwld  #####

test <- split(PAUL,PAUL[c(PAUL$Index %like% "Index"),],drop=F)
rownames(PAUL[c(PAUL$Index %like% "Index"),])

test<- PAUL[c(paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[4]):paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[5]))-2,]

test1<- PAUL[c(paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[1]):paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[2]))-2,]
test2<- PAUL[c(paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[4]):paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[5])),]

paste0(rownames(PAUL[c(PAUL$Index %like% "Index"),])[4])

#####



##############################
#         S C R A P          #
##############################

#  working on rteu tracking, the package not the gui
towers<-read_excel("data/Antennas.xlsx")

# # merge into single file, I guess
# dfsLIST<-mget(ls(pattern="_c")) # careful with this search string, not an entirely unique ID
# t<-rbindlist(dfsLIST)
# test<-time_match_signals(t)
# test2<-doa(test,towers) #doesn't like doing it on ALL towers...
# how about one tower all antennae?
skih_15<-time_match_signals(SKIH_c,station_time_error=15)
skih.doa15<-doa(skih_15,towers)
# niccccceeeee

write.csv(skih.doa,"SKIH_DOA_estimates_10162023.csv")





