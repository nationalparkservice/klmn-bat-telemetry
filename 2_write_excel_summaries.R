

# data from the last script:
head(SRX1200crtodata)
head(SRX1200individualdata)
head(SRX800receiverdata)

# subsetting master files and formatting columns -------------------------------

## individual records of deployed tags only -------
# have to cope first with the weird column types separately - 800 file is fixed-width, too many spaces to interact with

Indivrecords_12 <- SRX1200individualdata%>%
  select(receiver,loc,Date,Time,TagID.BPM,Antenna,RSSI,Gain)%>%
  rename(TagID=TagID.BPM)%>%
  mutate(Gain=as.numeric(Gain),
         Date=as.Date(Date, format=c("%m/%d/%Y")),
         Time=hms::as_hms(Time),
         Antenna=case_match(Antenna, "A1+A2+A3+A4" ~ "All",
                            "Antenna 1" ~ "1",
                            "Antenna 2" ~ "2",
                            "Antenna 3" ~ "3")
         )%>%
  filter(TagID %in% tags$'MOTUS ID')%>%
  filter(Date > start.date & Date < end.date)

Indivrecords_8 <- SRX800receiverdata%>%
  select(receiver,loc,Date,Time,TagID,Antenna,RSSI)%>%
  mutate(Gain=rep(-1),
         Date=as.Date(Date, format=c("%m/%d/%y")),
         Time=hms::as_hms(Time),
         Antenna=case_match(Antenna, "A1+A2+A3+A4   " ~ "All",
                                    "          1   " ~ "1",
                                    "          2   " ~ "2",
                                    "          3   " ~ "3"),
         )%>%
  filter(TagID %in% tags$'MOTUS ID')%>%
  filter(Date > start.date & Date < end.date)


# loc and receiver as factors later once it's all combined
Indivrecords <- rbind(Indivrecords_8,Indivrecords_12)
rm(Indivrecords_8,Indivrecords_12)

## crto records of deployed tags only, within specified date range -------
CRTOrecords<-SRX1200crtodata%>%
  select(receiver,loc,Date,Time,TagID,Antenna,Counts,CRTObin,RSSI,Gain)%>%
  mutate(Gain=as.numeric(Gain),
         Date=as.Date(Date, format=c("%m/%d/%Y")),
         Time=hms::as_hms(Time),
         Antenna=as.factor(case_match(Antenna, "Antenna 1" ~ "1",
                            "Antenna 2" ~ "2",
                            "Antenna 3" ~ "3",
                            "Error" ~ "err")),
         receiver=as.factor(receiver),
         loc=as.factor(loc),
         CRTObin=as.numeric(CRTObin),
         RSSI=as.numeric(RSSI)
  )%>%
  filter(TagID %in% tags$'MOTUS ID')%>%
  filter(Date > start.date & Date < end.date)

head(CRTOrecords)




# timestamp spaghetti ------------
CRTOrecords$DateTime=as.POSIXct(as.character(paste(CRTOrecords$Date, CRTOrecords$Time)), format="%Y-%m-%d %H:%M:%S")
Indivrecords$DateTime=as.POSIXct(as.character(paste(Indivrecords$Date, Indivrecords$Time)), format="%Y-%m-%d %H:%M:%S")

timecuts.c <- timeplyr::time_expandv(CRTOrecords$DateTime, time_by = summary.interval)
CRTOrecords<-CRTOrecords%>%
  mutate(timebin = cut(DateTime,breaks=timecuts.c))

timecuts.i <- timeplyr::time_expandv(Indivrecords$DateTime, time_by = summary.interval)
Indivrecords<-Indivrecords%>%
  mutate(timebin = cut(DateTime,breaks=timecuts.i),
         Counts = rep(1))

# timebin <- as.data.frame(timecuts.i)
# timebin$timebin <- as.factor(timebin$timebin)
# names(timebin) <- c("timebin")
# Indivrecords <- merge(Indivrecords,as.data.frame(timebin),by="timebin",all=T)
# # TODO: revisit this chunk, too many steps we don't need
#'[9/15 I think the chunk above this one takes care of it, but leaving this here until it's been run a couple times without error in case...]


# we added counts column... can we just shove the crto records in here too?

CombinedRecords<-rbind(Indivrecords[c("receiver","loc","Date","Time","TagID","Antenna","RSSI","Gain","DateTime","timebin","Counts")],
                       CRTOrecords[c("receiver","loc","Date","Time","TagID","Antenna","RSSI","Gain","DateTime","timebin","Counts")])

# Summaries ----------------------

# ALL summary functions need to incorporate start and end time variables, for date and times
#    plus tower name/serial #s, and interval to summarize over (probably in minutes)

# summaries requested:


# from Alice's example doc
## Table: by-hour; antenna # + direction count, average rssi, gain (average, median...?), AND na = station offline, NOT 0




# Summarize counts at each antenna BY BAT/TAG# ------------

## dcast to a summary table, fill in sum of counts and mean rssi/gain -----------
Summary.bigtbl.tags <- CombinedRecords%>%
  maditr::dcast( timebin~loc+Antenna+TagID, value.var=list("Counts",c("Gain","RSSI")), fun.aggregate=list(sum,mean))


## split each tag to its own df ------
# ... I guess need a list
tags.deployed<-tags$`MOTUS ID`
BatDetectionCts <- vector("list",length(tags.deployed))
names(BatDetectionCts)<-tags.deployed
j<-1
for(i in tags.deployed){
  cols <- c("timebin",grep(i, names(Summary.bigtbl.tags), value = TRUE))
  BatDetectionCts[[j]]<-subset(Summary.bigtbl.tags, select = cols)
  j<-j+1
} # TODO: why do I need j, why doesn't i work? [i] and [[i]] return different errors neither functions



## tidy up colnames -----------
# to only text before the first _, between the 2nd and 4th_s, AND preserve timebin

# beautiful... I think this becomes a fn just by replacing BatDetectionCts with DF?
for(j in 1:length(names(BatDetectionCts))){
  t<-vector("list",length(names(BatDetectionCts[[j]])))

  for(i in 1:length(names(BatDetectionCts[[j]]))){
    t[[i]]<-paste0(strsplit(names(BatDetectionCts[[j]][i]),"_")[[i]][3],"_",
                   strsplit(names(BatDetectionCts[[j]][i]),"_")[[i]][4],"_",
                   strsplit(names(BatDetectionCts[[j]][i]),"_")[[i]][1])
  } #return(t) populates only the first element, interesting

  t[1] <- "timebin" # rename
  names(BatDetectionCts[[j]])<-unlist(t)
}
# the above DOES NOT WORK if run alone, you have to start from line 148


## reorganize columns  ---------
for(k in 1:length(BatDetectionCts)){
  newnames<-vector("list",length(names(BatDetectionCts[[k]])))
  for(j in 1:length(names(BatDetectionCts[[k]]))){
    newnames[j]<-paste(strsplit(names(BatDetectionCts[[k]]),"_")[[j]][2:3],collapse = "_")
  }
  newnames <- newnames[2:length(newnames)]
  colReorder <- vector("list",length=length(newnames)*3+1)
  colReorder[1]<-"timebin"
  m<-2
  for(i in 1:length(newnames)){
    colReorder[m] <- paste0(newnames[i],"_Counts")
    m<-m+1
    colReorder[m] <- paste0(newnames[i],"_Gain")
    m<-m+1
    colReorder[m] <- paste0(newnames[i],"_RSSI")
    m<-m+1
  }
  BatDetectionCts[[k]]<-relocate(BatDetectionCts[[k]],unlist(colReorder))
}

## tidy NA/NAN/0s -----------------

BatDetectionCts <- lapply(BatDetectionCts,tidy_empties)


## export ----------

openxlsx::write.xlsx(BatDetectionCts, file = paste0("summaries/",MySummaryFilename))


# end summary by tagID ---------





# Summarize most recent record for each bat -------------
BatsLastHeard<-CombinedRecords %>%
  group_by(TagID) %>%
  slice(which.max(as.Date(Date, '%m/%d/%Y')))%>%
  select(TagID,loc,Antenna,Date,Time)

write.csv(BatsLastHeard,paste0("summaries/BatsLastHeard_",Sys.Date(),".csv"))



# working ------------------




# alice's
##  possibly gantt style chart of detections over time?




