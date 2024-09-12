


# Pull in files from input_data folder --------------

## SRX800 receiver:  ---------------
#import the day's download
dflist_800<-dir('input_data',pattern="TXT")

# create an object to hold data
dfs_800<-vector("list",length=length(dflist_800))

# fill the new object with data, replace all * with NA
for(i in seq_along(dflist_800)){
  dfs_800[[i]] <- read.fwf(paste0('input_data/',dflist_800[1],sep=""),widths=c(8,12,10,10,14,6))
  dfs_800[[i]][,1][grepl("[*]",dfs_800[[i]][,1])] <- NA #if there are asterisks in col 1, it is not a data row
} # the na.omit and grepl * to NA combine to replace the SKIP argument in 1200 below, but do mean we have to assign names
names(dfs_800)<-stringr::str_extract(dflist_800, "[:upper:]{1,}[0-9]{7}")

# not sure how to fix, having these all in a single loop doesn't seem to do it in order...
for(i in 1:length(dfs_800)){
  names(dfs_800[[i]]) <- SRX800_names
  dfs_800[[i]]$receiver <- names(dfs_800)[i] # there SHOULD only be one, with one serial, but just in case...
  dfs_800[[i]]$loc <- stringr::str_extract(dflist_800[i], "[:upper:]{4}")
  dfs_800[[i]][,4] <- as.numeric(dfs_800[[i]][,4])
  dfs_800[[i]] <- na.omit(dfs_800[[i]])
}

#' [ MAYBE need to change column classes? Kind of hoping exporting + reimporting auto-detects the classes/deals with the spaces for me...]




## SRX1200 receivers:  ---------------

#import the day's download
dflist_1200<-dir('input_data',pattern="CSV")

# create an object to hold data
dfs_1200<-vector("list",length=length(dflist_1200))

# fill the new object with data, skipping all rows until "Index" (r1c1 of data section) is encountered
for(i in seq_along(dflist_1200)){
  dfs_1200[[i]] <- read.csv(paste0('input_data/',dflist_1200[i],sep=""),
                            skip = find_skip(paste0('input_data/',dflist_1200[i],sep="")),
                            header=T)
} # we could use skip above as well, would need to look for "Data segment" and then skip some following rows I think

# rename the datasets according to the receiver's serial#
names(dfs_1200)<-stringr::str_extract(dflist_1200, "[:upper:]{1,}[0-9]{7}") #hopefully {1,} is taking 1 or more capital letters, so the MD receiver gets named nicely




# parsing individual vs crto records from the 1200 receivers ---------

# separate into individual and aggregated records datasets
count_datasets <- receivers$serial[which(receivers$records=="crto" & receivers$SRX == 1200)]
indiv_datasets <- receivers$serial[which(receivers$records=="indiv" & receivers$SRX == 1200)]
# the SRX800 receiver does not appear on these lists, it will remain in a separate master file
df1200_indiv <- vector("list",length=length(indiv_datasets))
df1200_crto <- vector("list",length=length(count_datasets))

# DELETE - coping with the camp bullshit
# names(dfs_1200) <- c("D0020007_c","D0020007_i","D0020006","D0020009","D0020008")
# count_datasets <- "D0020007_c"
# indiv_datasets <- c("D0020007_i","D0020006","D0020009","D0020008")


i_crto <- 1
i_indiv <- 1
for(i in 1:length(dfs_1200)){

  if (names(dfs_1200)[i] %in% count_datasets)
{  df1200_crto[[i_crto]]<-dfs_1200[[i]]
  i_crto <- i_crto+1}

  else if (names(dfs_1200)[i] %in% indiv_datasets)
    {df1200_indiv[[i_indiv]]<-dfs_1200[[i]]
  i_indiv <- i_indiv+1}

}


names(df1200_crto) <- count_datasets
#names(df1200_crto) <- "D0020007" # remove _c
#df1200_crto_backup <- df1200_crto
for(i in 1:length(df1200_crto)){
  df1200_crto[[i]]$receiver <- names(df1200_crto)[i]# the df name should be the receiver - i THINK this is how you call tat up?
  df1200_crto[[i]]$loc <- receivers$loc_code[which(receivers$serial == names(df1200_crto)[i])]# use the towers.csv key to match the dataset name to the loc
  df1200_crto[[i]][,4] <- as.numeric(df1200_crto[[i]][,4]) # col 4 is tagID
  df1200_crto[[i]] <- na.omit(df1200_crto[[i]])
  df1200_crto[[i]]$CRTObin <- rep(CRTObin)
}

names(df1200_indiv) <- indiv_datasets
#names(df1200_indiv) <- c("D0020007","D0020006","D0020009","D0020008") #remove _i from D0020007
#df1200_indiv_backup <- df1200_indiv
for(i in 1:length(df1200_indiv)){
  df1200_indiv[[i]]$receiver <- names(df1200_indiv)[i]# the df name should be the receiver - i THINK this is how you call tat up?
  df1200_indiv[[i]]$loc <- receivers$loc_code[which(receivers$serial == names(df1200_indiv)[i])]# use the towers.csv key to match the dataset name to the loc
  df1200_indiv[[i]][,5] <- as.numeric(df1200_indiv[[i]][,5]) # col 5 here is tagID
  df1200_indiv[[i]] <- na.omit(df1200_indiv[[i]])
}



# rbind lists of dataframes into dataframes ------------
# indiv can all be rbound,
df1200_indiv <- do.call("rbind", df1200_indiv)
# crto should only be one DF, but to get it out of a list we do.call
df1200_crto <- do.call("rbind", df1200_crto)
# same for srx800, single df but in a list anyway
dfs_800 <- do.call("rbind", dfs_800)



# Drop Index column (no longer an FID with multiple files combining) --------------
# dfs_800 shouldn't HAVE an index column to begin with
df1200_indiv <- df1200_indiv%>%
  select(-"Index")
df1200_crto <- df1200_crto%>%
  select(-"Index")
# can also remove first column with:
# for(i in seq_along(dflist_1200)){
#   dfs_1200[[i]] <- dfs_1200[[i]][,2:ncol(dfs_1200[[i]])]
# }

# Merge with master, drop duplicate rows --------------

SRX800receiverdata <- unique(rbind(masterfile_srx800, dfs_800))

SRX1200individualdata <- unique(rbind(masterfile_srx1200_i, df1200_indiv))

SRX1200crtodata <- unique(rbind(masterfile_srx1200_c, df1200_crto))




# Save the new master file --------------
# no Sys.Date because then we have to call up the last date we ran the
# script on to pull the master data file back in OR I have to do more crazy stuff with

#'*SRX800 receiver, aka LBRC*
write.csv(SRX800receiverdata,paste0("masters/LBRC_D000946_master_2024.csv"),row.names=F)

#'*individual records from the srx1200 series receivers*
write.csv(SRX1200individualdata,paste0("masters/SRX1200_individuals_master_2024.csv"),row.names=F)

#'*CRTO records from the srx1200 series receivers*
write.csv(SRX1200crtodata,paste0("masters/SRX1200_crto_master_2024.csv"),row.names=F)

#'









