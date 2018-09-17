## CLEAN FILES FOR STUDY : event and precip
## referencing original versions from master set "data/master.RData"

#load packages
install.packages("lubridate")
library(lubridate)
library(dplyr)

##### import raw df #####
#set working address to the VPN-connected drive where the data is stored
setwd("H:/WERFproject")

#event is "EVENT" table in ACCESS
m9 <- readRDS("H:/WERFproject/data/master.RData")[[9]]
event <- m9
#precip is "PRECIPITATION" table in ACCESS
m27 <- readRDS("H:/WERFproject/data/master.RData")[[27]]
precip <- m27

##### eliminate duplicate rows #####
#find primary keys (primary keys have zero duplicates)
sum(duplicated(event$`Record No`)) #zero duplicates on the primary key
sum(duplicated(precip$`Record Number`)) #zero duplicates on the primary key
#find duplicate entries (excluding the primary key)
sum(duplicated.data.frame(event[,-which(names(event) == "Record No")]))
sum(duplicated.data.frame(precip[,-which(names(precip) == "Record Number")])) #140 duplicates

names(event) #the event table uses a two-factor primary key: NSWID + Storm Event
#make a single primary key by combining NSWID and storm event numbers
event <- event %>% mutate(pk = paste(event$NSWID, event$`Storm Event`, sep = "_"))
#make a single primary key for precip by combining Test Site ID and storm event numbers
precip <- precip %>% mutate(pk = paste(precip$`Test Site ID`, precip$`Storm Event`, sep = "_"))

#make a list of 2-factor primary keys that have duplicates (event table)
dups <- event %>% 
  group_by(pk) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  select(pk) 

#find the duplicates in the event table
k <- which(event$pk %in% dups$pk)

#find the sites that have duplicates <-- there are only 5 sites with duplicates
event[k,] %>% arrange(pk) %>% group_by(`Site Name`) %>% summarize(n = n())

event[k,] %>% filter(`Site Name` == "1324 East 76th Street")
event[event$NSWID == "836379890",] #drop record number 22979
precip[precip$`Test Site ID` == "836379890",]
precip[precip$pk == "836379890_89",] #two events on the same date, but not on one from event table

event[k,] %>% filter(`Site Name` == "21st and Iris Rain Garden") #drop record number 22979
event[event$NSWID == "1708200120",] #drop record number 19502, 19503
precip[precip$pk == "1708200120_41",] #from may 7th 2014
precip[precip$pk == "1708200120_42",] #from may 8th 2014

#there are more than 100 duplicates for each of these sites, labelling is not clear, so they were eliminated
event[k,] %>% filter(`Site Name` == "Elm Drive") %>% arrange(pk) #this one reports end dates that are months/years befor start dates
event[k,] %>% filter(`Site Name` == "Lakeview 2") %>% arrange(pk) #this one reports at least 2 different sensors, but no way to connect that to event date reliably
precip[precip$`Test Site Name` == "Lakeview 2",] %>% arrange(pk)
event[k,] %>% filter(`Site Name` == "Lakeview 4") %>% arrange(pk) #this one reports at least 2 different sensors, but no way to connect that to event date reliably

#_drop duplicates from event______________________________________________________
#drops
event <- event[!(event$`Record No` %in% c(22979, 19502, 19503)), ]
event <- event[!(event$`Site Name` %in% c("Elm Drive", "Lakeview 2", "Lakeview 4")), ]
###NO MORE EVENT DUPLICATES (supposedly)

#find duplicate entries (excluding the primary key)
##sum(duplicated.data.frame(event[,-which(names(event) == "Record No")]))
##sum(duplicated.data.frame(precip[,-which(names(precip) == "Record Number")])) #141 duplicates
##precip[k,]
##k <- which(duplicated.data.frame(precip[,-which(names(precip) == "Record Number")]))
##precip <- precip[-k,]

##### clean event data frame #####
#rename variables to uniform names
names(event)
names <- c("SITEID", "EVENTNO", "SITENAME", "EVSTARTDATE", "EVSTARTTIME", "EVTYPE",      
           "EVANTEDRY", "EVDESCRIBEANTE", "EVQAQC", "EVComment", "EVRecordNO", "EVpk")
names(event) <- names
event$SITEID <- trimws(as.character(event$SITEID))
event$EVENTNO <- as.integer(event$EVENTNO)
event$EVSTARTDATE <- ymd(substr(event$EVSTARTDATE, start = 1, stop = 10))

names(precip) 
names <- c("SITEID","SITENAME", "MSID", "EVENTNO", "PSTARTDATE", "PSTARTTIME", "PENDDATE","PENDTIME",
           "PDEPTH","PDEPTHU", "P1HRPK", "P1HRPKU", "PComment", "PRecordNO", "PScrn", "Ppk")
names(precip) <- names
precip$SITEID <- trimws(as.character(precip$SITEID))
precip$EVENTNO <- as.integer(precip$EVENTNO)
precip$PSTARTDATE <- ymd(substr(precip$PSTARTDATE, start = 1, stop = 10))

#merge event and precip into one table based on two-factor pk
a <- merge(event, precip, all = F, by.x = "EVpk", by.y = "Ppk")
head(a)

# fix duplicate column names
names(a)
k <- which(names(a) %in% c("SITENAME.y", "SITEID.y", "EVENTNO.y"))
a <- a[,-k]
k <- which(names(a) %in% c("SITENAME.x", "SITEID.x", "EVENTNO.x"))
names(a)[k] <- c( "SITEID","EVENTNO","SITENAME")
head(a)


#trim character identifiers
a$MSID <- trimws(as.character(a$MSID))

#fix dates
#check which are more than 48 hours apart
k <- which(abs(ymd(a$EVSTARTDATE) - ymd(a$PSTARTDATE)) > 2) #(there are 300 matches with bad dates)
a[k,c("EVSTARTDATE", "PSTARTDATE")]
a <- a[-k,] #remove

#eliminate non-flow data and events with no depth recorded
k <- which(a$EVTYPE == "Runoff") #139 events that don't record runoff (Baseflow, Other, Snowmelt)
a <- a[k,]
a$EVTYPE <- droplevels(a$EVTYPE)
rmV <- which(a$PDEPTH < 0 | is.na(a$PDEPTH)) #135 with error codes reported or no precip value recorded
a <- a[-rmV,]

#correct factors
a$PScrn <- factor(tolower(a$PScrn))
table(a$PScrn)
rmV <- which(a$PScrn == "no") #230 values screened out
a <- a[-rmV,]

#convert units to uniform "cm" value
k <- which(a$PDEPTHU == "in")
a$PDEPTH[k] <- a$PDEPTH[k] * 2.54
a$PDEPTHU[k] <- "cm"
a$PDEPTHU <- droplevels(a$PDEPTHU)

ev.precip <- a

##### save event table #####
write.csv(ev.precip,"H:/WERFproject/data/eventprecip.csv") #save as .csv file in data folder in H drive remotely
saveRDS(ev.precip, "H:/WERFproject/data/eventprecip.Rdata") #save as .Rdata file to H drive remotely
saveRDS(ev.precip, "C:/Users/caite/Documents/WERF_SPEI/data/eventprecip.Rdata") #local backup on C Drive
ev.precip <- readRDS("data/eventprecip.Rdata") #command to read in remotely
