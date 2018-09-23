## CLEAN FILES FOR STUDY : wshed
## referencing original versions from master set "data/master.RData"

##### import raw df #####
#set working address
setwd("H:/WERFproject")

#wshed is "WATERSHED NS01" table in ACCESS
m59 <- readRDS("data/master.RData")[[59]]
wshed <- m59

##### eliminate duplicate rows #####
#find primary keys (primary keys have zero duplicates)
sum(duplicated(wshed$WSID)) # zero duplicate rows
#find duplicate entries (excluding the primary key)
sum(duplicated.data.frame(wshed[,-which(names(wshed) == "WSID")]))

##### clean wshed data frame #####
#set site and watershed IDs to character
wshed$SITEID <- as.character(trimws(wshed$SITEID))
wshed$WSID <- as.character(trimws(wshed$WSID))

#Trim whitespace from WSNAME
wshed$WSNAME <- as.character(trimws(wshed$WSNAME))

#WSTYPE factor descriptions 1 = Test, 2 = Reference
wshed$WSTYPE <- factor(wshed$WSTYPE, labels = c("Test", "Reference"))

#change watershed area (WA) and units (WSA_UNIT) from m^2 to ha (all in ha) 1 m^2 == 0.0001 ha
k <- which(wshed$WSA_UNIT == "m2")
wshed$WA[k] <- wshed$WA[k] * 0.0001
wshed$WSA_UNIT[k] <- "ha"
wshed$WSA_UNIT <- droplevels(wshed$WSA_UNIT)

#Remove columns with less than 60% data
#Column 19, "PERI" represents impervious surface, other columns have limited data. 
#Might be able to add them together for more info later, but for now, keep only PERI and basic WS characteristics
wshed <- wshed[ , c(1:4,6,7,20)]

#rename variables to uniform
names(wshed)[5] <- "WSArea"
names(wshed)[6] <- "WSArea_U"
names(wshed)[7] <- "WSPerImp"

##### save wshed table #####
write.csv(wshed,"H:/WERFproject/data/wshed.csv") #saved as .csv file to H drive remotely
saveRDS(wshed, "H:/WERFproject/data/wshed.Rdata") #saved as .Rdata to H drive remotely
saveRDS(wshed, "C:/Users/caite/Documents/WERF_SPEI/data/wshed.Rdata") #local backup

#command to read in doc from H drive
#wshed <- readRDS("data/wshed.Rdata")
