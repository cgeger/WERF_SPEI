## CLEAN FILES FOR STUDY : site
## referencing original versions from master set "data/master.RData"

##### import raw df #####
#set working address
setwd("H:/WERFproject")

#site is "TESTSITE A01" table in ACCESS
m56 <- readRDS("data/master.RData")[[56]]
site <- m56

##### eliminate duplicate rows #####
#find primary keys (primary keys have zero duplicates)
sum(duplicated(site$NSWID)) # zero duplicate rows
#find duplicate entries (excluding the primary key)
sum(duplicated.data.frame(site[,-which(names(site) == "NSWID")]))

##### clean site data frame #####
#remove variables with more than 60% missing data
rmV <- which(colSums(is.na(site)) > dim(site)[1]*0.6)
site <- site[,-rmV]

#change primary and secondary keys to characters instead of numbers
names(site)[1] <- "SITEID"
site$SITEID <- trimws(as.character(site$SITEID))

#Trim whitespace
site$SITENAME <- trimws(as.character(site$SITENAME))
site$CITY <- trimws(as.character(site$CITY))
site$DP <- trimws(as.character(site$DP))

#recode STRUCT variable as factor
site$STRUCT <- factor(site$STRUCT, levels = c(1:4), labels = c("Structural", "Non-Structural","Both","LID"))
#remove non-structural BMP sites
k <- which(site$STRUCT == "Non-Structural")
site <- site[-k,]
site$STRUCT <- droplevels(site$STRUCT)

#eliminate "ZZ" error code from STATE 
k <- which(site$STATE == "ZZ")
site$COUNTRY[k] #(non US sites)
site$STATE[k] <- NA
site$STATE <- droplevels(site$STATE)

#recode ELEVATION_MSR and ELVTN_UNT_CD into meters (conversion factor from ft to m is 0.3048 m/ft)
table(site$ELVTN_UNT_CD)
k <- which(site$ELVTN_UNT_CD == "ft")
site$ELEVATION_MSR[k]  <- round(site$ELEVATION_MSR[k] * 0.3048) #run only once
site$ELEVATION_MSR <- round(site$ELEVATION_MSR, 2) #round all elevations to the nearest cm
#now all elevations in meters
site$ELVTN_UNT_CD[k] <- "m"
site$ELVTN_UNT_CD <- droplevels(site$ELVTN_UNT_CD)

#select variables to keep
names(site)
keep <- c("SITEID", "SITENAME", "STRUCT", "CITY", "STATE", "COUNTRY",
          "ELEVATION_MSR", "ELVTN_UNT_CD", "RF3_RIVER_REACH_CD", 
          "NO_WS", "NO_BMPS", "FINAL_LAT", "FINAL_LONG", "EPA Rain Zone", "COMMENTS")
k <- which(names(site) %in% keep)
site <- site[,k]

head(site)

#rename all to uniform names
names(site)[3:14] <- paste("SITE", names(site)[3:14], sep = "")
names(site)[7] <- "SITE_ELVTN"
names(site)[8] <- "SITE_ELVTN_U"
names(site)[9] <- "SITEHUC8"

##### save site table #####
write.csv(site,"H:/WERFproject/data/site.csv") #saved as .csv file to H drive remotely
saveRDS(site, "H:/WERFproject/data/site.Rdata") #saved as .Rdata to H drive remotely
saveRDS(site, "C:/Users/caite/Documents/WERF_SPEI/data/site.Rdata") #local backup

#command to read in doc from H drive
#site <- readRDS("data/site.Rdata")
