## CLEAN FILES FOR STUDY : flow
## referencing original versions from master set "data/master.RData"

##### import raw df #####
#set working address
setwd("H:/WERFproject")

#flow is "FLOW" table in ACCESS
m10 <- readRDS("data/master.RData")[[10]]
flow <- m10

##### eliminate duplicate rows #####
#find primary keys (primary keys have zero duplicates)
sum(duplicated(flow$`Record Number`)) #zero duplicates
#find duplicate entries (excluding the primary key)
sum(duplicated.data.frame(flow[,-which(names(flow) == "Record Number")])) #129
k <- which(duplicated.data.frame(flow[,-which(names(flow) == "Record Number")]))
flow <- flow[-k,] #run only once
sum(duplicated.data.frame(flow[,-which(names(flow) == "Record Number")]))

##### clean flow data frame #####
#remove variables with more than 60% missing data
which(colSums(is.na(flow)) > dim(flow)[1]*0.6)
rmV <- which(colnames(flow) %in% c("ENDTIME", "TOTVOLBP", "TOTVOLBPU", "PEAKBPFLOW", "PEAKBPFLOWU", "FLOWRATE", "FLOWRATEU", "PERHYDROCAP", "EDMFC", "Flow Vol (CF)"))
rmV
flow <- flow[, -rmV]
str(flow)

#recode variables
flow$`Test Site ID` <- as.character(flow$`Test Site ID`)
flow$SITENAME <- trimws(as.character(flow$SITENAME))
flow$MSID <- as.character(flow$MSID)
flow$STARTDATE <- as.Date(flow$STARTDATE)
flow$STARTTIME <- as.character(flow$STARTTIME)
flow$ENDDATE <- as.Date(flow$ENDDATE)
flow$Comment <- trimws(as.character(flow$Comment))

##### recode TOTVOLEFF and TOTVOLEFFU to Liters (L) and remove -99999 #####
#remove negative numbers and remove units for missing values
k <- which(flow$TOTVOLEFF < 0)
flow$TOTVOLEFF[k] <- NA
flow$TOTVOLEFFU[k] <- NA

#Recode liters and Liters to L
unique(flow$TOTVOLEFFU)
k <- which(flow$TOTVOLEFFU %in% c("liters", "Liters", "L","l"))
flow$TOTVOLEFFU[k] <- "L"

#convert gal gallon and gallons to L (3.78541 Liters per gallon)
k <- which(flow$TOTVOLEFFU %in% c("gal", "gallon","gallons"))
x <- round(flow$TOTVOLEFF[k] * 3.78541,2)
flow$TOTVOLEFF[k] <- x
flow$TOTVOLEFFU[k] <- "L"

#convert cf to L (28.3168332462702 L per cf)
k <- which(flow$TOTVOLEFFU %in% c("cf"))
x <- round(flow$TOTVOLEFF[k] * 28.3168332462702, 2)
flow$TOTVOLEFF[k] <- x
flow$TOTVOLEFFU[k] <- "L"

#convert m3 to L (1000 L per m3)
k <- which(flow$TOTVOLEFFU %in% c("m3"))
x <- round(flow$TOTVOLEFF[k] * 1000,2)
flow$TOTVOLEFF[k] <- x
flow$TOTVOLEFFU[k] <- "L"

#convert AF (Acre Feet) to L (1000 L per m3, 1233.48 m3 per AF)
k <- which(flow$TOTVOLEFFU %in% c("AF"))
x <- round(flow$TOTVOLEFF[k] * 1000 * 1233.48, 2)
flow$TOTVOLEFF[k] <- x
flow$TOTVOLEFFU[k] <- "L"

#drop rows where units are not identified
k <- which(flow$TOTVOLEFFU %in% c("", " ","  "))
flow$TOTVOLEFFU[k] <- NA

#recode M (meters) to NA (values are missing)
k <- which(flow$TOTVOLEFFU == "M")
flow$TOTVOLEFFU[k] <- NA

#remove units for missing values
k <- which(is.na(flow$TOTVOLEFF) == T)
flow$TOTVOLEFFU[k] <- NA

flow$TOTVOLEFFU <- droplevels(flow$TOTVOLEFFU)
str(flow)

##### clean PEAKEFFFLOW and PEAKEFFFLOWU, recode to L/sec units #####
#recode -99999 values to NA
k <- which(flow$PEAKEFFFLOW < 0)
flow$PEAKEFFFLOW[k] <- NA 
flow$PEAKEFFFLOWU[k] <- NA

#drop rows where units are not identified
k <- which(flow$PEAKEFFFLOWU %in% c("", " ","  "))
flow$PEAKEFFFLOWU[k] <- NA

##convert all to L/sec
k <- which(flow$PEAKEFFFLOWU %in% c("L/s", "l/sec","L/sec"))
flow$PEAKEFFFLOWU[k] <- "L/sec"

#convert l/min to L/sec (60 seconds per minute)
k <- which(flow$PEAKEFFFLOWU %in% c("LPM", "L/min"))
flow$PEAKEFFFLOW[k] <- flow$PEAKEFFFLOW[k]/60
flow$PEAKEFFFLOWU[k] <- "L/sec"

#convert cfs to L/sec (1 cfs is 28.3168 L/sec)
k <- which(flow$PEAKEFFFLOWU %in% c("cfs", "CFS"))
flow$PEAKEFFFLOW[k] <- flow$PEAKEFFFLOW[k] * 28.3168
flow$PEAKEFFFLOWU[k] <- "L/sec"

#convert cms to L/sec (1 cms is 1000 L/sec)
k <- which(flow$PEAKEFFFLOWU %in% c("cms"))
flow$PEAKEFFFLOW[k] <- flow$PEAKEFFFLOW[k] * 1000
flow$PEAKEFFFLOWU[k] <- "L/sec"

#convert gpm to L/sec (1 gpm is 0.0630902 L/sec)
k <- which(flow$PEAKEFFFLOWU %in% c("gpm"))
flow$PEAKEFFFLOW[k] <- flow$PEAKEFFFLOW[k] * 0.0630902
flow$PEAKEFFFLOWU[k] <- "L/sec"

flow$PEAKEFFFLOWU <- droplevels(flow$PEAKEFFFLOWU)
str(flow)

#####simplify factors #####
#simplify factors for AFVOLCOMP, `Scrn 2` `Scrn 3`
k <- which(flow$AFVOLCOMP %in% c("NO", "No", "no", "n", "N"))
flow$AFVOLCOMP[k] <- "no"
k <- which(flow$AFVOLCOMP %in% c("YES", "Yes", "yes", "y", "Y"))
flow$AFVOLCOMP[k] <- "yes"
k <- which(flow$AFVOLCOMP %in% c("p", "P"))
flow$AFVOLCOMP[k] <- "P"
flow$AFVOLCOMP <- droplevels(flow$AFVOLCOMP)

k <- which(flow$`Scrn 2 In Out Diff`  == "No")
flow$`Scrn 2 In Out Diff`[k] <- "no"
flow$`Scrn 2 In Out Diff` <- droplevels(flow$`Scrn 2 In Out Diff`)

k <- which(flow$`Scrn 3 Pairing`  == "No")
flow$`Scrn 3 Pairing`[k] <- "no"
flow$`Scrn 3 Pairing` <- droplevels(flow$`Scrn 3 Pairing`)

#remove outlier enddate
k <- which(flow[,7] == "7200-01-23")
flow[k,7] <- NA

#######rename variables to uniform names#####
names(flow)
names <- c("SITEID",            "SITENAME",           "MSID",              
           "EVENTNO",           "FLOWSTARTDATE",      "FLOWSTARTTIME",         
           "FLOWENDDATE",       "FLOWTOTVOL",         "FLOWTOTVOLU",
           "FLOWPEAKEFF",       "FLOWPEAKEFFU",       "FLOWComment",           
           "FLOWScrn1",         "FLOWScrn2",          "FLOWScrn3",    
           "FLOWRecordNO")

names(flow) <- names
str(flow)


######remove events screened in Scrn 1 AFTOTVOLCOMP #####
#table(flow$FLOWScrn1)
#k <- which(flow$FLOWScrn1 == "no")
#unique(flow[k,"SITENAME"])
##### remove events screened in Screen 2 (Inflow=Outflow assumed) #####
#unique(flow$FLOWScrn2)
#k <- which(flow$FLOWScrn2 == "no")
#flow <- flow[-k,]'

##### remove events screened in Screen 3 (unpairables) #####
table(flow$FLOWScrn3)
k <- which(flow$FLOWScrn3 == "no")
flow <- flow[-k,]
str(flow)

##### save flow table #####
write.csv(flow,"data/flow.csv") #saved as .csv file in data folder
saveRDS(flow, "data/flow.Rdata")

flow <- readRDS("data/flow.Rdata")

