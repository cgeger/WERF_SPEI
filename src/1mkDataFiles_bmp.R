## CLEAN FILES FOR STUDY : bmp and bmp.types
## referencing original versions from master set "data/master.RData"

##### import raw df #####
#set working address to the VPN-connected drive where the data is stored
setwd("H:/WERFproject")

#in ACCESS, object bmp is "BMP INFO S02", bmp.types is"tblBMPCODES"
m5 <- readRDS("data/master.RData")[[5]]
bmp <- m5
m36 <- readRDS("data/master.RData")[[36]]
bmp.types <- m36

##### eliminate duplicate rows #####
#find primary keys (primary keys have zero duplicates)
sum(duplicated(bmp$BMPID)) # zero duplicate rows
#find duplicate entries (excluding the primary key)
sum(duplicated.data.frame(bmp[,-which(names(bmp) == "BMPID")]))

##### clean bmp data frame #####
#remove variables with more than 60% missing data
rmV <- which(colSums(is.na(bmp)) > dim(bmp)[1]*0.6)
bmp <- bmp[,-rmV]

#change primary and secondary keys to characters instead of numbers
bmp$BMPID <- as.character(trimws(bmp$BMPID))
bmp$SITEID <- as.character(trimws(bmp$SITEID))
bmp$WSID <- as.character(trimws(bmp$WSID))

#Trim whitespace
bmp$BMPNAME <- trimws(bmp$BMPNAME)
bmp$DTDO <- as.character(trimws(bmp$DTDO))
bmp$Comment <- as.character(trimws(bmp$Comment))

#remove non-structural bmps from dataset; they are not of interest for this study
k <- which(bmp$Category == "N") #30 observations of nonstructural bmps
bmp <- bmp[-k,]
#drop the variable "Category" now that all observations are structural ones
k <- which(names(bmp) == "Category") 
bmp <- bmp[,-k]

#recode Number of separate inflows (NSIP)
k <- which(bmp$NSIP < 0)
bmp$NSIP[k] <- NA

#recode bypass/overflow variable (BMPBYPAS)
k <- which(bmp$BMPBYPAS == "-9")
bmp$BMPBYPAS[k] <- NA
bmp$BMPBYPAS <- droplevels(bmp$BMPBYPAS)

#recode DesignInfo (yes/no)
k <- which(names(bmp) == "DesignInfo?")
names(bmp)[k] <- "DesignInfo"
unique(bmp$DesignInfo)
k <- which(bmp$DesignInfo %in% c("Yes","yes","Y"))
bmp$DesignInfo[k] <- "yes"
k <- which(bmp$DesignInfo %in% c("No","no","n"))
bmp$DesignInfo[k] <- "no"
bmp$DesignInfo <- droplevels(bmp$DesignInfo)

#examine facility type classification
table(bmp$Analysis_Category) #matches CAT_2016 except excludes certain types
table(bmp$`TBMPT 2009`)
table(bmp$xoldTBMPT)
table(bmp.types$CAT_2016) #full list of types

#Add column indicating new analysis category
k <- match(bmp$`TBMPT 2009`, bmp.types$TBMPT)
bmp$CAT_2016 <- bmp.types$CAT_2016[k] #new category contains all types
bmp$`TBMPT 2009` <- droplevels(bmp$`TBMPT 2009`) 
bmp$xoldTBMPT <- droplevels(bmp$xoldTBMPT)
bmp$CAT_2016 <- droplevels(bmp$CAT_2016)

#rename columns to uniform names
names(bmp) <- c("BMPID", "SITEID", "WSID", "BMPNAME",
                "TBMPT2009", "BMPFACDATE", "BMPNSIP",
                "BMP_BYPASS", "BMPDTDO",  "BMPComment",
                "BMP_PDFID", "BMPDesignInfo", "BMPAnalysis_Category",
                "BMPxoldTBMPT", "BMPCAT_2016")


#eliminate Categories that can't be analysed "CO" combined "LD" generic LID and "OT" other
bmp <- bmp[!bmp$BMPCAT_2016 %in% c("CO","LD","OT"),]
bmp$BMPCAT_2016 <- droplevels(bmp$BMPCAT_2016)

##### save bmp table #####
write.csv(bmp,"H:/WERFproject/data/bmp.csv") #saved as .csv file to H drive remotely
saveRDS(bmp, "H:/WERFproject/data/bmp.Rdata") #saved as .Rdata to H drive remotely
saveRDS(bmp, "C:/Users/caite/Documents/WERF_SPEI/data/bmp.Rdata") #local backup

#command to read in doc from H drive
#bmp <- readRDS("data/bmp.Rdata")
