##ACCESS ALL TABLES FROM THE DATABASE and CALL SPECIFIC ONES FOR STUDY
### THIS FILE IMPORTS THE BMP Database from Access into R
install.packages("RODBC")
library(RODBC)

#start RStudio using **32-bit R**
    #Tools > Global Options > R General > Select 32-bit version
    #Must restart R and RStudio for 32-bit R to load

#connect to Access DB
address <- "F:/WERFproject/Master_BMP_Database_v_2016_11_17"
name <- "Master BMP Database v11-17-16 -SELDM.accdb"
db <- paste(address, name, sep = "/")

#using MS Access 2013 (.accdb), need to specify driver
handle <- odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, sep = ""))

#get list of all tables in database
tbls <- sqlTables(handle) #list of all types
unique(tbls$TABLE_TYPE) #want table type "TABLE"
tbls <- tbls[tbls$TABLE_TYPE == "TABLE",3] #column 3 is "TABLE_NAME"
head(tbls) #first six tables in database

#this is what a query looks like:
df <- sqlQuery(handle, paste("SELECT * FROM [BMP INFO S02]"))
str(df)

#call individual tables from database and make into dataframes:
#wshed <- sqlQuery(handle, paste("SELECT * FROM [WATERSHED NS01]"))
#bmp <- sqlQuery(handle, paste("SELECT * FROM [BMP INFO S02]"))
#site <- sqlQuery(handle, paste("SELECT * FROM [TESTSITE A01]"))
#station <- sqlQuery(handle, paste("SELECT * FROM [MONITORING STATIONS]"))
#event <- sqlQuery(handle, paste("SELECT * FROM [EVENT]"))
#flow <- sqlQuery(handle, paste("SELECT * FROM [FLOW]"))
#wq <- sqlQuery(handle, paste("SELECT * FROM [WATER QUALITY]"))

#call all tables from database into master list [m]
tbls0 <- paste("SELECT * FROM [",tbls,"]", sep = "")
m <- list(0)
i = 1
for(i in 1:length(tbls)) {
  m[[i]] <- sqlQuery(handle, tbls0[i])
}
names(m) <- tbls

#examine individual dataframe elements in the list
head(m[[5]])
str(m[[5]])

#save master dataset
saveRDS(m, file = "E:/WERFproject/data/master.Rdata")

#read master dataset
m <- readRDS("H:/WERFproject/data/master.Rdata")