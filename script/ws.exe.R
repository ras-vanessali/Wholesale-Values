
library(RODBC)
library(RODBCext)
library(lattice)
library(latticeExtra)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(xlsx)
#library(readxl)
library(stringr)

# country
CountryCode = 'USA'
# db enviroment & connect
DBserver = 'production' 
#DBserver = 'rasquery'



## read input excel file and create a plot for storing the plots
file_path ="C:/Users/vanessa.li/Documents/GitHub/Wholesale-values/doc"

setwd(file_path)  
excelfile = '20200806 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)

## set directory of r scripts
scripts_path ="C:/Users/vanessa.li/Documents/GitHub/Wholesale-values/script"

setwd(scripts_path) 
runa<-parse('ws.input_func.r')
eval(runa)



starttime_dtload<-Sys.time()
channel<-odbcConnect(DBserver)
setwd(scripts_path) 
runwholesales<-parse('ws.sqlQueries.r')
eval(runwholesales)

# load each table in sql
wholesales <- sqlQuery(channel,wholesaledata)
schedule.cat <- sqlQuery(channel,sched.cat)
schedule.subcat <- sqlQuery(channel,sched.subcat)
schedule.make <- sqlQuery(channel,sched.make)
inprog.cat<-sqlQuery(channel,cat.inprog)
inprog.subcat<-sqlQuery(channel,subcat.inprog)
inprog.make<-sqlQuery(channel,make.inprog)
AllClass<-sqlQuery(channel,AllClass.sql)

end_time_dtload <- Sys.time()
end_time_dtload - starttime_dtload

# publish date
publishDate <- Sys.Date() - days(day(Sys.Date()))
priorMonth<-as.Date(publishDate- days(day(publishDate)))


## run model script
start_time_r <- Sys.time()
runmodel<-parse('ws.model.r')
eval(runmodel)
setwd(file_path)  
#write.xlsx2(as.data.frame(joinMakeOut),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjusters',row.names = F)

end_time_r <- Sys.time()
end_time_r - start_time_r
