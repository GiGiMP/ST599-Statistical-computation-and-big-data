# Code to download Oregon pop data 2012.
# part functional part pedagoical
# === download file === #
# download OR 2012_5yr
#GSnotes - had to get the working directory right to get this to run
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_pny.zip", 
              destfile = "data/csv_pny.zip")
# === unzip and get file === #

getwd()
# look into others function: unz, unzip, system('unzip ...'), 
# check files in the zip archive without extracting
unzip("data/csv_pny.zip", list = TRUE)

# extract and read in one go
ny_sm <- read.csv(unz("data/csv_pny.zip", "ss12pny.csv"), nrows = 10,  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(ny_sm)

which(names(ny_sm)%in%c("JWMNP", "JWRIP", "JWTR", "SEMP", "SSIP", "SSP", "WAGP","PERNP","PINCP", "PUMA10", "PUMA00"))
# got these results for the columns: 35  36  37  71  73  74  75 111 112 5
# cut out columns using a command line tool - did you do last week's readings?

system.time(system("cut -d, -f4,5,35,36,37,71,73,74,75,111,112 data/ss12pny.csv > data/ss12pny-cut.csv"))
system.time(ny4 <- read.csv("data/ss12pny-cut.csv",stringsAsFactors = FALSE))

head(ny4)
#now for dc data
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_pdc.zip", 
              destfile = "data/csv_pdc.zip")
# === unzip and get file === #
unzip("data/csv_pdc.zip", exdir = "data/")
system.time(system("cut -d, -f4,5,35,36,37,71,73,74,75,111,112 data/ss12pdc.csv > data/ss12pdc-cut.csv"))
system.time(dc4 <- read.csv("data/ss12pdc-cut.csv",stringsAsFactors = FALSE))

head(dc4)
rm(ny_sm)
#now to play with the data...
library(dplyr)
library(ggplot2)

#First issue is to figure out how to cut down NY data to just NYC (I am assuming all 5 burroughs, but maybe only Manhattan?!?n)
# get ready for dplyr
ny_df<-tbl_df(ny4)
dc_df<-tbl_df(dc4)
#Should we remove the dc4 or ny4 to reduce amount in memory?
#OK, now lets remove all the non NYC PUM10s
?dplyr
head(ny_df)
#need a for loop to select all the PUMAS and merge
#select the PUMA, 
rm(ny.pums00)
?merge
x <- (3710:4114)
?rbind
rbind(nyc.x[i]for(i in seq(x)){nyc.x[i]<-filter(ny_df,PUMA00=="x[i]"|PUMA10=="x[i]")}
      
nyc.3710<-filter(ny_df,PUMA00 =="3710"|PUMA10=="3710")


head(ny.pums)
