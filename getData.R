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
# got these results for the columns: 35  36  37  71  73  74  75 111 112 5 4
# cut out columns using a command line tool - did you do last week's readings?

system("cut -d, -f4,5,35,36,37,71,73,74,75,111,112 data/ss12pny.csv > data/ss12pny-cut.csv")
ny4 <- read.csv("data/ss12pny-cut.csv",stringsAsFactors = FALSE)

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
rm(ny4)
dc_df<-tbl_df(dc4)
rm(dc4)
#Should we remove the dc4 or ny4 to reduce amount in memory?

#need a for loop to select all the PUMAS and merge
#select the PUMA, 

#begin with Manhattan
manhat=data.frame()
for(i in 3801:3810){
  manhat<-rbind(manhat,filter(ny_df,PUMA00 ==i|PUMA10==i ))
}
head(manhat)
manhat <- mutate(manhat, Income=SSIP+SSP+WAGP)
#need to convert numeric codes to words
JWTR_codes <- c("1" = "Car/truck",
                "2" = "Bus",
                "3" = "Streetcar",
                "4" = "Subway",
                "5" = "Railroad",
                "6" = "Ferryboat",
                "7" = "Taxicab",
                "8" = "Motorcycle",
                "9" = "Bicycle",
                "10" = "Walked",
                "11" = "Work at home",
                "12" = "Other")
manhat<- mutate(manhat, trans_type = JWTR_codes[as.character(JWTR)])
summarise(manhat, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
#in manhattan, 11.8% is missing, 21.8% is 0

ggplot(manhat, aes(trans_type, log10(Income)), na.rm=TRUE, xlab="Transportation Type", ) +geom_violin()
#need to get better labels on graph
#may want to summarise the mean(log(income))

#want to get rid of all folks with zero income??? 
#check charlotte's code as to how she did this. 
manhat%.%
  group_by(trans_type)%.%
  summarise(
    medianInc = mean(log(Income), na.rm=TRUE))
#get data from other buroughs
#PUMAs 03701 - 03710 (bronx)
bronx=data.frame()
for(i in 3701:3710){
  bronx<-rbind(bronx,filter(ny_df,PUMA00 ==i|PUMA10==i ))
}

bronx <- mutate(bronx, Income=SSIP+SSP+WAGP, Income=SSIP+SSP+WAGP, trans_type = JWTR_codes[as.character(JWTR)])
summarise(bronx, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
#zero income summary for Bronx: 21.1% missing, 29.4%==0

#PUMAs 04001 - 04018 (Brooklyn)
brook=data.frame()
for(i in 4001:4018){
  brook<-rbind(brook,filter(ny_df,PUMA00 ==i|PUMA10==i ))
}

brook <- mutate(brook, Income=SSIP+SSP+WAGP, Income=SSIP+SSP+WAGP, trans_type = JWTR_codes[as.character(JWTR)])
summarise(brook, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
#zero income summary for Brooklyn: 19.2%missing, 26.5%=0

#PUMAs 04101 - 04114 (queens)
queens=data.frame()
for(i in 4101:4114){
  queens<-rbind(queens,filter(ny_df,PUMA00 ==i|PUMA10==i ))
}

queens<- mutate(queens, Income=SSIP+SSP+WAGP, trans_type = JWTR_codes[as.character(JWTR)])
summarise(queens, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
#zero income summary for Queens: 15.9% missing, 24.5%=0

#PUMAs 03901 - 03903 (Statan Island)
si=data.frame()
for(i in 3901:3903){
  si<-rbind(si,filter(ny_df,PUMA00 ==i|PUMA10==i ))
}

si<- mutate(si, Income=SSIP+SSP+WAGP, trans_type = JWTR_codes[as.character(JWTR)])
summarise(si, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
#zero income summary for Statan Island: 17.5% missing, 23.0%=0
rm(ny_df)
#Do we want to look at the buroughs separately or put together as a new df. I will code for new df.


#make DC data comparable to NYC data
dc<-mutate(dc_df, Income = SSIP + SSP + WAGP,
           trans_type = JWTR_codes[as.character(JWTR)])
summarise(dc, mean(is.na(Income)), mean(Income == 0, na.rm = TRUE))
summarise(dc, n

#similar to manhattan, missing= 12.8%, 0 income =21.4%
ggplot(dc, aes(trans_type, log10(Income), na.rm=TRUE))+geom_boxplot()

#couldn't find a sexier way to do this to get NYC together. 
bb<-rbind(bronx, brook)
bbm<-rbind(bb, manhat)
rm(bb)
bbmq<-rbind(bbm,queens)
rm(bbm)
nyc<-rbind(bbmq, si)
rm(bbmq)
# look at boxplot of nyc
n<-ggplot(dc, aes(trans_type, log10(Income), na.rm=TRUE))
n+violin()
n+geom_violin(color="blue", fill="blue", alpha=I(0.2)) + geom_violin(aes(trans_type, log10(Income)), data=nyc, color="red", fill="red", alpha=I(0.2)) 
#summarize by trans-group median income
nyc.trans<-group_by(nyc, trans_type)
summarise(nyc.trans, median(Income),
          n())

dc.trans<-group_by(dc, trans_type)

summarise(dc.trans, median(Income),
          n())
#now order these in decreasing order and without ferryboat
#NYC
nyc.nof<-filter(nyc, trans_type!="Ferryboat", Income!=0)
nyc.noft<-group_by(nyc.nof, trans_type)
nyc.s<-summarise(nyc.noft, log10(mean(Income)),
          n())

nyc.s <- nyc.s[order(nyc.s[,"log10(mean(Income))"],decreasing=T),]
#DC
dc.nof<-filter(dc, trans_type!="Ferryboat", Income!=0)
dc.noft<-group_by(dc.nof, trans_type)
dc.s<-summarise(dc.noft, log10(mean(Income)),
                n())
dc.s<- dc.s[order(dc.s[,"log10(mean(Income))"], decreasing=T),]
nyc.s
dc.s
 
ggplot(dat, aes(trans_type,Income, na.rm=TRUE)))+
  geom_violin(data=filter(dat,city=="DC"),color="blue", fill="blue", alpha=I(0.2))+
  geom_violin(data=filter(dat,city=="NYC"), color="red", fill="red", alpha=I(0.2))


n<-ggplot(dc.nof, aes(trans_type,Income, na.rm=TRUE))
big_font<-theme_gray(base_size=18)
n+geom_violin(color="blue", fill="blue", alpha=I(0.2)) +
  big_font + scale_fill_manual(values=c("red", "blue")) +
  geom_violin(aes(trans_type,Income),
              data=nyc.nof, color="red", fill="red", alpha=I(0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black")) +
    scale_x_discrete(limits = nyc.s$trans_type) +
     scale_y_log10(breaks=c(100,1000,10000,100000,1000000), limits=(c(100,1000000))) + 
  ggtitle("Comparing NYC and DC Income by Various Transportation Types") 


# Below represents a lot of work that didn't go anywhere...can't figure out how to control
# alpha so colors aren't so hideous (ouline of the violin plots really helps in the overlaid 
# graphs vs. combined data set graph below). I'm leaving this in incase anyone has an aha and
#figures it out!

dat = rbind(nyc.nof,dc.nof)
ggplot(dat,aes(x=trans_type,y= Income,fill=city),na.rm=TRUE) +
  geom_violin(position="identity", alpha=(.2))+
  scale_line_manual(values=c("red", "blue"), alpha=(1)) +
  scale_fill_manual(values=c("red", "blue")) + 
  scale_x_discrete(limits = nyc.s$trans_type) +
  scale_y_log10() 

