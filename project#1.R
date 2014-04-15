# problem so far: 1. The housing and the population is in different spreadsheets, 
# so we can hardly combine the two together. 2. What do we want to compare, the
# average income with different ratio in the transportation type occupancy? or in
# the specific income group, how the transportation type varies, and how the specific 
# transportation type varies from all different income group. 
# Based 



library(ggplot2)
library(dplyr)

download.file("http://www2.census.gov/acs2011_5yr/pums/csv_hdc.zip",
              destfile = "data/csv_hdc.zip")
download.file("http://www2.census.gov/acs2011_5yr/pums/csv_hny.zip",
              destfile = "data/csv_hny.zip")
download.file("http://www2.census.gov/acs2011_5yr/pums/csv_pdc.zip",
              destfile = "data/csv_pdc.zip")
download.file("http://www2.census.gov/acs2011_5yr/pums/csv_pny.zip",
              destfile = "data/csv_pny.zip")

unzip("data/csv_hdc.zip", list = TRUE)
unzip("data/csv_hny.zip", list = TRUE)
unzip("data/csv_pdc.zip", list = TRUE)
unzip("data/csv_pny.zip", list = TRUE)
#==============================================================================

hdc <- read.csv(unz("data/csv_pdc.zip", "ss12hdc.csv"),   # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(hdc)
hdc_df <- tbl_df(hdc)

select(hdc_df, HINCP, INTP, OIP, PAP, SSIP, ADJINC)

hdc_df <- mutate(hdc_df, income = HINCP + ADJINC)

income_type <- group_by(hdc_df, income)
group_by(income_type, JWTR)

# HINCP, INTP, OIP, PAP, SSIP, ADJINC

# JWMNP: Travel time to work; JWRIP: Vehicle occupancy: JWTR: means of transportation to work
which(names(hdc_df) == "WAGP")
which(names(hdc_df) %in% c("WAGP", "COW"))

#==============================================================================

hny <- read.csv(unz("data/csv_hny.zip", "ss12hny.csv"),  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(hny)
#==============================================================================
pdc <- read.csv(unz("data/csv_pdc.zip", "ss11pdc.csv"),  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(pdc)

pdc_df <- tbl_df(pdc)

select(pdc_df, JWMNP, WAGP, JWTR, COW)

JWTR_codes <- c("1" = "Car, truck, or van",
                "2" = "Bus or trolley bus",
                "3" = "Streetcar or trolley car",
                "4" = "Subway or elevated",
                "5" = "Railroad",
                "6" = "Ferryboat",
                "7" = "Taxicab",
                "8" = "Motorcycle",
                "9" = "Bicycle",
                "10" = "Walked",
                "11" = "Worked at home",
                "12" = "Other method")
pdc_df <- mutate(pdc_df, transport_type = JWTR_codes[as.character(JWTR)])
mutate(pdc_df,  Income = WAGP)
hist(pdc_df$WAGP)

# group by type of transport and summarise
trans_type <- group_by(pdc_df, JWTR)
Income_type <- group_by(pdc_df, function(WAGP))
# shangjia
time_summary <- summarise(trans_type, 
                          transport_type = first(transport_type),
                          avg_time = mean(JWMNP, na.rm = TRUE),
                          med_time = median(JWMNP, na.rm = TRUE),
                          n = n(),
                          n_missing = sum(is.na(JWMNP)))
mutate(time_summary, prop = n/sum(n))

# playing with plotting
time_summary2 <- mutate(time_summary, prop = n/sum(n))

qplot(transport_type, prop, data =time_summary2) 
qplot(reorder(transport_type, prop), prop, data =time_summary2) 
qplot( prop, reorder(transport_type, prop), data =time_summary2) 

# don't care about NAs in this plot
time_summary3 <- mutate(filter(time_summary, !is.na(transport_type)), prop = n/sum(n))
qplot( prop, reorder(transport_type, prop), data =time_summary3) 

# ok, really I want this state by state, see 03-mode-by-state.r

# TODO:
#  - look into distributions, shape, spread etc. boxplots, 5 number summary
#  - is only 56% reasonable for number of workers or are there other reasons this is missing?
#  - split car/truck/van users by number of people in vehicle (JWRIP)
#  - how do these numbers vary by state? year?

# === How does the time to commute vary for people === #
#        that commute via different methods? 

# look at some plots, but just use a sample to keep things quick
samp <- sample(nrow(or_df), size = 1000)

qplot(JWMNP, data = or_df[samp, ])
qplot(WAGP, data = or_df[samp, ])

summarise(or_df, mean(is.na(WAGP)), mean(WAGP == 0, na.rm = TRUE))
# 17% missing,  42% of non-missing earn $0

qplot(log(WAGP), JWMNP, data = or_df[samp, ])
qplot(JWMNP, log(WAGP), data = or_df[samp, ])

qplot(factor(JWMNP), log(WAGP), data = or_df[samp, ], geom = "boxplot")

qplot(factor(JWMNP), log(WAGP), data = or_df, geom = "boxplot")
# slight positive relationship between income and mean/median? commute time for commute
# times below 15 minutes? think about better plot
# worry about $0 earners
#==============================================================================

pny <- read.csv(unz("data/csv_pny.zip", "ss12pny.csv"), nrows = 10,  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(pny)
#==============================================================================
