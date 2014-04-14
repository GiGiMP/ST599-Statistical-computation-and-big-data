library(ggplot2)
library(dplyr)

download.file("http://www2.census.gov/acs2012_5yr/pums/csv_hdc.zip",
              destfile = "data/csv_hdc.zip")
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_hny.zip",
              destfile = "data/csv_hny.zip")
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_pdc.zip",
              destfile = "data/csv_pdc.zip")
download.file("http://www2.census.gov/acs2012_5yr/pums/csv_pny.zip",
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

# HINCP, INTP, OIP, PAP, SSIP, ADJINC

# JWMNP: Travel time to work; JWRIP: Vehicle occupancy: JWTR: means of transportation to work


#==============================================================================

hny <- read.csv(unz("data/csv_hny.zip", "ss12hny.csv"),  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(hny)
#==============================================================================
pdc <- read.csv(unz("data/csv_pdc.zip", "ss12pdc.csv"),  # notice just 10 lines! why?
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
pdc_df <- mutate(pdc_df, transport_type = JWTR_codes[JWTR])

# group by type of transport and summarise
trans_type <- group_by(pdc_df, JWTR)
time_summary <- summarise(trans_type, 
                          transport_type = first(transport_type),
                          avg_time = mean(JWMNP, na.rm = TRUE),
                          med_time = median(JWMNP, na.rm = TRUE),
                          n = n(),
                          n_missing = sum(is.na(JWMNP)))
mutate(time_summary, prop = n/sum(n))


#==============================================================================

pny <- read.csv(unz("data/csv_pny.zip", "ss12pny.csv"), nrows = 10,  # notice just 10 lines! why?
                  stringsAsFactors = FALSE)
str(pny)
#==============================================================================
