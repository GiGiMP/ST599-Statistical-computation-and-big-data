library(ggplot2)
library(dplyr)

download.file("http://www2.census.gov/acs2011_5yr/pums/csv_pny.zip",
              destfile = "data/csv_pny.zip")
unzip("data/csv_pny.zip", list = TRUE)

pny <- read.csv(unz("data/csv_pny.zip", "ss11pny.csv"), nrows = 10000, 
                stringsAsFactors = FALSE)
# I cannot load all of the data, so I just use first 10000 lines.
str(pny)

pny_df <- tbl_df(pny)

select(pny_df, JWMNP, WAGP, JWTR, COW)

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
pny_df <- mutate(pny_df, transport_type = JWTR_codes[as.character(JWTR)], 
                 income = WAGP + SSIP + OIP)
hist(pny_df$WAGP)

# group by type of transport and summarise
trans_type <- group_by(pny_df, JWTR)
income_summary <- summarise(trans_type, 
                          transport_type = first(transport_type),
                          avg_income = mean(income, na.rm = TRUE),
                          med_income = median(income, na.rm = TRUE),
                          n = n(),
                          n_missing = sum(is.na(WAGP)))

# It shows in each trasportation type, the commuter's average income and medium income


# playing with plotting
income_summary2 <- mutate(income_summary, prop = n/sum(n))

# The proportion of people take this transportation
qplot(transport_type, prop, data =income_summary2) 

# plot the propotion from a ordered result
qplot(reorder(transport_type, prop), prop, data =income_summary2) 
qplot(prop, reorder(transport_type, prop), data =income_summary2) 

# The box plot according to the income
qplot(factor(JWTR), log(income), data = pny_df, geom = "boxplot")
qplot(factor(JWTR), log(income), data = pny_df, geom = "violin")
# don't care about NAs in this plot
time_summary3 <- mutate(filter(time_summary, !is.na(transport_type)), prop = n/sum(n))
qplot( prop, reorder(transport_type, prop), data =time_summary3) 

summarise(pny_df, mean(is.na(income)), mean(income == 0, na.rm = TRUE))
# 18% missing,  34% of non-missing earn $0
pny_no0 = filter(pny_df, income!=0)
pny.aov <- aov(log(income)~factor(JWTR),data=pny_no0)
summary(pny.aov)

pny.tky <- TukeyHSD(pny.aov)

pny.tky[[1]][order(pny.tky[[1]][,4]),]

