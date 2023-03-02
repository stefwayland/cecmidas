# TOU to XML converter

# DOW
# Holidays
library(lubridate)
library(data.table)

holidays <- fread("./tests/Holiday_test.csv", colClasses = c("character", "IDate"))
tou <- fread("./tests/TOU_test.csv", colClasses = c(TimeStart = "ITime", TimeEnd = "ITime"))

# Create table of prices for all hours of the year ----------------------
prices <- CJ(DateStart = seq(as.IDate("2023-01-01"), as.IDate("2023-12-31"), by = "day"),
             TimeStart = seq(as.ITime("00:00"), as.ITime("23:00"), by = 3600))
# Set Day to equal Monday = 1 through Sunday = 7
prices[, Day := wday(DateStart) - 1]
prices[Day == 0, Day := 7]
# Set Day to 8 on holidays
prices[holidays, Day := 8, on = .(DateStart = date)]

p2 <- tou[prices, .(DateStart, TimeStart, value, Unit), on = .(DateStart <= DateStart, DateEnd >= DateStart, DayStart <= Day, DayEnd >= Day, TimeStart <= TimeStart, TimeEnd >= TimeStart)]

# Create datetime fields for conversion to UTC
p2[, starttime := as.POSIXct(DateStart, time = TimeStart, tz = "America/Los_Angeles")]
p2[, c("DateStart", "TimeStart") := IDateTime(with_tz(starttime, tz = "UTC"))]
p2[, c("DateEnd", "TimeEnd") := IDateTime(with_tz(starttime + 3599, tz = "UTC"))]


# Convert to XML for upload -------------------------------------------------
library(xml2)
schema <- read_xml("./tests/MIDAS upload XSD.xsd")
RIN = "USCA-TSTS-HTOU-TEST"
rate_name = "CEC TEST24HTOU"
rate_type = "TOU"

rate = xml_new_root("DemandData")
xml_add_child(rate, "RateInformation")
xml_add_child(xml_child(rate), "RateID", RIN)
xml_add_child(xml_child(rate), "RateName", rate_name)
xml_add_child(xml_child(rate), "RateType", rate_type)
vi <- xml_add_child(xml_child(rate), "ValueInformation")

# xml_child(xml_child(rate), search = "ValueInformation")
for(i in 1:nrow(p2)) {
  vd <- xml_add_child(vi, "ValueData")
  xml_add_child(vd, "DateStart", p2[i, DateStart])
  xml_add_child(vd, "DateEnd", p2[i, DateEnd])
  xml_add_child(vd, "TimeStart", p2[i, TimeStart])
  xml_add_child(vd, "TimeEnd", p2[i, TimeEnd])
  xml_add_child(vd, "Value", p2[i, value])
  xml_add_child(vd, "Unit", p2[i, Unit])
  print(i)
}

xml_validate(rate, schema)
write_xml(rate, file = "./test/Streaming_Test.xml", options =c("format"))

