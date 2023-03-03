# TOU to XML converter

holiday_file <- "./tests/Holiday_test.csv"
tou_file <- "./tests/TOU_test.csv"
start_date <- "2022-01-01"
end_date <- "2023-12-31"

TOU_to_streaming <- function(holiday_file, tou_file, start_date,
                             end_date, time_zone = "America/Los_Angeles") {
  requireNamespace("lubridate")
  require(data.table)

  # check function arguments -------------------------------------------------
  checkmate::assert_file_exists(holiday_file, extension = "csv")
  checkmate::assert_file_exists(tou_file, extension = "csv")
  tryCatch(start_date <- as.IDate(start_date), error = function(e) stop("start_date must be a date formatted like YYYY-MM-DD"))
  tryCatch(end_date <- as.IDate(end_date), error = function(e) stop("end_date must be a date formatted like YYYY-MM-DD"))
  checkmate::assert_date(start_date, any.missing = FALSE, len = 1)
  checkmate::assert_date(end_date, any.missing = FALSE, len = 1)
  checkmate::assert_choice(time_zone, OlsonNames())

  # read in holiday and tou data and check column names ----------------------
  holidays <- fread(holiday_file, colClasses = c("character", "IDate"))
  checkmate::assert_names(names(holidays), permutation.of = c("holiday_name", "date"))
  tou <- fread(tou_file, colClasses = c(DateStart = "IDate", DateEnd = "IDate",
                                        TimeStart = "ITime", TimeEnd = "ITime"))
  checkmate::assert_names(names(tou),
                          permutation.of = c("RIN", "AltRateName1", "AltRateName2", "SignupCloseDate", "RateName",
                                             "RatePlan_Url", "RateType", "Sector", "API_Url", "EndUse", "DateStart",
                                             "DateEnd", "DayStart", "DayEnd", "TimeStart", "TimeEnd", "ValueName",
                                             "Value", "Unit"))

  # Create table of prices for all hours of the year for each rate -----------
  prices <- CJ(RIN = unique(tou$RIN),
               DateStart = seq(as.IDate("2023-01-01"), as.IDate("2023-12-31"), by = "day"),
               TimeStart = seq(as.ITime("00:00"), as.ITime("23:00"), by = 3600))
  # Set Day to equal Monday = 1 through Sunday = 7
  prices[, DayType := wday(DateStart) - 1]
  prices[DayType == 0, DayType := 7]
  # Set Day to 8 on holidays
  prices[holidays, DayType := 8, on = .(DateStart = date)]

  p2 <- tou[prices, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
                      RatePlan_Url, RateType, Sector, API_Url, DateStart,
                      TimeStart, DayType, ValueName, Value, Unit),
            on = .(RIN = RIN, DateStart <= DateStart, DateEnd >= DateStart,
                   DayStart <= DayType, DayEnd >= DayType, TimeStart <= TimeStart, TimeEnd >= TimeStart)]


  # Create datetime fields and convert to UTC
  p2[, starttime := as.POSIXct(DateStart, time = TimeStart, tz = "America/Los_Angeles")]
  p2[, c("DateStart", "TimeStart") := IDateTime(lubridate::with_tz(starttime, tz = "UTC"))]
  p2[, c("DateEnd", "TimeEnd") := IDateTime(lubridate::with_tz(starttime + 3599, tz = "UTC"))]
  p2
}

p2 <- TOU_to_streaming(holiday_file, tou_file, start_date, end_date)


# Convert to XML for upload -------------------------------------------------
library(xml2)
# schema <- read_xml("./tests/MIDAS upload XSD.xsd")
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
for(i in 1:12) {
  vd <- xml_add_child(vi, "ValueData")

  xml_add_child(vd, "DateStart", as.character(p2[i, DateStart]))
  xml_add_child(vd, "DateEnd", as.character(p2[i, DateEnd]))
  xml_add_child(vd, "TimeStart", as.character(p2[i, TimeStart]))
  xml_add_child(vd, "TimeEnd", as.character(p2[i, TimeEnd]))
  xml_add_child(vd, "DayStart", p2[i, DayType])
  xml_add_child(vd, "DayEnd", p2[i, DayType])
  xml_add_child(vd, "ValueName", "price")
  xml_add_child(vd, "Value", p2[i, Value])
  xml_add_child(vd, "Unit", p2[i, Unit])
  print(i)
}

xml_validate(rate, schema)
write_xml(rate, file = "tests/Streaming_Test.xml", options =c("format"))

