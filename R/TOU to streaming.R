# TOU to XML converter

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
  ## TODO: Consider adding Unit to unique query in case of time-varying demand charges or inclusion of other units in TOU rates
  prices <- CJ(RIN = unique(tou$RIN),
               DateStart = seq(as.IDate(start_date), as.IDate(end_date), by = "day"),
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
  # Drop hours due to DST changes
  # p2 <- unique(p2, by = c("RIN", "Unit", "DateStart", "TimeStart"))
  p2[, c("DateEnd", "TimeEnd") := IDateTime(lubridate::with_tz(starttime + 3599, tz = "UTC"))]
  p2
}


# Convert to XML for upload -------------------------------------------------

xml_non_na <- function(value, name) {
  if(!is.na(value)) {
    paste0("<", name, ">", value, "</", name, ">")
  }
}

rate_to_xml <- function(DT, file_name) {
  rateinfo <- unique(DT[, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
                            RatePlan_Url, RateType, Sector, API_Url)])
  DemandData <- vector(mode = "list", length = nrow(rateinfo))
  for(rt in 1:nrow(rateinfo)) {
    RateInformation <- paste0(
      xml_non_na(rateinfo[rt, RIN], "RateID"),
      xml_non_na(rateinfo[rt, AltRateName1], "AltRateName1"),
      xml_non_na(rateinfo[rt, AltRateName2], "AltRateName2"),
      xml_non_na(rateinfo[rt, SignupCloseDate], "SignupCloseDate"),
      xml_non_na(rateinfo[rt, RateName], "RateName"),
      xml_non_na(rateinfo[rt, RatePlan_Url], "RatePlan_Url"),
      xml_non_na(rateinfo[rt, RateType], "RateType"),
      xml_non_na(rateinfo[rt, Sector], "Sector"),
      xml_non_na(rateinfo[rt, API_Url], "API_Url")
    )
    ratevalue <- DT[RIN == rateinfo[rt, RIN]]
    ValueInformation <- vector(mode = "list", length = nrow(ratevalue))
    for (vd in 1:length(ValueInformation)) {
      ValueData <- paste("<ValueData>",
                         paste0("<DateStart>", ratevalue[vd, DateStart], "</DateStart>"),
                         paste0("<DateEnd>", ratevalue[vd, DateEnd], "</DateEnd>"),
                         paste0("<TimeStart>", ratevalue[vd, TimeStart], "</TimeStart>"),
                         paste0("<TimeEnd>", ratevalue[vd, TimeEnd], "</TimeEnd>"),
                         paste0("<DayStart>", ratevalue[vd, DayType], "</DayStart>"),
                         paste0("<DayEnd>", ratevalue[vd, DayType], "</DayEnd>"),
                         paste0("<ValueName>", ratevalue[vd, ValueName], "</ValueName>"),
                         paste0("<Value>", ratevalue[vd, Value], "</Value>"),
                         paste0("<Unit>", ratevalue[vd, Unit], "</Unit>"),
                         "</ValueData>",
                         sep = "\n"
      )
      ValueInformation[[vd]] <- ValueData
    }
    ValueInformation <- paste("<ValueInformation>", paste(ValueInformation, collapse = "\n"), "</ValueInformation>", sep = "\n")
    RateInformation <- paste("<RateInformation>", RateInformation, ValueInformation, "</RateInformation>", sep = "\n")
    DemandData[[rt]] <- RateInformation
  }
  DemandData <- paste("<?xml version='1.0' encoding='UTF-8'?>",
                      "<DemandData>", paste(DemandData, collapse = "\n"), "</DemandData>", "\n", sep = "\n")

  writeChar(DemandData, file_name, eos = NULL)
}

