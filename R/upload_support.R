# cecmidas R package
# Software that connects to the CEC MIDAS Rate API
# Copyright (C) 2023 Stefanie Wayland
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# Import data from CSV -----------------------------------------------

#' Holiday CSV import
#'
#' @description
#' Function to import CSV files that contain holiday information. CSV must contain the two columns
#' "holiday_name" and "date". The date column must be formatted unambiguously, YYYY-MM-DD is best.
#' Holiday dates should be in local time. For example, New Year's Day 2023 is 2023-01-01.
#'
#' @param holiday_file atomic character with the path to a CSV file with holiday dates and information
#'
#' @import data.table
#' @export
import_holidays <- function(holiday_file) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # check function arguments -------------------------------------------------
  checkmate::assert_file_exists(holiday_file, extension = "csv")
  holidays <- fread(holiday_file, colClasses = c("character", "IDate"))
  checkmate::assert_names(names(holidays), permutation.of = c("holiday_name", "date"))
  holidays
}

# Convert holidays to JSON for upload ----------------------------------------

#' Convert streaming rate to JSON for upload
#'
#' @description
#' Function to encode the data.table output of the TOU_to_streaming function
#' into JSON for upload to MIDAS.
#'
#' @param holidays data.frame or data.table like the ones created by the import_holidays function
#' @param energy_code atomic character two character MIDAS energy code for uploading company
#' @param energy_desc atomic character name of uploading energy company from Energy lookup table
#' @param prettify atomic logical TRUE to generate prettified JSON, FALSE by default
#'
#' @import data.table
#' @export
holidays_to_json <- function(holidays, energy_code, energy_desc, prettify = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }
  checkmate::assert_flag(prettify)
  checkmate::assert_data_frame(holidays)
  checkmate::assert_names(names(holidays), permutation.of = c("holiday_name", "date"))
  setDT(holidays)

  holidayinfo <- unique(holidays[, .(holiday_name, date)])
  setnames(holidayinfo, c("HolidayDescription", "DateOfHoliday"))
  holidayinfo[, EnergyCode := energy_code]
  holidayinfo[, EnergyDescription := energy_desc]
  # Build list to convert to JSON
  rtl <- vector("list", length = nrow(holidayinfo))
  for (rt in seq_along(rtl)) {
    rtl[[rt]] <- c(as.list(holidayinfo[rt]))
  }

  hol <- jsonlite::toJSON(rtl, dataframe = "rows", auto_unbox = TRUE, pretty = prettify)

  hol
}

#' TOU to streaming rate converter
#'
#' @description
#' Function to import CSV files that contain TOU rate information and convert
#' the TOU structure to a streaming structure.
#'
#' @param holidays data.frame of holiday dates and information from import_holidays()
#' @param tou_file atomic character with the path to a CSV file with TOU rate information
#' @param start_date atomic character or Date for the beginning of the period the streaming rate should cover
#' @param end_date atomic character or Date for the end of the period the streaming rate should cover
#' @param time_zone atomic character the time zone where the rate is offered. This should be one of the time zones listed in OlsonNames(). Dates and times in the CSV are assumed to be in the specified time zone.
#'
#' @importFrom lubridate with_tz
#' @import data.table
#' @export
TOU_to_streaming <- function(holidays, tou_file, start_date,
                             end_date, time_zone = "America/Los_Angeles") {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop(
      "Package \"lubridate\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # check function arguments -------------------------------------------------
  checkmate::assert_data_frame(holidays)
  checkmate::assert_names(names(holidays), permutation.of = c("holiday_name", "date"))
  checkmate::assert_file_exists(tou_file, extension = "csv")
  tryCatch(start_date <- as.IDate(start_date),
           error = function(e) stop("start_date must be a date formatted like 'YYYY-MM-DD'"))
  tryCatch(end_date <- as.IDate(end_date),
           error = function(e) stop("end_date must be a date formatted like 'YYYY-MM-DD'"))
  checkmate::assert_date(start_date, any.missing = FALSE, len = 1)
  checkmate::assert_date(end_date, any.missing = FALSE, len = 1)
  checkmate::assert_choice(time_zone, OlsonNames())

  # read in tou data and check column names ----------------------
  tou <- fread(tou_file, colClasses = c(DateStart = "IDate", DateEnd = "IDate",
                                        TimeStart = "ITime", TimeEnd = "ITime"))
  checkmate::assert_names(names(tou),
                          permutation.of = c("RIN", "AltRateName1", "AltRateName2", "SignupCloseDate", "RateName",
                                             "RatePlan_Url", "RateType", "Sector", "API_Url", "EndUse", "DateStart",
                                             "DateEnd", "DayStart", "DayEnd", "TimeStart", "TimeEnd", "ValueName",
                                             "Value", "Unit"))

  # Create table of prices for all hours of the year for each rate -----------
  prices <- CJ(RIN = tou$RIN,
               DateStart = seq(as.IDate(start_date), as.IDate(end_date), by = "day"),
               TimeStart = seq(as.ITime("00:00"), as.ITime("23:00"), by = 3600),
               unique = TRUE)
  prices <- prices[unique(tou[, .(RIN, Unit)]), on = "RIN", allow.cartesian = TRUE]
  # Set Day to equal Monday = 1 through Sunday = 7
  prices[, DayType := wday(DateStart) - 1]
  prices[DayType == 0, DayType := 7]
  # Set Day to 8 on holidays
  prices[holidays, DayType := 8, on = .(DateStart = date)]

  p2 <- tou[prices, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
                      RatePlan_Url, RateType, Sector, API_Url, DateStart,
                      TimeStart, DayType, ValueName, Value, Unit),
            on = .(RIN = RIN, Unit = Unit, DateStart <= DateStart, DateEnd >= DateStart,
                   DayStart <= DayType, DayEnd >= DayType, TimeStart <= TimeStart, TimeEnd >= TimeStart)]


  # Create datetime fields and convert to UTC
  p2[, starttime := as.POSIXct(DateStart, time = TimeStart, tz = time_zone)]
  p2[, c("DateStart", "TimeStart") := IDateTime(lubridate::with_tz(starttime, tz = "UTC"))]
  # Drop hours due to DST changes
  # p2 <- unique(p2, by = c("RIN", "Unit", "DateStart", "TimeStart"))
  p2[, c("DateEnd", "TimeEnd") := IDateTime(lubridate::with_tz(starttime + 3599, tz = "UTC"))]
  p2[, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
         RatePlan_Url, RateType, Sector, API_Url, DateStart,
         TimeStart, DateEnd, TimeEnd, DayStart = DayType, DayEnd = DayType, ValueName, Value, Unit)]
}

# Streaming rate upload -----------------------------------------------

# csv_to_streaming <- function(holidays, stream_file, start_date,
#                              end_date, time_zone = "America/Los_Angeles") {
#   if (!requireNamespace("lubridate", quietly = TRUE)) {
#     stop(
#       "Package \"lubridate\" must be installed to use this function.",
#       call. = FALSE
#     )
#   }
#   if (!requireNamespace("data.table", quietly = TRUE)) {
#     stop(
#       "Package \"data.table\" must be installed to use this function.",
#       call. = FALSE
#     )
#   }
#
#   # check function arguments -------------------------------------------------
#   checkmate::assert_data_frame(holidays)
#   checkmate::assert_names(names(holidays), permutation.of = c("holiday_name", "date"))
#   checkmate::assert_file_exists(stream_file, extension = "csv")
#   tryCatch(start_date <- as.IDate(start_date),
#            error = function(e) stop("start_date must be a date formatted like 'YYYY-MM-DD'"))
#   tryCatch(end_date <- as.IDate(end_date),
#            error = function(e) stop("end_date must be a date formatted like 'YYYY-MM-DD'"))
#   checkmate::assert_date(start_date, any.missing = FALSE, len = 1)
#   checkmate::assert_date(end_date, any.missing = FALSE, len = 1)
#   checkmate::assert_choice(time_zone, OlsonNames())
#
#   # read in stream data and check column names ----------------------
#   stream <- fread(stream_file, colClasses = c(DateStart = "IDate", DateEnd = "IDate",
#                                            TimeStart = "ITime", TimeEnd = "ITime"))
#   checkmate::assert_names(names(stream),
#                           permutation.of = c("RIN", "AltRateName1", "AltRateName2", "SignupCloseDate", "RateName",
#                                              "RatePlan_Url", "RateType", "Sector", "API_Url", "EndUse", "DateStart",
#                                              "DateEnd", "DayStart", "DayEnd", "TimeStart", "TimeEnd", "ValueName",
#                                              "Value", "Unit"))
#
#   # Create table of prices for all hours of the year for each rate -----------
#   prices <- CJ(RIN = stream$RIN,
#                DateStart = seq(as.IDate(start_date), as.IDate(end_date), by = "day"),
#                TimeStart = seq(as.ITime("00:00"), as.ITime("23:00"), by = 3600),
#                unique = TRUE)
#   prices <- prices[unique(stream[, .(RIN, Unit)]), on = "RIN", allow.cartesian = TRUE]
#   # Set Day to equal Monday = 1 through Sunday = 7
#   prices[, DayType := wday(DateStart) - 1]
#   prices[DayType == 0, DayType := 7]
#   # Set Day to 8 on holidays
#   prices[holidays, DayType := 8, on = .(DateStart = date)]
#
#   p2 <- stream[prices, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
#                       RatePlan_Url, RateType, Sector, API_Url, DateStart,
#                       TimeStart, DayType, ValueName, Value, Unit),
#             on = .(RIN = RIN, Unit = Unit, DateStart <= DateStart, DateEnd >= DateStart,
#                    DayStart <= DayType, DayEnd >= DayType, TimeStart <= TimeStart, TimeEnd >= TimeStart)]
#
#
#   # Create datetime fields and convert to UTC
#   p2[, starttime := as.POSIXct(DateStart, time = TimeStart, tz = time_zone)]
#   p2[, c("DateStart", "TimeStart") := IDateTime(lubridate::with_tz(starttime, tz = "UTC"))]
#   # Drop hours due to DST changes
#   # p2 <- unique(p2, by = c("RIN", "Unit", "DateStart", "TimeStart"))
#   p2[, c("DateEnd", "TimeEnd") := IDateTime(lubridate::with_tz(starttime + 3599, tz = "UTC"))]
#   p2[, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
#          RatePlan_Url, RateType, Sector, API_Url, DateStart,
#          TimeStart, DateEnd, TimeEnd, DayStart = DayType, DayEnd = DayType, ValueName, Value, Unit)]
# }

# Convert rate to JSON for upload ------------------------------------------------

#' Convert streaming rate to JSON for upload
#'
#' @description
#' Function to encode the data.table output of TOU_to_streaming() or csv_to_streaming()
#' into JSON for upload to MIDAS.
#'
#' @param DT data.table created by the TOU_to_streaming function
#' @param prettify atomic logical TRUE to generate prettified JSON, FALSE by default
#'
#' @import data.table
#' @export
rate_to_json <- function(DT, prettify = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }
  checkmate::assert_flag(prettify)
  checkmate::assert_data_table(DT)
  checkmate::assert_names(names(DT),
                          permutation.of = c("RIN", "AltRateName1", "AltRateName2",
                                             "SignupCloseDate", "RateName",  "RatePlan_Url",
                                             "RateType", "Sector", "API_Url", "DateStart",
                                             "TimeStart", "DateEnd", "TimeEnd", "DayStart",
                                             "DayEnd", "ValueName", "Value", "Unit"))

  rateinfo <- unique(DT[, .(RIN, AltRateName1, AltRateName2, SignupCloseDate, RateName,
                            RatePlan_Url, RateType, Sector, API_Url)])
  # Build list to convert to JSON
  rtl <- vector("list", length = nrow(rateinfo))
  for (rt in seq_along(rtl)) {
    rtl[[rt]] <- c(as.list(rateinfo[rt]),
                   list(ValueInformation = DT[RIN == rateinfo[rt, RIN],
                                              .(ValueName, DateStart, DateEnd,
                                                DayStart, DayEnd, TimeStart,
                                                TimeEnd, Value, Unit)]))
  }
  DemandData <- jsonlite::toJSON(rtl, dataframe = "rows", auto_unbox = TRUE, pretty = prettify)
# FIXME: Add newline at end of file
  DemandData
}


# Convert to XML for upload -------------------------------------------------

#' @title Only write non-missing XML blocks
#'
#' @description
#' Returns a string of xml containing the name and value when value is non-missing
#'
#' @param value A value to be evaluated for missingness
#' @param name A character string to be used as the xml tag
#'
#' @return A character string of xml
xml_non_na <- function(value, name) {
  if (!is.na(value)) {
    paste0("<", name, ">", value, "</", name, ">")
  }
}

#' @title Convert streaming rate to XML for upload
#'
#' @description
#' Function to encode the data.table output of the TOU_to_streaming function
#' into XML for upload to MIDAS.
#'
#' @param DT data.table created by the TOU_to_streaming function
#' @return An atomic character containing the rate data encoded as xml
#'
#' @import data.table
#' @export
#' @examples
#' holidays <- import_holidays(holiday_file)
#' DT <- TOU_to_streaming(holidays, tou_file, start_date, end_date)
#'
#' rate_to_xml(DT)
rate_to_xml <- function(DT) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }
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
                         paste0("<DayStart>", ratevalue[vd, DayStart], "</DayStart>"),
                         paste0("<DayEnd>", ratevalue[vd, DayEnd], "</DayEnd>"),
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
  DemandData
}

# Save XML or JSON encoded files for later upload -----------------------

#' Save JSON or XML encoded rate or JSON encoded holiday to a file
#'
#' @description
#' Function to save JSON or XML encoded rate to a file.
#' This also works to save encoded holidays to a file.
#'
#' @param encoded_rate atomic character created by rate_to_xml or rate_to_json
#' @param file_name atomic character with the path where you want to save the file
#'
#' @export
save_rate <- function(encoded_rate, file_name) {
  checkmate::assert_character(encoded_rate, len = 1)
  checkmate::assert_character(file_name, len = 1)
  writeChar(encoded_rate, file_name, eos = NULL)
}
