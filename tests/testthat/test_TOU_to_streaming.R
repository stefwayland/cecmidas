# test_TOU_to_streaming.R
source("R/TOU to streaming.R")

holiday_file <- "./tests/Holiday_test.csv"
tou_file <- "./tests/TOU_test.csv"
start_date <- "2023-03-11"
end_date <- "2023-03-12"

p2 <- TOU_to_streaming(holiday_file, tou_file, start_date, end_date)
rate_to_xml(p2, file_name = "./tests/XML_streaming_upload.xml")
