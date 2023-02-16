source("R/midas.R")
m <- MIDAS$new(username = "stest",
               password = "Pantseveninghero1-",
               email = "<email>",
               fullname = "<full name>",
               organization = "<organization>")
# m$register()
crul::set_verbose()
m$get_token(new_token = TRUE)
m$value(rin = "USCA-TSTS-HTOU-0000", query_type = "realtime", verbose = TRUE)
m$lookups(table_name = "TimeZone", verbose = TRUE)
m$holiday()
m$upload_rate(filename = "./tests/MIDAS_Test_Rate_24TOU.xml", verbose = TRUE)
m$upload_holiday(filename = "./tests/MIDAS_Test_Holidays.xml", verbose = TRUE)
