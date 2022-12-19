source("R/midas.R")
               email = "<email>",
               fullname = "<full name>",
               organization = "<organization>")
# m$register()
m$get_token(new_token = TRUE)
m$value(rin = "USCA-TSTS-HTOU-0000", query_type = "realtime", verbose = TRUE)
m$lookups(table_name = "TimeZone", verbose = TRUE)
m$upload_rate(filename = "./tests/MIDAS_Test_Rate_24TOU.xml")
