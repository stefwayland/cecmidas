source("R/midas.R")
m <- MIDAS$new(username = "<username>",
               password = "<password>",
               email = "<email>",
               fullname = "<full name>",
               organization = "<organization>")
# m$register()
crul::set_verbose()
m$get_token(new_token = TRUE)
m$midas_url = "http://midasapi.energy.ca.gov"
m$value(rin = "USCA-TSTS-HTOU-0000", query_type = "realtime", verbose = TRUE)
m$lookups(table_name = "Location", verbose = TRUE)
m$holiday()

# Test classic TOU upload
m$upload_rate(filename = "./tests/MIDAS_Test_Rate_24TOU.xml", verbose = TRUE)

# Test streaming rate upload with two rates
m$upload_rate(filename = "./tests/XML_upload.xml", verbose = TRUE)

# Test holiday upload
m$upload_holiday(filename = "./tests/MIDAS_Test_Holidays.xml", verbose = TRUE)
