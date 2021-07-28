source("R/midas.R")
m <- MIDAS$new(username = "<user name>",
               password = "<password>",
               email = "<email>",
               fullname = "<full name>",
               organization = "<organization>")
# m$register()
m$get_token()
m$value(rin = "USCA-PGPG-EV2A-0000", query_type = "realtime", verbose = TRUE)
m$lookups(table_name = "TimeZone", verbose = TRUE)
