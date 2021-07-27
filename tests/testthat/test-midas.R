source("R/midas.R")
m <- MIDAS$new(username = "<user name>",
               password = "<password>",
               email = "<email>",
               fullname = "<full name>",
               organization = "<organization>")
m$register()
m$get_token()
