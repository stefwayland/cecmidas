#' MIDAS R6 class that connects to the MIDAS Rate API
#'
#' @description
#' This class holds all the necessary information to maintain a connection to
#' the API. It automatically retrieves a log-in token when necessary.
#'
#' @export
MIDAS <- R6::R6Class("MIDAS",
                    public = list(

                      #' @field base URL for SGIP API
                      midas_url = "https://midasapi.energy.ca.gov",

                      #' @description
                      #' Create a new MIDAS connection
                      #' @param username atomic character Username for login
                      #' @param password atomic character Password for login
                      #' @param email atomic character Email address for registration. Required if registering a new account
                      #' @param organization atomic character organization name for registration. Not required. Only used when registering a new account.
                      #' @return a new `MIDAS` connection object.
                      initialize = function(username,
                                            password,
                                            email = NA_character_,
                                            fullname = NA_character_,
                                            organization = NA_character_) {
                        checkmate::check_character(username, len = 1, any.missing = FALSE)
                        checkmate::check_character(password, len = 1, any.missing = FALSE)
                        checkmate::check_character(fullname, len = 1, any.missing = FALSE)
                        checkmate::check_character(email, len = 1, any.missing = FALSE)
                        checkmate::check_character(organization, len = 1)
                        private$username <- username
                        private$password <- password
                        private$email <- email
                        private$fullname <- fullname
                        private$organization <- organization
                      },

                      #' @description
                      #' Print some important characteristics of the SGIP object
                      print = function() {
                        print("MIDAS rate API service")
                        print(paste("username:", private$username))
                        print(paste("token:", private$token))
                        print(paste("token age:", format(Sys.time() - private$token_dt, units = "secs"), "seconds"))
                        print(paste("data format:", private$data_format))
                      },

                      #' @title Set preferred response data format
                      #' @description Set the preferred data format for responses from value requests. The default for cecmidas is JSON, but the MIDAS API will return either json or xml.
                      #' @param format atomic character One of "json" or "xml"
                      set_data_format = function(format) {
                        checkmate::assert_choice(format, c("json", "xml"))
                        private$data_format <- format
                        invisible(self)
                      },

                      #' @title Register a username and password
                      #' @description This function registers a username and password. The CEC requires that you be registered and respond to an authentication email before obtaining an access token. You should only have to do this once.
                      #' @param ... Additional parameters passed to curl
                      register = function(...) {
                        if (is.na(private$organization)) {
                          qy <- list(fullname = jsonlite::base64_enc(private$fullname),
                                     username = jsonlite::base64_enc(private$username),
                                     password = jsonlite::base64_enc(private$password),
                                     emailaddress = jsonlite::base64_enc(private$email))
                        } else {
                          qy <- list(fullname = jsonlite::base64_enc(private$fullname),
                                     organization = jsonlite::base64_enc(private$organization),
                                     username = jsonlite::base64_enc(private$username),
                                     password = jsonlite::base64_enc(private$password),
                                     emailaddress = jsonlite::base64_enc(private$email))
                        }

                        cli <- crul::HttpClient$new(self$midas_url, opts = list(...))
                        res <- cli$post(path = "api/registration", body = qy, encode = "json")
                        res$raise_for_status()
                        print(res$parse("UTF-8"))
                        if (res$status == 200) {
                          print("Look in your email for an authentication link. You can proceed using the API once you prove ownership of the email address.")
                        }
                      },

                      #' @title Get a token to authenticate data requests
                      #' @description
                      #' Get a token. This will usually be automatic when using other functions.
                      #' MIDAS tokens are valid for 10 minutes according to the CEC documentation.
                      #' @param new_token logical Force requesting a new token, FALSE by default
                      #' @param ... additional parameters passed to curl
                      get_token = function(new_token = FALSE, ...) {
                        checkmate::assert_logical(new_token, len = 1, any.missing = FALSE)
                        if (is.na(private$token_dt) | isTRUE(Sys.time() - private$token_dt >= 600) | new_token) {
                          cli <- crul::HttpClient$new(self$midas_url, opts = list(...),
                                                      headers = list(Authorization = paste("Basic", jsonlite::base64_enc(paste(private$username, private$password, sep = ":")))))
                          res <- cli$get(path = "api/Token")
                          res$raise_for_status()
                          private$token <- res$response_headers$token
                          private$token_dt <- Sys.time()
                        }
                        invisible(self)
                      },

                      #' @title Get value data from MIDAS
                      #' @description
                      #' Get real-time or current and forecast rate information
                      #' @param rin character Single RIN including dashes to request
                      #' @param query_type character One of "realtime" or "alldata"
                      #' @param new_token logical Force requesting a new token, FALSE by default
                      #' @param ... additional parameters passed to curl
                      value = function(rin,
                                       query_type = "realtime",
                                       response_encoding = private$data_format,
                                       new_token = FALSE,
                                       ...) {
                        checkmate::assert_character(rin, any.missing = FALSE, len = 1)
                        checkmate::assert_choice(query_type, c("realtime", "alldata"))
                        checkmate::assert_choice(response_encoding, c("json", "xml"))
                        self$get_token(new_token = new_token, ...)
                        cli <- crul::HttpClient$new(
                          self$midas_url,
                          headers = list(Accept = paste0("application/", private$data_format),
                                         Authorization = paste("Bearer", private$token)),
                          opts = list(...)
                        )
                        res <- cli$get(path = "api/valuedata",
                                       query = list(id = rin,
                                                    querytype = query_type))
                        res$raise_for_status()
                        res$raise_for_ct_json()
                        jsonlite::fromJSON(res$parse("UTF-8"))
                      },

                      #' @title Get lookup table data from MIDAS
                      #' @description
                      #' Get lookup table information. Possible lookup tables are currently Country, Daytype, Distribution, Enduse, Energy, Location, Ratetype, Sector, State, and TimeZone.
                      #' @param table_name atomic character lookup table name.
                      #' @param new_token logical Force requesting a new token, FALSE by default
                      #' @param ... additional parameters passed to curl
                      lookups = function(table_name,
                                         response_encoding = private$data_format,
                                         new_token = FALSE,
                                         ...) {
                        checkmate::check_choice(table_name, c())
                        self$get_token(new_token = new_token, ...)
                        if (is.na(table_name)) stop("Must provide lookup table name.")
                        if (!response_encoding %in% c("json", "xml")) stop("response_encoding must be either 'json' or 'xml'")
                        cli <- crul::HttpClient$new(
                          self$midas_url,
                          headers = list(Accept = paste0("application/", private$data_format),
                                         Authorization = paste("Bearer", private$token)),
                          opts = list(...)
                        )
                        res <- cli$get(path = "api/valuedata",
                                       query = list(LookupTable = table_name))
                        res$raise_for_status()
                        if (response_encoding == "xml") {

                        } else {
                          res$raise_for_ct_json()
                          return(jsonlite::fromJSON(res$parse("UTF-8")))
                        }
                      }
                    ),
                    active = list(

                    ),
                    private = list(
                      username = NA,
                      password = NA,
                      email = NA,
                      fullname = NA,
                      organization = NA,
                      token = NA,
                      token_dt = NA,
                      data_format = "json"
                    )
)
