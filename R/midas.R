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
                      midas_url = "https://cecwats2.org/api/",

                      #' @description
                      #' Create a new MIDAS connection
                      #' @param username atomic character Username for login
                      #' @param password atomic character Password for login
                      #' @param email atomic character Email address for registration. Required if registering a new account
                      #' @param organization atomic character organization name for registration. Not required. Only used when registering a new account.
                      #' @return a new `MIDAS` connection object.
                      initialize = function(username,
                                            password,
                                            email = NA,
                                            fullname = NA,
                                            organization = NA) {
                        stopifnot(is.character(username), length(username) == 1)
                        stopifnot(is.character(password), length(password) == 1)
                        stopifnot(is.character(fullname), length(fullname) == 1)
                        stopifnot(is.character(email), length(email) == 1)
                        stopifnot(is.character(organization), length(organization) == 1)
                        private$username <- username
                        private$password <- password
                        private$email <- email
                        private$fullname <- fullname
                        private$organization <- organization
                      },

                      #' @description
                      #' Print the SGIP object
                      print = function() {
                        print("MIDAS rate API service")
                        print(paste("username:", private$username))
                        print(paste("token:", private$token))
                        print(paste("token age:", Sys.time() - private$token_dt))
                      },

                      #' @title Register a username and password
                      #' @description You will only have to do this once. This function registers a username and password. WattTime requires that you be registered to obtain an access token.
                      #' @param ... additional parameters passed to curl
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
                        res$raise_for_ct_json()
                        js <- jsonlite::fromJSON(res$parse("UTF-8"))

                        if (js$ok == "User created") {
                          print(paste("Success: User", js$user, "created!"))
                          return(invisible(self))
                        } else if (!is.null(js$error)) {
                          stop(js$error)
                        }
                      },

                      #' @description
                      #' Get a token. This will usually be automatic when using other functions. MIDAS tokens are valid for 10 minutes according to the documentation.
                      #' @param ... additional parameters passed to curl
                      get_token = function(...) {
                        cli <- crul::HttpClient$new(self$midas_url, opts = list(...),
                                                    headers = list(Authorization = paste("Basic", jsonlite::base64_enc(paste(private$username, private$password, sep = ":")))))
                        res <- cli$get(path = "api/Token")
                        res$raise_for_status()
                        private$token <- res$response_headers$token
                        private$token_dt <- Sys.time()
                        invisible(self)
                      }#,

                      #' @description
                      #' Reset or change login password
                      #' @param ... additional parameters passed to curl
                      # reset_password = function(...) {
                      #   cli <- crul::HttpClient$new(self$midas_url, opts = list(...))
                      #   res <- cli$get(path = "password",
                      #                  query = list(username = private$username),
                      #                  encode = "json")
                      #   res$raise_for_status()
                      #   res$raise_for_ct_json()
                      #   js <- jsonlite::fromJSON(res$parse("UTF-8"))
                      #
                      #   if (!is.null(js$ok)) {
                      #     print(js$ok)
                      #     return(invisible(self))
                      #   } else if (!is.null(js$error)) {
                      #     stop(js$error)
                      #   }
                      # },

                      #' @description
                      #' Get real time or historical rate information
                      #' @param ba atomic character one of the balancing authority abbreviations
                      #' @param starttime atomic POSIXt may be omitted if endtime is also omitted. Character is converted to POSIX in the system time zone
                      #' @param endtime atomic POSIXt optional, if omitted, defaults to current time. Character is converted to POSIX in the system time zone
                    #   price = function(ba = NA, starttime = NA, endtime = NA, ...) {
                    #     if(is.na(token) | isTRUE(Sys.time() - private$token_dt > 1800)) self$login()
                    #     if (is.na(ba)) ba <- private$ba
                    #     if (is.na(ba)) stop("Provide balancing authority (ba) either in function or as a parameter when creating a new SGIP object.")
                    #     if (is.character(starttime)) starttime <- as.POSIXct(starttime)
                    #     if (is.character(endtime)) endtime <- as.POSIXct(endtime)
                    #     stopifnot(ba %in% self$ba_list)
                    #     cli <- crul::HttpClient$new(
                    #       self$midas_url,
                    #       headers = list(Authorization = paste("Bearer", private$token)),
                    #       opts = list(...)
                    #     )
                    #     res <- cli$get(path = "sgipmoer",
                    #                    body = list(ba = private$ba,
                    #                                starttime = format(starttime, "%FT%T%z"),
                    #                                endtime = format(endtime, "%FT%T%z")),
                    #                    encode = "json")
                    #     res$raise_for_status()
                    #     res$raise_for_ct_json()
                    #     js <- jsonlite::fromJSON(res$parse("UTF-8"))
                    #   }
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
                      token_dt = NA
                    )
)
