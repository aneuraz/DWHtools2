#' api_get_token
#' @param base_url: url for the api
#' @param login
#' @pwd
#' @return jwt token
#' @export
api_get_token <- function(base_url, username, password) {

  r <- httr::POST(stringr::str_interp("${base_url}/login"),
            encode = "json",
            body = list(username = username, password=password))

  status = httr::http_status(r)

  if (status$category != 'Success') {
    stop(status$message)
  }

  res <- httr::content(r, as = 'text')
  res <- jsonlite::fromJSON(res)

  res$access_token

}

#' api_query
#' @param base_url: url for the api
#' @param endpoint
#' @param token: jwt token (from get_token)
#' @param query_body: named list of parameters for the query
#' @return result of the query (assumes a json format for the result)
#' @export
api_query <- function(base_url,endpoint, token, query_body = list(), METHOD = httr::POST) {

  r <- METHOD(stringr::str_interp("${base_url}/${endpoint}"),
                  httr::add_headers(c(Authorization = stringr::str_interp("Bearer ${token}"))),
                  encode = "json",
                  body = query_body)

  print(httr::http_status(r))

  res <- httr::content(r, as = 'text')
  res <- jsonlite::fromJSON(res)
  res

}

#' api_match_patients
#' @export
api_match_patients <- function(base_url, token, num, num_type, birth_range, concepts_range, n_match, match_save=FALSE, match_save_title=NULL) {

  api_query(base_url, "match_patients_from_num",token, query_body = list(num = num,
                                                               num_type = num_type,
                                                               birth_range = birth_range,
                                                               concepts_range = concepts_range,
                                                               n_match = n_match,
                                                               match_save = match_save,
                                                               match_save_title=match_save_title) )

}

#' api_get_patients
#' @return a num_temp pointing to the list of patients matchted
#' @export
api_get_patients <- function(base_url, token, num, num_type, only_num = FALSE, count = FALSE) {

  api_query(base_url, "patients",token, query_body = list(num = num,
                                                          num_type = num_type,
                                                          only_num = only_num,
                                                          count = count) )

}

#' api_get_concepts
#' @export
api_get_concepts <- function(base_url, token, num, num_type) {

  api_query(base_url, "concepts",token, query_body = list(num = num,
                                                          num_type = num_type ))

}

#' api_get_data
#' @export
api_get_data <- function(base_url, token, num, num_type, data_type) {
  library(dplyr)
  res <- api_query(base_url, "data",token, query_body = list(num = num,
                                                      num_type = num_type,
                                                      data_type = data_type))

  if (data_type == 'bio_num') {
    res %>% mutate(VAL_NUMERIC = as.numeric(stringr::str_replace(VAL_NUMERIC,',','.')),
                   LOWER_BOUND = as.numeric(stringr::str_replace(LOWER_BOUND,'\\,','\\.')),
                   UPPER_BOUND = as.numeric(stringr::str_replace(UPPER_BOUND,'\\,','\\.')))
  } else {
    res
  }

}

#' api_get_num_temps
#' @export
api_get_num_temps <- function(base_url, token) {

  api_query(base_url,"numtemps", token, METHOD= httr::GET)

}

#' api_get_cohorts
#' @export
api_get_cohorts <- function(base_url, token) {

  res <- api_query(base_url,"cohorts_list", token, query_body=list(only_num=FALSE))

}
