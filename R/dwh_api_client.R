#' api_get_token
#' @param base_url: url for the api
#' @param login
#' @pwd
#' @return jwt token
#' @export
api_get_token <- function(base_url, login, pwd) {

  r <- httr::POST(stringr::str_interp("${base_url}/login"),
            encode = "form",
            body = list(login = login, pwd=pwd))

  res <- httr::content(r, as = 'text')
  res <- jsonlite::fromJSON(res)
  if(class(res) == 'character') {
    stop(stringr::str_interp('authentication failed: ${res}'))
  }
  res$id_token

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
                  httr::add_headers(c(Authorization = token)),
                  encode = "json",
                  body = query_body)

  res <- httr::content(r, as = 'text')
  res <- jsonlite::fromJSON(res)
  res

}

#' api_match_patients
#' @export
api_match_patients <- function(base_url, token, num, num_type, annee_range, count_range, n_match) {

  api_query(base_url, "matchPatients",token, query_body = list(num = num,
                                                               num_type = num_type,
                                                               annee_range = annee_range,
                                                               count_range = count_range,
                                                               n_match = n_match) )

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
                   BORNE_SUP = as.numeric(stringr::str_replace(BORNE_SUP,'\\,','\\.')),
                   BORNE_INF = as.numeric(stringr::str_replace(BORNE_INF,'\\,','\\.')),
                   INF = as.numeric(INF),
                   SUP = as.numeric(SUP))
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

  res <- api_query(base_url,"cohorts", token, METHOD= httr::GET)

}
