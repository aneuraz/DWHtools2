#' oracleQuery
#'
#' This function performs SQL queries to a database and returns the results.
#'
#' @param query SQL query in a character string
#' @param configFile location of the config file (default = 'config')
#' @param update set to `TRUE` if the query is an update. (default = `FALSE`)
#' @param data set to `TRUE` if the query returns data (default = `TRUE`)
#' @return the result of the query (data.frame)
#' @export
oracleQuery <- function(query, config = config, update = F, data = T) {


  result <- list()
  i = 1
  # set connection
  jdbcDriver <- RJDBC::JDBC(driverClass=config$driverClass, classPath=config$classPath)
  jdbcConnection <- RJDBC::dbConnect(jdbcDriver, config$connectPath, config$dbuser, config$dbpass)

  # query
  if (update) {
    result <- RJDBC::dbSendUpdate(jdbcConnection, query)
  } else {
    res<- RJDBC::dbSendQuery(jdbcConnection, query)
    #res <- dbFetch(res)

    if (data) {
      while(nrow(chunk <- DBI::dbFetch(res, n = 30000)) > 0 ) {
        result[[i]] <- chunk
        i <- i +1
      }

      result<- plyr::rbind.fill(result)

    }

    RJDBC::dbClearResult(res)
  }

  # disconnection
  RJDBC::dbDisconnect(jdbcConnection)
  #print(result)
  if (data) {
    if (!is.null(result)) {
      names(result) <- toupper(names(result))
    }
    return(result)
  }

}


