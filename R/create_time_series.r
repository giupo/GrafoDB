#' Converte un dataframe in timeseries
#'
#' It's used for converting data coming from the DB
#'
#' @name convert_data_frame
#' @usage convert_data_frame(df)
#' @param df data.frame with time series
#' @return a List
#' @export

convert_data_frame <- function(df) {
  z <- list();
  tol <- 0.01
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    id <- row$id
    anno <- row$anno
    periodo <- row$periodo
    freq <- row$freq
    dati <- as.character(row$dati)
    nome <- row$name
    stock <- row$stock
    json_data <- jsonlite::fromJSON(dati)
    if (anno < tol || periodo < tol || freq < tol) {
      z[[nome]] <- if (length(json_data) == 0) {
        numeric(0)
      } else {
        json_data
      }
    } else {
      serie <- stats::ts(json_data,
        start = c(anno, periodo), frequency = freq)

      attr(serie, "stock") <- stock
      attr(serie, "name") <- nome
      z[[nome]] <- serie
    }
  }

  z
}
