#' Converte un dataframe in timeseries
#'
#' Viene usato nella conversione dei dati dal db
#'
#' @name convert_data_frame
#' @usage convert_data_frame(df)
#' @param df data.frame con le serie storiche 
#' @return a List
#' @export

convert_data_frame <- function(df) {
  z <- list();
  tol = 0.01
  for(i in 1:nrow(df)) {
      row <- df[i, ]
      id <- row$id
      anno <- row$anno
      periodo <- row$periodo
      freq <- row$freq
      dati <- as.character(row$dati)
      nome <- row$name
      stock <- row$stock
      if(anno < tol  || periodo < tol || freq < tol) {
        json_data <- parseJSON(dati);
        
        z[[nome]]<- if (length(json_data) == 0) {
          numeric(0)
        } else {
          json_data
        }
      } else {	
         serie <- ts(json_data, start=c(anno, periodo), frequency=freq)
         attr(serie, 'stock') <- stock
         attr(serie, 'name') <- nome
         z[[nome]] = serie
      }
  }   
  z;
}