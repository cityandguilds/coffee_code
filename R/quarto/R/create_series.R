#' Create series from a date
#'
#' @param dt A date
#'
#' @return A series as a character vector
#' @export
#'
#' @examples
#' x <- c("2023-02-04", "2024-04-23", "2024-05-23", "2022-11-02")
#' as.Date(x) |> create_series("seasons")
#' #' as.Date(x) |> create_series("months")
#' #' as.Date(x) |> create_series("synoptic")
create_series <-
  function(dt){
    
    out <- base::paste(dplyr::case_when(lubridate::month(dt)>=1 & lubridate::month(dt)<=4 ~ "Spring",
                                     lubridate::month(dt)>=5 & lubridate::month(dt)<=8 ~ "Summer",
                                     lubridate::month(dt)>=9 & lubridate::month(dt)<=12 ~ "Autumn"),
                    lubridate::year(dt))

    return(out)
  }
