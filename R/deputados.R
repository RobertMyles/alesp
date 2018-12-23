#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom stringi stri_trans_general
#' @export
deputados_current <- function(){

  url <- "https://www.al.sp.gov.br/alesp/deputados-estaduais/"

  df <- read_html(url) %>%
    html_table() %>%
    .[[2]] %>%
    as_tibble() %>%
    mutate(`Área de Atuação` = gsub("([a-z])([A-Z])", "\\1 \\2",
                                    `Área de Atuação`),
           `Base Eleitoral` =  gsub("([a-z])([A-Z])", "\\1 \\2",
                                    `Base Eleitoral`)) %>%
    mutate_at(1:3, stringi::stri_trans_general, "Latin-ASCII")

  colnames(df) <- stringi::stri_trans_general(colnames(df), "Latin-ASCII")
  return(df)
}
