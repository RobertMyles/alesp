#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom stringi stri_trans_general
#' @importFrom magrittr "%>%"
#' @importFrom partycodesbr tse_codes
#' @param legislature Integer starting from 1. See Notes.
#' @note For the legislature parameter, 1 represents the first legislature
#' recorded, which sat during the  period of 1947-1951. Following numbers then
#' represent the other legislatures in sequence. As of late 2018, the latest
#' available is 18.
#' @export
deputados <- function(legislature = NULL){

  if(is.null(legislature)) {
    url <- "https://www.al.sp.gov.br/alesp/deputados-estaduais/"
  } else {
    base <- "https://www.al.sp.gov.br/alesp/deputados-estaduais"
    one <- "?filtroNome=&filtroAreaAtuacao=&filtroBaseEleitoral=&filtroPartido=&"
    lg <- paste0("filtroLegislatura=", legislature)
    two <- "&filtroEmExercicio=on&filtroEmExercicioPesquisa=S&filtroLegislaturaAtual=N"
    url <- paste0(base, one, lg, two)
  }

  df <- read_html(url) %>%
    html_table() %>%
    .[[2]] %>%
    as_tibble()

  colnames(df) <- stringi::stri_trans_general(colnames(df), "Latin-ASCII")
  return(df)
}


#' @param name Legislator name.
#' @param area Legislative area in which the deputy operates e.g. "Area Fiscal".
#' @param party Legislator's political party. These are numerical codes from the
#' Tribunal Superior Eleitoral (TSE). These may be seen with the \code{party_codes()}
#' function.
#' @param legislature Integer starting from 1. See Notes.
#' @param webpage In an interactive session, prints a HTML page of deputy info.
#' @note For the legislature parameter, 1 represents the first legislature
#' recorded, which sat during the  period of 1947-1951. Following numbers then
#' represent the other legislatures in sequence. As of late 2018, the latest
#' available is 18.
#' @export
deputado <- function(name = NULL, area = NULL, party = NULL,
                     electoral_base = NULL, legislature = NULL,
                     webpage = TRUE){
  name <- str_replace_all(name, " ", replacement = "+")

  base <- "https://www.al.sp.gov.br/alesp/deputados-estaduais/?"
  nm <- paste0("filtroNome=", name)
  ar <- paste0("&filtroAreaAtuacao=", area)
  be <- paste0("&filtroBaseEleitoral=", electoral_base)
  pt <- paste0("&filtroPartido=", party)
  lg <- paste0("&filtroLegislatura=", legislature)
  other <- "&filtroEmExercicio=on&filtroEmExercicioPesquisa=S&filtroLegislaturaAtual=S"

  url <- paste0(base, nm, ar, be, pt, lg, other)

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

  if(!is.null(legislature) | !interactive()){
    if(legislature < 15) webpage <- FALSE
  }

  if(webpage == TRUE) {

    url <- paste0(base, "filtroNome=", name, "&filtroAreaAtuacao=&filtroBaseEleitoral=&filtroPartido=&filtroLegislatura=18&filtroEmExercicio=on&filtroEmExercicioPesquisa=&filtroLegislaturaAtual=S")

    read_html(url) %>%
    html_node(css = "#conteudo > div.row > div.col-lg-9 > form > table.tabela > tbody > tr > td:nth-child(1) > a") %>%
    html_attr("href") -> href

    weburl <- paste0("https://www.al.sp.gov.br", href)

    dep <- read_html(weburl)

    bio <- dep %>%
      html_node(css = "#quadro_texto_deputado") %>%
      html_text() %>%
      str_remove_all("\\r") %>% str_remove_all("\\n") %>%
      str_remove_all("\\t") %>%
      str_trim()

    info <- function(path){
      x <- dep %>%
        html_nodes(xpath = path) %>%
        html_attr("value")
    }

    nome <- dep %>% html_node(xpath = '//*[@id="conteudo"]/div[1]/div[1]/h2') %>%
      html_text()
    partido <- info('//*[@id="infoGeral"]/div[1]/div[1]/input')
    base_eleitoral <- info('//*[@id="infoGeral"]/div[1]/div[2]/input')
    area_atuacao <- info('//*[@id="infoGeral"]/div[2]/div/input')
    fax <- info('//*[@id="infoGeral"]/div[3]/div[1]/input')
    telefone <- info('//*[@id="infoGeral"]/div[3]/div[2]/input')
    sala_andar <- info('//*[@id="infoGeral"]/div[3]/div[3]/input')
    veiculo <- info('//*[@id="infoGeral"]/div[3]/div[4]/input')
    email <- info('//*[@id="infoGeral"]/div[4]/div[1]/input')
    niver <- info('//*[@id="infoGeral"]/div[4]/div[2]/input')

    glue::glue('<p>
             <b>Nome:</b> {nome}
             <br>
             <b>Partido:</b> {partido}
             <br>
             <b>Base Eleitoral</b> {base_eleitoral}
             <br>
             <div>Fax: {fax}; Telefone: {telefone}; Sala/Andar: {sala_andar}</div>
             <div>Veiculo: {veiculo}; email: {email}; niverzinho: {niver}</div>
             <br>
             <br>
             <b>Bio:</b> {bio}
             </p>') %>%
        htmltools::HTML() %>%
        htmltools::html_print(background = "#fff")
  }
  return(df)
}


#' @export
party_codes <- function(){
  partycodesbr::tse_codes()
}
