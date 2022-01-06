user_age <- function(bday){
  length(seq.Date(from = as.Date("1984-09-17"), to = Sys.Date(), by = "year"))-1
}

overview_info <- tribble(
  ~name,                       ~age,                     ~origin,                    ~affiliations,                                                                                  ~background,                                                                                                   ~image_url,
  "Amaya, Carlos",             user_age("1984-09-17"),   "Socha, Boyacá",            c("Partido Alianza Verde", "Coalición por la Esperanza"),                                       c("Governor of Boyacá (2016-2019)", "Chamber of Representatives of Colombia (2010-2014)"),                     "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/Carlos_amaya_candidato.jpg/480px-Carlos_amaya_candidato.jpg",
  "Barguil, David",            user_age("1981-06-23"),   "Cereté, Córdoba",          c("Partido Conservador Colombiano", "NA"),                                                      c("Senate of the Republic of Colombia (2018-)", "Chamber of Representatives of Colombia (2010-2018)"),         "https://cdn.forbes.co/2020/07/David-Barguil.png",
  "Barreras, Roy Leonardo",    user_age("1963-11-27"),   "Cali, Valle del Cauca",    c("Colombia Humana", "Partido de la Unión por la Gente", "Pacto Histórico Colombia Puede"),     c("Senate of the Republic of Colombia (2010-)", "Chamber of Representatives of Colombia (2006-2010)"),         "https://cr00.epimg.net/radio/imagenes/2021/02/16/6am_hoy_por_hoy/1613475986_201969_1613476080_noticia_normal_recorte1.jpg",
  "Char, Alejandro",           user_age("1966-04-16"),   "Barranquilla, Atlántico",  c()
 )