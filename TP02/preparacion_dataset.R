# Directorio de trabajo----
setwd("/Users/jorgefernandez/Documents/Cienciadedatos/EEA2024/TP02")

# Carga de datos----
players_value <- read.csv("players.csv")
colnames(players_value)
players_metrics <- read.csv("top5-players.csv")
colnames(players_metrics)

#-------------

# Seleccionarlas 5 ligas top europeas en Players Value----
# Me quedo con los valores de los jugadores de las 5 ligas mas importantes
ligas <- c("IT1", "L1", "GB1", "ES1", "FR1")
players_value <- players_value[players_value$current_club_domestic_competition_id %in% ligas, ]

#--------------

# Saco tildes y mayúsculas de variables equipo y nombres de jugadores----
# Función para remover tildes y caracteres adicionales
remove_accents <- function(text) {
  # Convierte a ASCII eliminando tildes
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Elimina cualquier apóstrofe o caracteres no alfanuméricos sobrantes
  text <- gsub("[^[:alnum:] ]", "", text)
  return(text)
}

# Consolido nombre de la variable nombre de jugador
colnames(players_metrics)[colnames(players_metrics) == "Player"] <- "name"

# Aplicar la función a la variable (columna) 'nombre' y "Squad" en el data frame
players_value$name <- sapply(players_value$name, remove_accents)
players_metrics$name <- sapply(players_metrics$name, remove_accents)
players_metrics$Squad <- sapply(players_metrics$Squad, remove_accents)
players_value$current_club_name <- sapply(players_value$current_club_name, remove_accents)

#Elimino mayúsculas
players_metrics$name <- tolower(players_metrics$name)
players_value$name <- tolower(players_value$name)
players_metrics$Squad <- tolower(players_metrics$Squad)
players_value$current_club_name <- tolower(players_value$current_club_name)

#--------------

# Consolidar nombre de equipo----
# Vector con nombres de equipos como estan en players_metrics
equipos <- unique(players_metrics$Squad)
equipos
# Consolidar nombre de equipo----
# Agrega variable nueva "equipo" a value y luego agrega nombre de equipo como está en metrics
players_value$Squad <- NA
for (equipo in equipos){
  for (i in 1:32405) {
    if (grepl(equipo, players_value$current_club_name[i])){
      players_value$Squad[i] <- equipo
  }
}
}

unique(players_value$Squad)

# Son 96 equipos en Players Metric y 83 en Players Value.

#--------------

# MERGE INTERSECCION----
# Realiza un merge para combinar ambas tablas basado en name y Squad
merged_data <- merge(players_metrics, players_value, 
                     by = c("name", "Squad"), 
                     all = FALSE)  # Preserva todas las filas de players_metrics

# Eliminar los datos que no tiene precio de jugador
merged_data <- merged_data[!is.na(merged_data$market_value_in_eur),]


# Agregar variables Continente----

europa <- c("es ESP", "eng ENG", "pt POR", "no NOR", "ch SUI", "hr CRO", "nl NED", "be BEL", 
            "de GER", "hu HUN", "sct SCO", "fr FRA", "ie IRL", "nir NIR", "wls WAL", "ua UKR", 
            "pl POL", "it ITA", "se SWE", "sk SVK", "ro ROU", "il ISR", "dk DEN", "cz CZE", 
            "gr GRE", "re SRB", "at AUT", "is ISL", "ba BIH", "tr TUR")
america <- c("br BRA", "co COL", "ar ARG", "uy URU", "ec ECU", "py PAR", "jm JAM", "mx MEX", 
             "us USA", "gd GRN", "cm CMR")
africa <- c("eg EGY", "gh GHA", "sn SEN", "ml MLI", "ci CIV", "ng NGA", "gw GNB", "bf BFA",
            "ga GAB", "dz ALG", "zw ZIM", "cd COD", "za RSA", "ao ANG")
asia_oceania <- c("jp JPN", "nz NZL")


continentes <- data.frame(
  valor = c(europa, america, africa, asia_oceania),              # Combinar los datos de los dos vectores
  origen = c(rep("europa", length(europa)),  # Etiquetar de dónde viene cada dato
             rep("america", length(america)),
             rep("africa", length(africa)),
             rep("asia_oceania", length(asia_oceania))))



for (i in 1:length(merged_data$Nation)){
  for (j in 1:length(continentes$valor)) {
    if (merged_data$Nation[i] == continentes$valor[j]){
      merged_data$continente[i] <- continentes$origen[j]
    }
  }
}

unique(merged_data$continente)
sum(is.na(merged_data$continente))


#Algunos grafiquitos rápidos
boxplot(merged_data$market_value_in_eur)
plot(merged_data$Gls, merged_data$market_value_in_eur)
plot(merged_data$Age, merged_data$market_value_in_eur)


# Escribo el dataset
write.csv(merge_data, "dataset.csv", row.names = FALSE)
