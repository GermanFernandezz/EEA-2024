players_value <- read.csv("players.csv")
colnames(players_value)
players_metrics <- read.csv("premier-player-23-24.csv")
colnames(players_metrics)

# Función para remover tildes y caracteres adicionales
remove_accents <- function(text) {
  # Convierte a ASCII eliminando tildes
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Elimina cualquier apóstrofe o caracteres no alfanuméricos sobrantes
  text <- gsub("[^[:alnum:] ]", "", text)
  return(text)
}

# Aplicar la función a la variable (columna) 'nombre' en el data frame
players_value$name <- sapply(players_value$name, remove_accents)
players_metrics$Player <- sapply(players_metrics$Player, remove_accents)

#Elimino mayúsculas
players_metrics$Player <- tolower(players_metrics$Player)
players_value$name <- tolower(players_value$name)

# Vector con nombres de equipos como estan en players_metrics
unique(players_value$equipo)
unique(players_metrics$Team)
equipos <- unique(players_metrics$Team)

# Agrega variable nueva "equipo" a value y luego agrega nombre de equipo como está en metrics
players_value$equipo <- NA

for (equipo in equipos){
  for (i in 1:32405) {
    if (grepl(equipo, players_value$current_club_name[i])){
      players_value$equipo[i] <- equipo
  }
}
}


# Crear las variables "precio" y "precio mas alto" en metrics
players_metrics$precio <- NA
players_metrics$precio_mas_alto <- NA

# Agrega "precio" y "precio mas alto" a los datos que coinciden en nombre de jugador y equipo
for (i in 1:580){
  for (j in 1:32495){
    if (!is.na(players_metrics$Player[i]) && !is.na(players_value$name[j]) &&
        !is.na(players_metrics$Team[i]) && !is.na(players_value$equipo[j]) &&
        players_metrics$Player[i] == players_value$name[j] &&
        players_metrics$Team[i] == players_value$equipo[j]){
      players_metrics$precio[i] <- players_value$market_value_in_eur[j] 
      players_metrics$precio_mas_alto[i] <- players_value$highest_market_value_in_eur[j]
    }
  }
}

# elimina los NA. Los datos de values parecen estar mas actualizados que los de metrics
# ej: julian alvarez en values está en At. Madrid, en metrics en el City.
sum(is.na(players_metrics$precio))
dataset <- na.omit(players_metrics[!is.na(players_metrics$precio), ])

# Verifico que no haya mas NA
sum(is.na(dataset$precio))
sum(is.na(dataset$precio_mas_alto))

# Escribo el dataset
write.csv(dataset, "dataset.csv", row.names = FALSE)
