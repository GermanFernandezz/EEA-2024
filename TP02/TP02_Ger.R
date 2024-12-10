setwd("/Users/jorgefernandez/Documents/Cienciadedatos/EEA2024/TP02")

# Librerias----
library(tidyverse)
library(tidymodels)
library(GGally)
library(ggplot2)
library(MASS)
library(robustbase)
library(dplyr)
library(corrplot)
library(caret)
library(viridis)

# Carga de datset----
df <- as.data.frame(read.csv("dataset.csv"))
colnames(df)
str(df) 
df <- as.data.frame(df)
df <- na.omit(df)

colnames(df)[colnames(df) == "market_value_in_eur"] <- "precio"


# EDA----
df$continente <- factor(df$continente, levels = c("europa", "america", "africa", "asia_oceania"))
ggplot(df, aes(x=continente, fill = continente)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = "D") + # Paleta accesible
  labs(y = "Cantidad", 
       x = "Continente",
       title = "Cantidad de jugadores por continente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        legend.position = "none")
  
df$Comp <- factor(df$Comp, levels = c("es La Liga", "it Serie A", "fr Ligue 1", "de Bundesliga",
                                      "eng Premier League"))
ggplot(df, aes(x=Comp, fill = Comp)) + 
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = "D") + # Paleta accesible
  labs(y = "Cantidad", x = "Liga", title = "Cantidad de jugadores por liga") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        legend.position = "none")

posiciones <- table(df$sub_position)
posiciones <- names(sort(posiciones))
df$sub_position <- factor(df$sub_position, levels = posiciones)

ggplot(df, aes(x=position, fill = position)) + 
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = "D") + # Paleta accesible
  labs(y = "Count", x = "Liga", title = "Cantidad de jugadores por posición") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1,),
        legend.position = "none")
  
posiciones <- table(df$sub_position)
posiciones <- names(sort(posiciones))
df$sub_position <- factor(df$sub_position, levels = posiciones)
ggplot(df, aes(x=sub_position, fill = sub_position)) + 
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = "D") + # Paleta accesible
  labs(y = "Count", x = "Liga", title = "Cantidad de jugadores por posicion") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1,),
        legend.position = "none")


ggplot(df, aes(x=foot, fill = foot)) + 
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = "D") + # Paleta accesible
  labs(y = "Count", x = "Liga", title = "Cantidad de jugadores por pie habil") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1,),
        legend.position = "none")

ggplot(df, aes(x=Age))+
  geom_histogram(bins=10) 

unique(df$foot)


# Split Train-Test----
# Agregar nueva variable que tenga en cuenta los precios de los jugadores y poder hacer un split 
# test-train estratificado
caja_precios <- boxplot(df$precio)
caja_precios$stats

df$precio_cat <- NA

for (i in 1:length(df$precio)) {
  if (df$precio[i] >= 100000000) { 
    df$precio_cat[i] <- "muy_alto"}
  if (df$precio[i] < 100000000 && df$precio[i] >= caja_precios$stats[5]) {
    df$precio_cat[i] <- "alto"}
  if (df$precio[i] < caja_precios$stats[5] && df$precio[i] >= caja_precios$stats[4]) {
    df$precio_cat[i] <- "medio"}
  if (df$precio[i] < caja_precios$stats[4] && df$precio[i] >= caja_precios$stats[3]) {
    df$precio_cat[i] <- "bajo"}
  if (df$precio[i] < caja_precios$stats[3]){
    df$precio_cat[i] <- "muy_bajo"}
}


table(df$precio_cat)

# Suponiendo que `df$clase` es la variable categórica
set.seed(28749658)  # Fijar semilla para reproducibilidad

# Crear índices estratificados basados en la variable `clase`
train_indices <- createDataPartition(df$precio_cat, p = 0.7, list = FALSE)  # 70% entrenamiento

# Dividir los datos
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Verificar la distribución de clases
table(train_data$precio_cat)
table(test_data$precio_cat)

# Algunos gráficos como ir entendiendo
boxplot(df$precio)
boxplot(df$highest_precio)
plot(df$Age, df$precio)
plot(df$Min, df$precio)
plot(df$X90s, df$precio)
plot(df$Gls, df$precio)
plot(df$Ast, df$precio)
plot(df$Gls_90, df$precio)
plot(df$G.A, df$precio)
plot(df$G.PK, df$precio)
plot(df$PK, df$precio)

# Los 10 jugadores con precio mas alto y con precio mas alto alcanzado
mas_caros_nombre <- df %>% 
  slice_max(order_by = precio, n=10)  %>% 
  pull(name)
mas_caros_equipo <- df %>% 
  slice_max(order_by = precio, n=10)  %>% 
  pull(Squad)
mas_caros_precio <- df %>% 
  slice_max(order_by = precio, n=10)  %>% 
  pull(precio)
mas_caro <- data.frame(Nombre = mas_caros_nombre, 
                       Equipo = mas_caros_equipo, 
                       Precio = mas_caros_precio)

mas_baratos_nombre <- df %>% 
  slice_min(order_by = precio, n=10)  %>% 
  pull(name)
mas_baratos_equipo <- df %>% 
  slice_min(order_by = precio, n=10)  %>% 
  pull(Squad)
mas_baratos_precio <- df %>% 
  slice_min(order_by = precio, n=10)  %>% 
  pull(precio)
mas_barato <- data.frame(Nombre = mas_baratos_nombre, 
                       Equipo = mas_baratos_equipo, 
                       Precio = mas_baratos_precio)

df %>% slice_max(order_by = highest_market_value_in_eur, n=10) %>% pull(name)
# A simple vista se nota la diferencia en las edades

# Correlograma
selected_data <- df %>% dplyr::select(Age, Gls, Ast, precio)
df %>% 
  dplyr::select(Age, Gls, Ast, precio) %>% 
  mutate(liga = df$current_club_domestic_competition_id) %>%
  ggpairs(., mapping = aes(colour = liga),
          upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), progress=FALSE) + 
  scale_color_viridis_d(option = "D") + 
  scale_fill_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  theme_bw() +
  labs(title='Correlograma variables continuas')



m_cor <- df %>%
  dplyr::select(Age, Starts, Min, Gls, Ast, G.A, G.PK, PK, npxG, xG, xAG, PrgC, PrgP, PrgR,
                precio) %>%
  cor()
corrplot(m_cor,
         method="circle",
         type = "upper",
         addCoef.col = "black",
         diag= FALSE) 

# ------------------------------------

#Crear nuevas variables

divisions <- df$npxG / df$G.PK
max_value <- max(divisions[is.finite(divisions)], na.rm = TRUE)
divisions[is.nan(divisions)] <- 0
divisions[is.infinite(divisions)] <- max_value
df$npxG_Gls <- divisions
#---------------------------------------------------------------------------------------

# Modelos clásicos----

# Precio vs Goles

modelo_clasico_goles = lm(data = train_data, formula = precio ~ Gls)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_goles = tidy(modelo_clasico_goles, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_goles
# Observamos los valores de la evaluación global
glance(modelo_clasico_goles)
#Metricas
pred_modelo_clasico_goles <- augment(modelo_clasico_goles, newdata = train_data)
rmse(data = pred_modelo_clasico_goles, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_goles, truth = precio, estimate = .fitted)$.estimate

pred_modelo_clasico_goles <- augment(modelo_clasico_goles, newdata = test_data)
rmse(data = pred_modelo_clasico_goles, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_goles, truth = precio, estimate = .fitted)$.estimate

#Diagnóstico
datos_augmentados <- augment(modelo_clasico_goles)
ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")


#----------------------------------------------------------------------------------
# Asistencias
modelo_clasico_aasistencias = lm(data = train_data, formula = precio ~ Ast)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_asistencias = tidy(modelo_clasico_aasistencias, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_asistencias

# Observamos los valores de la evaluación global
glance(modelo_clasico_aasistencias)

#-----------

# Edad

modelo_clasico_edad = lm(data = train_data, formula = precio ~ Age)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_edad = tidy(modelo_clasico_edad, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_edad

# Observamos los valores de la evaluación global
glance(modelo_clasico_edad)

modelo_clasico_edad2 = lm(data = train_data, formula = precio ~ Age + I(Age^2))
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_edad2 = tidy(modelo_clasico_edad2, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_edad2



# ---------------

# Goles + Asistencias

# G.A = Goles + Asistencias
modelo_clasico_g_a <- lm(data = train_data, formula = precio ~ G.A)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_g_a = tidy(modelo_clasico_g_a, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_g_a

# Observamos los valores de la evaluación global
glance(modelo_clasico_g_a)


# ------------------------------

# Multiple: Goles + Edad + Asistencia + continente

# Fiteamos el modelo multiple
modelo_clasico_gls_age_ast = lm(data = train_data, 
                                formula = precio ~ Gls + I(Age^2) + Ast + continente)
summary(modelo_clasico_gls_age_ast)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_gls_age_ast = tidy(modelo_clasico_gls_age_ast, conf.int = TRUE)
coef_modelo_clasico_gls_age_ast

# Observamos los valores de la evaluación global
glance(modelo_clasico_gls_age_ast)

#Metricas
pred_modelo_clasico_gls_age_ast <- augment(modelo_clasico_gls_age_ast, newdata = train_data)
rmse(data = pred_modelo_clasico_gls_age_ast, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_gls_age_ast, truth = precio, estimate = .fitted)$.estimate

pred_modelo_clasico_gls_age_ast <- augment(modelo_clasico_gls_age_ast, newdata = test_data)
rmse(data = pred_modelo_clasico_gls_age_ast, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_gls_age_ast, truth = precio, estimate = .fitted)$.estimate

# -----------------------------------



# Multiple 1: precio = Goles + Edad + Asistencia + continenete + liga----

# Fiteamos el modelo multiple
modelo_clasico_multiple_1 = lm(data = train_data, 
                             formula = precio ~ Gls + Age + I(Age^2) + Ast + 
                               continente + current_club_domestic_competition_id)
summary(modelo_clasico_multiple_1)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_multiple_1 = tidy(modelo_clasico_multiple_1, conf.int = TRUE)
coef_modelo_clasico_multiple_1

# Observamos los valores de la evaluación global
glance(modelo_clasico_multiple_1)

#Metricas
pred_modelo_clasico_multiple_1 <- augment(modelo_clasico_multiple_1, newdata = train_data)
rmse(data = pred_modelo_clasico_multiple_1, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_multiple_1, truth = precio, estimate = .fitted)$.estimate

pred_modelo_clasico_multiple_1 <- augment(modelo_clasico_multiple_1, newdata = test_data)
rmse(data = pred_modelo_clasico_multiple_1, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_clasico_multiple_1, truth = precio, estimate = .fitted)$.estimate


#Diagnóstico
datos_augmentados <- augment(modelo_clasico_multiple_1)
ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")


# -----------------------------------


# Multiple 2: log(precio) = Goles + Edad + Asistencia + continenete + liga----

# Fiteamos el modelo multiple
modelo_clasico_multiple_2 = lm(data = train_data, 
                                formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                  continente + current_club_domestic_competition_id)
summary(modelo_clasico_multiple_2)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_multiple_2 = tidy(modelo_clasico_multiple_2, conf.int = TRUE)
coef_modelo_clasico_multiple_2

# Observamos los valores de la evaluación global
glance(modelo_clasico_multiple_2)

#Metricas
pred_modelo_clasico_multiple_2 <- augment(modelo_clasico_multiple_2, newdata = train_data)
pred_modelo_clasico_multiple_2$exp_fitted <- exp(pred_modelo_clasico_multiple_2$.fitted)
rmse(data = pred_modelo_clasico_multiple_2, truth = precio, estimate = exp_fitted)$.estimate
mae(data = pred_modelo_clasico_multiple_2, truth = precio, estimate = exp_fitted)$.estimate

pred_modelo_clasico_multiple_2 <- augment(modelo_clasico_multiple_2, newdata = test_data)
pred_modelo_clasico_multiple_2$exp_fitted <- exp(pred_modelo_clasico_multiple_2$.fitted)
rmse(data = pred_modelo_clasico_multiple_2, truth = precio, estimate = exp_fitted)$.estimate
mae(data = pred_modelo_clasico_multiple_2, truth = precio, estimate = exp_fitted)$.estimate


#Diagnóstico
datos_augmentados <- augment(modelo_clasico_multiple_2)
ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")


# -----------------------------------

# Metricas de ambos modelos clásicos multiples en TRAIN
modelos <- list(multiple_1 = modelo_clasico_multiple_1, multiple_2 = modelo_clasico_multiple_2)

lista_predicciones_testing = map(.x = modelos, .f = augment, newdata = train_data) 

metricas1_train = lista_predicciones_testing$multiple_1 %>%  
  metrics(truth=precio, estimate=.fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas2_train = lista_predicciones_testing$multiple_2 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas1_train
metricas2_train


# Metricas de ambos modelos clásicos multiples en TEST
modelos <- list(multiple_1 = modelo_clasico_multiple_1, multiple_2 = modelo_clasico_multiple_2)

lista_predicciones_testing = map(.x = modelos, .f = augment, newdata = test_data) 

metricas1_test = lista_predicciones_testing$multiple_1 %>%  
  metrics(truth=precio, estimate=.fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas2_test = lista_predicciones_testing$multiple_2 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas1_test
metricas2_test


#------------------------------------
# Modelo robusto robustbase : precio----
modelo_multiple_lmrob <- lmrob(formula = precio ~ Gls + Age + I(Age^2) + Ast + 
                                      continente + current_club_domestic_competition_id, 
                                 data=train_data)
resumen_rob <- summary(modelo_multiple_lmrob)
resumen_rob
resumen_rob$r.squared
resumen_rob$adj.r.squared

#Metricas
pred_modelo_multiple_robustbase <- augment(modelo_multiple_lmrob, newdata = train_data)

rmse(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate

pred_modelo_multiple_robustbase <- augment(modelo_multiple_lmrob, newdata = test_data)
rmse(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate


#Diagnóstico
datos_augmentados <- augment(modelo_multiple_lmrob)
datos_augmentados$.std.resid <- datos_augmentados$.resid/(modelo_multiple_lmrob$scale*sqrt(1-hatvalues(modelo_multiple_lmrob)))
datos_augmentados$.hat <- hatvalues(modelo_multiple_lmrob)
ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
plot(modelo_multiple_lmrob)



# Modelo robusto robustbase lmrob : log(precio)----
modelo_multiple_lmrob_2 <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                 continente + current_club_domestic_competition_id, 
                               data=train_data)
resumen_rob <- summary(modelo_multiple_lmrob_2)
resumen_rob
resumen_rob$r.squared
resumen_rob$adj.r.squared

#Metricas
pred_modelo_multiple_lmrob_2 <- augment(modelo_multiple_lmrob_2, newdata = train_data)

rmse(data = pred_modelo_multiple_lmrob_2, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_lmrob_2, truth = precio, estimate = .fitted)$.estimate

pred_modelo_multiple_lmrob_2 <- augment(modelo_multiple_lmrob_2, newdata = test_data)
rmse(data = pred_modelo_multiple_lmrob_2, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_lmrob_2, truth = precio, estimate = .fitted)$.estimate


#Diagnóstico
datos_augmentados <- augment(modelo_multiple_lmrob_2)
datos_augmentados$.std.resid <- datos_augmentados$.resid/(modelo_multiple_lmrob_2$scale*sqrt(1-hatvalues(modelo_multiple_lmrob_2)))
datos_augmentados$.hat <- hatvalues(modelo_multiple_lmrob_2)
ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")



# Modelo robusto robustbase lmrob, otras phi: log(precio)----
modelo_multiple_lmrob_lqq <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                   continente + current_club_domestic_competition_id, 
                                 data=train_data,
                           psi = "lqq")

modelo_multiple_lmrob_welsh <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                     continente + current_club_domestic_competition_id, 
                                   data=train_data,
                                   psi = "welsh")

modelo_multiple_lmrob_optimal <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                       continente + current_club_domestic_competition_id, 
                                     data=train_data,
                                     psi = "optimal")


modelo_multiple_lmrob_hampel <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                         continente + current_club_domestic_competition_id, 
                                       data=train_data,
                                       psi = "hampel")

modelo_multiple_lmrob_ggw <- lmrob(formula = log(precio) ~ Gls + Age + I(Age^2) + Ast + 
                                         continente + current_club_domestic_competition_id, 
                                       data=train_data,
                                       psi = "ggw")


modelos <- list(multiple_1 = modelo_clasico_multiple_1, 
                multiple_2 = modelo_clasico_multiple_2,
                robusto_2 = modelo_multiple_lmrob_2,
                robusto_3 = modelo_multiple_lmrob_lqq,
                robusto_4 = modelo_multiple_lmrob_welsh,
                robusto_5 = modelo_multiple_lmrob_optimal,
                robusto_6 = modelo_multiple_lmrob_hampel,
                robusto_7 = modelo_multiple_lmrob_ggw)

lista_predicciones_testing = map(.x = modelos, .f = augment, newdata = test_data) 


metricas1_test = lista_predicciones_testing$multiple_1 %>%  
  metrics(truth=precio, estimate=.fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas2_test = lista_predicciones_testing$multiple_2 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas3_test = lista_predicciones_testing$robusto_2 %>% 
  mutate(exp_fitted= exp(.fitted)) %>%
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas4_test = lista_predicciones_testing$robusto_3 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas5_test = lista_predicciones_testing$robusto_4 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas6_test = lista_predicciones_testing$robusto_5 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas7_test = lista_predicciones_testing$robusto_6 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas8_test = lista_predicciones_testing$robusto_7 %>%  
  mutate(exp_fitted= exp(.fitted)) %>% 
  metrics(truth=precio, estimate=exp_fitted) %>%
  mutate(.estimate=round(.estimate, 4))

metricas1_test
metricas2_test
metricas3_test
metricas4_test
metricas5_test
metricas6_test
metricas7_test
metricas8_test

metricas <- rbind(metricas1_test, metricas2_test)
metricas <- rbind(metricas, metricas3_test)
metricas <- rbind(metricas, metricas4_test)
metricas <- rbind(metricas, metricas5_test)
metricas <- rbind(metricas, metricas6_test)
metricas <- rbind(metricas, metricas7_test)
metricas <- rbind(metricas, metricas8_test)

modelitos <- c(rep("Multiple - Precio",3),rep("Multiple - log(Precio)",3),
               rep("Robusto - log(Precio) - phi = bisqare",3),rep("Robusto - log(Precio) - phi = lqq",3),
               rep("Robusto - log(Precio) - phi = welsh",3), rep("Robusto - log(Precio) - phi = optimal",3),
               rep("Robusto - log(Precio) - phi = hampel",3),rep("Robusto - log(Precio) - phi = ggw",3))
metricas <- cbind(modelitos, metricas)
metricas


library(car)
vif(lm(precio ~ Gls + I(Age^2) + Ast + 
         continente + current_club_domestic_competition_id, 
       data = train_data))
