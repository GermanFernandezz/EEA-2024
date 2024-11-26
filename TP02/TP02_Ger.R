setwd("/Users/jorgefernandez/Documents/Cienciadedatos/EEA2024/TP02")

# Librerias
library(tidyverse)
library(tidymodels)
library(GGally)
library(ggplot2)
library(MASS)
library(robustbase)
library(dplyr)
library(corrplot)
library(caret)

# Carga de datset
df <- as.data.frame(read.csv("dataset.csv"))
colnames(df)
str(df) 
df <- as.data.frame(df)

colnames(df)[colnames(df) == "market_value_in_eur"] <- "precio"

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
df %>% slice_max(order_by = precio, n=10)  %>% pull(name)
df %>% slice_max(order_by = highest_market_value_in_eur, n=10) %>% pull(name)
# A simple vista se nota la diferencia en las edades

# Correlograma
selected_data <- df %>% dplyr::select(Age, Gls, Ast, precio)
df %>% 
  dplyr::select(Age, Gls, Ast, precio) %>% 
  mutate(liga = df$Comp) %>%
  ggpairs(., mapping = aes(colour = liga),
          upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), progress=FALSE) + 
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

# Modelos clásicos

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

modelo_clasico_edad2 = lm(data = train_data, formula = precio ~ I(Age^2))
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_edad2 = tidy(modelo_clasico_edad2, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_edad2

# Observamos los valores de la evaluación global
glance(modelo_clasico_edad2)

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

# Multiple: Goles + Edad + Asistencia + continenete

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

# Modelo robusto robustbase----
modelo_multiple_robustbase <- lmrob(formula = precio ~ Gls + 
                                            Ast + 
                                            I(Age^2) +
                                      continente, 
                                 data=train_data)
resumen_rob <- summary(modelo_multiple_robustbase)
resumen_rob
resumen_rob$r.squared
resumen_rob$adj.r.squared

#Metricas
pred_modelo_multiple_robustbase <- augment(modelo_multiple_robustbase, newdata = train_data)
rmse(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate

pred_modelo_multiple_robustbase <- augment(modelo_multiple_robustbase, newdata = test_data)
rmse(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate
mae(data = pred_modelo_multiple_robustbase, truth = precio, estimate = .fitted)$.estimate




# Modelo robusto MASS
modelo_multiple_mass <- rlm (formula = precio ~ Gls + 
                               Ast + 
                               I(Age^2) +
                               continente, 
                             data=train_data)
summary(modelo_multiple_mass)



# Residuos del modelo lm
residuos_lm <- residuals(modelo_clasico_gls_age_ast)

# Residuos del modelo rlm
residuos_rlm <- residuals(modelo_multiple_robustbase)

# Graficar los residuos
par(mfrow = c(1, 2))
plot(residuos_lm, main = "Residuos LM")
plot(residuos_rlm, main = "Residuos RLM")

BIC(modelo_clasico_gls_age_ast, modelo_multiple_mass)
