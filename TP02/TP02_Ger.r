setwd("/Users/jorgefernandez/Documents/Cienciadedatos/EEA2024/TP02")

# Librerias
library(tidyverse)
library(tidymodels)
library(GGally)
library(ggplot2)

# Carga de datset
df <- read.csv("dataset.csv")
colnames(df)
str(df)

# Algunos gráficos como ir entendiendo
boxplot(df$precio)
boxplot(df$precio_mas_alto)
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
df %>% slice_max(order_by = precio, n=10)
df %>% slice_max(order_by = precio_mas_alto, n=10)
# A simple vista se nota la diferencia en las edades

# Correlograma
df %>% 
  select(Age, Gls, Ast, G.A, precio) %>% 
  ggpairs(upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), progress=FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  theme_bw() +
  labs(title='Correlograma variables continuas')


# Modelos clásicos
modelo_clasico_goles = lm(data = df, formula = precio ~ Gls)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_goles = tidy(modelo_clasico_goles, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_goles
# Observamos los valores de la evaluación global
glance(modelo_clasico_goles)

modelo_clasico_aasistencias = lm(data = df, formula = precio ~ Ast)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_asistencias = tidy(modelo_clasico_aasistencias, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_asistencias

# Observamos los valores de la evaluación global
glance(modelo_clasico_aasistencias)

modelo_clasico_edad = lm(data = df, formula = precio ~ Age)
# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_edad = tidy(modelo_clasico_edad, conf.int = TRUE, conf.level = 0.95)
coef_modelo_clasico_edad

# Observamos los valores de la evaluación global
glance(modelo_clasico_edad)

# Fiteamos el modelo multiple
modelo_clasico_gls_age_ast = lm(data = df, formula = precio ~ Gls + Age + Ast)

# Observamos los valores de los coeficientes estimados
coef_modelo_clasico_gls_age_ast = tidy(modelo_clasico_gls_age_ast, conf.int = TRUE)
coef_modelo_clasico_gls_age_ast

# Observamos los valores de la evaluación global
glance(modelo_clasico_gls_age_ast)


