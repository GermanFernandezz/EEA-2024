#Librerias
library(tidyverse)
library(tidymodels)
library(GGally)
library(corrplot)
library(corrr)
library(gridExtra)
library(MASS)



#Directorio de trabajo
setwd("D:/MaestriaDataMining/EEA2024/TP")

#Cargo los datos
datos <- read.csv("eph_train_2023.csv")

#1)Analisis exploratorio----

#Estructura de los datos
datos %>% glimpse()
datos %>% str()
#Quito años, trimesrte y fecha de nacimiento que no los voy a usar
datos %>% colnames()
datos <- datos[-c(2,3,6)]

#Paso a factor algunas variables que originalmente son numericas pero en realidad son categoricas
datos$aglomerado <- as.factor(datos$aglomerado)
datos$codigo_actividad <- as.factor(datos$codigo_actividad)

#Verifico valores úncios y valores faltantes
tabla_exploratorios =  datos %>%
  gather(., 
         key = "variables", 
         value = "valores") %>% # agrupamos por las variables del set
  group_by(variables) %>% 
  summarise(valores_unicos = n_distinct(valores),
            porcentaje_faltantes = sum(is.na(valores))/nrow(datos)*100) %>% 
  arrange(desc(porcentaje_faltantes), valores_unicos) # ordenamos por porcentaje de faltantes y valores unicos
tabla_exploratorios

#Saco ese único NA
datos <- subset(datos, !is.na(asistencia_educacion))

g1 <- ggplot(datos, aes(x=region, fill=region))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("Distribución de trabajadores por región")

g2 <- ggplot(datos, aes(x=nivel_ed, fill=nivel_ed))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("Distribución de trabajadores por nivel educativo")

g3 <- ggplot(datos, aes(x=tipo_establecimiento, fill=tipo_establecimiento))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("Distribución de trabajadores por tipo de establecimiento")

g4 <- ggplot(datos, aes(x=categoria_ocupacion, fill=categoria_ocupacion))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("Distribución de trabajadores por puesto laboral")+
  scale_x_discrete(labels = c("Trabajador sin remuneracion" = "Sin salario"))
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)

#Grafico de correlaciones
grafico1 <- datos %>%
  select_if(is.numeric) %>%
  ggpairs()

grafico1

m_cor <- datos %>%
  select_if(is.numeric) %>%
  cor()
corrplot(m_cor,
         method="circle",
         type = "upper",
         addCoef.col = "black",
         diag= FALSE)  


#Correlacion de Spearman
datos %>% 
  select_if(is.numeric) %>% # selecciona las variables numericas 
  correlate(method = 'spearman') %>% # convierte la matriz de corr en dataframe
  shave() %>% # solo muestra información debajo de la diagonal principal
  fashion(decimals = 3) # acomoda los datos en forma tidy (por ej. redondeo de decimales)

#Apertura por sexo
grafico2 <- datos %>% 
  select_if(is.numeric) %>% 
  mutate(sexo = datos$sexo) %>%
  ggpairs(., mapping = aes(colour = sexo), title = "Matriz de correlaciones",
          upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), legend = 25) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")

grafico2 #no veo inconsistencias severas en este gráfico


#2)Modelos lineales experiencia----
ggplot(datos, aes(x=experiencia_potencial, y= salario_horario))+
  geom_point()+
  theme_bw()

#No parece una relación muy lineal pero miremos el modelo: salario_horario vs exp_pot
modelo_simple_salhor_hr = lm(formula = salario_horario ~ experiencia_potencial, data = datos)
# Observamos que devuelve el modelo
modelo_simple_salhor_hr

# Accedemos a la información de los coeficientes estimados
intercepto = modelo_simple_salhor_hr$coefficients[1]
pendiente = modelo_simple_salhor_hr$coefficients[2]

nuevo_x <- data.frame(experiencia_potencial=6)
predict(modelo_simple_salhor_hr,nuevo_x)


#Modelo salario_horario vs exp_pot + exp_pot^2
modelo_multiple_salhr_hr = lm(formula = salario_horario ~ experiencia_potencial + I(experiencia_potencial^2), data = datos)
# Observamos que devuelve el modelo
modelo_multiple_salhr_hr

#Modelo salario_horario vs exp_pot + exp_pot^2 pero haciendo la nueva variable
datos$experiencia_potencial_cuad <- datos$experiencia_potencial^2
modelo_multiple_salhr_hr_bis = lm(formula = salario_horario ~ experiencia_potencial + experiencia_potencial_cuad , data = datos)
# Observamos que devuelve el modelo
modelo_multiple_salhr_hr_bis


# Graficamos el dataset y los modelos
datos %>% ggplot(., aes(x = experiencia_potencial, y = salario_horario)) + 
  geom_point(color="grey") + #capa de los datos
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, color="forestgreen", se = FALSE) + # capa del modelo
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = FALSE) +  # Ajuste del modelo
  
  #scale_x_continuous(limits = c(0,65)) +
  #scale_y_continuous(limits = c(0,550000)) +
  labs(title="Modelo Lineal Simple: Salario Horario", x="Exp. Pot.", y="Salario Hora") 



summary(modelo_simple_salhor_hr)
summary(modelo_multiple_salhr_hr)
summary(modelo_multiple_salhr_hr_bis)

tidy(modelo_simple_salhor_hr, conf.int = T)
tidy(modelo_multiple_salhr_hr, conf.int = T)

glance(modelo_simple_salhor_hr)
glance(modelo_multiple_salhr_hr)

plot(modelo_simple_salhor_hr)
plot(modelo_multiple_salhr_hr)


df_predicciones <- data.frame(experiencia_potencial=c(6,7,35,36))
df_predicciones$predicciones <- predict(modelo_multiple_salhr_hr,df_predicciones)

experiencia_6 <- df_predicciones$predicciones[2] - df_predicciones$predicciones[1]
experiencia_35 <- df_predicciones$predicciones[4] - df_predicciones$predicciones[3]


#3) Modelo lineal multiple----
datos$sexo <- factor(datos$sexo)

modelo_mincer1 <- lm(formula = salario_horario ~ educacion + 
                       experiencia_potencial + 
                       I(experiencia_potencial^2) +
                       sexo +
                       sexo:educacion,
                     data=datos)


modelo_mincer1
#Interpretación: Este es el coeficiente de la interacción entre educacion y sexoVaron. Indica que el 
#efecto de la educación sobre el salario es ligeramente menor para los hombres que para las mujeres. 
#En concreto, por cada año adicional de educación, los hombres reciben aproximadamente 6.44 unidades 
#menos de salario horario comparado con las mujeres.

summary(modelo_mincer1)
#Menos educacion:varon, el resto de las variables son significativas para explicar salario_horario
tidy(anova(modelo_mincer1))
#La variable sexo resulta significativa para explicar el salario_horario. No así la variable educacion:sexo
#El modelo en su totatlidad tiene una variable que sirve para predecir el salario_horario
#El modelo explica un 16.7% de variabilidad

tidy_mincer1 <- tidy(modelo_mincer1, conf.int = TRUE) %>% arrange(p.value)
tidy_mincer1

ggplot(tidy_mincer1, aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
  geom_point(color = "forestgreen",size=2) +
  geom_vline(xintercept = 0, lty = 4, color = "black") +
  geom_errorbarh(color = "forestgreen", size=1) +
  theme_bw() +
  labs(y = "Coeficientes β", x = "Estimación")

#Graficos de R base para ver si cumple supuestos de normalidad
plot(modelo_mincer1)

#Uso augment para calcular las variables necesarios para hacer los gráficos con ggplot
datos_augmentados <- augment(modelo_mincer1)

g1 = ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
g2 = ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
g3 = ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
g4 = ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)


#4)Modelo de Mincer enriquecido----
modelo_mincer2 <- lm(formula = log(salario_horario) ~ educacion + 
                       experiencia_potencial + 
                       I(experiencia_potencial^2) +
                       sexo +
                       sexo:educacion,
                     data=datos)

modelo_mincer2

summary(modelo_mincer2)

tidy(anova(modelo_mincer2))

tidy_mincer2 <- tidy(modelo_mincer2, conf.int = TRUE) %>% arrange(p.value)
tidy_mincer2

ggplot(tidy_mincer2, aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
  geom_point(color = "forestgreen",size=2) +
  geom_vline(xintercept = 0, lty = 4, color = "black") +
  geom_errorbarh(color = "forestgreen", size=1) +
  theme_bw() +
  labs(y = "Coeficientes β", x = "Estimación")

#Graficos de R base para ver si cumple supuestos de normalidad
plot(modelo_mincer2)

#Uso augment para calcular las variables necesarios para hacer los gráficos con ggplot
datos_augmentados <- augment(modelo_mincer2)

g1 = ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
g2 = ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
g3 = ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
g4 = ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)


# Predicciones en el logaritmo del salario
pred_log_salario <- predict(modelo_mincer2)

# Convertir las predicciones al salario horario original (inverso del logaritmo)
pred_salario <- exp(pred_log_salario)

# Salario horario observado (original)
salario_observado <- datos$salario_horario

# Calcular el R^2 corregido
r2_salario <- 1 - sum((salario_observado - pred_salario)^2) / sum((salario_observado - mean(salario_observado))^2)

print(r2_salario)

#5)Modelos propios y evaluacion----

#Modelo propio 1----
modelo_propio1 <- lm(formula = log(salario_horario) ~ educacion +
                                     region +
                                     experiencia_potencial,
                                   data = datos)


summary(modelo_propio1)

#Uso augment para calcular las variables necesarios para hacer los gráficos con ggplot
datos_augmentados <- augment(modelo_propio1)

g1 = ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
g2 = ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
g3 = ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
g4 = ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)


# Predicciones en el logaritmo del salario
pred_log_salario <- predict(modelo_propio1)

# Convertir las predicciones al salario horario original (inverso del logaritmo)
pred_salario <- exp(pred_log_salario)

# Salario horario observado (original)
salario_observado <- datos$salario_horario

# Calcular el R^2 corregido
r2_salario <- 1 - sum((salario_observado - pred_salario)^2) / sum((salario_observado - mean(salario_observado))^2)

print(r2_salario)  


#Modelo propio 2----
modelo_propio2 <- lm(formula = log(salario_horario) ~ edad/experiencia_potencial +
                                     sexo:experiencia_potencial + tipo_establecimiento,
                     data = datos)

summary(modelo_propio2)

#Uso augment para calcular las variables necesarios para hacer los gráficos con ggplot
datos_augmentados <- augment(modelo_propio2)

g1 = ggplot(datos_augmentados, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
g2 = ggplot(datos_augmentados, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
g3 = ggplot(datos_augmentados, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
g4 = ggplot(datos_augmentados, aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)


# Predicciones en el logaritmo del salario
pred_log_salario <- predict(modelo_propio1)

# Convertir las predicciones al salario horario original (inverso del logaritmo)
pred_salario <- exp(pred_log_salario)

# Salario horario observado (original)
salario_observado <- datos$salario_horario

# Calcular el R^2 corregido
r2_salario <- 1 - sum((salario_observado - pred_salario)^2) / sum((salario_observado - mean(salario_observado))^2)

print(r2_salario)  



#testeo----
test <- read.csv("eph_test_2023.csv")

pred_modelo_propio1 <- augment(modelo_propio1, newdata = test)
pred_modelo_propio1$exp_fitted <- exp(pred_modelo_propio1$.fitted)

pred_modelo_mincer1 <- augment(modelo_mincer1, newdata = test)

pred_modelo_mincer2 <- augment(modelo_mincer2, newdata = test)
pred_modelo_mincer2$exp_fitted <- exp(pred_modelo_mincer2$.fitted)

pred_modelo_propio2 <- augment(modelo_propio2, newdata = test)
pred_modelo_propio2$exp_fitted <- exp(pred_modelo_propio2$.fitted)

rmse(data=pred_modelo_propio1, truth = salario_horario, estimate=exp_fitted)
rmse(data=pred_modelo_mincer1, truth = salario_horario, estimate=.fitted)
rmse(data=pred_modelo_mincer2, truth = salario_horario, estimate=exp_fitted)
rmse(data=pred_modelo_propio2, truth = salario_horario, estimate=exp_fitted)

mae(data=pred_modelo_propio1, truth = salario_horario, estimate=exp_fitted)
mae(data=pred_modelo_mincer1, truth = salario_horario, estimate=.fitted)
mae(data=pred_modelo_mincer2, truth = salario_horario, estimate=exp_fitted)
mae(data=pred_modelo_propio2, truth = salario_horario, estimate=exp_fitted)



#6) Modelo lineal robusto----
# cargo datos train con outliers

train <- read.csv("eph_train_outliers_2023.csv")
train <- train[-c(2,3,6)]

datos$tipo <- "sin_outliers"
train$tipo <- "con_outliers"
prueba <- rbind(datos, train)

ggplot(data = prueba, aes(y = salario_horario, group = tipo, fill = tipo)) +
  geom_boxplot() + 
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "Boxplots de salario por hora trabajada", subtitle = "En pesos") +
  labs(y = "Salario por hora en pesos") +
  labs(x = "") +
  facet_wrap(~tipo)


train %>% 
  select_if(is.numeric) %>% 
  mutate(sexo = train$sexo) %>%
  ggpairs(., mapping = aes(colour = sexo), title = "Matriz de correlaciones",
          upper = list(continuous = wrap("cor", size = 3, hjust=0.5)), legend = 25) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")


modelo_mincer1_outliers <- lm(formula = salario_horario ~ educacion + 
                               experiencia_potencial + 
                               I(experiencia_potencial^2) +
                               sexo +
                               sexo:educacion,
                             data=train)

modelo_mincer1_robusto_outliers <- rlm(formula = salario_horario ~ educacion + 
                       experiencia_potencial + 
                       I(experiencia_potencial^2) +
                       sexo +
                       sexo:educacion,
                     data=train)

modelo_mincer2_outliers <- lm(formula = log(salario_horario) ~ educacion + 
                                experiencia_potencial + 
                                I(experiencia_potencial^2) +
                                sexo +
                                sexo:educacion,
                              data=train)


pred_modelo_mincer1_outliers <- augment(modelo_mincer1_outliers, newdata = test)

pred_modelo_mincer1_outliers_robusto <- augment(modelo_mincer1_robusto_outliers, newdata = test)

pred_modelo_mincer2_outliers <- augment(modelo_mincer2_outliers, newdata = test)
pred_modelo_mincer2_outliers$exp_fitted <- exp(pred_modelo_mincer2_outliers$.fitted)


rmse(data=pred_modelo_mincer1_outliers, truth = salario_horario, estimate=.fitted)
rmse(data=pred_modelo_mincer1_outliers_robusto, truth = salario_horario, estimate=.fitted)
rmse(data=pred_modelo_mincer2_outliers, truth = salario_horario, estimate=exp_fitted)

mae(data=pred_modelo_mincer1_outliers, truth = salario_horario, estimate=.fitted)
mae(data=pred_modelo_mincer1_outliers_robusto, truth = salario_horario, estimate=.fitted)
mae(data=pred_modelo_mincer2_outliers, truth = salario_horario, estimate=exp_fitted)

