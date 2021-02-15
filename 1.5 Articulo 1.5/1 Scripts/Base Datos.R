
library(tidyverse)
library(ISLR)
datos <- Default

# Se recodifican los niveles No, Yes a 1 y 0
datos <- datos %>%
  select(default, balance) %>%
  mutate(default = recode(default,
                          "No"  = 0,
                          "Yes" = 1))
head(datos)


# Ajuste de un modelo lineal por mínimos cuadrados.
modelo_lineal <- lm(default ~ balance, data = datos)

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "lm", color = "gray20", se = FALSE) +
  theme_bw()  +
  labs(title = "Regresión lineal por mínimos cuadrados",
       y = "Probabilidad default") +
  theme(legend.position = "none")

predict(object = modelo_lineal, newdata = data.frame(balance = 10000))


# Ajuste de un modelo logístico.
modelo_logistico <- glm(default ~ balance, data = datos, family = "binomial")

# Representación gráfica del modelo.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(balance = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

# Con geom_smooth se puede obtener el gráfico directamente.
ggplot(data = datos, aes(x = balance, y = default)) +
  geom_point(aes(color = as.factor(default)), shape = 1) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE) +
  theme_bw() +
  theme(legend.position = "none")




### EJEMPLO

datos <- data.frame(sexo = c("hombre", "hombre", "mujer", "mujer", "mujer", "hombre",
                             "mujer", "hombre", "mujer", "mujer", "hombre", "hombre",
                             "hombre", "hombre", "mujer", "mujer", "hombre", "mujer",
                             "hombre", "mujer", "hombre", "mujer", "mujer", "hombre",
                             "hombre", "mujer", "mujer", "mujer", "hombre", "hombre",
                             "hombre", "mujer", "hombre", "mujer", "hombre", "mujer",
                             "mujer", "mujer", "mujer", "mujer", "hombre", "mujer",
                             "hombre", "mujer", "mujer", "mujer", "mujer", "hombre",
                             "mujer", "hombre", "mujer", "hombre", "mujer", "mujer",
                             "hombre", "hombre", "hombre", "hombre", "hombre", "hombre",
                             "hombre", "hombre", "hombre", "mujer", "hombre", "hombre",
                             "hombre", "hombre", "mujer", "hombre", "mujer", "hombre",
                             "hombre", "hombre", "mujer", "hombre", "mujer", "mujer",
                             "hombre", "mujer", "mujer", "mujer", "hombre", "hombre",
                             "hombre", "hombre", "hombre", "mujer", "mujer", "mujer",
                             "mujer", "hombre", "mujer", "mujer", "mujer", "mujer",
                             "mujer", "mujer", "mujer", "mujer","mujer", "mujer",
                             "hombre", "mujer", "hombre", "hombre", "mujer", "mujer",
                             "mujer", "hombre","mujer", "hombre", "mujer", "mujer",
                             "mujer", "hombre", "mujer", "hombre", "mujer", "hombre",
                             "mujer", "hombre", "mujer", "mujer", "mujer", "mujer",
                             "mujer", "mujer", "mujer", "mujer", "hombre", "mujer",
                             "hombre", "hombre", "hombre", "hombre", "hombre", "hombre",
                             "hombre", "mujer","mujer", "mujer", "hombre", "hombre",
                             "mujer", "mujer", "hombre", "mujer", "hombre", "hombre",
                             "hombre", "mujer", "mujer", "mujer", "mujer", "hombre",
                             "hombre", "mujer", "hombre", "hombre", "mujer", "hombre",
                             "hombre", "hombre", "hombre", "mujer", "hombre", "hombre",
                             "mujer", "mujer", "hombre", "hombre", "hombre", "hombre",
                             "hombre", "mujer", "mujer", "mujer", "mujer", "hombre",
                             "hombre", "hombre", "mujer", "hombre", "mujer", "hombre",
                             "hombre", "hombre", "mujer"),
                    examen_lectura = c(91, 77.5, 52.5, 54, 53.5, 62, 59, 51.5,
                                       61.5, 56.5, 47.5, 75, 47.5, 53.5, 50, 50,
                                       49, 59, 60, 60, 60.5, 50, 101, 60, 60,
                                       83.5, 61, 75, 84, 56.5, 56.5, 45, 60.5,
                                       77.5, 62.5, 70, 69, 62, 107.5, 54.5, 92.5,
                                       94.5, 65, 80, 45, 45, 66, 66, 57.5, 42.5,
                                       60, 64, 65, 47.5, 57.5, 55, 55, 76.5,
                                       51.5, 59.5, 59.5, 59.5, 55, 70, 66.5,
                                       84.5, 57.5, 125, 70.5, 79, 56, 75, 57.5,
                                       56, 67.5, 114.5, 70, 67, 60.5, 95, 65.5,
                                       85, 55, 63.5, 61.5, 60, 52.5, 65, 87.5,
                                       62.5, 66.5, 67, 117.5, 47.5, 67.5, 67.5,
                                       77, 73.5, 73.5, 68.5, 55, 92, 55, 55, 60,
                                       120.5, 56, 84.5, 60, 85, 93, 60, 65, 58.5,
                                       85, 67, 67.5, 65, 60, 47.5, 79, 80, 57.5,
                                       64.5, 65, 60, 85, 60, 58, 61.5, 60, 65,
                                       93.5, 52.5, 42.5, 75, 48.5, 64, 66, 82.5,
                                       52.5, 45.5, 57.5, 65, 46, 75, 100, 77.5,
                                       51.5, 62.5, 44.5, 51, 56, 58.5, 69, 65,
                                       60, 65, 65, 40, 55, 52.5, 54.5, 74, 55,
                                       60.5, 50, 48, 51, 55, 93.5, 61, 52.5,
                                       57.5, 60, 71, 65, 60, 55, 60, 77, 52.5,
                                       95, 50, 47.5, 50, 47, 71, 65),
                    clases_repaso = c("0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "0", "0", "0", "0", "0", "0",
                                      "0", "0", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1", "1", "1", "1",
                                      "1", "1", "1", "1", "1"))

datos$sexo <- as.factor(datos$sexo)
datos$clases_repaso  <- as.factor(datos$clases_repaso)

library(ggplot2)
tabla <- table(datos$clases_repaso, datos$sexo,
               dnn = c("clases de repaso","sexo"))
addmargins(tabla)

tabla_frecuencias <- prop.table(tabla)*100
addmargins(tabla_frecuencias)

ggplot(data = datos, aes(x = clases_repaso, y = examen_lectura, colour = sexo)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")

modelo_glm <- glm(clases_repaso ~ examen_lectura + sexo, data = datos,
                  family = "binomial")
summary(modelo_glm)

confint(modelo_glm)

# En caso de querer los intervalos basados en el error estándar.
confint.default(modelo_glm)


library(ggplot2)
# Para graficar los valores en ggplot junto con la curva, la variable respuesta
# tiene que ser numérica en lugar de factor.
datos$clases_repaso <- as.numeric(as.character(datos$clases_repaso))

# Se crea un dataframe que contenga la probabilidad de que se necesiten clases
# de repaso dada una determinada nota en el examen de lectura y siendo hombre.
# Vector con nuevos valores interpolados en el rango de observaciones.
nuevos_valores_examen <- seq(from = min(datos$examen_lectura),
                             to = max(datos$examen_lectura), by = 0.5)
sexo <- as.factor(rep(x = "hombre", length(nuevos_valores_examen)))
# Predicciones de los nuevos puntos según el modelo. type = "response" devuelve
# las predicciones en forma de probabilidad en lugar de en log_ODDs.
predicciones <- predict(object = modelo_glm,
                        newdata=data.frame(examen_lectura=nuevos_valores_examen,
                                           sexo = sexo),
                        type = "response")
# Se crea un data frame con los nuevos puntos y sus predicciones para graficar
# la curva.
datos_curva_hombre <- data.frame(examen_lectura = nuevos_valores_examen, 
                                 sexo = sexo,
                                 clases_repaso = predicciones)

# Mismo proceso para mujeres (sexo = 0).
nuevos_valores_examen <- seq(from = min(datos$examen_lectura),
                             to = max(datos$examen_lectura), by = 0.5)
sexo <- as.factor(rep("mujer", length(nuevos_valores_examen)))
predicciones <- predict(object = modelo_glm,
                        newdata=data.frame(examen_lectura=nuevos_valores_examen,
                                           sexo = sexo),
                        type = "response")
datos_curva_mujer <- data.frame(examen_lectura = nuevos_valores_examen,
                                sexo = sexo, clases_repaso = predicciones)

# Se unifican los dos dataframe.
datos_curva <- rbind(datos_curva_hombre, datos_curva_mujer)

ggplot(data = datos, aes(x = examen_lectura, y = as.numeric(clases_repaso),
                         color = sexo)) +
  geom_point() +
  geom_line(data = datos_curva, aes(y = clases_repaso)) + 
  geom_line(data = datos_curva, aes(y = clases_repaso)) +
  theme_bw() +
  labs(title = "P. clases repaso en función de nota lectura y sexo",
       y = "P(clase de repaso)") +
  theme(plot.title = element_text(size = 10))

# Diferencia de residuos
dif_residuos <- modelo_glm$null.deviance - modelo_glm$deviance

# Grados libertad
df <- modelo_glm$df.null - modelo_glm$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

paste("Grados de libertad:", df)

paste("p-value:", round(p_value, 4))

predicciones <- ifelse(test = modelo_glm$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo_glm$model$clases_repaso, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

library(vcd)
predicciones <- ifelse(test = modelo_glm$fitted.values > 0.45, yes = 1, no = 0)
matriz_confusion <- table(modelo_glm$model$clases_repaso, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

