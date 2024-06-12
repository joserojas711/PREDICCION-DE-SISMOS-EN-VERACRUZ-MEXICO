datos

estados <- c("TRES VALLES", "TIERRA BLANCA", "SAYULA", "COSCOMATEPEC", "CHOAPAS", "J RODRIGUEZ", "HUATUSCO", "ALLENDE", "ISLA", "SAYULA")
datos_filtrados <- subset(datos, Referencia.de.localizacion %in% estados)

TRES_VALLES <- subset(datos, Referencia.de.localizacion %in% "TRES VALLES")
SAYULA <- subset(datos, Referencia.de.localizacion %in% "SAYULA")
CHOAPAS <- subset(datos, Referencia.de.localizacion %in% "CHOAPAS")
ISLA <- subset(datos, Referencia.de.localizacion %in% "ISLA")



datos <- ISLA[,c(2,5)]
Iteraciones <- 10000
     set.seed(Semilla)
      Variables <- names(datos)[-1]
      V.Dependiente <-  paste(names(datos)[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
	      R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
      	unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables) 
      n <- seq_along(Com.Var) 
      m <- length(n)
      Sep <- round(0.8 * nrow(datos))
      Matriz_ECM <- matrix(numeric(), m, Iteraciones) 
      Variables_del_Modelo <- numeric(m)
      Betas <- list() 
      PromBetas <- list()

      for (i in n) {
      Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, Iteraciones)  
      	for (k in 1:Iteraciones) {
  		  M.Smp <- sample(1:nrow(datos), Sep)
		  D.Ent <- datos[M.Smp, ] 
		  D.Pru <- datos[-M.Smp, ]
		  Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
		  modelo <- lm(Formula, data = D.Ent)
		  N.Var <- paste(Com.Var[[i]], collapse = " y ")
		  Predicciones <- predict(modelo, newdata = D.Pru)
		  ECM <- mean((D.Pru[, names(datos)[1]] - Predicciones)^2)
	        Matriz_ECM[i,k] <- ECM
      	  Coef[,k] <- coef(modelo)
	      }
      Variables_del_Modelo[i] <- N.Var
      Betas[[i]] <- Coef
      PromBetas[[i]] <- rowMeans(Betas[[i]])
      }
      Promedio_ECM <- rowMeans(Matriz_ECM)
      DF <- data.frame(Variables_del_Modelo, Promedio_ECM)
      DF <- DF[order(DF$Promedio_ECM), ]
      #PromBetas
      # print(paste("El modelo con el ECM más chico es", Variables_del_Modelo[Min], "con", min(Promedio_ECM)))
      # print(paste("Por lo tanto el promedio de las Betas del modelo es el siguiente: ", PromBetas[Min]))      



############################################################
Min <- which.min(Promedio_ECM)
Mejor_Modelo <- DF$Variables_del_Modelo[Min]

Formula_Final <- as.formula(paste(V.Dependiente, Mejor_Modelo))
modelo_final <- lm(Formula_Final, data = datos)

datosPred <- read.csv("sismosPredicciones.csv", header=TRUE)
datosPred <- datosPred[,c(3,6)]
nuevos_datos <- as.data.frame(datosPred)

nuevas_predicciones <- predict(modelo_final, newdata = nuevos_datos)

library(ggplot2)
resultados_grafico <- data.frame(
  x = nuevos_datos[[1]],  # Asumimos que la primera columna de nuevos_datos es la variable en el eje x
  y = nuevas_predicciones
)

ggplot(resultados_grafico, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagrama de dispersión con recta de regresión",
       x = "Variable predictora",
       y = "Predicción") +
  theme_minimal()



##############################################################
Min <- which.min(Promedio_ECM)
Mejor_Modelo <- DF$Variables_del_Modelo[Min]

Formula_Final <- as.formula(paste(V.Dependiente, Mejor_Modelo))
modelo_final <- lm(Formula_Final, data = datos)

datosPred <- read.csv("sismosPredicciones.csv", header=TRUE)
datosPred <- datosPred[,c(3,6)]
nuevos_datos <- as.data.frame(datosPred)

nuevas_predicciones <- predict(modelo_final, newdata = nuevos_datos)

datos$tipo <- "Original"  # Etiquetar los datos originales
nuevos_datos$tipo <- "Nuevo"
nuevos_datos$Prediccion <- nuevas_predicciones

# Asumimos que la primera columna de nuevos_datos es la variable en el eje x
columna_x <- names(nuevos_datos)[1]
columna_y <- "Profundidad"

resultados_grafico <- rbind(
  data.frame(x = datos[[columna_x]], y = datos[[names(datos)[1]]], tipo = datos$tipo),
  data.frame(x = nuevos_datos[[columna_x]], y = nuevos_datos[[columna_y]], tipo = nuevos_datos$tipo)
)



library(ggplot2)

ggplot(resultados_grafico, aes(x = x, y = y, color = tipo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = tipo), size = 1) +
  scale_color_manual(values = c("Original" = "green", "Nuevo" = "blue")) +
  labs(title = "Diagrama de dispersión con recta de regresión",
       x = "Variable predictora",
       y = "Valor",
       color = "Tipo de datos") +
  theme_minimal()


#########################################
Min <- which.min(Promedio_ECM)
Mejor_Modelo <- DF$Variables_del_Modelo[Min]

# Crear y ajustar el modelo final usando todas las observaciones
Formula_Final <- as.formula(paste(V.Dependiente, Mejor_Modelo))
modelo_final <- lm(Formula_Final, data = datos)

datosPred <- read.csv("sismosPredicciones.csv", header=TRUE)
datosPred <- datosPred[,c(3,6)]
nuevos_datos <- as.data.frame(datosPred)


nuevas_predicciones <- predict(modelo_final, newdata = nuevos_datos)

# Crear un data frame con los resultados para graficar
datos$tipo <- "Original"  # Etiquetar los datos originales
nuevos_datos$tipo <- "Nuevo"
nuevos_datos$Prediccion <- nuevas_predicciones

resultados_grafico <- rbind(
  data.frame(Magnitud = datos$Magnitud, Profundidad = datos$Profundidad, tipo = datos$tipo),
  data.frame(Magnitud = nuevos_datos$Magnitud, Profundidad = nuevos_datos$Profundidad, tipo = nuevos_datos$tipo)
)

# Crear el gráfico de dispersión con la recta de regresión
library(ggplot2)

ggplot(resultados_grafico, aes(x = Profundidad, y = Magnitud, color = tipo)) +
  geom_point() +
  scale_color_manual(values = c("Original" = "gray", "Nuevo" = "blue")) +
  labs(title = "Diagrama de dispersión de Magnitud vs Profundidad",
       y = "Magnitud",
       x = "Profundidad",
       color = "Tipo de datos") +
  theme_minimal()





