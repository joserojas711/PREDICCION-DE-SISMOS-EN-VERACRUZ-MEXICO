library(mice)
library(kableExtra)
library(VIM)
library(zoo)
library(ggplot2)
##########################################################
datos <- read.csv("datos.csv", header=TRUE)
datosExtraccion <- datos[,c(3:6)]
datosExtraccion <- as.data.frame(datosExtraccion)
datosExtraccion$Magnitud <- ifelse(datosExtraccion$Magnitud == "no calculable", NA, datosExtraccion$Magnitud)
datosExtraccion <- data.frame(lapply(datosExtraccion, function(x) as.numeric(as.character(x))))
md.pattern(datosExtraccion)
aggr(datosExtraccion, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(datosExtraccion), 
                             cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Se puede cambiar el method = 'pmm' y el maxit = 50
imputed_data1 <- mice(datosExtraccion, m = 1, method = "norm.nob", maxit=1, seed = 2018, print=F)
datosImputados <- mice::complete(imputed_data1)
datos[,3] <- round(datosImputados$Magnitud, 1)
fecha <- datos[,1]
datos[,1] <- as.Date(fecha, "%d/%m/%Y")
municipios <- datos[, 7]
municipios <- gsub(".* de ([^,]+),.*", "\\1", municipios)
municipios <- trimws(municipios_limpios)
datos[, 7] <- municipios
datos <- datos[, c(1,3,4,5,6,7)]
#names(datos)





####################################
serie_tiempo <- as.ts(datos[,"Magnitud"], start = datos[,"Fecha"], freq = delta(days = 1))
plot(serie_tiempo)





############################################################################################
datos[,6] <- ifelse(datos[,6] == "JALTIPAN DE MORELOS", "JALTIPAN", datos[,6])
datos[,6] <- ifelse(datos[,6] == "SAYULA DE ALEMAN", "SAYULA", datos[,6])
datos[,6] <- ifelse(datos[,6] == "J RODRIGUEZ CLARA", "J RODRIGUEZ", datos[,6])
datos[,6] <- ifelse(datos[,6] == "JOSE CARDEL", "CARDEL", datos[,6])
datos[,6] <- ifelse(datos[,6] == "LAS CHOAPAS", "CHOAPAS", datos[,6])
frecuencias <- sort(table(datos[,6]), decreasing = TRUE)
frecuencias <- frecuencias[1:10]
frecuencias 


barplot(frecuencias, main = "Municipios con mayor cantidad de sismos",
        xlab = "Municipios", ylab = "Frecuencia",
        col = "skyblue", border = "black")


#datosTerremotos <- datos[,c(6,2)]
datosTerremotos <- datos[,c(1,2)]
if (is.character(datosTerremotos[,2])) {
  datosTerremotos[,2] <- factor(datosTerremotos[,2])
}
plot(datosTerremotos[,1], datosTerremotos[,2], ylabel = "Referencia de ubicación", 
     xlabel = "Magnitud del Terremoto", main = "Magnitud vs Referencia de ubicación")

############################################################################################
terremotos <- filter(datos, Magnitud > 5.9);  print(terremotos)
#unique(terremotos[,2])
#head(datosTerremotos[-c(1:8),])

ggplot(datosTerremotos[-c(1:8),], aes(x = Fecha, y = Magnitud)) +
  geom_point(aes(color = ifelse(Magnitud > 5.9, "rojo", "negro"))) +
  scale_color_manual(values = c("rojo" = "red", "negro" = "black")) +
  labs(title = "Magnitud vs Referencia de ubicación", x = "Referencia de ubicación", y = "Magnitud") +
  theme(legend.position = "none")



############################################################################################


datosExt <- datos[,c(2,5)]
write.table(datosExt, file = "datos.txt", sep = "\t", col.names = TRUE, row.names = FALSE)
