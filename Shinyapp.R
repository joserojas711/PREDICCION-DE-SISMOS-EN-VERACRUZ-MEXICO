library(shiny)
ui <- fluidPage(
  titlePanel("Modelo con menor ECM"),
  sidebarLayout(
    sidebarPanel(
      ###
      fileInput("file", "Cargar archivo .txt"),
      ###  
      numericInput("iteraciones", "Número de iteraciones", 100),
	###  
      numericInput("semilla", "Ingrese la semilla", 1234),
      ###
	radioButtons(
        "dist", "Eliga el método de aprendizaje supervisado que quiere usar:",
        c("Regresión Lineal Simple" = "RLS",
        "Holdout" = "holdout",
        "Submuestreo Aleatorio" = "SubMA",
        "Validación Cruzada" = "ValCrz", 
        "Validación Bootstrap" = "Boot")
      ),
      ###
      actionButton("ejecutar", "Ejecutar"),
    ),

    mainPanel(
      tableOutput("tabla_resultados")
    )
  )
)

#############################################################
server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  ###
  observeEvent(input$file, {
    datos(read.table(input$file$datapath, header = TRUE))
  })
  ###
  output$tabla_resultados <- renderTable({
    req(input$ejecutar, datos())
  ###
  Iteraciones <- input$iteraciones
  Semilla <- input$semilla


######################
    if (input$dist == "RLS") {
######
      Variables <- names(datos())[-1]
      V.Dependiente <- paste(names(datos())[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
        R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
             unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables) 
      n <- seq_along(Com.Var) 
      Matriz_ECM <- matrix(numeric(), length(n), 1)
      Variables_del_Modelo <- numeric(length(n))
      Betas <- list() 
      
      for (i in n) {  
        Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
        modelo <- lm(Formula, data = datos())
        Predicciones <- predict(modelo)
        ECM <- mean((datos()[, names(datos())[1]] - Predicciones)^2)
        Matriz_ECM[i,] <- ECM
        Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, 1)  
        Coef[,1] <- coef(modelo) 
        Betas[[i]] <- Coef
        N.Var <- paste(Com.Var[[i]], collapse = " y ")
        Variables_del_Modelo[i] <- N.Var    
      }
      DF <- data.frame(Variables_del_Modelo, Matriz_ECM)
      DF <- DF[order(DF$Matriz_ECM), ]
      #Betas
#####
    } else if (input$dist == "holdout") {
#####
      set.seed(Semilla)
      Variables <- names(datos())[-1]
      V.Dependiente <-  paste(names(datos())[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
	R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
	      unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables) 
      n <- seq_along(Com.Var) 
      Betas <- list() 
      Matriz_ECM <- matrix(numeric(), length(n), 1)
      Variables_del_Modelo <- numeric(length(n))
      Sep <- round(0.8 * nrow(datos()))

      for (i in n) {
	      M.Smp <- sample(1:nrow(datos()), Sep)
      	D.Ent <- datos()[M.Smp, ] 
      	D.Pru <- datos()[-M.Smp, ]
         	Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
      	modelo <- lm(Formula, data = D.Ent)
      	Predicciones <- predict(modelo, newdata = D.Pru)
      	ECM <- mean((D.Pru[, names(datos())[1]] - Predicciones)^2)
      	Matriz_ECM[i,] <- ECM
      	Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, 1)  
	      Coef[,1] <- coef(modelo) 
      	Betas[[i]] <- Coef
      	N.Var <- paste(Com.Var[[i]], collapse = " y ")
	      Variables_del_Modelo[i] <- N.Var    	
      }
      DF <- data.frame(Variables_del_Modelo, Matriz_ECM)
      DF <- DF[order(DF$Matriz_ECM), ]
      #Betas
######
    } else if (input$dist == "SubMA") {
######
      set.seed(Semilla)
      Variables <- names(datos())[-1]
      V.Dependiente <-  paste(names(datos())[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
	      R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
      	unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables) 
      n <- seq_along(Com.Var) 
      m <- length(n)
      Sep <- round(0.8 * nrow(datos()))
      Matriz_ECM <- matrix(numeric(), m, Iteraciones) 
      Variables_del_Modelo <- numeric(m)
      Betas <- list() 
      PromBetas <- list()

      for (i in n) {
      Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, Iteraciones)  
      	for (k in 1:Iteraciones) {
  		  M.Smp <- sample(1:nrow(datos()), Sep)
		  D.Ent <- datos()[M.Smp, ] 
		  D.Pru <- datos()[-M.Smp, ]
		  Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
		  modelo <- lm(Formula, data = D.Ent)
		  N.Var <- paste(Com.Var[[i]], collapse = " y ")
		  Predicciones <- predict(modelo, newdata = D.Pru)
		  ECM <- mean((D.Pru[, names(datos())[1]] - Predicciones)^2)
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
######
    } else if (input$dist == "ValCrz") {
######
      set.seed(Semilla)
      filas <- nrow(datos())
      Variables <- names(datos())[-1]
      V.Dependiente <-  paste(names(datos())[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
      	R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
      	unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables) 
      n <- seq_along(Com.Var)
      Betas <- list() 
      Matriz_ECM <- matrix(numeric(), length(n), 1)
      Variables_del_Modelo <- numeric(length(n))
      Predicciones <- numeric(filas)

      for (i in n) {
	for (k in 1:filas) {			
	  D.Ent <- datos()[-k,]
	  N.Var <- paste(Com.Var[[i]], collapse = " y ")
	  Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
  	  modelo <- lm(Formula, data = D.Ent)
	  Predicciones[k] <- predict(modelo, datos()[k,])
	  Matriz_ECM[i,1] <- mean((datos()[, names(datos())[1]] - Predicciones)^2)
	}
      Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, 1)  
      Coef[,1] <- coef(modelo) 
      Betas[[i]] <- Coef
      Variables_del_Modelo[i] <- N.Var
      }
      DF <- data.frame(Variables_del_Modelo, Matriz_ECM)
      DF <- DF[order(DF$Matriz_ECM), ]
      #Betas
      #print(paste("El modelo con el ECM más chico es", Variables_del_Modelo[which.min(Matriz_ECM)], "con", min(Matriz_ECM)))
######
    } else if (input$dist == "Boot") {
######
      set.seed(Semilla)
      Variables <- names(datos())[-1]
      V.Dependiente <-  paste(names(datos())[1], "~", collapse = " ")
      Combinaciones <- function(lista) {
      	R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
      	unlist(R, recursive = FALSE) 
      }
      Com.Var <- Combinaciones(Variables)
      l <- length(Variables)
      n <- seq_along(Com.Var)
      m <- length(n)
      Matriz_ECM <- matrix(numeric(), m, Iteraciones)  
      Variables_del_Modelo <- numeric(m)
      Betas <- list() 
      PromBetas <- list()

      for (i in n) {
      Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, Iteraciones)  
	for (k in 1:Iteraciones) {
	  NumAlea <- sample(1:nrow(datos()), replace=TRUE) 
	  N.NumAlea <- setdiff(1:nrow(datos()), NumAlea)
	  D.Ent <- datos()[NumAlea,] 
	  D.Pru <- datos()[N.NumAlea,]
	  N.Var <- paste(Com.Var[[i]], collapse = " y ")
	  Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
	  modelo <- lm(Formula, data = D.Ent)
	  Predicciones <- predict(modelo, newdata = D.Pru)
	  ECM <- mean((D.Pru[, names(datos())[1]] - Predicciones)^2)
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
      # PromBetas
      # print(paste("El modelo con el ECM más chico es", Variables_del_Modelo[which.min(Promedio_ECM)], "con", min(Promedio_ECM)))
    }
    DF
  })
}

shinyApp(ui, server)