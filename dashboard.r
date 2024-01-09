datacsv <- read.csv("C:/Users/Acer/Downloads/ventas (12).csv")
View(datacsv)


library(naniar)
library(plotly)

# Convertir las cadenas "null" a valores NA
datacsv[datacsv == "null"] <- NA

# Crear gráfico de cantidad de NA y NULL por columna
grafico_na_null <- gg_miss_var(datacsv) +
  theme_minimal() +
  ggtitle("Cantidad de NA y NULL por Columna")

# Convertir a plotly
grafico_interactivo_na_null <- ggplotly(grafico_na_null)

# Mostrar el gráfico interactivo
grafico_interactivo_na_null
##########################################################################################
#if (!requireNamespace("naniar", quietly = TRUE)) {
#  install.packages("naniar")
#}
#if (!requireNamespace("dplyr", quietly = TRUE)) {
# install.packages("dplyr")
#}
str(datacsv)
#install.packages("forcats")
library(naniar)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(forcats)

# Eliminar la columna "Más.encabezados..."
#datacsv <- select(datacsv, -Más.encabezados...)
datacsv <- datacsv[, -which(names(datacsv) == "Más.encabezados...")]


# Eliminar filas con valores "null" en cualquier columna
datacsv_sin_null <- datacsv[!apply(datacsv == "null", 1, any), ]

# Eliminar filas con valores NA en cualquier columna
datacsv_sin_na <- na.omit(datacsv_sin_null)




data <- datacsv_sin_na %>% filter(nzchar(trimws(Tipo.Aceite)))
data




str(data)

#tipo de dato

data_filtrada <- data %>%
  mutate(
    Cantidad.Aceite = as.numeric(Cantidad.Aceite),
    Kilometraje.Actual = as.integer(Kilometraje.Actual),
    Costo.Aceite = as.numeric(Costo.Aceite),
    Costo.Servicio = as.numeric(Costo.Servicio),
    Año.Vehículo = as.integer(Año.Vehículo)
  )


#ESTADISTICAS DESCRIPTIVAS
# Instalar y cargar paquetes si no están instalados
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(plotly)
library(dplyr)
#########################Estadisticas
# Función para generar gráfico de estadísticas descriptivas
generar_grafico_estadisticas <- function(dataframe, columnas) {
  # Resumen de estadísticas descriptivas
  estadisticas_descriptivas <- dataframe %>%
    summarise(across(all_of(columnas), list(mean = mean, sd = sd, min = min, max = max)))
  
  # Convertir dataframe a formato largo
  estadisticas_descriptivas_largo <- tidyr::gather(estadisticas_descriptivas, key = "Estadistica", value = "Valor")
  
  # Crear gráfico de barras usando plotly
  grafico_estadisticas <- plot_ly(estadisticas_descriptivas_largo, x = ~Estadistica, y = ~Valor, type = "bar") %>%
    layout(title = "Estadísticas Descriptivas",
           yaxis = list(title = "Valor"),
           barmode = "group")  # Agregado para agrupar las barras
  
  return(grafico_estadisticas)
}

# Ejemplo de uso con tu dataframe data_filtrada y columnas específicas
data_filtrada <- data_filtrada  # Reemplaza con tu propio dataframe
columnas_interesantes <- c("Cantidad.Aceite", "Kilometraje.Actual", "Costo.Aceite")  # Reemplaza con tus columnas de interés

# Generar y mostrar el gráfico
grafico_resultante <- generar_grafico_estadisticas(data_filtrada, columnas_interesantes)
print(grafico_resultante)
##########################
#########################matriz de correlaciones

# Crear dataframe de ejemplo (puedes reemplazarlo con tu propio dataframe)
dataframe_ejemplo <- data_filtrada  # Reemplaza con tu propio dataframe

# Filtrar columnas numéricas
columnas_numericas <- dataframe_ejemplo %>%
  select_if(is.numeric) %>%
  names()

# Calcular la matriz de correlación solo para columnas numéricas
matriz_correlacion <- cor(dataframe_ejemplo[, columnas_numericas])

# Imprimir la matriz de correlación
print(matriz_correlacion)

library(corrplot)
#install.packages("heatmaply")
library(heatmaply)
# Crear dataframe de ejemplo (puedes reemplazarlo con tu propio dataframe)
# Crear dataframe de ejemplo (puedes reemplazarlo con tu propio dataframe)
dataframe_ejemplo <- data_filtrada  # Reemplaza con tu propio dataframe

# Filtrar columnas numéricas
columnas_numericas <- dataframe_ejemplo %>%
  select_if(is.numeric) %>%
  names()

# Calcular la matriz de correlación solo para columnas numéricas
matriz_correlacion <- cor(dataframe_ejemplo[, columnas_numericas])

# Crear gráfico interactivo de la matriz de correlación con heatmaply
grafico_heatmaply <- heatmaply(matriz_correlacion, 
                               labCol = colnames(matriz_correlacion),
                               labRow = rownames(matriz_correlacion),
                               width = 800, height = 800)

# Mostrar el gráfico
print(grafico_heatmaply)
####################################################

str(data_filtrada)
#
#if (!requireNamespace("DAAG", quietly = TRUE)) {
#  install.packages("DAAG")
#}

# Cargar el paquete DAAG
library(DAAG)
library(boot)

# Cargar el paquete DAAG
library(DAAG)

# Ajustar el modelo lineal
modelo <- lm(Costo.Servicio ~ Cantidad.Aceite + Kilometraje.Actual + Costo.Aceite + Año.Vehículo, data = data_filtrada)

# Realizar la validación cruzada
cv_resultados <- cv.lm(data_filtrada, modelo)

# Ver los resultados
print(cv_resultados)

#
library(MASS)
# Realizar selección de características basada en AIC
modelo_AIC <- stepAIC(modelo, direction = "both")

# Imprimir el modelo seleccionado
print(modelo_AIC)


modelo_final <- lm(formula = Costo.Servicio ~ Cantidad.Aceite + Kilometraje.Actual + Costo.Aceite, data = data_filtrada)


# Observaciones para predecir
nuevos_datos <- data.frame(
  Cantidad.Aceite = c(5.5, 6.0, 4.5),
  Kilometraje.Actual = c(50000, 55000, 48000),
  Costo.Aceite = c(25.99, 30.50, 20.75)
)

# Realizar predicciones
predicciones <- predict(modelo_final, newdata = nuevos_datos)

# Mostrar las predicciones
print(predicciones)



##

library(ggplot2)

# Crear un dataframe con los datos reales y pronosticados
resultados_prediccion <- data.frame(
  Real = data_filtrada$Costo.Servicio,
  Predicciones = predict(modelo_final)
)

# Gráfico de dispersión con línea de ajuste
ggplot(resultados_prediccion, aes(x = Real, y = Predicciones)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Comparación entre Valores Reales y Predicciones",
       x = "Valores Reales",
       y = "Valores Predichos")



#

# Crear un dataframe con los datos reales y pronosticados
resultados_prediccion <- data.frame(
  Real = data_filtrada$Costo.Servicio,
  Predicciones = predict(modelo_final)
)

# Añadir datos de predicción
nuevos_datos <- data.frame(
  Real = c(5.5, 6.0, 4.5),
  Predicciones = predict(modelo_final, nuevos_datos)
)

# Combinar dataframes
resultados_prediccion <- rbind(resultados_prediccion, nuevos_datos)

# Crear gráfico de dispersión con línea de ajuste y leyenda
ggplot(resultados_prediccion, aes(x = Real, y = Predicciones, color = "Datos")) +
  geom_point(aes(shape = "Real"), size = 3) +
  geom_point(data = nuevos_datos, aes(x = Real, y = Predicciones, color = "Nuevos Datos"), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Comparación entre Valores Reales y Predicciones",
       x = "Valores Reales",
       y = "Valores Predichos") +
  scale_shape_manual(name = "Datos", values = c(16, NA)) +
  scale_color_manual(name = "Datos", values = c("blue", NA, "red")) +
  theme_minimal() +
  theme(legend.position = "top")

#







##3♣3♣3♣3♣#
#listar:
library(shiny)
library(naniar)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(forcats)





# Crear la interfaz de usuario
ui <- fluidPage(
  titlePanel("Dashboard de Predicciones"),
  sidebarLayout(
    sidebarPanel(
      h4("Nuevos Datos"),
      numericInput("cantidad_aceite", "Cantidad de Aceite:", value = 5.5, min = 0, max = 10),
      numericInput("kilometraje", "Kilometraje Actual:", value = 50000, min = 0, max = 100000),
      numericInput("costo_aceite", "Costo de Aceite:", value = 25.99, min = 0, max = 100),
      actionButton("predecir_btn", "Predecir")
    ),
    mainPanel(
      plotOutput("scatter_plot"),
      h5("Resultados de Predicción:"),
      verbatimTextOutput("prediccion_resultados"),
      h5("Ecuación del Modelo:"),
      verbatimTextOutput("modelo_ecuacion")
    )
  )
)

# Crear el servidor
server <- function(input, output) {
  #
  datacsv <- read.csv("C:/Users/Acer/Downloads/ventas (12).csv")
  View(datacsv)
  
  
  #if (!requireNamespace("naniar", quietly = TRUE)) {
  #  install.packages("naniar")
  #}
  #if (!requireNamespace("dplyr", quietly = TRUE)) {
  # install.packages("dplyr")
  #}
  str(datacsv)
  #install.packages("forcats")
  library(naniar)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(tidyverse)
  library(forcats)
  
  # Eliminar la columna "Más.encabezados..."
  #datacsv <- select(datacsv, -Más.encabezados...)
  datacsv <- datacsv[, -which(names(datacsv) == "Más.encabezados...")]
  
  
  # Eliminar filas con valores "null" en cualquier columna
  datacsv_sin_null <- datacsv[!apply(datacsv == "null", 1, any), ]
  
  # Eliminar filas con valores NA en cualquier columna
  datacsv_sin_na <- na.omit(datacsv_sin_null)
  
  
  
  
  data <- datacsv_sin_na %>% filter(nzchar(trimws(Tipo.Aceite)))
  data
  
  
  
  
  str(data)
  
  #tipo de dato
  
  data_filtrada <- data %>%
    mutate(
      Cantidad.Aceite = as.numeric(Cantidad.Aceite),
      Kilometraje.Actual = as.integer(Kilometraje.Actual),
      Costo.Aceite = as.numeric(Costo.Aceite),
      Costo.Servicio = as.numeric(Costo.Servicio),
      Año.Vehículo = as.integer(Año.Vehículo)
    )
  
  
  str(data_filtrada)
  #
  #if (!requireNamespace("DAAG", quietly = TRUE)) {
  #  install.packages("DAAG")
  #}
  
  # Cargar el paquete DAAG
  library(DAAG)
  library(boot)
  
  # Cargar el paquete DAAG
  library(DAAG)
  
  # Ajustar el modelo lineal
  modelo <- lm(Costo.Servicio ~ Cantidad.Aceite + Kilometraje.Actual + Costo.Aceite + Año.Vehículo, data = data_filtrada)
  
  # Realizar la validación cruzada
  cv_resultados <- cv.lm(data_filtrada, modelo)
  
  # Ver los resultados
  print(cv_resultados)
  
  #
  library(MASS)
  # Realizar selección de características basada en AIC
  modelo_AIC <- stepAIC(modelo, direction = "both")
  
  # Imprimir el modelo seleccionado
  print(modelo_AIC)
  
  
  modelo_final <- lm(formula = Costo.Servicio ~ Cantidad.Aceite + Kilometraje.Actual + Costo.Aceite, data = data_filtrada)
  
  
  # Observaciones para predecir
  nuevos_datos <- data.frame(
    Cantidad.Aceite = c(5.5, 6.0, 4.5),
    Kilometraje.Actual = c(50000, 55000, 48000),
    Costo.Aceite = c(25.99, 30.50, 20.75)
  )
  
  # Realizar predicciones
  predicciones <- predict(modelo_final, newdata = nuevos_datos)
  
  # Mostrar las predicciones
  print(predicciones)
  
  
  
  ##
  
  library(ggplot2)
  
  # Crear un dataframe con los datos reales y pronosticados
  resultados_prediccion <- data.frame(
    Real = data_filtrada$Costo.Servicio,
    Predicciones = predict(modelo_final)
  )
  
  # Gráfico de dispersión con línea de ajuste
  ggplot(resultados_prediccion, aes(x = Real, y = Predicciones)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Comparación entre Valores Reales y Predicciones",
         x = "Valores Reales",
         y = "Valores Predichos")
  
  
  
  #
  
  # Crear un dataframe con los datos reales y pronosticados
  resultados_prediccion <- data.frame(
    Real = data_filtrada$Costo.Servicio,
    Predicciones = predict(modelo_final)
  )
  
  # Añadir datos de predicción
  nuevos_datos <- data.frame(
    Real = c(5.5, 6.0, 4.5),
    Predicciones = predict(modelo_final, nuevos_datos)
  )
  
  # Combinar dataframes
  resultados_prediccion <- rbind(resultados_prediccion, nuevos_datos)
  
  # Crear gráfico de dispersión con línea de ajuste y leyenda
  ggplot(resultados_prediccion, aes(x = Real, y = Predicciones, color = "Datos")) +
    geom_point(aes(shape = "Real"), size = 3) +
    geom_point(data = nuevos_datos, aes(x = Real, y = Predicciones, color = "Nuevos Datos"), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
    labs(title = "Comparación entre Valores Reales y Predicciones",
         x = "Valores Reales",
         y = "Valores Predichos") +
    scale_shape_manual(name = "Datos", values = c(16, NA)) +
    scale_color_manual(name = "Datos", values = c("blue", NA, "red")) +
    theme_minimal() +
    theme(legend.position = "top")
  
  #
  
  
  
  # Instalar y cargar el paquete Shiny si no está instalado
  #if (!requireNamespace("shiny", quietly = TRUE)) {
  #  install.packages("shiny")
  #}
  
  # Instalar y cargar el paquete Shiny si no está instalado
  #if (!requireNamespace("shiny", quietly = TRUE)) {
  #  install.packages("shiny")
  #}
  
  
  # Datos iniciales para el gráfico
  resultados_prediccion <- data.frame(
    Real = data_filtrada$Costo.Servicio,
    Predicciones = predict(modelo_final)
  )
  
  # Observador para la acción de predicción
  observeEvent(input$predecir_btn, {
    # Crear dataframe con nuevos datos
    nuevos_datos <- data.frame(
      Real = input$cantidad_aceite,  # Usaremos el campo Real para representar los datos nuevos
      Predicciones = predict(modelo_final, newdata = data.frame(
        Cantidad.Aceite = input$cantidad_aceite,
        Kilometraje.Actual = input$kilometraje,
        Costo.Aceite = input$costo_aceite
      ))
    )
    
    # Añadir datos nuevos al dataframe de resultados
    resultados_prediccion <- rbind(resultados_prediccion, nuevos_datos)
    
    # Actualizar el gráfico
    output$scatter_plot <- renderPlot({
      # Gráfico de dispersión con colores diferentes para datos nuevos y existentes
      ggplot(resultados_prediccion, aes(x = Real, y = Predicciones, color = factor(ifelse(Real %in% c(5.5, 6.0, 4.5), "Nuevos Datos", "Datos existentes")))) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
        labs(title = "Comparación entre Valores Reales y Predicciones",
             x = "Valores Reales",
             y = "Valores Predichos") +
        scale_color_manual(name = "Datos", values = c("blue", "red"))
    })
    
    # Resultados de predicción
    output$prediccion_resultados <- renderText({
      mensaje <- paste("Según los datos insertados, se pronostica que el costo de servicio será de", round(nuevos_datos$Predicciones, 2))
      return(mensaje)
    })
  })
  
  # Ecuación del modelo
  output$modelo_ecuacion <- renderText({
    coeficientes <- coef(modelo_final)
    ecuacion <- paste("Ecuación del Modelo: Costo.Servicio =", round(coeficientes[1], 2), "+", 
                      round(coeficientes[2], 2), "* Cantidad.Aceite +", 
                      round(coeficientes[3], 6), "* Kilometraje.Actual +", 
                      round(coeficientes[4], 6), "* Costo.Aceite")
    return(ecuacion)
  })
  
}

# Crear la aplicación Shiny
shinyApp(ui, server,options = list(host = '0.0.0.0', port = 4040))

