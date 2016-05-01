# Definción del UI
shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Una regresión de jalmen"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("grossor",
                  "Tamaño de los puntos verdes:",
                  min = 1,max = 21,value = 3,step=1),
      sliderInput("trans",
                  "Grado de trnsparencia:",
                  min = 0,max = 1,value =.5,step=0.1),
      sliderInput("suavizado",
                  "Grado de suavizado de la curva de ajuste:",
                  min = 1,max = 10,value =0,step=1)
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("grafico")
    )
  )
))
