# Definción del UI
shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Magnitud de terremotos"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Tamaño de la muestra:",
                  min = 1,max = 1000,value=20,step=1)
      ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("grafico"),
      br(),
      dataTableOutput("tabla")
    )
  )
))


