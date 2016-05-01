# Definición del UI
shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Longitudes y Latitudes del territorio español"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("lng","Longitud:",min = -10,max = 0,value = -4,step=0.1), 
	  sliderInput("lat","Latitud:",min = 36,max = 44,value = 40,step=0.1)
    ),
	
    # Mostrar mapa
    mainPanel(
      leafletOutput('Mapa')
	  #plotOutput("Mapa")
    )
  )
))
