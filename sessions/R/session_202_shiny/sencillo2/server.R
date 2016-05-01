# Definición de la parte server
shinyServer(function(input, output) {
  
  # Cambia con sencillo1 en que solo se actualiza grafico y tabla cuando presiono boton no cuando muevo deslizador
  
  datos <- reactive({
              input$ahora
              n <- isolate(input$tamano)
              data.frame(clientes=1:n,ingresos=round(rexp(n)*1000,2))
  })
  
  output$grafico <- renderPlot({
    hist(datos()$ingresos)
  })
  output$tabla <- renderDataTable({
    datos()
  })
})
