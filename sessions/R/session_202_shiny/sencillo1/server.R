# Definición de la parte server
shinyServer(function(input, output) {
  
  # IMP!!!! Hay que crear un reactive porque con un solo parametro tendré dos cosas a pintar o usar
  
  datos <- reactive({
              n=input$n
              data.frame(clientes=1:n,ingresos=round(rexp(n)*1000,2))
  })
  
  output$grafico <- renderPlot({
    hist(datos()$ingresos)
  })
  output$tabla <- renderDataTable({
    datos()
  })
})
