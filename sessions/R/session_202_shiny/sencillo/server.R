# Definición de la parte server
shinyServer(function(input, output) {
  
  output$grafico <- renderPlot({
    ggplot(cars,aes(x=dist,y=speed)) +
		geom_point(size=input$grossor,col="green3",alpha=input$trans) +
		geom_smooth(method='lm',formula=y~ns(x,df=input$suavizado))
  })
})
