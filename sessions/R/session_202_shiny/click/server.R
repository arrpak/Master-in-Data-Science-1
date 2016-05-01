# Clic
shinyServer(function(input, output) {
   data <- reactive({
      input$newplot
      # Add a little noise to the cars data so the points move
      cars + rnorm(nrow(cars))
    })
    output$plot <- renderPlot({
      d <- data()
      plot(d$speed, d$dist)
    })
    output$plot_clickinfo <- renderPrint({
      cat("Click:\n")
      str(input$plot_click)
    })
    output$plot_hoverinfo <- renderPrint({
      cat("Hover (throttled):\n")
      str(input$plot_hover)
    })
    output$plot_brushinfo <- renderPrint({
      cat("Brush (debounced):\n")
      str(input$plot_brush)
    })
    output$plot_clickedpoints <- renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(data(), input$plot_click, "speed", "dist")
      if (nrow(res) == 0)
        return()
      res
    })
    output$plot_brushedpoints <- renderDataTable({
      res <- brushedPoints(data(), input$plot_brush, "speed", "dist")
      if (nrow(res) == 0)
        return()
      res
    })
})
