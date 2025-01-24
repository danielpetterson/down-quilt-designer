library(shiny)
library(ggplot2)

ui <- shiny::pageWithSidebar(
  headerPanel("Example"),
  sidebarPanel(
      radioButtons("color", "Pick Color", c("Pink", "Green", "Blue")),
      selectInput("shape", "Select Shape:", c("Circle", "Triangle"))
  ),
  mainPanel(
      fluidRow(column(width = 6,
                      h4("Click plot to add points"),
                      actionButton("rem_point", "Remove Last Point"),
                      plotOutput("plot1", click = "plot_click")),
               column(width = 6,
                      h4("Table of points on plot"),
                      tableOutput("table")))
  )
)

server = function(input, output){

  ## 1. set up reactive dataframe ##
  values <- shiny::reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          color = factor(),
                          shape = factor())

  ## 2. Create a plot ##
  output$plot1 = shiny::renderPlot({
     ggplot(values$DT, aes(x = x, y = y)) +
          geom_point(aes(color = color,
                         shape = shape), size = 5) +
          lims(x = c(0, 100), y = c(0, 100)) +
          theme(legend.position = "bottom") +
          # include so that colors don't change as more color/shape chosen
          scale_color_discrete(drop = FALSE) +
          scale_shape_discrete(drop = FALSE)
  })

  ## 3. add new row to reactive dataframe upon clicking plot ##
  shiny::observeEvent(input$plot_click, {
      # each input is a factor so levels are consistent for plotting characteristics
      add_row <- data.frame(x = input$plot_click$x,
                            y = input$plot_click$y,
                            color = factor(input$color, levels = c("Pink", "Green", "Blue")),
                            shape = factor(input$shape, levels = c("Circle", "Triangle")))
      # add row to the data.frame
      values$DT <- rbind(values$DT, add_row)
  })

  ## 4. remove row on actionButton click ##
  shiny::observeEvent(input$rem_point, {
      rem_row <- values$DT[-nrow(values$DT), ]
      values$DT <- rem_row
  })

  ## 5. render a table of the growing dataframe ##
  output$table <- shiny::renderTable({
      values$DT
  })
}

shiny::shinyApp(ui, server)