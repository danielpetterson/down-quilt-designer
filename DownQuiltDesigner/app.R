pacman::p_load(shiny, tidyverse, shinydashboard, lubridate, scales)

# source("R/server.R")
# source("R/ui.R")

options(digits=2)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

ui <- pageWithSidebar(
  headerPanel("Down Quilt Designer"),
  sidebarPanel(
  #   # radioButtons("color", "Pick Color", c("Pink", "Green", "Blue")),
  #   # selectInput("shape", "Select Shape:", c("Circle", "Triangle"))
  ),
  mainPanel(
    fluidRow(column(width = 6,
                    h4("Enter the length of the quilt"),
                    numericInput( 
                      "quilt_length", 
                      "Quilt Length (cm)", 
                      value = 200
                    ), 
                    h4("Click to draw half of the quilt"),
                    plotOutput("plot1",
                               #add plot click functionality
                               click = "plot_click",
                               #add the hover options
                               hover = hoverOpts(
                                 id = "plot_hover",
                                 nullOutside = TRUE))),
             verbatimTextOutput("hover_info"),
             column(width = 6,
                    h4("Table of points on plot"),
                    tableOutput("table"))),
      
      actionButton("rem_point", "Remove Last Point"),


)
)

server = function(input, output){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric())
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      geom_point(aes(), size = 2) +
      geom_path() +
      lims(x = c(0, 200), y = c(0, 200)) +
      theme(legend.position = "bottom")
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = round_any(input$plot_click$x, 0.5),
                          y = round_any(input$plot_click$y, 0.5))
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
  })
  
  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  
  # 5. render a table of the growing dataframe ##
  output$table <- renderTable({
    values$DT
  })
  
  output$hover_info <- renderPrint({
      hover=input$plot_hover
      cat("X value:", hover$x, "\n")
      cat("Y value:", hover$y)
  })

}

shinyApp(ui, server)
