# pacman::p_load(shiny, tidyverse, shinydashboard, lubridate, scales)
library(shiny)
library(ggplot2)

# source("R/server.R")
# source("R/ui.R")

##TODO:
# Make mirror x negative

# Use completed dataset for constructing final plot
# Look into webGL for second plot
# As other specs added we add lines to plot and baffle height?
# Does webgl support 3d structure
# rgl is an option. Others should be considered

options(digits=2)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

ui <- pageWithSidebar(
  headerPanel("Down Quilt Designer"),
  sidebarPanel(
  ),
  mainPanel(
    fluidRow(column(width = 6,
                    h4("Enter the length of the quilt"),
                    numericInput( 
                      "quilt_length", 
                      "Quilt Length (cm)", 
                      value = 200
                    ), 
                    h4("Click to draw the right half of the quilt"),
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
  values$DT <- data.frame(x = c(0,0),
                          y = c(0,0))
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      geom_point(aes(), size = 2) +
      geom_path() +
      lims(x = c(0, 200), y = c(0, input$quilt_length)) +
      theme(legend.position = "bottom")
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = round_any(input$plot_click$x, 0.5),
                          y = round_any(input$plot_click$y, 0.5))
    # add row to the data.frame
    values$DT <- rbind(values$DT[1:nrow(values$DT)-1,], add_row, values$DT[nrow(values$DT),])
    values$points <- rbind(values$DT, values$DT[nrow(values$DT):1,])
  })
  
  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  
  # 5. render a table of the growing dataframe ##
  output$table <- renderTable({
    values$points
  })
  
  output$hover_info <- renderPrint({
      hover=input$plot_hover
      cat("X value:", formatC(round_any(hover$x, 0.5), digits = 1, format = "f"), "\n")
      cat("Y value:", formatC(round_any(hover$y, 0.5  ), digits = 1, format = "f"))
  })

}

shinyApp(ui, server)
