# pacman::p_load(shiny, tidyverse, shinydashboard, lubridate, scales)
library(shiny)
# library(bslib)
library(ggplot2)
library(sf)


##TODO:
# fix ggplot dimensions
# Make mirror x negative

# Look into webGL for second plot
# As other specs added we add lines to plot and baffle height?
# Does webgl support 3d structure
# rgl is an option. Others should be considered

options(digits=2)

# Functions
#---------------------------
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
#---------------------------


# Frontend
#---------------------------
design_accordion <- bslib::accordion_panel(
  "Design",# icon = bsicons::bs_icon("menu-app"),
  numericInput('maxDim','Longest Dimension (cm)', 210, min = 0),
  numericInput('baffleHeight','Baffle Height (cm)', 2, min = 0),
  numericInput('chamberHeight','Max Chamber Height (cm)', 2.5, min = 0),
  numericInput('chamberWidth','Chamber Width (cm)', 15, min = 0),
  numericInput('percVertBaffle','% Length with Vertical Baffles', 100, min = 0, max = 100)
)

materials_accordion <- bslib::accordion_panel(
  "Materials",# icon = bsicons::bs_icon("sliders"),
  numericInput('FP','Fill Power', 750, min = 500, max = 1000, step = 50),
  numericInput('overstuff','% Overstuff', 10),
  numericInput('innerWeight','Inner Fabric Weight (gsm)', 50, min = 0),
  numericInput('outerWeight','Outer Fabric Weight (gsm', 50, min = 0),
  numericInput('baffleWeight','Baffle Material Weight (gsm)', 25, min = 0),
  numericInput('seamAllowance','Seam Allowance (cm)', 1, min = 0, step = 0.25)
)

manual_entry_card <- bslib::card(
  bslib::card_body(
    bslib::layout_column_wrap(
      width = 1/2,
      # manual input
      shiny::numericInput('x_add','X', 0, min = 0),
      shiny::numericInput('y_add','Y', 0, min = 0)
    )
  )
)

plot_input_card <- bslib::card(
  bslib::card_header("Define vertices manually or click to draw right side of the quilt"),
    manual_entry_card, 
    bslib::card_body(fillable = T,
    # button to add vertices
    actionButton("add_point", "Add Point"),
    
    ),
    verbatimTextOutput("hover_info"),
    plotOutput("input_plot",
            #add plot click functionality
              click = "plot_click",
            #add the hover options
              hover = hoverOpts(
                id = "plot_hover",
                nullOutside = TRUE)
              ),
    # button to remove last vertex
    actionButton("rem_point", "Remove Last Point"),
    actionButton("rem_all_points", "Clear")
)

selected_points_card <- bslib::card(
  bslib::card_header("Selected Points"),
  tableOutput("table")
)

cross_section_card <- bslib::card(
  bslib::card_header("Scrolling content"),
  # plotOutput("poly_plot")
  verbatimTextOutput("test")
)
card2 <- bslib::card(
  bslib::card_header("Nothing much here"),
  "This is it."
)


# UI layout
ui <- bslib::page_navbar(
  title = "Down Quilt Designer",
  theme = bslib::bs_theme(version=5, bootswatch = "sketchy"), # Can specify base_font and code_font
  sidebar = bslib::sidebar(
    bslib::accordion(
      design_accordion,
      materials_accordion
  )
),
bslib::nav_panel(
  title = "Dimensions",
  bslib::layout_column_wrap(
    width = NULL,
    height = 800,
    fill = FALSE,
    style = bslib::css(grid_template_columns = "2fr 1fr"),
    plot_input_card, 
    selected_points_card)
  ),
bslib::nav_panel(
  title = "Output",
  bslib::layout_column_wrap(
                  width = 1/4,
                  height = 300,
                  cross_section_card,
                  card2)
                )

)
#---------------------------


# Backend
#---------------------------
server = function(input, output){
  
  # set up reactive dataframe with example data
  values <- shiny::reactiveValues()
  values$user_input <- data.frame(x = c(0, 71, 71, 50, 0),
                                  y = c(210, 210, 100, 0, 0))

  # # test
  # values$user_input <- data.frame(x = c(0, 5, 5, 0),
  # y = c(10, 10, 0, 0))
  
all_selected_points_x <- shiny::reactive({
  req(values$user_input)
  c(values$user_input$x, -rev(values$user_input$x))
})

all_selected_points_y <- shiny::reactive({
  req(values$user_input)
  c(values$user_input$y, rev(values$user_input$y))
})
  
  
  # create design plot
  output$input_plot = shiny::renderPlot({
    ggplot(values$user_input, aes(x = x, y = y)) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_point(aes()) +
      geom_path(linewidth = 1.5) +
      lims(x = c(0, input$maxDim/2), y = c(0, input$maxDim)) +
      theme(legend.position = "bottom")
  })
  
  # add new row to reactive dataframe upon clicking plot
  shiny::observeEvent(input$plot_click, {
    add_row <- data.frame(x = round_any(input$plot_click$x, 0.5),
                          y = round_any(input$plot_click$y, 0.5))
    # add row to the data.frame
    values$user_input <- rbind(values$user_input[1:nrow(values$user_input)-1,], add_row, values$user_input[nrow(values$user_input),])
  })

  # add row on actionButton click
  shiny::observeEvent(input$add_point, {
    add_row <- rbind(values$user_input, c(input$x_add, input$y_add))
    values$user_input <- add_row
  })
  
  # remove row on actionButton click
  shiny::observeEvent(input$rem_point, {
    rem_row <- values$user_input[-nrow(values$user_input), ]
    values$user_input <- rem_row
  })

  # clear all selected points on actionButton click
    # remove row on actionButton click
    shiny::observeEvent(input$rem_all_points, {
      values$user_input <- data.frame(x=double(),
                              y=double()
    )
    })
  
  
  # render a table of the dataframe
  output$table <- shiny::renderTable({
    values$user_input
    ## Test
    # all_selected_points()
  })
  
  output$hover_info <- shiny::renderPrint({
      hover=input$plot_hover
      cat("X value:", formatC(round_any(hover$x, 0.5), digits = 1, format = "f"), "\n")
      cat("Y value:", formatC(round_any(hover$y, 0.5  ), digits = 1, format = "f"))
  })

  output$test <- shiny::renderPrint({sf::st_area(sf::st_polygon(list(cbind(all_selected_points_x(), all_selected_points_y()))))})

}
#---------------------------

# Run app
shiny::shinyApp(ui, server)