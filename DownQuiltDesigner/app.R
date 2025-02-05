# pacman::p_load(shiny, tidyverse, shinydashboard, lubridate, scales)
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(sf)


##TODO:
# (gg)Plot subpolygons with pointer displaying info about:
# - Area
# - Volume
# - Grams of down needed

# Cross section image

# Calculate dims for upper if diff cut

# Info panel with expected weight and total down, baffle material needed

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
  numericInput('outerWeight','Outer Fabric Weight (gsm)', 50, min = 0),
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
              height = 600,
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

card2 <- bslib::card(
  bslib::card_header("Text Output"),
  # plotOutput("poly_plot")
  verbatimTextOutput("test")
)


cross_section_card <- bslib::card(
  bslib::card_header("Cross Sectional View"),
  plotOutput("area_plot")
)

area_card <- bslib::card(
  bslib::card_header("Aerial View"),
  plotOutput("area_plot")
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
    height = NULL,
    fill = FALSE,
    style = bslib::css(grid_template_columns = "2fr 1fr"),
    plot_input_card, 
    selected_points_card)
  ),
bslib::nav_panel(
  title = "Output",
  bslib::layout_column_wrap(
                  width = 1/2,
                  height = 300,
                  area_card,
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
  
all_selected_points_x <- shiny::reactive({
  req(values$user_input)
  c(values$user_input$x, -rev(values$user_input$x))
})

all_selected_points_y <- shiny::reactive({
  req(values$user_input)
  c(values$user_input$y, rev(values$user_input$y))
})
  
#reactive expression to calculate subpolygons
polygon_df <- shiny::reactive({
  req(all_selected_points_x)
  req(all_selected_points_y)
  req(input$chamberWidth)
  req(input$baffleHeight)

  #create polygon from selected points
  poly <- sf::st_polygon(list(cbind(all_selected_points_x(), all_selected_points_y())))
  #create bounding boxes that are chamberWidth apart until greatest width
  bbox <- st_bbox(poly)
  # Create vertical lines spaced chamberWidth apart
  x_seq <- seq(0, (bbox["xmax"] + input$chamberWidth), by = input$chamberWidth)
  lines <- lapply(x_seq, function(x) {
    st_linestring(rbind(c(x, bbox["ymin"]), c(x, bbox["ymax"])))
  })
  # Combine lines into a multi-line geometry
  multiline <- st_sfc(lines, crs = st_crs(polygon))
  bboxes <- list()
  for (i in 1:(length(multiline)-1))
    {
    current_line <- multiline[i]
    next_line <- multiline[i+1]

    bbox_section <- st_polygon(list(rbind(
      st_coordinates(current_line)[1:2,1:2],
      st_coordinates(next_line)[2:1,1:2],
      st_coordinates(current_line)[1,1:2]
    )))

    bboxes[i] <- bbox_section
  }
  #use intersection to find input polygon values within each bounding box
  subpolys <- list()
  area <- list()
  id = list()
  for (i in 1:length(bboxes))
  {
    intersect <- st_intersection(poly, st_polygon(bboxes[i]))
    subpolys[i] <- st_segmentize(intersect, 1)
    # area[i] <- st_area(intersect)
    id[i] <-i
  }
  subpolys <- lapply(subpolys, as.data.frame)
  for (i in 1:length(subpolys))
    {
      names(subpolys[[i]]) <- c('x','y')
      subpolys[[i]]['ID'] <- id[i]
      # subpolys[[i]]['segmentWidth'] <- subpolys[[i]]['x'] - min(subpolys[[i]]['x'])
      # subpolys[[i]]['Area'] <- area[i]
    # Placehold volume calc. Needs to factor in max baffle height
      # subpolys[[i]]['Volume'] <- as.numeric(area[i]) * as.numeric(input$baffleHeight)
      
  }
  polygon_df <- do.call(rbind, subpolys)
  polygon_df

  cross_section_df <- polygon_df %>%
    group_by(ID) %>%
    mutate(segmentWidth = x - min(x)) %>%
    distinct(y, .keep_all = T) %>%
    filter(segmentWidth > 0)
    # filter(segmentWidth > 0 |((x == min(x)) & (y == max(y) | y == min(y))) | x == max(x))

  as.data.frame(cross_section_df)
  })

  
# cross_section_x <- shiny::reactive({
#   req(all_selected_points_x)
#   width <- max(all_selected_points_x()) * 2
#   c(0, seq(0,width))
# })
  
# cross_section_y <- shiny::reactive({
#   req(input$baffleHeight)
#   # req(input$chamberHeight)
#   req(cross_section_x)
#   # c(0,input$baffleHeight + seq(1,length(cross_section_x()),1), 0)
#   c(0, seq(1,length(cross_section_x())-2) + input$baffleHeight, 0)
# })
  
  # create design plot
  output$input_plot <- shiny::renderPlot({
    ggplot(values$user_input, aes(x = x, y = y)) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_point(aes()) +
      geom_path(linewidth = 1.5) +
      lims(x = c(0, input$maxDim), y = c(0, input$maxDim)) +
      theme(legend.position = "bottom") +
      coord_fixed()
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

  output$area_plot <- shiny::renderPlot({
    req(polygon_df)

    area_plot <- ggplot() +
      # lims(x = c(0, input$maxDim/2), y = c(0, input$maxDim)) +
      geom_path(data = polygon_df(), aes(x = x, y = y, group = ID)) +
      theme(legend.position = "bottom")

    area_plot
  })

  # output$cross_section_plot <- plotly::renderPlotly({
  #   plotly::plot_ly(values$user_input, x = ~x, y = ~y, type = 'scatter', mode = 'markers')
  # })

  output$test <- shiny::renderPrint({polygon_df()})

}
#---------------------------

# Run app
shiny::shinyApp(ui, server)