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

# navset_card_tab(
#   height = 450,
#   full_screen = TRUE,
#   title = "",
#   nav_panel(
#     "Inner",
#     card_title("Inner Layer"),
#     inner_plot
#   ),
#   nav_panel(
#     "Outer",
#     card_title("Outer Layer"),
#     outer_plot
#   ),
#   nav_panel(
#     shiny::icon("circle-info"),
#     markdown("Info placeholder)")
#   )
# )

# Info panel with expected weight and total down, baffle material needed
##TODO: 
#Verify FP metric conversion
#Display measurements in table
# Use value boxes


# Troubleshoot:
# max width can't evenly divisible by chamber_width or 'names' attribute [2] must be the same length as the vector [1]

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
  numericInput('baffleSplitHeight','Baffle Orientation Change Height', 50, min = 0, max = 100),
  numericInput('baffleHeight','Baffle Height (cm)', 2, min = 0),
  numericInput('verticalChamberHeight','Max Vertical Chamber Height (cm)', 2.5, min = 0),
  numericInput('verticalChamberWidth','Vertical Chamber Width (cm)', 15, min = 0),
  numericInput('horizontalChamberHeight','Max Horizontal Chamber Height (cm)', 2, min = 0),
  numericInput('horizontalChamberWidth','Horizontal Chamber Width (cm)', 11, min = 0)
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
  verbatimTextOutput("test")
)

card3 <- bslib::card(
  bslib::card_header("Test Output"),
  verbatimTextOutput("cross_section_plot_data")
)


cross_section_card <- bslib::card(
  bslib::card_header("Cross Sectional View"),
  plotOutput("cross_section_plot")
)

area_card <- bslib::card(
  bslib::card_header("Aerial View"),
  plotOutput("area_plot")
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

# UI layout
ui <- bslib::page_navbar(
  title = "Down Quilt Designer",
  theme = bslib::bs_theme(version=5, bootswatch = "sketchy", base_font = "sans-serif"),
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
                  card2,
                cross_section_card,
                card3
              )
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
  
  # values$user_input <- data.frame(x = c(0, 50, 50, 0),
  #                                 y = c(100, 100, 0, 0))
  
  # add opposing points to user selected points
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
  req(input$verticalChamberWidth)
  req(input$horizontalChamberWidth)
  req(input$baffleSplitHeight)

  points <- data.frame(
    x = all_selected_points_x(),
    y = all_selected_points_y()
)

  #create polygon from selected points
  poly <- st_sfc(sf::st_polygon(list(cbind(points$x, points$y))))

  split_height <- input$baffleSplitHeight
  upper_bbox <- st_sfc(st_polygon(list(cbind(
    c(0, max(points$x), max(points$x), 0, 0), 
    c(max(points$y), max(points$y), split_height, split_height, max(points$y))))))
  
  lower_bbox <- st_sfc(st_polygon(list(cbind(
    c(0, max(points$x), max(points$x), 0, 0), 
    c(min(points$y), min(points$y), split_height, split_height, min(points$y))))))

  #define upper and lower polygons
  upper <- st_crop(st_segmentize(poly, 0.1), upper_bbox)
  lower <- st_crop(st_segmentize(poly, 0.1), lower_bbox)

  define_chambers <- function(polygon, orientation) {

    #create bounding boxes that are ChamberWidth apart until greatest width
    bbox <- st_bbox(polygon)

    if (orientation == 'vertical'){
      chamberWidth <- input$verticalChamberWidth
        #create vertical lines spaced verticalChamberWidth apart
      x_seq <- seq(0, (bbox["xmax"] + chamberWidth), by = chamberWidth)
      lines <- lapply(x_seq, function(x) {
        st_linestring(rbind(c(x, bbox["ymin"]), c(x, bbox["ymax"])))
      })
    } else if (orientation == 'horizontal') {
      chamberWidth <- input$horizontalChamberWidth
      #create horizontal lines spaced horizontalChamberWidth apart
    y_seq <- seq(bbox["ymax"], (0 - chamberWidth), by = -chamberWidth)
    lines <- lapply(y_seq, function(y) {
      st_linestring(rbind(c(bbox["xmin"], y), c(bbox["xmax"], y)))
    })
    }


    #combine lines into a multi-line geometry
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
    id = list()
    for (i in 1:length(bboxes))
    {
      subpolys[i] <- st_intersection(poly, st_polygon(bboxes[i]))
      id[i] <- i
    }
    #combine to dataframe with identifying info
    for (i in 1:length(subpolys))
      {
      subpolys[[i]] <- as.data.frame(st_coordinates(subpolys[[i]]))
      subpolys[[i]]['ID'] <- id[i]
    }
    polygon_df <- do.call(rbind, subpolys)
    polygon_df$orientation <- orientation
    polygon_df

  }

  rbind(define_chambers(upper, 'vertical'), define_chambers(lower, 'horizontal'))
})

# #reactive expression to calculate subpolygons
# cross_section_df <- shiny::reactive({
#   req(polygon_df)
#   req(input$verticalChamberHeight)
#   req(input$baffleHeight)

#   # subset to only single observation per y unit per group
#   # This retains one verticalChamberWidth value per y allowing for calculation of area of each slice and thus diff cut calculations.
#   cross_section_df <- polygon_df() %>%
#     group_by(ID) %>%
#     mutate(segmentWidth = x - min(x)) %>%
#     distinct(y, .keep_all = T) %>%
#     filter(segmentWidth > 0) %>%
#     as.data.frame(.)

#   # Define the parameters for the ellipse
#   a <- cross_section_df$segmentWidth / 2  # Semi-major axis (half of width)
#   b <- input$verticalChamberHeight - input$baffleHeight # Semi-minor axis (half of height)
#   # Calculate perimeter of ellipse
#   h <- ((a-b)/(a+b))^2
#   p <- pi * (a + b) * (1 + 3 * h / (10 + sqrt((4 - 3 * h))))
#   # Calculate length of chamber roof (half perimeter)
#   cross_section_df$chamberRoofLength <- p / 2
#   # Calculate half ellipse area
#   cross_section_df$chamberUpperArea <- (pi * a * b) / 2
#   # Calculate lower chamber area
#   cross_section_df$chamberLowerArea <- cross_section_df$segmentWidth * input$baffleHeight
#   # Area of each slice (defined by st_segmentize as 1cm so area == volume per sice)
#   cross_section_df$sliceArea <- cross_section_df$chamberUpperArea + cross_section_df$chamberLowerArea

#   cross_section_df
#   })

  # cross_section_plot_data <- shiny::reactive({
  #   req(cross_section_df)
  #   req(input$verticalChamberHeight)
  #   req(input$baffleHeight)

  #   df <- 
  #     cross_section_df() %>%
  #     group_by(ID) %>%
  #     filter(y == max(y)) %>%
  #     mutate(
  #   # Semi-major axis per slice
  #   a = segmentWidth / 2
  #     ) %>%
  #     ungroup()
    
  #     # define constants
  #     # Semi-minor axis
  #     b <- input$verticalChamberHeight - input$baffleHeight 
  #     # Create a sequence of t values from 0 to 2*pi
  #     t <- seq(0, 2 * pi, length.out = 100)
    
  #   all_coords <- list()
    
  #   for (i in 1:length(df$ID))
  #   {
  #     x_start <- df$x[i] - df$segmentWidth[i]
  #     # Parametric equations for the ellipse
  #     x_coords <- df$a[i] * cos(t) + df$a[i] + x_start
  #     y_coords <- b * sin(t) + input$baffleHeight
  #     # Combine the x and y coordinates into a matrix and close the curve
  #     coords <- cbind(x_coords[1:length(t)/2], y_coords[1:length(t)/2])
  #     coords <- rbind(coords, c(x_start, 0), c(df$x[i], 0), coords[1,])
  #     all_coords[[i]] <- coords
  #   }

  #   all_coords
  # })

  # material_output <- shiny::reactive({
  #   req(polygon_df)
  #   req(input$baffleHeight)
  #   req(input$seamAllowance)
  #   req(input$overstuff)
  #   req(all_selected_points_x)
  #   req(all_selected_points_y)
  #   req(cross_section_df)

  #   # Function to add seam allowance to an sf object
  #   add_seam_allowance <- function(sf_object, seam_allowance = input$seamAllowance) {
  #     # Add a buffer (seam allowance) to the sf object
  #     # seam_allowance is in the same unit as the CRS of the sf object
  #     sf_object_with_seam <- st_buffer(sf_object, dist = seam_allowance)
      
  #     # Return the new sf object with seam allowance
  #     return(sf_object_with_seam)
  #   }

  #   # ---Baffle Material---
  #   baffle_mat_height <- input$baffleHeight + (2 * input$seamAllowance)
  #   baffle_mat_length_by_chamber <- polygon_df() %>%
  #     group_by(ID) %>%
  #     filter(x == min(x)) %>%
  #     summarize(length = max(y) - min(y)) %>%
  #     ungroup()
  #   baffle_mat_length <- (sum(baffle_mat_length_by_chamber$length) * 2) - baffle_mat_length_by_chamber$length[1]
  #   baffle_mat_area <- baffle_mat_height * baffle_mat_length
  #   baffle_mat_weight <- baffle_mat_area / 10000 * input$baffleWeight 
  #   # Area (cm^2)
  #   baffle_mat_area
  #   # Weight (g)
  #   baffle_mat_weight

  #   # ---Inner Layer---
  #   inner_poly <- sf::st_polygon(list(cbind(all_selected_points_x(), all_selected_points_y())))
  #   # Area
  #   inner_mat_area <- st_area(add_seam_allowance(inner_poly))
  #   # Weight
  #   inner_mat_weight <- inner_mat_area / 10000 * input$innerWeight

  #   # ---Outer Layer---
  #   right_points <- cross_section_df() %>%
  #     group_by(y) %>%
  #     summarize(x = sum(chamberRoofLength) + input$baffleHeight)
  #   outer_points <- data.frame(
  #     x = c(right_points$x, -rev(right_points$x)),
  #     y = c(right_points$y, rev(right_points$y))
  # )
  #   # close polygon
  #   outer_points <- rbind(outer_points, outer_points[1,])
  #   outer_poly <- sf::st_polygon(list(cbind(outer_points$x, outer_points$y)))

  #   # Area
  #   outer_mat_area <- st_area(add_seam_allowance(outer_poly))
  #   # Weight
  #   outer_mat_weight <- outer_mat_area / 10000 * input$outerWeight

  #   # ---Volume---
  #   # exclude final slice from cumulative total
  #   vol_data <- cross_section_df() %>%
  #     filter(y > 0)
  #   volume <- sum(vol_data$sliceArea) * 2
  #   # down volume per gram CUIN/oz to CUCM/g
  #   FP_metric <- (input$FP * 16.387064) / 28.34952
  #   # number of grams needed
  #   grams_down <- volume/FP_metric
  #   # adjusted for over/underfill
  #   grams_down_adj <- grams_down * (1 + (input$overstuff/100))

  #   # baffle_mat_area
  #   # baffle_mat_weight
  #   # inner_mat_area
  #   # inner_mat_weight
  #   # right_points
  #   # outer_points
  #   # outer_mat_area
  #   # outer_mat_weight
  #   volume
  #   # FP_metric
  #   # grams_down
  #   # grams_down_adj
  # })
  
  ## --- Page: Dimensions ---
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
    shiny::observeEvent(input$rem_all_points, {
      values$user_input <- data.frame(x=double(),
                              y=double()
    )
    })
  
  
  # render a table of the dataframe
  output$table <- shiny::renderTable({
    values$user_input
  })
  
  output$hover_info <- shiny::renderPrint({
      hover=input$plot_hover
      cat("X value:", formatC(round_any(hover$x, 0.5), digits = 1, format = "f"), "\n")
      cat("Y value:", formatC(round_any(hover$y, 0.5  ), digits = 1, format = "f"))
  })

  # -------------------------

  ## --- Page: Output ---
  output$area_plot <- shiny::renderPlot({
    req(polygon_df)

    upper <- polygon_df() %>%
      filter(orientation == 'vertical')

    lower <- polygon_df() %>%
      filter(orientation == 'horizontal')


    ggplot() +
      geom_path(data = upper, aes(x = X, y = Y, group = ID)) +
      geom_path(data = lower, aes(x = X, y = Y, group = ID)) +
      theme(legend.position = "bottom")
  })

  output$cross_section_plot <- shiny::renderPlot({

    data <- cross_section_plot_data()

    plot <- ggplot() +
      ggtitle("Chamber Cross-section") +
      theme_minimal() 

    # add each baffle cross-section
    for (i in 1:length(data))
    # for (i in 1:1)
      {
      coords <- data[[i]]
      chamber_polygon <- st_sfc(st_polygon(list(coords)))
      # Combine them into an sf object
      cross_sect_sf <- st_sf(geometry = c(chamber_polygon))
      plot <- plot + geom_sf(data = cross_sect_sf, fill = "lightblue", color = "black")
    }

    plot
  })

  output$cross_section_plot_data <- shiny::renderPrint({
    cross_section_plot_data()
  })

  # output$test <- shiny::renderPrint({
  #   material_output()
  # })
  output$test <- shiny::renderPrint({
    polygon_df()
  })


}
#---------------------------

# Run app
shiny::shinyApp(ui, server)