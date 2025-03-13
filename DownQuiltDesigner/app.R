library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(sf)

thematic::thematic_shiny()


##TODO:
# (gg)Plot subpolygons with pointer displaying info about:
# - Area
# - Volume
# - Grams of down needed

# Info panel with expected weight and total down, baffle material needed
##TODO: 
#Verify FP metric conversion 
#Display measurements in table
# Fix final vertical scaling. Issue arrises when diagonal line cuts through multiple subpolygons.

# list of dataframes includes:
# 1 segmentized_poly which is the dataframe containing all inner layer polygons
# 2 outer_poly contains all inner layer polygons with chamber roof length calculations
# 3 cross section plot data
# 4 outer_poly_update. DF of polygon points grouped by chamber/orientation. For cutting dimensions.

# Issues:
# 100% baffles in either direction. Could consider offsetting by a tiny amount.


# options(digits=2)

# Functions
#---------------------------
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
#---------------------------


# Frontend
#---------------------------
design_accordion <- bslib::accordion_panel(
  "Design",# icon = bsicons::bs_icon("menu-app"),
  numericInput('maxDim','Longest Dimension (cm)', 210, min = 0),
  numericInput('orientationSplitHeight','Baffle Orientation Change Height', 50, min = 0, max = 100),
  numericInput('baffleHeight','Baffle Height (cm)', 2, min = 0),
  numericInput('verticalChamberHeight','Max Vertical Chamber Height (cm)', 2.5, min = 0),
  numericInput('verticalChamberWidth','Vertical Chamber Width (cm)', 15, min = 0),
  numericInput('horizontalChamberHeight','Max Horizontal Chamber Height (cm)', 2.5, min = 0),
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

inner_card <- bslib::card(
  plotOutput("inner_plot")
)

outer_vert_card <- bslib::card(
  plotOutput("outer_vert_plot")
)

outer_hor_card <- bslib::card(
  plotOutput("outer_hor_plot")
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

specs <- list()

# specs_name <- c(
#   "baffle_mat_area",
#   "baffle_mat_weight",
#   "inner_mat_area",
#   "inner_mat_weight",
#   "right_points",
#   "outer_points",
#   "outer_mat_area",
#   "outer_mat_weight",
#   "volume",
#   "FP_metric",
#   "grams_down",
#   "grams_down_adj"
# )

specs_name <- c(
  "Baffle Material Area",
  "Baffle Material Weight",
  "Inner Layer Area",
  "Inner Layer Weight",
  "Outer Layer Area",
  "Outer Layer Weight",
  "Volume",
  "Grams of Down",
  "Estimated Weight"
)

for (i in 1:length(specs_name)) {
  specs[[i]] <- bslib::value_box(
    title = specs_name[i],
    value = "123",
    theme = "purple",
  )
}

# UI layout
ui <- bslib::page_navbar(
  title = "Down Quilt Designer",
  theme = bs_theme(brand=T),
  sidebar = bslib::sidebar(
    bslib::accordion(
      design_accordion,
      materials_accordion
  )
),
bslib::nav_panel(
  title = "Input Dimensions",
  bslib::layout_column_wrap(
    width = NULL,
    height = NULL,
    fill = FALSE,
    style = bslib::css(grid_template_columns = "2fr 1fr"),
    plot_input_card, 
    selected_points_card)
  ),
bslib::nav_panel(
  title = "Output Dimensions",
  navset_card_tab(
  # height = 1800,
  full_screen = TRUE,
  # title = "",
    nav_panel(
      "Inner Layer",
      # card_title("Inner Layer"),
      inner_card,
                # card3
      # inner_plot
    ),
    nav_panel(
      "Outer Vertical",

      # card_title("Outer Vertical Layer"),
      outer_vert_card
    ),
    nav_panel(
      "Outer Horizontal",
      # card_title("Outer Horizontal Layer"),
      outer_hor_card
    ),
    nav_panel(
      shiny::icon("circle-info"),
      markdown("Info placeholder"),
            card2,
    )
),
                ),
bslib::nav_panel(
  title = "Specifications",
    layout_column_wrap(
      width = "250px",
      !!!specs
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
data_list <- shiny::reactive({
  req(all_selected_points_x)
  req(all_selected_points_y)
  req(input$verticalChamberWidth)
  req(input$horizontalChamberWidth)
  req(input$orientationSplitHeight)

  

  points <- data.frame(
    x = all_selected_points_x(),
    y = all_selected_points_y()
)

  #create polygon from selected points
  poly <- st_sfc(sf::st_polygon(list(cbind(points$x, points$y))))

  split_height <- input$orientationSplitHeight
  vert_bbox <- st_sfc(st_polygon(list(cbind(
    c(0, max(points$x), max(points$x), 0, 0), 
    c(max(points$y), max(points$y), split_height, split_height, max(points$y))))))
  
  hor_bbox <- st_sfc(st_polygon(list(cbind(
    c(0, max(points$x), max(points$x), 0, 0), 
    c(min(points$y), min(points$y), split_height, split_height, min(points$y))))))

  #define vertical and horizontal chamber polygons
  vert <- st_crop(st_segmentize(poly, 0.1), vert_bbox)
  hor <- st_crop(st_segmentize(poly, 0.1), hor_bbox)

  define_chambers <- function(polygon, orientation) {

    #create bounding boxes that are ChamberWidth apart until greatest width
    bbox <- st_bbox(polygon)

    if (orientation == 'vertical'){
      chamberWidth <- input$verticalChamberWidth
        #create vertical lines spaced verticalChamberWidth apart
      if (bbox["xmax"]%%chamberWidth == 0) {
        x_seq <- seq(0, bbox["xmax"], by = chamberWidth)
      } else {
      x_seq <- seq(0, (bbox["xmax"] + chamberWidth), by = chamberWidth)
      }
      lines <- lapply(x_seq, function(x) {
        st_linestring(rbind(c(x, bbox["ymin"]), c(x, bbox["ymax"])))
      })
      } else if (orientation == 'horizontal') {
      chamberWidth <- input$horizontalChamberWidth
      #create horizontal lines spaced horizontalChamberWidth apart
      if (bbox["ymax"]%%chamberWidth == 0) {
        y_seq <- seq(bbox["ymax"], 0, by = -chamberWidth)
      } else {
    y_seq <- seq(bbox["ymax"], (0 - chamberWidth), by = -chamberWidth)
    }
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
      subpolys[i] <- st_segmentize(st_intersection(poly, st_polygon(bboxes[i])), 1)
      id[i] <- i
    }
    #combine to dataframe with identifying info
    for (i in 1:length(subpolys))
      {
      subpolys[[i]] <- as.data.frame(st_coordinates(subpolys[[i]]))
      subpolys[[i]]['ID'] <- id[i]
    }
    segmentized_poly <- do.call(rbind, subpolys) %>%
      select(X, Y, ID) %>%
      mutate(orientation = orientation)

    if (orientation == 'vertical'){
      outer_poly <- segmentized_poly %>%
        #group byb chamber ID
        group_by(ID) %>%
        #segmentwidth is width of inner layer in a given chamber at a given y value (1cm increments)
        mutate(segmentWidth = X - min(X)) %>%
        #subset to only single observation per y unit per group
        distinct(Y, .keep_all = T) %>%
        #keep only rightmost observations since measuring from left to right
        filter(segmentWidth > 0)
    } else if (orientation == 'horizontal'){
      outer_poly <- segmentized_poly %>%
        group_by(ID) %>%
        #width is instead the length of horizontal segment
        mutate(segmentWidth = max(Y) - Y) %>%
        distinct(X, .keep_all = T) %>%
        filter(segmentWidth > 0)
    }

    # Define the parameters for the ellipse
    a <- outer_poly$segmentWidth / 2  # Semi-major axis (half of width)
    if (orientation == "vertical"){
        b <- input$verticalChamberHeight - input$baffleHeight # Semi-minor axis (half of height)
    } else if (orientation == "horizontal") {
        b <- input$horizontalChamberHeight - input$baffleHeight # Semi-minor axis (half of height)
      }
    # Calculate perimeter of ellipse
    h <- ((a-b)/(a+b))^2
    p <- pi * (a + b) * (1 + 3 * h / (10 + sqrt((4 - 3 * h))))

    # Calculate length of chamber roof (half perimeter)
    outer_poly$chamberRoofLength <- p / 2
    # Calculate half ellipse area
    outer_poly$chamberUpperArea <- (pi * a * b) / 2
    # Calculate lower chamber area
    outer_poly$chamberLowerArea <- outer_poly$segmentWidth * input$baffleHeight
    # Area of each slice (defined by st_segmentize as 1cm so area == volume per slice)
    outer_poly$sliceArea <- outer_poly$chamberUpperArea + outer_poly$chamberLowerArea

    if (orientation == "vertical"){
      df <- outer_poly %>%
        group_by(ID) %>%
        filter(Y == max(Y)) %>%
        mutate(
      # Semi-major axis per slice
      a = segmentWidth / 2
        ) %>%
        ungroup()
  } else if (orientation == "horizontal") {
    df <- outer_poly %>%
      group_by(ID) %>%
      filter(X == min(X)) %>%
      mutate(
    # Semi-major axis per slice
    a = segmentWidth / 2
      ) %>%
      ungroup()
  }

      # Create a sequence of t values from 0 to 2*pi
      t <- seq(0, 2 * pi, length.out = 100)
    
    
    
    if (orientation == 'vertical'){
      cross_section_plot_data <- list()
    for (i in 1:length(df$ID))
    {
      #positioning within plot
      y_translate <- max(segmentized_poly$Y) + (max(segmentized_poly$Y) / 40)
      
      x_start <- df$X[i] - df$segmentWidth[i]
      # Parametric equations for the ellipse
      x_coords <- df$a[i] * cos(t) + df$a[i] + x_start
      y_coords <- b * sin(t) + input$baffleHeight
      y_coords <- y_coords + y_translate
      # Combine the x and y coordinates into a matrix and close the curve
      coords <- cbind(x_coords[1:length(t)/2], y_coords[1:length(t)/2])
      coords <- rbind(coords, c(x_start, y_translate), c(df$X[i], y_translate), coords[1,])

      cross_section_plot_data[[i]] <- coords
    }
  } else if (orientation == 'horizontal'){
    cross_section_plot_data <- list()
    for (i in 1:length(df$ID))
      {
        #positioning within plot
        x_translate <- max(segmentized_poly$X) + (max(segmentized_poly$X) / 20)
        
        y_start <- df$Y[i] + df$segmentWidth[i]
        # Parametric equations for the ellipse
        y_coords <- y_start - (df$a[i] * cos(t) + df$a[i])
        x_coords <- b * sin(t) + input$baffleHeight
        x_coords <- x_coords + x_translate
        # Combine the x and y coordinates into a matrix and close the curve
        coords <- cbind(x_coords[1:length(t)/2], y_coords[1:length(t)/2])
        coords <- rbind(coords, c(x_translate, y_start), c(x_translate, df$Y[i]), coords[1,])
  
        cross_section_plot_data[[i]] <- coords
    }

  }
    
    outer_segmented_poly <- data.frame(
      X=double(),
      Y=double(),
      ID=integer(),
      orientation=character(),
      chamberRoofLength=double()
  )

    # Outer layer dimensions
    if (orientation == 'vertical'){
      # Keep only the lowest value of X for each level of Y within each group
      start_points <- segmentized_poly %>%
        group_by(ID) %>%
        filter(X == min(X)) %>%
        ungroup()
      end_point_data <- outer_poly %>% select("Y", "ID", "orientation", "chamberRoofLength")
      start_points_crl <- left_join(start_points, end_point_data, by = c("ID", "Y", "orientation"))
      # the start is the leftmost x value of the subpolygon
      start <- 0
      for (i in 1:max(start_points_crl$ID)){
        end_points <- start_points_crl %>%
          filter(ID == i) %>%
          mutate(X = chamberRoofLength + start)
        start_points <- end_points %>%
          mutate(X = X - chamberRoofLength)

        end <- max(end_points$X)

        outer_segmented_poly <- rbind(outer_segmented_poly, end_points, start_points)
        start <- end
        outer_poly_update <- outer_segmented_poly
      }
      } else if (orientation == 'horizontal'){
      # Keep only the highest value of Y within each group
      start_points <- segmentized_poly %>%
        group_by(ID) %>%
        filter(Y == max(Y)) %>%
        ungroup()
      # outer_segmented_poly <- rbind(outer_segmented_poly, start_points[ID==1])
      end_point_data <- outer_poly %>% select("X", "ID", "orientation", "chamberRoofLength")
      start_points_crl <- right_join(start_points, end_point_data, by = c("ID", "X", "orientation"))
      # the start is the uppermost y value of the subpolygon
      start <- input$orientationSplitHeight
      # Add starting horizontal line at orientation split height
      outer_segmented_poly <- start_points_crl %>% filter(ID==1) %>% mutate(Y=start)
      for (i in 1:max(start_points_crl$ID)){
        end_points <- start_points_crl %>%
          filter(ID == i) %>%
          mutate(Y = start - chamberRoofLength)
        start_points <- end_points %>%
          mutate(Y = Y + chamberRoofLength)

        end <- min(end_points$Y)

        outer_segmented_poly <- rbind(outer_segmented_poly, end_points)
        start <- end
        outer_poly_update <- outer_segmented_poly
    }

    # Subset to chamber wall points
    baffle_y <- outer_poly_update %>%
      filter(X == 0)
    outer_poly_update <- outer_poly_update %>%
      filter(Y %in% c(input$orientationSplitHeight, baffle_y$Y, min(Y))) %>%
      # Apply offset
      mutate(Y = Y - min(Y))
      }
    return(list(segmentized_poly, outer_poly, cross_section_plot_data, outer_poly_update))
      }

  # Cross-section plot data may need to be imported separately 
  vert_list <- define_chambers(vert, 'vertical')
  hor_list <- define_chambers(hor, 'horizontal')

  list(
    rbind(vert_list[[1]], hor_list[[1]]),
    rbind(vert_list[[2]], hor_list[[2]]),
    rbind(vert_list[[3]], hor_list[[3]]),
    list(vert_list[[4]], hor_list[[4]])
  )

  })


  # material_output <- shiny::reactive({
  #   req(data_list)
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
  #   baffle_mat_length_by_chamber <- data_list() %>%
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
  ## IDFL paper suggests using 28.77 for inch/cm conversions
  ## Samples are usually 28.4/28.5 or 30 grams depending on type of test
  ## Volume / FP = weight
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
  
  ## --- Page: Input Dimensions ---
  # create design plot
  output$input_plot <- shiny::renderPlot({
    ggplot(values$user_input, aes(x = x, y = y)) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_point(aes()) +
      geom_path(linewidth = 1.5) +
      lims(x = c(0, input$maxDim), y = c(0, input$maxDim)) +
      theme_minimal() +
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

  ## --- Page: Output Dimensions ---
  output$inner_plot <- shiny::renderPlot({
    req(data_list)

    vert <- data_list()[[1]] %>%
      filter(orientation == 'vertical')

    hor <- data_list()[[1]] %>%
      filter(orientation == 'horizontal')

    inner_plot <- ggplot() +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_path(data = vert, aes(x = X, y = Y, group = ID)) +
      geom_path(data = hor, aes(x = X, y = Y, group = ID)) +
      theme_minimal() +
      coord_fixed() 
    
      data <- data_list()[[3]]
    
      # add each baffle cross-section
      for (i in 1:length(data))
        {
        coords <- data[[i]]
        chamber_polygon <- st_sfc(st_polygon(list(coords)))
        # Combine them into an sf object
        cross_sect_sf <- st_sf(geometry = c(chamber_polygon))
        inner_plot <- inner_plot + 
          geom_sf(data = cross_sect_sf, fill = "lightblue", color = "black")
      }
    
    inner_plot
  })

  output$outer_vert_plot <- shiny::renderPlot({
    req(data_list)

    vert <- data_list()[[4]][[1]]# %>%
      # filter(orientation == 'vertical')
    

    outer_vert_plot <- ggplot() +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_point(data = vert, aes(x = X, y = Y, group = ID)) +
      theme_minimal() +
      coord_fixed() #+
      # coord_flip()
    
    outer_vert_plot
  })

  output$outer_hor_plot <- shiny::renderPlot({
    req(data_list)

    hor <- data_list()[[4]][[2]] #%>%
      # filter(orientation == 'horizontal')

    outer_hor_plot <- ggplot() +
      # geom_vline(xintercept = 0, linetype = "dotted", linewidth = 2) +
      geom_point(data = hor, aes(x = X, y = Y, group = ID)) +
      theme_minimal() +
      coord_fixed() #+
      # coord_flip()
    
    outer_hor_plot
  })


  # # output$test <- shiny::renderPrint({
  # #   material_output()
  # # })
  output$test <- shiny::renderPrint({
    data_list()[[4]] %>%
      filter(orientation == 'horizontal')
  })


}
#---------------------------

# Run app
shiny::shinyApp(ui, server)