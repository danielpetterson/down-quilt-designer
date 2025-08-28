library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(sf)
library(stats)
library(gt)

options(shiny.sanitize.errors = FALSE)

## TODO:
# Test brand.yml
# Validate that baffle height is less than or equal to max chamber heights

# Issues:

# Helper Functions
#---------------------------

# Rounding for input due to inaccuracy of clicking
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

# Fixes issue with gt package where input (where you click on the table)
# and output (what is in the table is created with the same call
gt_output <- function(outputId) {
  rlang::check_installed("shiny", "to use `gt_output()`.")

  shiny::htmlOutput(outputId)
}

#---------------------------

test <- 1


# Frontend
#---------------------------
design_accordion <- bslib::accordion_panel(
  "Design", # icon = bsicons::bs_icon("menu-app"),
  numericInput("maxDim", "Longest Dimension (cm)", 210, min = 0),
  numericInput("orientationSplitHeight", "Baffle Orientation Change Height", 50, min = 0.5),
  numericInput("baffleHeight", "Baffle Height (cm)", 2, min = 0),
  numericInput("verticalChamberHeight", "Max Vertical Chamber Height (cm)", 2.5, min = 0),
  numericInput("verticalChamberWidth", "Vertical Chamber Width (cm)", 15, min = 0),
  numericInput("horizontalChamberHeight", "Max Horizontal Chamber Height (cm)", 2.5, min = 0),
  numericInput("horizontalChamberWidth", "Horizontal Chamber Width (cm)", 11, min = 0),
  numericInput("seamAllowance", "Seam Allowance (cm)", 1, min = 0, step = 0.25),
  checkboxInput("baffleWallExtension", "Include Edge Chamber Wall", TRUE)
)

materials_accordion <- bslib::accordion_panel(
  "Materials", # icon = bsicons::bs_icon("sliders"),
  numericInput("FP", "Fill Power", 750, min = 500, max = 1000, step = 50),
  numericInput("overstuff", "% Overstuff", 10),
  numericInput("innerWeight", "Inner Fabric Weight (gsm)", 50, min = 0),
  numericInput("outerWeight", "Outer Fabric Weight (gsm)", 50, min = 0),
  numericInput("baffleWeight", "Baffle Material Weight (gsm)", 25, min = 0)
)

manual_entry_card <- bslib::card(
  bslib::card_body(
    bslib::layout_column_wrap(
      width = 1 / 2,
      # manual input
      shiny::numericInput("x_add", "X", 0, min = 0),
      shiny::numericInput("y_add", "Y", 0, min = 0)
    )
  )
)

plot_input_card <- bslib::card(
  bslib::card_header("Define vertices manually or click to draw right side of the quilt"),
  manual_entry_card,
  bslib::card_body(
    fillable = T,
    # button to add vertices
    actionButton("add_point", "Add Point"),
  ),
  verbatimTextOutput("hover_info"),
  plotOutput("input_plot",
    height = 600,
    # add plot click functionality
    click = "plot_click",
    # add the hover options
    hover = hoverOpts(
      id = "plot_hover",
      nullOutside = TRUE
    )
  ),
  # button to remove last vertex
  actionButton("rem_point", "Remove Last Point"),
)

selected_points_card <- bslib::card(
  bslib::card_header("Selected Points"),
  tableOutput("table")
)

card2 <- bslib::card(
  bslib::card_header("Text Output"),
  verbatimTextOutput("test")
)

inner_card <- bslib::card(
  girafeOutput("inner_plot")
)

outer_vert_card <- bslib::card(
  girafeOutput("outer_vert_plot")
)

outer_hor_card <- bslib::card(
  girafeOutput("outer_hor_plot")
)

plot_input_card <- bslib::card(
  bslib::card_header("Define vertices manually or click to draw right side of the quilt"),
  manual_entry_card,
  bslib::card_body(
    fillable = T,
    # button to add vertices
    actionButton("add_point", "Add Point"),
    # button to remove last vertex
    actionButton("rem_point", "Remove Last Point"),
  ),
  verbatimTextOutput("hover_info"),
  plotOutput("input_plot",
    height = 600,
    # add plot click functionality
    click = "plot_click",
    # add the hover options
    hover = hoverOpts(
      id = "plot_hover",
      nullOutside = TRUE
    )
  ),
)

# UI layout
ui <- bslib::page_navbar(
  title = "Down Quilt Designer",
  theme = bs_theme(brand = T),
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
      selected_points_card
    )
  ),
  bslib::nav_panel(
    title = "Output Dimensions",
    navset_card_tab(
      full_screen = TRUE,
      nav_panel(
        "Inner Layer",
        inner_card
      ),
      nav_panel(
        "Outer Vertical",
        outer_vert_card
      ),
      nav_panel(
        "Outer Horizontal",
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
    gt_output("specifications")
  )
)
#---------------------------


# Backend
#---------------------------
server <- function(input, output) {
  #---------------------------
  # Coordinate Inputs
  #---------------------------
  values <- shiny::reactiveValues()

  values$user_input <- data.frame(
    x = c(0, 71, 71, 50, 0),
    y = c(210, 210, 100, 0, 0)
  )

  # For validation
  values$user_input <- data.frame(
    x = c(0, 50, 50, 0),
    y = c(100, 100, 0, 0)
  )

  # Add opposing points to user selected points
  all_selected_points_x <- shiny::reactive({
    req(values$user_input)
    c(values$user_input$x, -rev(values$user_input$x))
  })

  all_selected_points_y <- shiny::reactive({
    req(values$user_input)
    c(values$user_input$y, rev(values$user_input$y))
  })

  #---------------------------
  # Geometric Data List
  #---------------------------
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

    # Create polygon from selected points
    # Serves as inner layer w/o seam allowance
    inner <- st_sfc(sf::st_polygon(list(cbind(points$x, points$y))))

    # Catch issues with full length vertical/horizontal chambers
    if (input$orientationSplitHeight == 0) {
      split_height <- 0.00001
    } else if (input$orientationSplitHeight >= max(points$y)) {
      split_height <- max(points$y) - 0.5
    } else {
      split_height <- input$orientationSplitHeight
    }

    # Define bounding boxes for vertical/horizontal chamber segments
    vert_bbox <- st_sfc(st_polygon(list(cbind(
      c(-max(points$x), max(points$x), max(points$x), -max(points$x), -max(points$x)),
      c(max(points$y), max(points$y), split_height, split_height, max(points$y))
    ))))

    hor_bbox <- st_sfc(st_polygon(list(cbind(
      c(-max(points$x), max(points$x), max(points$x), -max(points$x), -max(points$x)),
      c(min(points$y), min(points$y), split_height, split_height, min(points$y))
    ))))

    # Split base polygon into vertical/horizontal chamber segments
    vert_simple <- st_crop(inner, vert_bbox)
    hor_simple <- st_crop(inner, hor_bbox)

    # Calculate scale factor for differential cut
    scale_factor <- function(baffle_orientation) {
      if (baffle_orientation == "vertical") {
        a <- input$verticalChamberWidth / 2 # Semi-major axis (half of width)
        b <- input$verticalChamberHeight - input$baffleHeight
      } else if (baffle_orientation == "horizontal") {
        a <- input$horizontalChamberWidth / 2 # Semi-major axis (half of width)
        b <- input$horizontalChamberHeight - input$baffleHeight # Semi-minor axis (half of height)
      }

      # Calculate perimeter of ellipse
      h <- ((a - b) / (a + b))^2
      p <- pi * (a + b) * (1 + 3 * h / (10 + sqrt((4 - 3 * h))))

      # Calculate length of chamber roof (half perimeter)
      maxChamberRoofLength <- round(p / 2, 2)
      scale_factor <- maxChamberRoofLength / (a * 2)

      return(scale_factor)
    }

    vert_baffle_scale_factor <- scale_factor("vertical")
    hor_baffle_scale_factor <- scale_factor("horizontal")

    # Scale outer segments for differential cut
    # Vertical chambers are scaled on the X axis
    # Horizontal chambers are scaled in the Y axis
    scale_geometry <- function(sf_poly, x_scaling_factor, y_scaling_factor, chamber_orientation) {
      coords <- st_coordinates(sf_poly)
      coords[, "X"] <- coords[, "X"] * x_scaling_factor
      coords[, "Y"] <- coords[, "Y"] * y_scaling_factor
      # Offset y-axis
      new_polygon <- st_polygon(list(coords[, c("X", "Y")]))
      scaled_polygon <- st_sfc(new_polygon, crs = st_crs(sf_poly)) %>% st_sf()

      return(scaled_polygon)
    }

    outer_vert_simple <- scale_geometry(vert_simple, vert_baffle_scale_factor, 1)
    outer_hor_simple <- scale_geometry(hor_simple, 1, hor_baffle_scale_factor)

    # Adds baffle wall height to the outermost vertical and horizontal chambers
    add_baffle_wall_allowance <- function(sf_poly, chamber_orientation) {
      coords <- st_coordinates(sf_poly)[, c("X", "Y")]
      # Add outer chamber wall
      if (chamber_orientation == "vertical") {
        coords[, "X"] <- ifelse(coords[, "X"] < 0, coords[, "X"] - input$baffleHeight, coords[, "X"] + input$baffleHeight)
        altered_polygon <- st_polygon(list(coords[, c("X", "Y")]))
      } else if (chamber_orientation == "horizontal") {
        # Find indices of points with minimum y
        min_y <- min(coords[, "Y"])
        min_y_indices <- which(abs(coords[, "Y"] - min_y) < 1e-10) # Use tolerance for floating-point

        # Select three points with minimum y
        idx1 <- min_y_indices[1]
        idx2 <- min_y_indices[2]
        idx3 <- min_y_indices[3]

        # Create new points: same x, y-y_diff
        new_point1 <- c(coords[idx1, 1], min_y - input$baffleHeight)
        new_point2 <- c(coords[idx2, 1], min_y - input$baffleHeight)
        new_point3 <- c(coords[idx3, 1], min_y - input$baffleHeight)

        # Insert new points
        if (idx1 < nrow(coords)) {
          coords <- rbind(
            coords[1:idx1, ],
            new_point1,
            new_point2,
            new_point3,
            coords[(idx3):nrow(coords), ]
          )
        }

        # # Create new points: same x, y - baffle_height
        # new_points <- lapply(seq_along(min_y_indices), function(i) {
        #   c(coords[min_y_indices[i], "X"], min_y - input$baffleHeight)
        # })

        # # Convert new_points list to matrix for rbind
        # new_points <- do.call(rbind, new_points)


        # # Determine insertion point (after the last min_y point)
        # insert_after <- min(min_y_indices)

        # # Combine original and new points
        # if (insert_after < nrow(coords)) {
        #   new_coords <- rbind(
        #     coords[1:insert_after, ],
        #     new_points,
        #     coords[(insert_after + 1):nrow(coords), ]
        #   )
        # } else {
        #   # If min_y points are at the end, append new points
        #   new_coords <- rbind(coords, new_points)
        # }

        # Form polygon from coordinates
        altered_polygon <- st_polygon(list(coords[, c("X", "Y")]))
      }

      baffle_wall_polygon <- st_sfc(altered_polygon, crs = st_crs(sf_poly)) %>% st_sf()
      return(baffle_wall_polygon)
    }

    # Add fabric to outer layer to cover outer edge walls if checkbox selected
    if (input$baffleWallExtension) {
      outer_vert_extend <- add_baffle_wall_allowance(outer_vert_simple, "vertical")
      # if (input$orientationSplitHeight == 0) {
      #   outer_vert_extend <- add_baffle_wall_allowance(outer_vert_extend, "horizontal")
      # }
      outer_hor_extend <- add_baffle_wall_allowance(outer_hor_simple, "horizontal")
    } else {
      outer_vert_extend <- outer_vert_simple
      outer_hor_extend <- outer_hor_simple
    }

    test <- st_coordinates(outer_vert_extend)

    # Add seam allowance w/ non-rounded vertices
    inner_seam <- st_buffer(inner, input$seamAllowance, joinStyle = "MITRE", mitreLimit = 5)
    outer_vert_simple_seam <- st_buffer(outer_vert_extend, input$seamAllowance, joinStyle = "MITRE", mitreLimit = 5)
    outer_hor_simple_seam <- st_buffer(outer_hor_extend, input$seamAllowance, joinStyle = "MITRE", mitreLimit = 5)

    # Adjust y-axis to start at zero
    adjust_y_zero <- function(sf_poly, sf_poly_seam) {
      coords <- st_coordinates(sf_poly)
      coords_seam <- st_coordinates(sf_poly_seam)
      # Difference in lower bound of each polygon
      y_diff_polygon <- min(coords_seam[, "Y"]) - min(coords[, "Y"])
      # Difference in min(y) between poly w/ seam and y=0
      y_diff <- min(coords_seam[, "Y"])
      # Translate polygons
      coords[, "Y"] <- coords[, "Y"] - y_diff
      coords_seam[, "Y"] <- coords_seam[, "Y"] - y_diff

      zero_adj_polygon <- st_polygon(list(coords[, c("X", "Y")]))
      adjusted_polygon <- st_sfc(zero_adj_polygon, crs = st_crs(sf_poly)) |>
        st_sf()
      zero_adj_polygon_seam <- st_polygon(list(coords_seam[, c("X", "Y")]))
      adjusted_polygon_seam <- st_sfc(zero_adj_polygon_seam, crs = st_crs(sf_poly)) |>
        st_sf()

      output_list <- list(adjusted_polygon, adjusted_polygon_seam)

      return(output_list)
    }

    inner_list <- adjust_y_zero(inner, inner_seam)
    inner <- inner_list[[1]]
    inner_seam <- inner_list[[2]]

    outer_vert_list <- adjust_y_zero(outer_vert_simple, outer_vert_simple_seam)
    outer_vert_simple <- outer_vert_list[[1]]
    outer_vert_simple_seam <- outer_vert_list[[2]]

    outer_hor_list <- adjust_y_zero(outer_hor_simple, outer_hor_simple_seam)
    outer_hor_simple <- outer_hor_list[[1]]
    outer_hor_simple_seam <- outer_hor_list[[2]]

    # Adjust y-axis to be in line
    vert_simple <- vert_simple + c(0, input$seamAllowance)
    hor_simple <- hor_simple + c(0, input$seamAllowance)

    # Create a regular grid that covers the outer layer segment
    inner_vert_grid <- st_make_grid(vert_simple, cellsize = c(input$verticalChamberWidth, max(points$y) + input$seamAllowance), square = TRUE, offset = c(0, 0))
    inner_hor_grid <- st_make_grid(hor_simple, cellsize = c((max(points$x * 2)), input$horizontalChamberWidth), square = TRUE)
    outer_vert_grid <- st_make_grid(outer_vert_simple, cellsize = c(input$verticalChamberWidth * vert_baffle_scale_factor, max(points$y * 2)), square = TRUE, offset = c(0, 0))
    outer_hor_grid <- st_make_grid(outer_hor_simple, cellsize = c((max(points$x * 2)), input$horizontalChamberWidth * hor_baffle_scale_factor), square = TRUE)

    # Apply the rotation to the grid geometry so that the chambers complete from the top of the horizontal sections
    rot_angle <- pi
    rotation_matrix <- matrix(c(cos(rot_angle), sin(rot_angle), -sin(rot_angle), cos(rot_angle)), nrow = 2)
    inner_hor_grid <- st_geometry(inner_hor_grid) * rotation_matrix + c(0, max(st_coordinates(hor_simple)[, "Y"]) + input$seamAllowance)
    outer_hor_grid <- st_geometry(outer_hor_grid) * rotation_matrix + c(0, max(st_coordinates(outer_hor_simple_seam)[, "Y"]))

    # Translate horizontal grid if baffle wall extension is selected
    if (input$baffleWallExtension) {
      outer_hor_grid <- st_geometry(outer_hor_grid) + c(0, input$baffleHeight)
    }

    # Function to mirror chamber polygons across the y-axis
    mirror_vert_chambers <- function(sf_poly) {
      reflection_matrix <- matrix(c(-1, 0, 0, 1), nrow = 2, ncol = 2)
      mirrored_geom <- st_geometry(sf_poly) * reflection_matrix
      poly <- c(mirrored_geom, sf_poly)

      return(poly)
    }

    # Split polygon into subpolygons(chambers) by grid
    inner_vert_segmented <- st_intersection(inner_vert_grid, vert_simple)
    inner_vert_segmented <- mirror_vert_chambers(inner_vert_segmented)

    inner_hor_segmented <- st_intersection(inner_hor_grid, hor_simple)

    outer_vert_segmented <- st_intersection(outer_vert_grid, outer_vert_simple)
    outer_vert_segmented <- mirror_vert_chambers(outer_vert_segmented)

    outer_hor_segmented <- st_intersection(outer_hor_grid, outer_hor_simple)

    # Extract vertices of an sf polygon
    extract_polygon_vertices <- function(sf_poly) {
      vertices <- st_cast(st_geometry(sf_poly), "POINT")
      vertices <- vertices[1:length(vertices) - 1]
      sf_vertices <- st_sf(
        id = seq_along(vertices),
        x = round(st_coordinates(vertices)[, 1], 1),
        y = round(st_coordinates(vertices)[, 2], 1),
        geometry = vertices
      )

      # Create tooltip labels with coordinates
      sf_vertices$tooltip <- paste0("(", sf_vertices$x, ", ", sf_vertices$y, ")")

      return(sf_vertices)
    }

    inner_seam_vertices <- extract_polygon_vertices(inner_seam)
    outer_vert_simple_seam_vertices <- extract_polygon_vertices(outer_vert_simple_seam)
    outer_hor_simple_seam_vertices <- extract_polygon_vertices(outer_hor_simple_seam)


    # Extract vertices of individual chambers and remove duplicates for plotting
    extract_chamber_vertices <- function(sf_object) {
      # Apply function to each polygon and coalesce results
      vertices_list <- lapply(seq_len(length(sf_object)), function(i) {
        extract_polygon_vertices(sf_object[i, ])
      })
      vertices <- bind_rows(vertices_list)

      # Remove duplicate vertices based on x, y coordinates
      vertices_formatted <- vertices |>
        distinct(x, y, .keep_all = TRUE) |>
        mutate(tooltip = paste0("(", x, ", ", y, ")"))

      return(vertices_formatted)
    }

    inner_chamber_vertices <- rbind(
      extract_chamber_vertices(inner_vert_segmented),
      extract_chamber_vertices(inner_hor_segmented)
    )

    outer_vert_chamber_vertices <- extract_chamber_vertices(outer_vert_segmented)
    outer_hor_chamber_vertices <- extract_chamber_vertices(outer_hor_segmented)


    # Function to calculate lengths of vertical/horizontal lines in an sfc
    calculate_baffle_lengths <- function(sf_vertices, reference_axis) {
      # Extract coordinates from sf object
      coords <- st_coordinates(sf_vertices)
      if (reference_axis == "Y") {
        data <- data.frame(other = coords[, 1], reference = coords[, 2])
        data <- data |>
          # Remove furthest baffle lines since this function is served by outer layer fabric
          slice_max(order_by = other, n = nrow(data) - 2) |>
          slice_min(order_by = other, n = nrow(data) - 4) |>
          mutate(other = round(other, 2))
      } else if (reference_axis == "X") {
        data <- data.frame(reference = coords[, 1], other = coords[, 2]) |>
          # Remove rows where x is approximately 0 (within a tolerance)
          filter(abs(reference) > 1e-10)
        data <- data |>
          # Remove lowest horizontal line. Covered by outer fabric layer.
          slice_max(order_by = other, n = nrow(data) - 2) |>
          mutate(other = round(other, 2))
      }

      # Group by other and calculate differences in reference for points with same other value
      length_by_baffle <- data %>%
        group_by(other) %>%
        summarise(baffle_length = if (n() > 1) abs(diff(reference)) else NA_real_) %>%
        filter(!is.na(baffle_length))

      total_baffle_length <- sum(length_by_baffle$baffle_length)


      return(total_baffle_length)
    }


    hor_baffle_lengths <- calculate_baffle_lengths(outer_hor_chamber_vertices, "X")
    vert_baffle_lengths <- calculate_baffle_lengths(outer_vert_chamber_vertices, "Y")


    # Calculate base area/volume
    # Extrude 2D chambers to 3D shapes (Uses area of polygon to optimise accuracy with little overhead)
    calculate_volume_by_chamber <- function(inner_section_poly, outer_section_poly, reference_axis) {
      # Calculate area of polygons
      chamber_attributes_df <- data.frame(
        poly_id = 1:length(inner_section_poly),
        base_area = as.numeric(st_area(inner_section_poly))
      )
      # Extrude into 3D space
      chamber_attributes_df$base_volume <- chamber_attributes_df$base_area * input$baffleHeight

      # Calculate volume of differential curve (if applicable)
      calculate_chamber_width_at_reference <- function(sf_obj_inner, sf_obj_outer, reference_axis) {
        if (!reference_axis %in% c("X", "Y")) {
          stop("Reference axis must be 'X' or 'X'")
        }

        lengths_df <- data.frame(
          poly_id = integer(),
          reference_vals = double(),
          length = double()
        )

        extract_lengths <- function(sf_obj, reference_axis) {
          # Process each polygon
          for (i in seq_len(length(sf_obj))) {
            poly <- sf_obj[i, ]

            # Get bounding box
            bbox <- st_bbox(poly)

            # Create lines at integer intervals
            if (reference_axis == "X") {
              reference_vals <- seq(bbox["xmin"], bbox["xmax"], by = 1)
              # Vertical lines at x values
              lines <- lapply(reference_vals, function(x) {
                st_linestring(matrix(c(x, bbox["ymin"], x, bbox["ymax"]), ncol = 2, byrow = TRUE))
              })
            } else if (reference_axis == "Y") {
              reference_vals <- seq(bbox["ymin"], bbox["ymax"], by = 1)
              # Horizontal lines at y values
              lines <- lapply(reference_vals, function(y) {
                st_linestring(matrix(c(bbox["xmin"], y, bbox["xmax"], y), ncol = 2, byrow = TRUE))
              })
            }

            # Convert lines to an sf object with the same CRS
            lines_sfc <- st_sfc(lines, crs = st_crs(poly))

            # Intersect lines with the polygon
            intersections <- st_intersection(lines_sfc, st_geometry(poly))

            # Filter to keep only LINESTRING or MULTILINESTRING results
            valid_types <- st_geometry_type(intersections) %in% c("LINESTRING", "MULTILINESTRING")
            intersections <- intersections[valid_types]

            # Compute lengths of intersecting lines
            if (length(intersections) > 0) {
              lengths <- as.numeric(st_length(intersections))
            } else {
              lengths <- numeric(0) # No intersections
            }
            poly_id <- rep(i, length(lengths))

            # Add lengths to a df. Remove last row.
            lengths_df <- rbind(lengths_df, cbind(poly_id, reference_vals, lengths)[-length(lengths), ])
          }
          return(lengths_df)
        }

        # Width of chamber base at each 0.5cm increment
        inner_chamber_widths <- extract_lengths(sf_obj_inner, reference_axis)
        # Width of chamber upper / Length of curve of the semi-ellipse chamber upper
        outer_chamber_widths <- extract_lengths(sf_obj_outer, reference_axis)
        if (reference_axis == "Y") {
          outer_chamber_widths$reference_vals <- outer_chamber_widths$reference_vals + split_height
        }

        # Join inner and outer widths to a single dataframe
        attributes <- inner_chamber_widths |>
          rename(inner_widths = lengths) |>
          inner_join(outer_chamber_widths |>
            rename(outer_widths = lengths), by = c("poly_id", "reference_vals"))

        return(attributes)
      }

      attributes <- calculate_chamber_width_at_reference(inner_section_poly, outer_section_poly, reference_axis)

      # Function to create a semi-ellipse and calculate their area from base and arc lengths
      calculate_semi_ellipse_area <- function(base_length, curve_length) {
        # Helper function to calculate the complete elliptic integral of the second kind, E(m).
        # E(m) = integral from 0 to pi/2 of sqrt(1 - m * sin^2(theta)) d(theta)
        E <- function(m) {
          integrand <- function(theta) {
            sqrt(1 - m * sin(theta)^2)
          }
          # Use integrate function for numerical integration.
          result <- integrate(integrand, lower = 0, upper = pi / 2)
          return(result$value)
        }

        # Major axis (a) is half the base_length
        a <- base_length / 2

        # Define the objective function whose root we want to find
        objective_function <- function(b_val) {
          # The 'm' parameter for the elliptic integral is e^2, where e is eccentricity
          # m = 1 - (b^2 / a^2)
          m <- 1 - (b_val / a)^2

          # Ensure m is not negative due to floating point inaccuracies
          m <- max(0, m)

          # The arc length of a semi-ellipse is 2 * a * E(m), where E is the
          # complete elliptic integral of the second kind
          calculated_length <- 2 * a * E(m)

          # Return the difference between the calculated length and the target length
          return(calculated_length - curve_length)
        }

        # Uniroott to minimise objective function
        # Use a very small positive number for the lower bound to avoid issues at zero
        solution <- uniroot(
          f = objective_function,
          interval = c(1e-09, a)
        )

        # Semi-minor axis 'b'
        b <- solution$root

        # Calculate area of slice
        area <- 0.5 * pi * a * b

        return(area)
      }

      # Calculate the area of each slice of the differential curve if applicable
      if ((reference_axis == "Y" & input$verticalChamberHeight > input$baffleHeight) |
        (reference_axis == "X" & input$horizontalChamberHeight > input$baffleHeight)) {
        attributes$slice_area_differential <-
          mapply(calculate_semi_ellipse_area, attributes$inner_widths, attributes$outer_widths)
      } else if ((reference_axis == "Y" & input$verticalChamberHeight == input$baffleHeight) |
        (reference_axis == "X" & input$horizontalChamberHeight == input$baffleHeight)) {
        attributes$slice_area_differential <- 0
      }

      # Aggregate differential slice areas to approximate volume
      diff_volume_by_chamber <- attributes |>
        group_by(poly_id) |>
        summarise(
          # Sum area of each 1cm slice to estimate volume
          differential_volume = sum(slice_area_differential, na.rm = TRUE)
        )

      # Join volumes of base shapes and semi-ellipses that make up the differential cut
      chamber_attributes <- inner_join(chamber_attributes_df, diff_volume_by_chamber, by = "poly_id") |>
        mutate(total_volume = base_volume + differential_volume)

      return(chamber_attributes)
    }

    vert_chamber_volumes <- calculate_volume_by_chamber(inner_vert_segmented, outer_vert_segmented, "Y")
    hor_chamber_volumes <- calculate_volume_by_chamber(inner_hor_segmented, outer_hor_segmented, "X")



    ### -------------------------------------------------------------
    ## Specifications

    # ---Volume---
    volume <- sum(vert_chamber_volumes$total_volume) + sum(hor_chamber_volumes$total_volume)
    # down volume per gram CUIN/oz to CUCM/g
    # IDFL paper suggests using 28.77 for inch/cm conversions
    # Samples are usually 28.4/28.5 or 30 grams depending on type of test
    # Volume / FP = weight
    FP_metric <- (input$FP * 16.387064) / 28.349525440835
    average_loft_vert <- sum(vert_chamber_volumes$total_volume) / st_area(vert_simple)
    average_loft_hor <- sum(hor_chamber_volumes$total_volume) / st_area(hor_simple)


    # ---Area---
    inner_layer_area <- st_area(inner_seam)
    outer_layer_area <- st_area(outer_hor_simple_seam) + st_area(outer_vert_simple_seam)
    baffle_material_height <- input$baffleHeight + (2 * input$seamAllowance)
    baffle_material_length <- sum(vert_baffle_lengths, hor_baffle_lengths)
    baffle_material_area <- baffle_material_height * baffle_material_length

    # ---Weight---
    inner_layer_weight <- inner_layer_area * (input$innerWeight / 10000)
    outer_layer_weight <- outer_layer_area * (input$outerWeight / 10000)
    baffle_material_weight <- baffle_material_area * (input$baffleWeight / 10000)
    # Number of grams needed to fill chambers
    grams_down <- volume / FP_metric
    # Adjusted for over/underfill
    grams_down_adj <- grams_down * (1 + (input$overstuff / 100))
    total_weight <- sum(inner_layer_weight, outer_layer_weight, baffle_material_weight, grams_down_adj)


    # Specification data to present
    spec_data <- data.frame(
      Metric = c(
        "Inner Layer Area",
        "Inner Layer Weight",
        "Outer Layer Area",
        "Outer Layer Weight",
        "Baffle Material Height",
        "Baffle Material Length",
        "Baffle Material Area",
        "Baffle Material Weight",
        "Volume",
        "Grams of Down",
        "Average Loft Vertical Chambers",
        "Average Loft Horizontal Chambers",
        "Total Weight"
      ),
      Value = c(
        inner_layer_area,
        inner_layer_weight,
        outer_layer_area,
        outer_layer_weight,
        baffle_material_height,
        baffle_material_length,
        baffle_material_area,
        baffle_material_weight,
        volume,
        grams_down_adj,
        average_loft_vert,
        average_loft_hor,
        total_weight
      ),
      Unit = c(
        "cm^2",
        "grams",
        "cm^2",
        "grams",
        "cm",
        "cm",
        "cm^2",
        "grams",
        "cm^3",
        "grams",
        "cm",
        "cm",
        "grams"
      )
    )

    # Truncate to 2 d.p.
    spec_data$Value <- round(spec_data$Value, digits = 2)
    spec_data$Value <- paste(spec_data$Value, spec_data$Unit, sep = " ")
    ### -------------------------------------------------------------
    # Plotting Data




    inner_vert_segmented <- st_sf(inner_vert_segmented)
    inner_vert_segmented$tooltip <- paste0(
      "Volume: ", round(vert_chamber_volumes$total_volume, 2), " cm^3",
      "<br>Down Required: ", round((vert_chamber_volumes$total_volume / FP_metric) * (1 + (input$overstuff / 100)), 2), " grams"
    )

    inner_hor_segmented <- st_sf(inner_hor_segmented)
    inner_hor_segmented$tooltip <- paste0(
      "Volume: ", round(hor_chamber_volumes$total_volume, 2), " cm^3",
      "<br>Down Required: ", round((hor_chamber_volumes$total_volume / FP_metric) * (1 + (input$overstuff / 100)), 2), " grams"
    )

    # Named list/pseudo-dictionary of output data
    list(
      inner_layer = list(inner, inner_seam, inner_seam_vertices, inner_vert_segmented, inner_hor_segmented, inner_chamber_vertices), # Inner layer polygons
      cross_section = NULL, # Cross-section data for inner plot
      outer_layer_vert = list(outer_vert_segmented, outer_vert_simple_seam, outer_vert_simple_seam_vertices, outer_vert_chamber_vertices), # Outer layer polygons with vertical chambers
      outer_layer_hor = list(outer_hor_segmented, outer_hor_simple_seam, outer_hor_simple_seam_vertices, outer_hor_chamber_vertices), # Outer layer polygons with horizontal chambers
      specifications = spec_data, # DF of quilt specs
      test = test
    )
  })

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
    add_row <- data.frame(
      x = round_any(input$plot_click$x, 0.5),
      y = round_any(input$plot_click$y, 0.5)
    )
    # add row to the data.frame
    values$user_input <- rbind(values$user_input[1:nrow(values$user_input) - 1, ], add_row, values$user_input[nrow(values$user_input), ])
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

  # render a table of the dataframe
  output$table <- shiny::renderTable({
    values$user_input
  })

  output$hover_info <- shiny::renderPrint({
    hover <- input$plot_hover
    cat("X value:", formatC(round_any(hover$x, 0.5), digits = 1, format = "f"), "\n")
    cat("Y value:", formatC(round_any(hover$y, 0.5), digits = 1, format = "f"))
  })

  # -------------------------

  ## --- Page: Output Dimensions ---


  #   data <- data_list()$cross_section

  #   # add each baffle cross-section
  #   for (i in 1:length(data))
  #   {
  #     coords <- data[[i]]
  #     chamber_polygon <- st_sfc(st_polygon(list(coords)))
  #     # Combine them into an sf object
  #     cross_sect_sf <- st_sf(geometry = c(chamber_polygon))
  #     inner_plot <- inner_plot +
  #       geom_sf(data = cross_sect_sf, fill = "lightblue", color = "black")
  #   }

  #   inner_plot
  # })

  output$inner_plot <- renderGirafe({
    req(data_list)
    inner_simple <- data_list()$inner_layer[[1]]
    with_seam <- data_list()$inner_layer[[2]]
    seam_vertices <- data_list()$inner_layer[[3]]
    vert_chambers <- data_list()$inner_layer[[4]]
    hor_chambers <- data_list()$inner_layer[[5]]
    chamber_vertices <- data_list()$inner_layer[[6]]


    gg_poly_inner <- ggplot() +
      geom_sf_interactive(data = with_seam) +
      geom_sf_interactive(data = inner_simple) +
      geom_sf_interactive(data = vert_chambers, aes(tooltip = tooltip)) +
      geom_sf_interactive(data = hor_chambers, aes(tooltip = tooltip)) +
      geom_sf_interactive(
        data = seam_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "blue", size = 1, shape = 21
      ) +
      geom_sf_interactive(
        data = chamber_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "red", size = 0.5, shape = 5, fill = "white"
      ) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
      theme(legend.position = "none") +
      theme_minimal()

    girafe(ggobj = gg_poly_inner)
  })

  output$outer_vert_plot <- renderGirafe({
    req(data_list)
    vert_simple <- data_list()$outer_layer_vert[[1]]
    with_seam <- data_list()$outer_layer_vert[[2]]
    seam_vertices <- data_list()$outer_layer_vert[[3]]
    chamber_vertices <- data_list()$outer_layer_vert[[4]]


    gg_poly_vert_simple <- ggplot() +
      geom_sf_interactive(data = with_seam) +
      geom_sf_interactive(data = vert_simple) +
      geom_sf_interactive(
        data = seam_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "blue", size = 1, shape = 21
      ) +
      geom_sf_interactive(
        data = chamber_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "red", size = 0.5, shape = 5, fill = "white"
      ) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
      theme_minimal()


    girafe(ggobj = gg_poly_vert_simple)
  })

  output$outer_hor_plot <- renderGirafe({
    req(data_list)
    hor_simple <- data_list()$outer_layer_hor[[1]]
    with_seam <- data_list()$outer_layer_hor[[2]]
    seam_vertices <- data_list()$outer_layer_hor[[3]]
    chamber_vertices <- data_list()$outer_layer_hor[[4]]

    gg_poly_hor_simple <- ggplot() +
      geom_sf_interactive(data = with_seam) +
      geom_sf_interactive(data = hor_simple) +
      geom_sf_interactive(
        data = seam_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "blue", size = 1, shape = 21
      ) +
      geom_sf_interactive(
        data = chamber_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "red", size = 0.5, shape = 5, fill = "white"
      ) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
      theme_minimal()

    girafe(ggobj = gg_poly_hor_simple)
  })

  output$specifications <- gt::render_gt({
    validate(
      need(input$verticalChamberHeight >= input$baffleHeight, "Error: Max Vertical Chamber Height is less than Baffle Height."),
      need(input$horizontalChamberHeight >= input$baffleHeight, "Error: Max Horizontal Chamber Height is less than Baffle Height."),
      need((input$horizontalChamberHeight - input$baffleHeight) < (input$horizontalChamberWidth / 2), "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."),
      need((input$horizontalChamberHeight - input$baffleHeight) < (input$horizontalChamberWidth / 2), "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width.")
    )

    spec_data <- data_list()$specifications
    spec_data[, c(1, 2)] |>
      gt(rowname_col = "Metric") |>
      cols_align(
        align = "right",
        columns = Value
      ) |>
      cols_width(Metric ~ px(400)) |>
      tab_footnote(
        footnote = paste0("Adjusted to include ", input$overfill, "% overstuff."),
        locations = cells_stub(rows = "Grams of Down")
      ) |>
      tab_footnote(
        footnote = "Does not include hardware.",
        locations = cells_stub(rows = "Total Weight")
      )
  })

  output$test <- shiny::renderPrint({
    data_list()$test
  })
}

#---------------------------

# Run app
shiny::shinyApp(ui, server)
