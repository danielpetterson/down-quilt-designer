library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(sf)
library(stats)
library(DT)
library(gt)

options(shiny.sanitize.errors = FALSE)

## TODO:
# Add constraint to chamber optimisation to ensure outer circumference is sufficient
# Add plot of chambers in arc

# Issues:
# Create_trapezoid_sf scale

test <- NULL


#---------------------------
# UI Elements
#---------------------------
design_accordion <- bslib::accordion_panel(
  "Design",
  icon = bsicons::bs_icon("grid"),
  numericInput("maxDim", "Longest Dimension (cm)", 210, min = 0),
  numericInput(
    "orientationSplitHeight",
    "Baffle Orientation Change Height",
    50,
    min = 0.5
  ),
  numericInput(
    "baffleHeight",
    label = p(
      "Baffle Height (cm)",
      bslib::tooltip(
        bsicons::bs_icon("info-circle"),
        "For sewn-through baffles it is advised to use the dimension of the outer layer and double
       all volumetric measurements."
      )
    ),
    5,
    min = 0
  ),
  numericInput(
    "verticalChamberHeight",
    "Max Vertical Chamber Height (cm)",
    7.5,
    min = 0
  ),
  numericInput(
    "verticalChamberWidth",
    "Vertical Chamber Width (cm)",
    13,
    min = 0
  ),
  numericInput(
    "horizontalChamberHeight",
    "Max Horizontal Chamber Height (cm)",
    7,
    min = 0
  ),
  numericInput(
    "horizontalChamberWidth",
    "Horizontal Chamber Width (cm)",
    10,
    min = 0
  ),
  numericInput("seamAllowance", "Seam Allowance (cm)", 1, min = 0, step = 0.25),
  selectInput(
    "footboxShape",
    "Select Footbox Type:",
    choices = c(
      "None",
      "Ellipse",
      "Rectangle" # ,
      # "Trapezoid"
    ),
    selected = "None"
  ),
  # conditionalPanel(
  #   condition = "input.footboxShape == 'Trapezoid'",
  #   numericInput("trapezoidRatio", "Ratio", 0.5),
  #   numericInput("trapezoidHeight", "Height", 10),
  # ),
  conditionalPanel(
    condition = "input.footboxShape != 'None'",
    numericInput("ratio", "Ratio", 0.75),
    # numericInput(
    #   "footboxChamberHeight",
    #   "Max Footbox Chamber Height (cm)",
    #   7.5,
    #   min = 0
    # ),
    #   numericInput(
    #     "footboxChamberWidth",
    #     "Footbox Chamber Width (cm)",
    #     10,
    #     min = 0
    #   ),
  ),
  checkboxInput(
    "baffleWallExtension",
    label = p(
      "Include Edge Chamber Wall"
      # bslib::tooltip(
      #   bsicons::bs_icon("info-circle"),
      #   "Use of this feature may cause issues. Please refer to the instruction tab if this occurs."
      # )
    ),
    TRUE
  ),
)

materials_accordion <- bslib::accordion_panel(
  "Materials",
  icon = bsicons::bs_icon("sliders"),
  numericInput("FP", "Fill Power", 850, min = 500, max = 1000, step = 50),
  numericInput("overstuff", "% Overstuff", 20),
  numericInput("innerWeight", "Inner Fabric Weight (gsm)", 35, min = 0),
  numericInput("outerWeight", "Outer Fabric Weight (gsm)", 35, min = 0),
  numericInput("baffleWeight", "Baffle Material Weight (gsm)", 25, min = 0)
)

instruction_card <- bslib::card(
  bslib::card_header("User Guide"),
  # For validation. Comment out in production.
  verbatimTextOutput("test"),
  uiOutput("intro")
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

selected_points_card <- bslib::card(
  bslib::card_header("Input"),
  manual_entry_card,
  # Button to add vertices
  actionButton("add_point", "Add Point"),
  # Button to remove last vertex
  actionButton("rem_point", "Remove Last Point"),
  hr(),
  h5("Selected Points"),
  helpText(
    "Click on any cell in the table to edit its value. Press the 'Enter' key to save the
   change."
  ),
  # Table of selected vertices for right side
  DT::dataTableOutput("input_table"),
)

inner_card <- bslib::card(
  helpText("Scroll over the plot to see the chamber and vertices attributes."),
  # Plot of inner layer dimensions
  # Includes baffle endpoints and volume information
  girafeOutput("inner_plot")
)

outer_vert_card <- bslib::card(
  # Plot of outer layer with vertical baffles/chambers
  girafeOutput("outer_vert_plot")
)

outer_hor_card <- bslib::card(
  # Plot of outer layer with horizontal baffles/chambers
  girafeOutput("outer_hor_plot")
)

inner_footbox_card <- bslib::card(
  # Plot of inner footbox cap/plug layer
  # Circumference = length of bottom edge of inner layer - seam allowances
  # Includes volume information
  girafeOutput("inner_footbox_plot")
)

outer_footbox_card <- bslib::card(
  # Plot of outer footbox cap/plug layer
  # Circumference = length of bottom edge of lower outer layer segment - seam allowances
  girafeOutput("outer_footbox_plot")
)

plot_input_card <- bslib::card(
  bslib::card_header(
    "Define vertices manually or click to draw right side of the quilt"
  ),
  helpText(
    "X must equal zero for the first point and the final point should be at the
  origin (0,0).
  To prevent some issues with the baffle wall extension it is recommended that the penultimate
  Y value also be zero."
  ),
  # X/Y values for mouse hover
  verbatimTextOutput("hover_info"),
  # Reactive plot that displays selected points and accepts new points via click
  plotOutput(
    "input_plot",
    height = 600,
    # Add plot click functionality
    click = "plot_click",
    # Add the hover options
    hover = hoverOpts(
      id = "plot_hover",
      nullOutside = TRUE
    )
  ),
)

#---------------------------
# UI layout
#---------------------------
ui <- bslib::page_navbar(
  title = "Down Quilt Designer",
  # TODO: Check necessity
  # theme = bs_theme(brand = T),
  sidebar = bslib::sidebar(
    bslib::accordion(
      # Add accordion of design parameters
      design_accordion,
      # Add accordion of material parameters
      materials_accordion
    )
  ),
  bslib::nav_panel(
    title = "Instructions",
    # Instructions on how to use the app, notes, troubleshooting, acknowledgements of prior work.
    instruction_card
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
    ),
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
        # Show card only if the design is not only horizontal chambers
        conditionalPanel(
          "!output.full_horizontal",
          outer_vert_card
        )
      ),
      nav_panel(
        "Outer Horizontal",
        # Show card only if the design is not only vertical chambers
        conditionalPanel(
          "!output.full_vertical",
          outer_hor_card
        )
      ),
      nav_panel(
        "Inner Footbox",
        # Show card only if footbox shape is selected
        conditionalPanel(
          'input.footboxShape != "None"',
          inner_footbox_card
        )
      ),
      nav_panel(
        "Outer Footbox",
        # Show card only if footbox shape is selected
        conditionalPanel(
          'input.footboxShape != "None"',
          outer_footbox_card
        )
      ),
    ),
  ),
  bslib::nav_panel(
    title = "Specifications",
    gt_output("specifications"),
  ),
  bslib::nav_panel(
    title = "Temperature Rating",
    plotOutput("temp_rating"),
    gt_output("temp_model"),
    helpText(
      "There are many factors the impact the warmth of a quilt but measured loft serves as a guideline."
    ),
  ),
  bslib::nav_panel(
    title = "Chamber Optimisation",
    helpText(
      "If we assume the material weights and chamber widths to remain constant we can attempt to optimise
  for weight while retaining a target average loft."
    ),
    column(
      12,
      align = "center",
      numericInput("targetLoft", "Target Average Loft (cm)", 5, min = 0)
    ),
    gt_output("optim_table")
  ),
  bslib::nav_panel(
    title = "Technical Information",
    tags$head(
      tags$style(HTML(
        "
        #scrollable_div {
          max-height: 5000px;
          overflow-y: auto;  /* Enable vertical scrolling */
          padding: 15px;
          border: 1px solid #ddd;
        }
        p { margin-bottom: 15px; }
        .note { font-style: italic; color: #555; border-left: 3px solid #007bff; padding-left: 10px; }
        .highlight { font-weight: bold; color: #007bff; }
      "
      ))
    ),
    div(id = "scrollable_div", uiOutput("tech_info"))
  )
)
#---------------------------

#---------------------------
# Backend
#---------------------------
server <- function(input, output) {
  # Rounding for input due to inaccuracy of clicking
  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  # Fixes issue with gt package where input (where you click on the table)
  # and output (what is in the table) is created with the same call
  gt_output <- function(outputId) {
    rlang::check_installed("shiny", "to use `gt_output()`.")

    shiny::htmlOutput(outputId)
  }

  # Calculate scale factor for differential cut
  scale_factor <- function(baffle_orientation) {
    if (baffle_orientation == "vertical") {
      a <- input$verticalChamberWidth / 2 # Semi-major axis (half of width)
      b <- input$verticalChamberHeight - input$baffleHeight # Semi-minor axis (half of height)
    } else if (baffle_orientation == "horizontal") {
      a <- input$horizontalChamberWidth / 2
      b <- input$horizontalChamberHeight - input$baffleHeight
    } else if (baffle_orientation == "footbox") {
      a <- input$footboxChamberWidth / 2
      b <- input$footboxChamberHeight - input$baffleHeight
    }

    # Calculate perimeter of ellipse
    h <- ((a - b) / (a + b))^2
    p <- pi * (a + b) * (1 + 3 * h / (10 + sqrt((4 - 3 * h))))

    # Calculate length of chamber roof (half perimeter)
    maxChamberRoofLength <- round(p / 2, 2)

    # Value to scale inner by in order to achieve disired outer dim
    scale_factor <- maxChamberRoofLength / (a * 2)

    return(scale_factor)
  }

  # Scale outer segments for differential cut
  # Vertical chambers are scaled on the X axis
  # Horizontal chambers are scaled in the Y axis
  scale_geometry <- function(
    sf_poly,
    x_scaling_factor,
    y_scaling_factor,
    chamber_orientation
  ) {
    # Get vertices and scale accordingly
    coords <- st_coordinates(sf_poly)
    coords[, "X"] <- coords[, "X"] * x_scaling_factor
    coords[, "Y"] <- coords[, "Y"] * y_scaling_factor
    # Offset y-axis
    new_polygon <- st_polygon(list(coords[, c("X", "Y")]))
    scaled_polygon <- st_sfc(new_polygon, crs = st_crs(sf_poly)) |> st_sf()

    return(scaled_polygon)
  }

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
    # Form to sf objects
    zero_adj_polygon <- st_polygon(list(coords[, c("X", "Y")]))
    adjusted_polygon <- st_sfc(zero_adj_polygon, crs = st_crs(sf_poly)) |>
      st_sf()
    zero_adj_polygon_seam <- st_polygon(list(coords_seam[, c("X", "Y")]))
    adjusted_polygon_seam <- st_sfc(
      zero_adj_polygon_seam,
      crs = st_crs(sf_poly)
    ) |>
      st_sf()

    output_list <- list(adjusted_polygon, adjusted_polygon_seam)

    return(output_list)
  }

  invert_polygon <- function(polygon) {
    # Invert y-coordinates (reflect across x-axis, y = 0)
    coords <- st_coordinates(polygon)
    coords[, "Y"] <- -coords[, "Y"]

    # Get geometry type and ring structure
    geom_type <- st_geometry_type(polygon)[1]
    rings <- if (geom_type == "POLYGON") {
      coords[, "L1"]
    } else {
      coords[, c("L1", "L2")]
    }

    # Reconstruct geometry
    if (geom_type == "POLYGON") {
      new_geom <- lapply(unique(rings), function(r) {
        ring_coords <- coords[rings == r, c("X", "Y")]
        # Ensure ring is closed
        if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
          ring_coords <- rbind(ring_coords, ring_coords[1, ])
        }
        st_polygon(list(ring_coords))
      }) %>%
        st_sfc(crs = st_crs(polygon))
    } else {
      # MULTIPOLYGON case
      new_geom <- lapply(unique(rings[, "L1"]), function(p) {
        poly_rings <- unique(rings[rings[, "L1"] == p, "L2"])
        rings_list <- lapply(poly_rings, function(r) {
          ring_coords <- coords[
            rings[, "L1"] == p & rings[, "L2"] == r,
            c("X", "Y")
          ]
          if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
            ring_coords <- rbind(ring_coords, ring_coords[1, ])
          }
          ring_coords
        })
        st_polygon(rings_list)
      }) %>%
        st_sfc(crs = st_crs(polygon))
    }

    # Create new sf object
    inverted_poly <- st_sf(geometry = new_geom, st_drop_geometry(polygon))

    # Validate geometry
    if (!all(st_is_valid(inverted_poly))) {
      warning("Resulting geometry may be invalid; attempting to fix")
      inverted_poly <- st_make_valid(inverted_poly)
    }

    return(inverted_poly)
  }

  # Adds baffle wall height to the outermost vertical and horizontal chambers
  add_baffle_wall_allowance <- function(
    sf_poly,
    seam_allowance,
    baffle_height
  ) {
    # Define expanded polygons
    # Seam allowance + baffle height
    poly1 <- st_buffer(
      sf_poly,
      seam_allowance + baffle_height,
      joinStyle = "MITRE",
      mitreLimit = 5
    )
    # Only seam allowance
    poly2 <- st_buffer(
      sf_poly,
      seam_allowance,
      joinStyle = "MITRE",
      mitreLimit = 5
    )

    # Get coordinates of both polygons
    coords1 <- st_coordinates(poly1)[, c("X", "Y")]
    coords2 <- st_coordinates(poly2)[, c("X", "Y")]

    # Find ymax in both polygons
    ymax1 <- max(coords1[, "Y"])
    ymax2 <- max(coords2[, "Y"])

    # Identify indices of ymax coordinates in poly1
    ymax1_idx <- which(coords1[, "Y"] == ymax1)

    # Get x-coordinate(s) of ymax in poly2
    ymax2_x <- coords2[coords2[, "Y"] == ymax2, "X"]

    # Replace ymax coordinates in poly1 with ymax2 value
    coords1[ymax1_idx, "Y"] <- ymax2

    # Reconstruct the geometry for poly1
    geom_type <- st_geometry_type(poly1)[1]

    if (geom_type == "POLYGON") {
      # Get ring structure (L1 from st_coordinates)
      rings1 <- st_coordinates(poly1)[, "L1"]
      new_geom <- lapply(unique(rings1), function(r) {
        ring_coords <- coords1[rings1 == r, c("X", "Y")]
        # Ensure ring is closed
        if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
          ring_coords <- rbind(ring_coords, ring_coords[1, ])
        }
        st_polygon(list(ring_coords))
      }) %>%
        st_sfc(crs = st_crs(poly1))
    } else {
      # MULTIPOLYGON case
      rings1 <- st_coordinates(poly1)[, c("L1", "L2")]
      new_geom <- lapply(unique(rings1[, "L1"]), function(p) {
        poly_rings <- unique(rings1[rings1[, "L1"] == p, "L2"])
        rings <- lapply(poly_rings, function(r) {
          ring_coords <- coords1[
            rings1[, "L1"] == p & rings1[, "L2"] == r,
            c("X", "Y")
          ]
          if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
            ring_coords <- rbind(ring_coords, ring_coords[1, ])
          }
          ring_coords
        })
        st_polygon(rings)
      }) %>%
        st_sfc(crs = st_crs(poly1))
    }

    # Create new sf object
    expanded_poly <- st_sf(geometry = new_geom, st_drop_geometry(poly1))

    # Validate geometry
    if (!all(st_is_valid(expanded_poly))) {
      warning("Resulting geometry may be invalid; attempting to fix")
      expanded_poly <- st_make_valid(expanded_poly)
    }

    return(expanded_poly)
  }

  # Function to mirror chamber polygons across the y-axis
  # Only applied to vertical chambers
  mirror_vert_chambers <- function(sf_poly) {
    reflection_matrix <- matrix(c(-1, 0, 0, 1), nrow = 2, ncol = 2)
    mirrored_geom <- st_geometry(sf_poly) * reflection_matrix
    poly <- c(mirrored_geom, sf_poly)

    return(poly)
  }

  # Extract vertices of an sf polygon
  extract_polygon_vertices <- function(sf_poly) {
    # Cast to points geometries
    vertices <- st_cast(st_geometry(sf_poly), "POINT")
    # Remove final point which closes polygons
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

  # Function to calculate lengths of vertical/horizontal lines in an sfc
  calculate_baffle_lengths <- function(sf_vertices, reference_axis) {
    # Extract coordinates from sf object
    coords <- st_coordinates(sf_vertices)
    if (reference_axis == "Y") {
      data <- data.frame(nonreference = coords[, 1], reference = coords[, 2])
      data <- data |>
        # Remove furthest baffle lines since this function is served by outer layer fabric
        slice_max(order_by = nonreference, n = nrow(data) - 2) |>
        slice_min(order_by = nonreference, n = nrow(data) - 4) |>
        mutate(nonreference = round(nonreference, 2))
    } else if (reference_axis == "X") {
      data <- data.frame(reference = coords[, 1], nonreference = coords[, 2]) |>
        # Remove rows where x is approximately 0 (within a tolerance)
        filter(abs(reference) > 1e-10)
      data <- data |>
        # Remove lowest horizontal line. Covered by outer fabric layer.
        slice_max(order_by = nonreference, n = nrow(data) - 2) |>
        mutate(nonreference = round(nonreference, 2))
    }

    # Group by non-reference value and calculate differences in reference axis for
    # points with same non-reference value
    length_by_baffle <- data |>
      group_by(nonreference) |>
      reframe(
        baffle_length = if (n() > 1) abs(diff(reference)) else NA_real_
      ) |>
      filter(!is.na(baffle_length))

    total_baffle_length <- sum(length_by_baffle$baffle_length)

    return(total_baffle_length)
  }

  # Calculate base area/volume
  # Extrude 2D chambers to 3D shapes (Uses area of polygon to optimise accuracy with little overhead)
  calculate_volume_by_chamber <- function(
    inner_section_poly,
    outer_section_poly,
    reference_axis
  ) {
    # Calculate area of polygons
    chamber_attributes_df <- data.frame(
      poly_id = 1:length(inner_section_poly),
      base_area = as.numeric(st_area(inner_section_poly))
    )
    # Extrude into 3D space
    chamber_attributes_df$base_volume <- chamber_attributes_df$base_area *
      input$baffleHeight

    # Calculate volume of differential curve (if applicable)
    calculate_chamber_width_at_reference <- function(
      sf_obj_inner,
      sf_obj_outer,
      reference_axis
    ) {
      if (!reference_axis %in% c("X", "Y")) {
        stop("Reference axis must be 'X' or 'Y'")
      }

      lengths_df <- data.frame(
        poly_id = integer(),
        reference_vals = double(),
        length = double()
      )

      extract_lengths <- function(sf_obj, reference_axis) {
        # Process each polygon/chamber
        for (i in seq_len(length(sf_obj))) {
          poly <- sf_obj[i, ]

          # Get bounding box
          bbox <- st_bbox(poly)

          # Create lines at integer intervals
          if (reference_axis == "X") {
            reference_vals <- seq(bbox["xmin"], bbox["xmax"], by = 1)
            # Vertical lines at x values
            lines <- lapply(reference_vals, function(x) {
              st_linestring(matrix(
                c(x, bbox["ymin"], x, bbox["ymax"]),
                ncol = 2,
                byrow = TRUE
              ))
            })
          } else if (reference_axis == "Y") {
            reference_vals <- seq(bbox["ymin"], bbox["ymax"], by = 1)
            # Horizontal lines at y values
            lines <- lapply(reference_vals, function(y) {
              st_linestring(matrix(
                c(bbox["xmin"], y, bbox["xmax"], y),
                ncol = 2,
                byrow = TRUE
              ))
            })
          }

          # Convert lines to an sf object with the same CRS
          lines_sfc <- st_sfc(lines, crs = st_crs(poly))

          # Intersect lines with the polygon
          intersections <- st_intersection(lines_sfc, st_geometry(poly))

          # Filter to keep only LINESTRING or MULTILINESTRING results
          valid_types <- st_geometry_type(intersections) %in%
            c("LINESTRING", "MULTILINESTRING")
          intersections <- intersections[valid_types]

          # Compute lengths of intersecting lines
          if (length(intersections) > 0) {
            lengths <- as.numeric(st_length(intersections))
          } else {
            lengths <- numeric(0) # No intersections
          }
          poly_id <- rep(i, length(lengths))
          # Round for floating point innaccuracies. Necessary for correct join.
          reference_vals <- round(reference_vals, 10)
          # Add lengths to a df. Remove last row.
          lengths_df <- rbind(
            lengths_df,
            # TODO: solve vector length discrepency.
            suppressWarnings(cbind(poly_id, reference_vals, lengths))[
              -length(lengths),
            ]
          )
        }
        return(lengths_df)
      }

      # Width of chamber base at each 1cm increment
      inner_chamber_widths <- extract_lengths(sf_obj_inner, reference_axis)
      # Width of chamber upper / Length of curve of the semi-ellipse chamber upper
      outer_chamber_widths <- extract_lengths(sf_obj_outer, reference_axis)
      # Standardise axis between inner and outer layers
      if (reference_axis == "Y") {
        outer_chamber_widths$reference_vals <- outer_chamber_widths$reference_vals +
          input$orientationSplitHeight
      }

      # Join inner and outer widths to a single dataframe
      attributes <- inner_chamber_widths |>
        rename(inner_widths = lengths) |>
        inner_join(
          outer_chamber_widths |>
            rename(outer_widths = lengths),
          by = c("poly_id", "reference_vals")
        )

      return(attributes)
    }

    # DF with polygon ID, inner and outer chamber widths
    attributes <- calculate_chamber_width_at_reference(
      inner_section_poly,
      outer_section_poly,
      reference_axis
    )

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

      # Uniroot to minimise objective function
      # Issues if lower bound is zero
      # Use tryCatch to handle errors relating to bisection method of rootfinding
      solution <- tryCatch(
        {
          uniroot(
            f = objective_function,
            interval = c(1e-09, a)
          )
        },
        error = function(e) {
          # This ensures that the calculated area for a problematic diff slice should be ~0
          return(list(root = 0.0001))
        }
      )

      # Semi-minor axis b
      b <- solution$root

      # Calculate area of slice
      area <- 0.5 * pi * a * b

      return(area)
    }

    # Calculate the area of each slice of the differential curve if applicable
    if (
      (reference_axis == "Y" &
        input$verticalChamberHeight > input$baffleHeight) |
        (reference_axis == "X" &
          input$horizontalChamberHeight > input$baffleHeight)
    ) {
      attributes$slice_area_differential <-
        mapply(
          calculate_semi_ellipse_area,
          attributes$inner_widths,
          attributes$outer_widths
        )
    } else if (
      (reference_axis == "Y" &
        input$verticalChamberHeight == input$baffleHeight) |
        (reference_axis == "X" &
          input$horizontalChamberHeight == input$baffleHeight)
    ) {
      attributes$slice_area_differential <- 0
    }

    # Aggregate differential slice areas to approximate volume
    diff_volume_by_chamber <- attributes |>
      group_by(poly_id) |>
      reframe(
        # Sum area of each 1cm slice to estimate volume
        differential_volume = sum(slice_area_differential, na.rm = TRUE)
      )

    # Join volumes of base shapes and semi-ellipses that make up the differential cut
    chamber_attributes <- inner_join(
      chamber_attributes_df,
      diff_volume_by_chamber,
      by = "poly_id"
    ) |>
      mutate(total_volume = base_volume + differential_volume)

    return(list(chamber_attributes, attributes))
  }

  # Function to create an sf object of an ellipse (or circle if a = b)
  # perimeter: The target perimeter for the ellipse.
  # ratio: The shape parameter (theta), defined as b/a, where 0 < ratio <= 5.
  # A ratio of 1 is a circle, greater than 1 changes the orientation, and a
  # ratio close to 0 is a very flat ellipse.
  create_ellipse_sf <- function(perimeter, ratio) {
    # Validate the ratio input
    if (ratio <= 0 || ratio > 5) {
      stop("The 'ratio' parameter must be between 0 and 5 (exclusive of 0).")
    }

    # Helper function to calculate ellipse perimeter
    # a: semi-major axis
    # b: semi-minor axis
    calculate_perimeter <- function(a, b) {
      pi * (3 * (a + b) - sqrt((3 * a + b) * (a + 3 * b)))
    }

    # Define central point
    center <- c(0, 0)

    # Define unscaled axes based on the desired shape ratio.
    # We start with a base ellipse of size 1.
    a_unscaled <- 1
    b_unscaled <- ratio

    # Calculate the perimeter of this small, unscaled ellipse.
    perimeter_unscaled <- calculate_perimeter(a_unscaled, b_unscaled)

    # Determine the scaling factor needed to reach the desired perimeter.
    scale_factor <- perimeter / perimeter_unscaled

    # Calculate the final, scaled semi-axes.
    a <- a_unscaled * scale_factor
    b <- b_unscaled * scale_factor

    # Generate points along the ellipse boundary using parametric equations.
    # More points result in a smoother shape.
    angles <- seq(0, 2 * pi, length.out = 100)
    x_coords <- center[1] + a * cos(angles)
    y_coords <- center[2] + b * sin(angles)

    # Combine coordinates into a matrix for the polygon definition.
    ellipse_points <- cbind(x_coords, y_coords)
    # Add final point to close polygon
    ellipse_points <- rbind(ellipse_points, ellipse_points[1, ])
    # Convert the points into an sf polygon object.
    ellipse_poly <- st_polygon(list(ellipse_points))

    # Create the final sf data frame with useful metadata.
    ellipse_sf <- st_as_sf(data.frame(
      geometry = st_sfc(ellipse_poly),
      semi_major_a = a,
      semi_minor_b = b
    ))

    return(ellipse_sf)
  }

  # perimeter: The target perimeter for the rectangle.
  # ratio: The shape parameter, defined as height / width, where 0 < ratio <= 5.
  create_rectangle_sf <- function(perimeter, ratio) {
    # Validate the ratio input
    if (ratio <= 0 || ratio > 5) {
      stop("The 'ratio' parameter must be between 0 and 5 (exclusive of 0).")
    }

    # Define central point
    center <- c(0, 0)

    # Define unscaled dimensions based on the desired shape ratio.
    w_unscaled <- 1
    h_unscaled <- ratio

    # Calculate the perimeter of this small, unscaled rectangle.
    perimeter_unscaled <- 2 * (w_unscaled + h_unscaled)

    # Determine the scaling factor.
    scale_factor <- perimeter / perimeter_unscaled

    # Calculate the final, scaled dimensions.
    w <- w_unscaled * scale_factor
    h <- h_unscaled * scale_factor

    # Generate the four corner points for the polygon.
    x_coords <- center[1] + c(-w / 2, w / 2, w / 2, -w / 2, -w / 2)
    y_coords <- center[2] + c(-h / 2, -h / 2, h / 2, h / 2, -h / 2)
    rectangle_points <- cbind(x_coords, y_coords)

    # Convert the points into an sf polygon object.
    rectangle_poly <- st_polygon(list(rectangle_points))

    # Create the final sf data frame with useful metadata.
    rectangle_sf <- st_as_sf(data.frame(
      geometry = st_sfc(rectangle_poly),
      width_w = w,
      height_h = h
    ))

    return(rectangle_sf)
  }

  # # Assumes an isosceles trapezoid where height is the average of the parallel sides.
  # # perimeter: The target perimeter for the trapezoid.
  # # ratio: short_base / long_base, where 0 < ratio < 5.
  # # height: height of the shape
  # create_trapezoid_sf <- function(perimeter, ratio, height) {
  #   # Validate input parameters
  #   if (ratio <= 0 || ratio >= 5) {
  #     stop("The 'ratio' parameter (short_base / long_base) must be between 0 and 1 (exclusive of 0 and 1).")
  #   }
  #   if (height <= 0 || height >= perimeter) {
  #     stop("The 'height' parameter must be positive and less than the perimeter.")
  #   }

  #   # Define central point
  #   center <- c(0, 0)

  #   # Define unscaled dimensions
  #   # a = long base
  #   # b = short base
  #   # h = height
  #   # c = non-parallel side
  #   a_unscaled <- 1
  #   b_unscaled <- ratio * a_unscaled # ratio = b / a
  #   h_unscaled <- height # Use the provided height directly

  #   # Calculate the length of the non-parallel side 'c' using Pythagoras
  #   # c^2 = h^2 + ((a - b) / 2)^2
  #   c_unscaled <- sqrt(h_unscaled^2 + ((a_unscaled - b_unscaled) / 2)^2)

  #   # Calculate the unscaled perimeter
  #   perimeter_unscaled <- a_unscaled + b_unscaled + 2 * c_unscaled

  #   # Determine the scaling factor to achieve the target perimeter
  #   scale_factor <- perimeter / perimeter_unscaled

  #   # Calculate scaled dimensions
  #   a <- a_unscaled * scale_factor
  #   b <- b_unscaled * scale_factor
  #   h <- h_unscaled
  #   c <- c_unscaled * scale_factor

  #   # Generate the four corner points for the polygon
  #   x_coords <- center[1] + c(-a / 2, a / 2, b / 2, -b / 2, -a / 2)
  #   y_coords <- center[2] + c(-h / 2, -h / 2, h / 2, h / 2, -h / 2)

  #   # Combine coordinates into a matrix
  #   trapezoid_points <- cbind(x_coords, y_coords)

  #   # Convert to sf polygon object
  #   trapezoid_poly <- st_polygon(list(trapezoid_points))

  #   # Create the final sf data frame with metadata
  #   trapezoid_sf <- st_as_sf(data.frame(
  #     geometry = st_sfc(trapezoid_poly),
  #     long_base_a = a,
  #     short_base_b = b,
  #     height_h = h,
  #     side_c = c
  #   ))

  #   return(trapezoid_sf)
  # }

  # Assumptions:
  # - The width w is fixed for both the rectangle and the base of the semi-ellipse.
  # - The semi-ellipse has base length w (major axis 2a with a = w/2) and height b (semi-minor axis).
  # - The rectangle has height h_r.
  # - Average loft height H = h_r + (pi * b)/4
  # - Constraint: h_r = H - (pi * b)/2 >= 0, b >= 0
  # - Objective: Minimize total weight weight = outer layer weight + baffle weight
  optimise_chambers <- function(
    orientation,
    attributes_by_cm,
    specifications_list,
    baffle_length_list
  ) {
    # Function to compute the semi-elliptical arc length using numerical integration
    arc_length <- function(b, a) {
      integrand <- function(theta) {
        sqrt(a^2 * sin(theta)^2 + b^2 * cos(theta)^2)
      }
      result <- integrate(integrand, lower = 0, upper = pi)
      return(result$value)
    }

    # Weight function to minimize
    weight_func <- function(b) {
      h_r <- input$targetLoft - (pi * b) / 4
      if (h_r < 0 || b < 0) {
        return(Inf)
      }
      # Length of outer shell in cm increments in each chamber
      L_curve_by_chamber_cm <- mapply(
        arc_length,
        b,
        attributes_by_cm$inner_widths / 2
      )
      # Total area of outer shell used in chambers
      arc_area_total <- sum(L_curve_by_chamber_cm)

      # Existing shell weight to try to minimise
      # Inner layer not factored in because that is a constant.
      weight <- ((input$outerWeight / 10000) * arc_area_total) +
        (baffle_length_list[[orientation]] * (input$baffleWeight / 10000)) *
          (h_r + (input$seamAllowance * 2))

      return(weight)
    }

    # Find the optimal b in the range [0, 2*H/pi]
    b_min <- 0
    b_max <- 2 * input$targetLoft / pi
    opt <- optimize(weight_func, interval = c(b_min, b_max))

    # Output results
    out_l <- list()
    # optimal value of h_r
    out_l$optim_baffle_height <- input$targetLoft - (pi * opt$minimum) / 4
    # optimal max chamber height
    out_l$optim_max_chamber_height <- opt$minimum + out_l$optim_baffle_height
    # # Objective value
    # min_weight <- opt$objective

    return(out_l)
  }

  # Function to generate reactive chamber plots
  generate_chamber_plot <- function(
    a,
    b,
    baffle_height,
    title
  ) {
    # Create angle sequence
    theta <- seq(0, pi, length.out = 100)

    # Parametric equations for ellipse
    x <- a * cos(theta)
    y <- b * sin(theta)

    # Raise by baffle height
    y <- y + baffle_height

    # Shift of x-axis so bottom left corner is at origin
    x <- x + a

    # Create data frame
    ellipse_curve <- data.frame(x = x, y = y, theta = theta)

    # Add baffle walls
    ellipse_curve_data <- rbind(
      c(max(ellipse_curve$x), 0, NA),
      ellipse_curve,
      c(min(ellipse_curve$x), 0, NA)
    )

    p <- ggplot() +
      coord_fixed(ratio = 1) +
      theme_minimal() +
      labs(title = title, x = "Width (cm)", y = "Height (cm)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_path(
        data = ellipse_curve_data,
        aes(x = x, y = y),
        color = "black"
      ) +
      annotate(
        "segment",
        x = 0,
        xend = a * 2,
        y = 0,
        yend = 0,
        linetype = "dashed",
        linewidth = 0.8,
        color = "black"
      ) +
      annotate(
        "text",
        x = a,
        y = 0,
        label = "ILW",
        size = 4,
        vjust = 1.5
      ) +
      annotate(
        "text",
        x = a,
        y = 0,
        label = a * 2,
        size = 4,
        vjust = 3
      ) +
      annotate(
        "segment",
        x = a * 2.1,
        xend = a * 2.1,
        y = 0,
        yend = baffle_height + b,
        linetype = "dashed",
        linewidth = 0.8,
        color = "black",
      ) +
      annotate(
        "text",
        x = a * 2,
        y = (baffle_height + b) / 2,
        label = "Hc",
        size = 4,
        hjust = -2.5
      ) +
      annotate(
        "text",
        x = a * 2,
        y = (baffle_height + b) / 2,
        label = baffle_height + b,
        size = 4,
        hjust = -3.25
      )
    if (b == 0) {
      p <- p +
        lims(
          y = c(-0.5, max(ellipse_curve_data$y) + 1),
          x = c(-0.5, max(ellipse_curve_data$x) + 1)
        ) +
        annotate(
          "text",
          x = a,
          y = baffle_height,
          label = "OLW",
          size = 4,
          vjust = -0.5
        )
    } else if (b > 0) {
      # Create OLW annotation
      x_OLW <- ((a + 0.5) * cos(theta)) + a
      y_OLW <- ((b + 0.5) * sin(theta)) + baffle_height
      OLW_curve <- data.frame(x = x_OLW, y = y_OLW)

      p <- p +
        lims(
          y = c(-0.5, max(OLW_curve$y) + 0.5),
          x = c(-0.5, max(OLW_curve$x) + 1)
        ) +
        annotate(
          "segment",
          x = a,
          xend = a,
          y = 0,
          yend = baffle_height,
          linetype = "dashed",
          linewidth = 0.8
        ) +
        annotate(
          "text",
          x = a,
          y = baffle_height / 2,
          label = "Hb",
          size = 4,
          hjust = 1.5
        ) +
        annotate(
          "text",
          x = a,
          y = baffle_height / 2,
          label = baffle_height,
          size = 4,
          hjust = -1.5
        ) +
        geom_line(
          data = OLW_curve,
          aes(x = x, y = y),
          linetype = "dashed",
          linewidth = 0.8
        ) +
        annotate(
          "text",
          x = a,
          y = (baffle_height + b),
          label = "OLW",
          size = 4,
          vjust = -3
        ) +
        annotate(
          "text",
          x = a,
          y = (baffle_height + b),
          label = round(
            (pi * (3 * (a + b) - sqrt((3 * a + b) * (a + 3 * b)))) / 2,
            2
          ),
          size = 4,
          vjust = -4.5
        ) +
        annotate(
          "segment",
          x = a,
          xend = a,
          y = baffle_height,
          yend = baffle_height + b,
          linetype = "dashed",
          linewidth = 0.8,
          color = "red"
        ) +
        annotate(
          "text",
          x = a,
          y = baffle_height + (b / 2),
          label = "b",
          size = 4,
          hjust = 1.5
        ) +
        annotate(
          "text",
          x = a,
          y = baffle_height + (b / 2),
          label = b,
          size = 4,
          hjust = -0.5
        ) +
        annotate(
          "segment",
          x = a,
          xend = a * 2,
          y = baffle_height,
          yend = baffle_height,
          linetype = "dashed",
          linewidth = 0.8,
          color = "green"
        ) +
        annotate(
          "text",
          x = a * 1.5,
          y = baffle_height,
          label = "a",
          size = 4,
          vjust = 1.5
        ) +
        annotate(
          "text",
          x = a * 1.5,
          y = baffle_height,
          label = a,
          size = 4,
          vjust = -0.5
        )
    }

    return(p)
  }

  #---------------------------
  # Coordinate Inputs
  #---------------------------
  values <- shiny::reactiveValues()

  values$user_input <- data.frame(
    x = c(0, 68, 72, 76, 76, 75, 72, 67, 62, 57, 52, rep(50, 4), 0),
    y = c(190, 190, seq(175, 10, -15), 0, 0)
  )

  # # For validation
  # values$user_input <- data.frame(
  #   x = c(0, 50, 50, 0),
  #   y = c(100, 100, 0, 0)
  # )

  # Reactive values for conditional output of outer layer plots
  output$full_horizontal <- reactive({
    max(values$user_input$y) == input$orientationSplitHeight
  })
  output$full_vertical <- reactive({
    input$orientationSplitHeight == 0
  })

  outputOptions(output, "full_horizontal", suspendWhenHidden = FALSE)
  outputOptions(output, "full_vertical", suspendWhenHidden = FALSE)

  # Add opposing points to user selected points
  all_selected_points_x <- shiny::reactive({
    req(values$user_input)
    c(values$user_input$x, -rev(values$user_input$x))
  })
  all_selected_points_y <- shiny::reactive({
    req(values$user_input)
    c(values$user_input$y, rev(values$user_input$y))
  })

  # Logical values to denote a quilt being all vertical/horizontal chambers
  full_vertical_chambers <- shiny::reactive({
    input$orientationSplitHeight == 0
  })
  full_horizontal_chambers <- shiny::reactive({
    req(values$user_input)
    max(values$user_input$y) == input$orientationSplitHeight
  })

  #---------------------------
  # Geometric Data List
  #---------------------------
  # Reactive dataframe to hold values for output
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
    # Add seam allowance w/ non-rounded vertices
    inner_seam <- st_buffer(
      inner,
      input$seamAllowance,
      joinStyle = "MITRE",
      mitreLimit = 5
    )

    # Initialise empty vectors and dfs
    outer_layer_area <- 0
    baffle_length <- 0
    inner_chamber_vertices <- st_sfc()
    inner_segmented <- st_sfc()
    chamber_volumes <- data.frame(
      poly_id = numeric(),
      base_area = double(),
      base_volume = double(),
      differential_volume = double(),
      total_volume = double()
    )

    if (!full_horizontal_chambers()) {
      # Define bounding boxes for vertical/horizontal chamber segments
      vert_bbox <- st_sfc(st_polygon(list(cbind(
        c(
          -max(points$x),
          max(points$x),
          max(points$x),
          -max(points$x),
          -max(points$x)
        ),
        c(
          max(points$y),
          max(points$y),
          input$orientationSplitHeight,
          input$orientationSplitHeight,
          max(points$y)
        )
      ))))
      # Split base polygon into vertical/horizontal chamber segments
      vert_simple <- st_crop(inner, vert_bbox)

      # Add seam allowance w/ non-rounded vertices
      inner_vert_simple_seam <- st_buffer(
        vert_simple,
        input$seamAllowance,
        joinStyle = "MITRE",
        mitreLimit = 5
      )

      # Calculate scale factor
      vert_baffle_scale_factor <- scale_factor("vertical")

      # Apply scaling
      outer_vert_simple <- scale_geometry(
        vert_simple,
        vert_baffle_scale_factor,
        1
      )

      # Add seam allowance
      outer_vert_simple_seam <- st_buffer(
        outer_vert_simple,
        input$seamAllowance,
        joinStyle = "MITRE",
        mitreLimit = 5
      )

      # adjust so that y axis starts at zero
      outer_vert_list <- adjust_y_zero(
        outer_vert_simple,
        outer_vert_simple_seam
      )
      outer_vert_simple <- outer_vert_list[[1]]
      outer_vert_simple_seam <- outer_vert_list[[2]]

      # Add area to running total
      outer_layer_area <- outer_layer_area + st_area(outer_vert_simple_seam)

      # Adjust y-axis to be in line
      vert_simple <- vert_simple + c(0, input$seamAllowance)

      # Create a regular grid that covers the inner layer segment
      inner_vert_grid <- st_make_grid(
        vert_simple,
        cellsize = c(
          input$verticalChamberWidth,
          max(points$y) + input$seamAllowance
        ),
        square = TRUE,
        offset = c(0, 0)
      )
      # Create a regular grid that covers the outer layer segment
      outer_vert_grid <- st_make_grid(
        outer_vert_simple,
        cellsize = c(
          input$verticalChamberWidth * vert_baffle_scale_factor,
          max(points$y * 2)
        ),
        square = TRUE,
        offset = c(0, 0)
      )

      # Split polygon into subpolygons(chambers) by grid
      # Inner layer
      inner_vert_segmented <- st_intersection(inner_vert_grid, vert_simple)
      inner_vert_segmented <- mirror_vert_chambers(inner_vert_segmented)

      #Outer layer
      outer_vert_segmented <- st_intersection(
        outer_vert_grid,
        outer_vert_simple
      )
      outer_vert_segmented <- mirror_vert_chambers(outer_vert_segmented)

      # Extract intersection points for plotting
      outer_vert_simple_seam_vertices <- extract_polygon_vertices(
        outer_vert_simple_seam
      )
      outer_vert_chamber_vertices <- extract_chamber_vertices(
        outer_vert_segmented
      )

      # Baffle length for only the section with vertical chambers
      vert_baffle_length <- sum(calculate_baffle_lengths(
        outer_vert_chamber_vertices,
        "Y"
      ))

      # Running total of baffle length
      baffle_length <- baffle_length + vert_baffle_length

      # Calculate volume data for each chamber
      vert_chamber_volumes <- calculate_volume_by_chamber(
        inner_vert_segmented,
        outer_vert_segmented,
        "Y"
      )[[1]]
      vert_chamber_by_cm <- calculate_volume_by_chamber(
        inner_vert_segmented,
        outer_vert_segmented,
        "Y"
      )[[2]]
      chamber_volumes <- rbind(chamber_volumes, vert_chamber_volumes)

      # Plotting Data
      inner_chamber_vertices <- rbind(
        inner_chamber_vertices,
        extract_chamber_vertices(inner_vert_segmented)
      )

      inner_vert_segmented <- st_sf(inner_vert_segmented)
      st_geometry(inner_vert_segmented) <- "inner_segmented"
      # Add tooltip for hovering
      inner_vert_segmented$tooltip <- paste0(
        "Volume: ",
        round(vert_chamber_volumes$total_volume, 2),
        " cm^3",
        "<br>Down Required: ",
        round(
          (vert_chamber_volumes$total_volume /
            ((input$FP * 16.387064) / 28.349525440835)) *
            (1 + (input$overstuff / 100)),
          2
        ),
        " grams"
      )

      # Add fabric to outer layer to cover outer edge walls if checkbox selected
      if (input$baffleWallExtension & full_vertical_chambers()) {
        outer_vert_simple_seam <- st_buffer(
          outer_vert_simple,
          input$seamAllowance + input$baffleHeight,
          joinStyle = "MITRE",
          mitreLimit = 5
        )
        # Adjust so the y-axis starts at zero
        outer_vert_simple_seam <- st_geometry(outer_vert_simple_seam) +
          c(0, input$baffleHeight)
        outer_vert_segmented <- st_geometry(outer_vert_segmented) +
          c(0, input$baffleHeight)
        # Update vertices
        outer_vert_simple_seam_vertices <- extract_polygon_vertices(
          outer_vert_simple_seam
        )
        outer_vert_chamber_vertices <- extract_chamber_vertices(
          outer_vert_segmented
        )
      } else if (input$baffleWallExtension) {
        # Invert polygon since we want the short edge to be on the bottom
        outer_vert_simple_inverted <- invert_polygon(outer_vert_simple)
        outer_vert_simple_seam_inverted <- add_baffle_wall_allowance(
          outer_vert_simple_inverted,
          input$seamAllowance,
          input$baffleHeight
        )
        # Invert back to normal orientation
        outer_vert_simple_seam <- invert_polygon(
          outer_vert_simple_seam_inverted
        )
        # Update vertices
        outer_vert_simple_seam_vertices <- extract_polygon_vertices(
          outer_vert_simple_seam
        )
        outer_vert_chamber_vertices <- extract_chamber_vertices(
          outer_vert_segmented
        )
      }
      # Join segmented poly data
      inner_segmented <- rbind(inner_segmented, inner_vert_segmented)
    }

    # Same as above unless otherwise commented
    if (!full_vertical_chambers()) {
      hor_bbox <- st_sfc(st_polygon(list(cbind(
        c(
          -max(points$x),
          max(points$x),
          max(points$x),
          -max(points$x),
          -max(points$x)
        ),
        c(
          min(points$y),
          min(points$y),
          input$orientationSplitHeight,
          input$orientationSplitHeight,
          min(points$y)
        )
      ))))

      hor_simple <- st_crop(inner, hor_bbox)

      hor_baffle_scale_factor <- scale_factor("horizontal")
      outer_hor_simple <- scale_geometry(hor_simple, 1, hor_baffle_scale_factor)

      outer_hor_simple_seam <- st_buffer(
        outer_hor_simple,
        input$seamAllowance,
        joinStyle = "MITRE",
        mitreLimit = 5
      )

      outer_hor_list <- adjust_y_zero(outer_hor_simple, outer_hor_simple_seam)
      outer_hor_simple <- outer_hor_list[[1]]
      outer_hor_simple_seam <- outer_hor_list[[2]]

      outer_layer_area <- outer_layer_area + st_area(outer_hor_simple_seam)

      hor_simple <- hor_simple + c(0, input$seamAllowance)

      inner_hor_grid <- st_make_grid(
        hor_simple,
        cellsize = c((max(points$x * 2)), input$horizontalChamberWidth),
        square = TRUE
      )
      outer_hor_grid <- st_make_grid(
        outer_hor_simple,
        cellsize = c(
          (max(points$x * 2)),
          input$horizontalChamberWidth * hor_baffle_scale_factor
        ),
        square = TRUE
      )

      # Apply the rotation to the grid geometry so that the chambers complete from the top of the horizontal sections
      rot_angle <- pi
      rotation_matrix <- matrix(
        c(cos(rot_angle), sin(rot_angle), -sin(rot_angle), cos(rot_angle)),
        nrow = 2
      )
      inner_hor_grid <- st_geometry(inner_hor_grid) *
        rotation_matrix +
        c(0, max(st_coordinates(hor_simple)[, "Y"]) + input$seamAllowance)
      outer_hor_grid <- st_geometry(outer_hor_grid) *
        rotation_matrix +
        c(0, max(st_coordinates(outer_hor_simple_seam)[, "Y"]))

      inner_hor_segmented <- st_intersection(inner_hor_grid, hor_simple)
      outer_hor_segmented <- st_intersection(outer_hor_grid, outer_hor_simple)

      outer_hor_chamber_vertices <- extract_chamber_vertices(
        outer_hor_segmented
      )

      hor_baffle_length <- sum(calculate_baffle_lengths(
        outer_hor_chamber_vertices,
        "X"
      ))
      baffle_length <- baffle_length + hor_baffle_length

      hor_chamber_volumes <- calculate_volume_by_chamber(
        inner_hor_segmented,
        outer_hor_segmented,
        "X"
      )[[1]]
      hor_chamber_by_cm <- calculate_volume_by_chamber(
        inner_hor_segmented,
        outer_hor_segmented,
        "X"
      )[[2]]
      chamber_volumes <- rbind(chamber_volumes, hor_chamber_volumes)

      inner_chamber_vertices <- rbind(
        inner_chamber_vertices,
        extract_chamber_vertices(inner_hor_segmented)
      )

      inner_hor_segmented <- st_sf(inner_hor_segmented)
      st_geometry(inner_hor_segmented) <- "inner_segmented"
      inner_hor_segmented$tooltip <- paste0(
        "Volume: ",
        round(hor_chamber_volumes$total_volume, 2),
        " cm^3",
        "<br>Down Required: ",
        round(
          (hor_chamber_volumes$total_volume /
            ((input$FP * 16.387064) / 28.349525440835)) *
            (1 + (input$overstuff / 100)),
          2
        ),
        " grams"
      )

      inner_segmented <- rbind(inner_segmented, inner_hor_segmented)

      if (input$baffleWallExtension & full_horizontal_chambers()) {
        outer_hor_simple_seam <- st_buffer(
          outer_hor_simple,
          input$seamAllowance + input$baffleHeight,
          joinStyle = "MITRE",
          mitreLimit = 5
        )
        outer_hor_simple_seam <- st_geometry(outer_hor_simple_seam) +
          c(0, input$baffleHeight)
        outer_hor_segmented <- st_geometry(outer_hor_segmented) +
          c(0, input$baffleHeight)
        outer_hor_simple_seam_vertices <- extract_polygon_vertices(
          outer_hor_simple_seam
        )
        outer_hor_chamber_vertices <- extract_chamber_vertices(
          outer_hor_segmented
        )
      } else if (input$baffleWallExtension) {
        outer_hor_simple_seam <- add_baffle_wall_allowance(
          outer_hor_simple,
          input$seamAllowance,
          input$baffleHeight
        )
        outer_hor_simple_seam <- st_geometry(outer_hor_simple_seam) +
          c(0, input$baffleHeight)
        outer_hor_segmented <- st_geometry(outer_hor_segmented) +
          c(0, input$baffleHeight)
        outer_hor_simple_seam_vertices <- extract_polygon_vertices(
          outer_hor_simple_seam
        )
        outer_hor_chamber_vertices <- extract_chamber_vertices(
          outer_hor_segmented
        )
      }
    }

    # Adjust y-axis to start at zero
    inner_list <- adjust_y_zero(inner, inner_seam)
    inner <- inner_list[[1]]
    inner_seam <- inner_list[[2]]

    # Extract vertices
    inner_seam_vertices <- extract_polygon_vertices(inner_seam)

    if (input$footboxShape != "None") {
      # Calculate perimeter of footbox layers
      footbox_perimeter <- function(sf_poly) {
        coords <- st_coordinates(sf_poly)
        bottom_edge_x <- coords[, "X"][
          coords[, "Y"] == min(coords[, "Y"])
        ]
        perimeter <- diff(range(bottom_edge_x, na.rm = TRUE))
        perimeter_minus_seam_allowance <- perimeter - (2 * input$seamAllowance)

        return(perimeter_minus_seam_allowance)
      }

      fb_inner_perimeter <- footbox_perimeter(inner)

      if (!full_vertical_chambers()) {
        fb_outer_perimeter <- footbox_perimeter(outer_hor_simple_seam)
      } else {
        fb_outer_perimeter <- footbox_perimeter(outer_vert_simple_seam)
      }

      if (input$footboxShape == "Ellipse") {
        inner_footbox <- create_ellipse_sf(fb_inner_perimeter, input$ratio)
        outer_footbox <- create_ellipse_sf(fb_outer_perimeter, input$ratio)
      } else if (input$footboxShape == "Rectangle") {
        inner_footbox <- create_rectangle_sf(fb_inner_perimeter, input$ratio)
        outer_footbox <- create_rectangle_sf(fb_outer_perimeter, input$ratio)
        # } else if (input$footboxShape == "Trapezoid") {
        #   inner_footbox <- create_trapezoid_sf(fb_inner_perimeter, input$trapezoidRatio, input$trapezoidHeight)
      }

      inner_footbox_seam <- st_buffer(
        inner_footbox,
        input$seamAllowance,
        joinStyle = "MITRE",
        mitreLimit = 5
      )
      outer_footbox_seam <- st_buffer(
        outer_footbox,
        input$seamAllowance,
        joinStyle = "MITRE",
        mitreLimit = 5
      )

      # Define vertices of shape w/ seam allowance for cutting
      inner_footbox_vertices <- extract_polygon_vertices(inner_footbox_seam)
      outer_footbox_vertices <- extract_polygon_vertices(outer_footbox_seam)

      # Volumes for version without differential baffles
      footbox_area <- st_area(outer_footbox)
      footbox_volume <- footbox_area * input$baffleHeight
      chamber_volumes <- rbind(
        chamber_volumes,
        c(0, footbox_area, footbox_volume, 0, footbox_volume)
      )

      # Define tooltip in same way as inner layer
      inner_footbox$tooltip <- paste0(
        "Volume: ",
        round(footbox_volume, 2),
        " cm^3",
        "<br>Down Required: ",
        round(
          (footbox_volume / ((input$FP * 16.387064) / 28.349525440835)) *
            (1 + (input$overstuff / 100)),
          2
        ),
        " grams"
      )

      # footbox_baffle_scale_factor <- scale_factor("footbox")

      # # Create a regular grid that covers the outer layer segment
      # inner_footbox_grid <- st_make_grid(inner_footbox, cellsize = c(input$footboxChamberWidth, fb_inner_perimeter/2), square = TRUE, offset = c(0,-fb_inner_perimeter/4))
      # outer_footbox_grid <- st_make_grid(outer_footbox, cellsize = c(input$footboxChamberWidth * footbox_baffle_scale_factor, fb_inner_perimeter/2), square = TRUE, offset = c(0,-fb_inner_perimeter/4))

      # # Split polygon into subpolygons(chambers) by grid
      # inner_footbox_segmented <- st_intersection(inner_footbox_grid, inner_footbox)
      # inner_footbox_segmented <- mirror_vert_chambers(inner_footbox_segmented)

      # outer_footbox_segmented <- st_intersection(outer_footbox_grid, outer_footbox)
      # outer_footbox_segmented <- mirror_vert_chambers(outer_footbox_segmented)

      # inner_footbox_chamber_vertices <- extract_chamber_vertices(inner_footbox_segmented)
      # outer_footbox_chamber_vertices <- extract_chamber_vertices(outer_footbox_segmented)

      # # Baffle length for footbox (excluding external ring)
      # footbox_baffle_length <- sum(calculate_baffle_lengths(outer_footbox_chamber_vertices, "Y"))
      # # Running total of baffle length
      # baffle_length <- baffle_length + footbox_baffle_length

      # footbox_chamber_volumes <- calculate_volume_by_chamber(inner_footbox_segmented, outer_footbox_segmented, "Y")[[1]]
      # footbox_chamber_by_cm <- calculate_volume_by_chamber(inner_footbox_segmented, outer_footbox_segmented, "Y")[[2]]
      # chamber_volumes <- rbind(chamber_volumes, footbox_chamber_volumes)

      # inner_footbox_segmented <- st_sf(inner_footbox_segmented)
      # st_geometry(inner_footbox_segmented) <- "inner_footbox_segmented"
      # inner_footbox_segmented$tooltip <- paste0(
      #   "Volume: ", round(footbox_chamber_volumes$total_volume, 2), " cm^3",
      #   "<br>Down Required: ", round((footbox_chamber_volumes$total_volume / ((input$FP * 16.387064) / 28.349525440835)) * (1 + (input$overstuff / 100)), 2), " grams"
      # )
    }

    ### -------------------------------------------------------------
    ## Specifications

    # ---Volume---
    volume <- sum(chamber_volumes$total_volume)
    # down volume per gram CUIN/oz to CUCM/g
    # IDFL paper suggests using 28.77 for inch/cm conversions
    # Samples are usually 28.4/28.5 or 30 grams depending on type of test
    # Volume / FP = weight
    FP_metric <- (input$FP * 16.387064) / 28.349525440835
    if (max(all_selected_points_y()) != input$orientationSplitHeight) {
      average_loft_vert <- sum(vert_chamber_volumes$total_volume) /
        st_area(vert_simple)
    } else {
      average_loft_vert <- 0
    }
    if (input$orientationSplitHeight != 0) {
      average_loft_hor <- sum(hor_chamber_volumes$total_volume) /
        st_area(hor_simple)
    } else {
      average_loft_hor <- 0
    }

    # ---Area---
    if (input$footboxShape == "None") {
      inner_layer_area <- st_area(inner_seam)
      outer_layer_area <- outer_layer_area
      baffle_material_length <- baffle_length
    } else {
      inner_layer_area <- st_area(inner_seam) + st_area(inner_footbox_seam)
      outer_layer_area <- outer_layer_area + st_area(outer_footbox_seam)
      baffle_material_length <- baffle_length + fb_inner_perimeter
    }
    if (input$baffleHeight != 0) {
      baffle_material_height <- input$baffleHeight + (2 * input$seamAllowance)
    } else {
      baffle_material_height <- 0
    }
    baffle_material_area <- baffle_material_height * baffle_material_length

    # ---Weight---
    inner_layer_weight <- inner_layer_area * (input$innerWeight / 10000)
    outer_layer_weight <- outer_layer_area * (input$outerWeight / 10000)
    baffle_material_weight <- baffle_material_area *
      (input$baffleWeight / 10000)
    shell_weight <- inner_layer_weight +
      outer_layer_weight +
      baffle_material_weight
    # # Number of grams needed to fill chambers
    grams_down <- volume / FP_metric
    # Adjusted for over/underfill
    grams_down_adj <- grams_down * (1 + (input$overstuff / 100))
    total_weight <- sum(
      inner_layer_weight,
      outer_layer_weight,
      baffle_material_weight,
      grams_down_adj
    )

    # Named list/dict of specifications
    specifications_l <- list(
      "Inner Layer Area" = inner_layer_area,
      "Inner Layer Weight" = inner_layer_weight,
      "Outer Layer Area" = outer_layer_area,
      "Outer Layer Weight" = outer_layer_weight,
      "Baffle Material Height" = baffle_material_height,
      "Baffle Material Length" = baffle_material_length,
      "Baffle Material Area" = baffle_material_area,
      "Baffle Material Weight" = baffle_material_weight,
      "Volume" = volume,
      "Grams of Down" = grams_down_adj,
      "Average Loft Vertical Chambers" = average_loft_vert,
      "Average Loft Horizontal Chambers" = average_loft_hor,
      "Shell Weight" = shell_weight,
      "Total Weight" = total_weight
    )

    ### -------------------------------------------------------------
    # Optimisation
    baffle_length_l <- list()
    optim_vals <- list()

    if (!full_horizontal_chambers()) {
      baffle_length_l$vertical <- vert_baffle_length
      # Generate optimal values for baffle height and max vertical chamber height
      optim_vals$Vertical <- optimise_chambers(
        "vertical",
        vert_chamber_by_cm,
        specifications_l,
        baffle_length_l
      )
    }

    if (!full_vertical_chambers()) {
      baffle_length_l$horizontal <- hor_baffle_length
      # Generate optimal values for baffle height and max vertical chamber height
      optim_vals$Horizontal <- optimise_chambers(
        "horizontal",
        hor_chamber_by_cm,
        specifications_l,
        baffle_length_l
      )
    }

    c(
      list(test = test),
      list(specifications = specifications_l), # DF of quilt specs
      list(optimisation = optim_vals),
      list(
        inner_layer = list(
          inner,
          inner_seam,
          inner_seam_vertices,
          inner_segmented,
          inner_chamber_vertices
        )
      ), # Inner layer polygons
      # Named list/pseudo-dictionary of output data
      if (!full_horizontal_chambers()) {
        # Outer layer polygons with vertical chambers
        list(
          outer_layer_vert = list(
            outer_vert_segmented,
            outer_vert_simple_seam,
            outer_vert_simple_seam_vertices,
            outer_vert_chamber_vertices
          )
        )
      } else {
        list(outer_layer_vert = list())
      },
      if (!full_vertical_chambers()) {
        # Outer layer polygons with horizontal chambers
        list(
          outer_layer_hor = list(
            outer_hor_segmented,
            outer_hor_simple_seam,
            outer_hor_simple_seam_vertices,
            outer_hor_chamber_vertices
          ) # Outer layer polygons with horizontal chambers
        )
      } else {
        list(outer_layer_hor = list())
      },
      if (input$footboxShape != "None") {
        list(
          inner_footbox = list(
            inner_footbox,
            inner_footbox_seam,
            inner_footbox_vertices
          ), #, inner_footbox_segmented, inner_footbox_chamber_vertices), # Inner footbox polygons
          outer_footbox = list(
            outer_footbox,
            outer_footbox_seam,
            outer_footbox_vertices
          ) #, outer_footbox_segmented, outer_footbox_chamber_vertices) # Outer footbox polygons
        )
      } else {
        list(
          inner_footbox = list(),
          outer_footbox = list()
        )
      }
    )
  })

  #---------------------------
  # Temerature Reference Data
  #---------------------------
  BPL_rating <- data.frame(
    Temperature = c(10, 4.44, -1.11, -6.67, -12.22, -17.78, -23.33, -28.89),
    Loft = c(3.05, 3.81, 4.57, 5.59, 6.60, 7.62, 8.89, 10.16),
    Source = "BPL"
  )

  katabatic_rating <- data.frame(
    Temperature = c(4.44, -1.11, -5.56, -9.44, -15),
    Loft = c(4.5, 5.7, 7, 8.2, 9.5),
    Source = "Katabatic"
  )

  c_equation_rating <- data.frame(
    Temperature = c(10.5, 3.5, -3.5, -10.5, -17.5),
    Loft = c(2, 4, 6, 8, 10),
    Source = "17.5°C - (Loft * 3.5)"
  )

  timmermade_new_rating <- data.frame(
    Temperature = c(10, 4.44, -1.11, -6.67, -12.22, -17.78, -23.33, -28.89),
    Loft = c(3.81, 5.69, 7.59, 9.5, 11.4, 13.28, 15.19, 17.09),
    Source = "Timmermade (New)"
  )

  timmermade_old_rating <- data.frame(
    Temperature = c(10, 4.44, -1.11, -6.67, -12.22, -17.78, -23.33, -28.89),
    Loft = c(2.79, 4.95, 6.60, 8.26, 9.91, 11.56, 13.21, 14.86),
    Source = "Timmermade (Old)"
  )

  EE_rating <- data.frame(
    Temperature = c(10, 4.44, -1.11, -6.67, -12.22, -17.78, -23.33),
    Loft = c(2.54, 3.81, 5.08, 6.35, 7.62, 8.89, 10.16),
    Source = "Enlightened Equipment/Warbonnet"
  )

  nunatak_rating <- data.frame(
    Temperature = c(7.22, 1.67, -2.22, -5.56, -7.78, -12.22),
    Loft = c(2.54, 4.064, 5.33, 6.35, 7.366, 9.144),
    Source = "Nunatak"
  )

  temp_rating_df <- rbind(
    c_equation_rating,
    BPL_rating,
    EE_rating,
    katabatic_rating,
    timmermade_old_rating,
    timmermade_new_rating,
    nunatak_rating
  )

  # Generate regression models: one for each Source group
  temp_models <- temp_rating_df |>
    group_by(Source) |>
    do(model = lm(Temperature ~ Loft, data = .))

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
    values$user_input <- rbind(values$user_input, add_row)
  })

  # add row on actionButton click
  shiny::observeEvent(input$add_point, {
    add_row <- rbind(values$user_input, c(input$x_add, input$y_add))
    colnames(add_row) <- c("x", "y")
    values$user_input <- add_row
  })

  # remove row on actionButton click
  shiny::observeEvent(input$rem_point, {
    rem_row <- values$user_input[-nrow(values$user_input), ]
    # colnames(rem_row) <- c("x", "y")
    values$user_input <- rem_row
  })

  output$input_table <- DT::renderDataTable({
    DT::datatable(
      values$user_input,
      rownames = FALSE,
      editable = list(target = "cell"), # This enables cell-level editing
      options = list(
        "info" = FALSE,
        "paging" = FALSE,
        "searching" = FALSE,
        "ordering" = FALSE
      )
    )
  })

  output$hover_info <- shiny::renderPrint({
    hover <- input$plot_hover
    cat(
      "X value:",
      formatC(round_any(hover$x, 0.5), digits = 1, format = "f"),
      "\n"
    )
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

  output$intro <- shiny::renderUI({
    tagList(
      h5("How To Use"),
      h6("Input Dimensions"),
      p(
        "Enter the coordinates for each vertex of the right side of your desired shape."
      ),
      h6("Sidebar Parameters"),
      p("Customize the parameters in the sidebar to meet your requirements."),
      h6("Parameter Definitions:"),
      tags$ul(
        tags$li(
          "Longest Dimension: Sets the axis limits for the Input Dimensions graph."
        ),
        tags$li(
          "Baffle Orientation Change Height: Divides the shape into vertical chambers
        above this height and horizontal chambers below. For a single chamber orientation,
        set this to the minimum or maximum y-value."
        ),
        tags$li(
          "Baffle Height: The height of the finished baffle, excluding seam allowance.
         Set to zero for sewn-through baffles."
        ),
        tags$li(
          "Maximum Chamber Height: TThe distance from the chamber's center
         base to the highest point of the semi-ellipse. For a non-differential cut, set this
         equal to the baffle height."
        ),
        tags$li(
          "Chamber Width: The maximum distance between baffle walls."
        ),
      ),
      h5("Output"),
      h6("Shape Plot Legend"),
      tags$ul(
        tags$li(
          "Blue circles denote the outer edge vertices of the shape, including seam allowance."
        ),
        tags$li(
          "Red circles denote the vertices of the chambers and guide where to sew baffles."
        ),
        tags$li(
          "Scrolling over chamber areas of the Inner Layer and Inner Footbox tabs will display
        information about the volume of the chamber and the amount of down required to fill them to
        your desired level of over/underfill."
        ),
      ),
      h6("Temperature Rating"),
      p(
        "Temperature ratings are very subjective in an area where there are no standardised
          tests. Larger manufacturers tend to advertise limit ratings whereas smaller producers tend
          to value customer satisfaction (and warmth) so the ratings are more conservative. The
          Temperature Rating tab allows you to see where your projected average loft sits relative
          to these estimates."
      ),
      h6("Chamber Optimisation"),
      p(
        "This is for the gram weenies amoung us! If you have decided on you ideal target loft,
      chamber widths and material weights then you can use this feature to optimise the baffle
       height and maximum chamber height for the lowest weight finished product. Generally it
       should favour a slight differential cut but you may want to increase this difference to
        make full use of the main purpose of a differential cut, limiting down compression when
         force is applied from inside."
      ),
      h6("Acknowledgements"),
      p(
        "This app was created to extend on the work of CatSplat's Underquilt Calculator which has been an
      invaluable tool to the community."
      ),
      tags$p(
        "Please direct any queries or issues to the  ",
        tags$a(
          href = "https://github.com/danielpetterson/down-quilt-designer",
          "Github repository"
        ),
        ".",
      ),
    )
  })

  output$inner_plot <- renderGirafe({
    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    req(data_list)
    inner_simple <- data_list()$inner_layer[[1]]
    with_seam <- data_list()$inner_layer[[2]]
    seam_vertices <- data_list()$inner_layer[[3]]
    chambers <- data_list()$inner_layer[[4]]
    chamber_vertices <- data_list()$inner_layer[[5]]

    gg_poly_inner <- ggplot() +
      geom_sf_interactive(data = with_seam) +
      geom_sf_interactive(data = inner_simple) +
      geom_sf_interactive(data = chambers, aes(tooltip = tooltip)) +
      geom_sf_interactive(
        data = seam_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "blue",
        size = 1,
        shape = 21
      ) +
      geom_sf_interactive(
        data = chamber_vertices,
        aes(tooltip = tooltip, data_id = id),
        color = "red",
        size = 0.5,
        shape = 5,
        fill = "white"
      ) +
      geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
      theme(legend.position = "none") +
      theme_minimal()

    girafe(ggobj = gg_poly_inner)
  })

  output$outer_vert_plot <- renderGirafe({
    req(data_list)

    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    gg_poly_vert <- ggplot()

    if (!full_horizontal_chambers()) {
      vert_simple <- data_list()$outer_layer_vert[[1]]
      with_seam <- data_list()$outer_layer_vert[[2]]
      seam_vertices <- data_list()$outer_layer_vert[[3]]
      chamber_vertices <- data_list()$outer_layer_vert[[4]]

      gg_poly_vert <- gg_poly_vert +
        geom_sf_interactive(data = with_seam) +
        geom_sf_interactive(data = vert_simple) +
        geom_sf_interactive(
          data = seam_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "blue",
          size = 1,
          shape = 21
        ) +
        geom_sf_interactive(
          data = chamber_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "red",
          size = 0.5,
          shape = 5,
          fill = "white"
        ) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
        theme_minimal()
    }

    girafe(ggobj = gg_poly_vert)
  })

  output$outer_hor_plot <- renderGirafe({
    req(data_list)

    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    gg_poly_hor <- ggplot()

    if (!full_vertical_chambers()) {
      hor_simple <- data_list()$outer_layer_hor[[1]]
      with_seam <- data_list()$outer_layer_hor[[2]]
      seam_vertices <- data_list()$outer_layer_hor[[3]]
      chamber_vertices <- data_list()$outer_layer_hor[[4]]

      gg_poly_hor <- gg_poly_hor +
        geom_sf_interactive(data = with_seam) +
        geom_sf_interactive(data = hor_simple) +
        geom_sf_interactive(
          data = seam_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "blue",
          size = 1,
          shape = 21
        ) +
        geom_sf_interactive(
          data = chamber_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "red",
          size = 0.5,
          shape = 5,
          fill = "white"
        ) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 1) +
        theme_minimal()
    }

    girafe(ggobj = gg_poly_hor)
  })

  output$inner_footbox_plot <- renderGirafe({
    req(data_list)

    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    gg_footbox_inner <- ggplot()

    if (input$footboxShape != "None") {
      inner <- data_list()$inner_footbox[[1]]
      inner_seam <- data_list()$inner_footbox[[2]]
      seam_vertices <- data_list()$inner_footbox[[3]]
      # chambers <- data_list()$inner_footbox[[4]]
      # chamber_vertices <- data_list()$inner_footbox[[5]]

      gg_footbox_inner <- gg_footbox_inner +
        geom_sf_interactive(data = inner_seam) +
        geom_sf_interactive(data = inner, aes(tooltip = tooltip)) +
        # geom_sf_interactive(data = chambers) +#, aes(tooltip = tooltip)) +
        geom_sf_interactive(
          data = seam_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "blue",
          size = 1,
          shape = 21
        ) +
        # geom_sf_interactive(
        #   data = chamber_vertices,
        #   aes(tooltip = tooltip, data_id = id),
        #   color = "red", size = 0.5, shape = 5, fill = "white"
        # ) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.5) +
        theme_minimal()
    }

    girafe(ggobj = gg_footbox_inner)
  })

  output$outer_footbox_plot <- renderGirafe({
    req(data_list)

    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    gg_footbox_outer <- ggplot()

    if (input$footboxShape != "None") {
      outer <- data_list()$outer_footbox[[1]]
      outer_seam <- data_list()$outer_footbox[[2]]
      seam_vertices <- data_list()$outer_footbox[[3]]
      # chambers <- data_list()$outer_footbox[[4]]
      # chamber_vertices <- data_list()$inner_footbox[[5]]

      gg_footbox_outer <- gg_footbox_outer +
        geom_sf_interactive(data = outer_seam) +
        geom_sf_interactive(data = outer) +
        # geom_sf_interactive(data = chambers) +#, aes(tooltip = tooltip)) +
        geom_sf_interactive(
          data = seam_vertices,
          aes(tooltip = tooltip, data_id = id),
          color = "blue",
          size = 1,
          shape = 21
        ) +
        # geom_sf_interactive(
        #   data = chamber_vertices,
        #   aes(tooltip = tooltip, data_id = id),
        #   color = "red", size = 0.5, shape = 5, fill = "white"
        # ) +
        geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.5) +
        theme_minimal()
    }

    girafe(ggobj = gg_footbox_outer)
  })

  output$specifications <- gt::render_gt({
    validate(
      need(
        input$verticalChamberHeight >= input$baffleHeight,
        "Error: Max Vertical Chamber Height is less than Baffle Height."
      ),
      need(
        input$horizontalChamberHeight >= input$baffleHeight,
        "Error: Max Horizontal Chamber Height is less than Baffle Height."
      ),
      need(
        (input$horizontalChamberHeight - input$baffleHeight) <=
          (input$horizontalChamberWidth / 2),
        "Error: (Max Horizontal Chamber Height - Baffle Height) is greater than half the Horizontal Chamber Width."
      ),
      need(
        (input$verticalChamberHeight - input$baffleHeight) <=
          (input$verticalChamberWidth / 2),
        "Error: (Max Vertical Chamber Height - Baffle Height) is greater than half the Vertical Chamber Width."
      )
    )

    spec_data <- data_list()$specifications
    spec_df <- data.frame(
      Metric = names(spec_data),
      Value = unlist(spec_data)
    )

    # Format decimal places and units
    decimals <- c(0, 2, 0, 2, 2, 2, 0, 2, 0, 2, 2, 2, 0)
    spec_df$Value <- mapply(
      function(num, dec) format(round(num, dec), nsmall = dec),
      spec_df$Value,
      decimals,
      SIMPLIFY = FALSE
    )
    spec_units <- c(
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
      "grams",
      "grams"
    )
    spec_df$Value <- paste(spec_df$Value, spec_units, sep = " ")

    spec_df |>
      gt(rowname_col = "Metric") |>
      cols_align(
        align = "right",
        columns = Value
      ) |>
      cols_width(Metric ~ px(400)) |>
      tab_footnote(
        footnote = paste0(
          "Adjusted to include ",
          input$overfill,
          "% overstuff."
        ),
        locations = cells_stub(rows = "Grams of Down")
      ) |>
      tab_footnote(
        footnote = "Does not include hardware.",
        locations = cells_stub(rows = "Total Weight")
      )
  })

  output$temp_rating <- renderPlot({
    spec_data <- data_list()$specifications
    vert_intercept <- spec_data[["Average Loft Vertical Chambers"]]
    hor_intercept <- spec_data[["Average Loft Horizontal Chambers"]]

    ggplot(temp_rating_df, aes(x = Loft, y = Temperature, color = Source)) +
      labs(
        title = "Temperature Rating as a Function of Loft",
        x = "Loft (cm)",
        y = "Temperature Rating (C)"
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_vline(
        aes(
          xintercept = vert_intercept,
          linetype = "Average Loft Vertical Chambers"
        ),
        color = "red",
      ) +
      geom_vline(
        aes(
          xintercept = hor_intercept,
          linetype = "Average Loft Horizontal Chambers"
        ),
        color = "blue",
      ) +
      theme_minimal()
  })

  output$temp_model <- gt::render_gt({
    spec_data <- data_list()$specifications
    vert_intercept <- spec_data[["Average Loft Vertical Chambers"]]
    hor_intercept <- spec_data[["Average Loft Horizontal Chambers"]]

    # Predict temperature rating for each Source
    vertical_chamber_predictions <- temp_models |>
      mutate(
        predicted_temperature = round(
          predict(model, newdata = data.frame(Loft = vert_intercept)),
          2
        ),
        chamber_orientation = "Vertical"
      )

    horizontal_chamber_predictions <- temp_models |>
      mutate(
        predicted_temperature = round(
          predict(model, newdata = data.frame(Loft = hor_intercept)),
          2
        ),
        chamber_orientation = "Horizontal"
      )

    temp_predictions <- rbind(
      vertical_chamber_predictions,
      horizontal_chamber_predictions
    ) |>
      select(Source, chamber_orientation, predicted_temperature) |>
      arrange(desc(chamber_orientation), Source)

    # Output the predicted values
    temp_predictions |>
      gt(rowname_col = "Source") |>
      cols_align(
        align = "right",
        columns = chamber_orientation
      ) |>
      cols_label(
        chamber_orientation = "Chamber Orientation",
        predicted_temperature = "Temperature Rating (C)"
      )
  })

  output$optim_table <- render_gt({
    optim_data <- unlist(data_list()$optimisation)

    split_data <- strsplit(
      names(unlist(optim_data)),
      "\\."
    )
    optim_df <- data.frame(
      "Chamber Orientation" = sapply(split_data, `[`, 1),
      Parameter = sapply(split_data, `[`, 2),
      "Optimal Value" = round(optim_data, 2)
    ) |>
      mutate(
        Parameter = recode(
          Parameter,
          "optim_baffle_height" = "Baffle Height",
          "optim_max_chamber_height" = "Maximum Chamber Height",
          .default = "keep_as_is"
        )
      ) |>
      rename_with(~ gsub("\\.", " ", .))
  })

  output$tech_info <- shiny::renderUI({
    tagList(
      h3("Measurements"),
      h6("Chamber Dimensions"),
      # Plot showing specfic chamber parameters
      # if (!full_horizontal_chambers) {
      plotOutput("vert_chamber_slice_plot"),
      # },
      # if (!full_vertical_chambers) {
      plotOutput("hor_chamber_slice_plot"), #, height = "500px"),
      # },
      p(
        "These plots provide a cross-sectional view of the chambers as defined. The notation is as follows:"
      ),
      tags$ul(
        tags$li("ILW = Internal Layer Width"),
        tags$li(
          "OLW = Outer Layer Width, the length of the curve of the semi-ellipse if using a differential cut. Otherwise equivalent to the ILW."
        ),
        tags$li("Hb = Baffle Height"),
        tags$li("Hc = Chamber Height. The maximum height of the chamber"),
        tags$li(
          "b = The length of the semi-minor axis, the difference between the maximum chamber height and the baffle height."
        ),
        tags$li(
          "a = The length of the semi-major axis, equivalent to half of the ILW."
        )
      ),
      div(
        class = "note",
        p(
          "OLW is calculated using Ramanujan's first approximation for the perimeter of an ellipse and then halved:",
          withMathJax(
            "$$ 1/2\\pi * (3 (a+b) - \\sqrt((3 * a + b) * (a +3 * b))) $$"
          )
        )
      ),
      h4("How Area and Volume Are Calculated"),

      p(
        "To find the ",
        span("area of the base rectangle", class = "highlight"),
        ", we multiply the ",
        span("internal layer width (ILW)", class = "highlight"),
        " by the ",
        span("baffle height (Hb)", class = "highlight"),
        ".",
        withMathJax("$$ ILW * Hb $$")
      ),

      p(
        "If you're using a ",
        span("differential cut", class = "highlight"),
        ", we add the area of a semi-ellipse to this calculation, using the formula: ",
        withMathJax("$$ 1/2\\pi ab $$")
      ),

      p(
        "For chambers that have a consistent ",
        span("ILW", class = "highlight"),
        " throughout their length, we calculate the ",
        span("volume", class = "highlight"),
        " by simply multiplying the area by the length."
      ),

      p(
        "For chambers where the ",
        span("ILW", class = "highlight"),
        " changes, we calculate the area at every 1 cm interval along the length. This method maintains the same ratio between the internal and external layer widths (",
        span("ILW", class = "highlight"),
        " and ",
        span("OLW", class = "highlight"),
        ")."
      ),

      div(
        class = "note",
        p("A few things to note about the calculations:"),
        tags$ul(
          tags$li(
            "Using 1 cm increments can slightly reduce the volume accuracy for chambers with non-parallel walls."
          ),
          tags$li(
            "As the ",
            span("ILW", class = "highlight"),
            " decreases, the value of",
            span("b", class = "highlight"),
            " (in the semi-ellipse formula) decreases by the same percentage."
          ),
          tags$li(
            "It is necessary to select",
            span("Include Edge Chamber Wall", class = "highlight"),
            "for the calculations to be valid for any edge chambers. The additional fabric on the outer layer becomes the outer walls of these chambers in place of baffle material."
          ),
        ),
        p(
          "However, these factors are unlikely to affect your results in a meaningful way. The slight trade-off in accuracy is insignificant compared to potential manual errors during construction. Additionally, chambers with non-parallel baffle walls are typically edge chambers, which are more likely to be compressed during use meaning that a reduced maximum chamber height has little impact during use."
        )
      ),
      h3("Chamber Optimisation"),
      p(
        "Considering only the chamber widths, weights of the baffle and outer material, along with the quantities needed for each section, we can optimize the baffle height (Hb) and maximum chamber height (Hc), excluding seam allowance. Typically, if the baffle material weighs significantly less than the outer material, the baffle height (b) will be small but will increase as the weight difference decreases. Note that if b is too small, some chambers may compress when the quilt is wrapped around a body."
      ),
      h3("Sewn-Through Baffles"),
      p(
        "In sewn-through baffle designs, commonly used in down garments or warm weather quilts, a differential cut may reduce shrinkage in the dimension perpendicular to the baffle orientation, but it's not guaranteed to eliminate it. A more effective approach could be to account for the inner layer's tendency to curve (and thus shorten) by using the outer layer's dimensions for both the inner and outer layers. The end result would be a chain of ellipses, but be cautious of overstuffing, which can reduce the eccentricity of the ellipse and cause further shrinkage."
      ),
      h3("Footbox Design"),
      tags$ul(
        tags$li(
          "The footbox shape is defined by a single parameter, the ratio between the length
         of the two sides (rectangular) or the ratio between the semi-minor and semi-major axes
         (ellipse). In order to rotate the footbox 90 degrees use 1 / current value. This also
         serves to change the chamber orientation."
        ),
        tags$li(
          "Currently oval and rectangular footbox shapes are supported with a non-differential cut. 
        You may want to use wider baffle walls here to allow for a higher average loft if the rest
         of the quilt is differentially cut. If you need this functionality
           urgently you can recreate the footbox shape by entering the Inner Footbox vertices in the
           Input Dimensions tab and deselecting Include Edge Chamber Wall."
        ),
        tags$li(
          "The perimeters of the layers of the footbox plug is equivalent to the length of the bottom
         edge of the respective layer of the quilt excluding the seam allowance."
        ),
      ),
      h6("Footbox Connection"),
      p(
        "As a disclaimer I have yet to make a quilt with a sewn-footbox so I'm hoping for input from
         the community regarding best practices for a future version. It seems like the least complex 
         solution is to sew baffle material around the perimeter of the outer layer and then pleat or 
         gather periodically when sewing the other side of the baffle material to the inner layer. 
         The plug could then be sewn to the rest of the quilt. It may be wise to create msaller 
         chambers in the plug to mitigate down migration."
      ),
      h6("Troubleshooting"),
      p(
        "If the footbox appears incorrectly sized please note that the perimeter of the footbox is
      equal to the length of the bottom edge of the inner layer, minus the seam allowances, i.e. where
      x = 0 on the input graph. Ensure that this length is correct when defining the shape."
      ),
    )
  })

  output$vert_chamber_slice_plot <- renderPlot(
    {
      req(
        input$baffleHeight,
        input$verticalChamberHeight,
        input$horizontalChamberHeight,
        input$verticalChamberWidth,
        input$horizontalChamberWidth
      )

      vert_chamber_plot <- generate_chamber_plot(
        input$verticalChamberWidth / 2,
        input$verticalChamberHeight - input$baffleHeight,
        input$baffleHeight,
        "Vertical Chamber Visualisation"
      )
      vert_chamber_plot
    } #,
    # res = 100
  )

  output$hor_chamber_slice_plot <- renderPlot(
    {
      req(
        input$baffleHeight,
        input$verticalChamberHeight,
        input$horizontalChamberHeight,
        input$verticalChamberWidth,
        input$horizontalChamberWidth
      )

      hor_chamber_plot <- generate_chamber_plot(
        input$horizontalChamberWidth / 2,
        input$horizontalChamberHeight - input$baffleHeight,
        input$baffleHeight,
        "Horizontal Chamber Visualisation"
      )
      hor_chamber_plot
    }
  )

  output$test <- shiny::renderPrint({
    data_list()$test
  })
}

#---------------------------

# Run app
shiny::shinyApp(ui, server)
