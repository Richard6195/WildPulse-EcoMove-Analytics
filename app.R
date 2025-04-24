################################################################################
##                      WildPulse EcoMove Analytics - Setup                   ##
################################################################################
#
# This Shiny web application can be run by clicking the "Run App" button
# in RStudio or calling shinyApp(...) in an R session.
#
# More on Shiny: https://shiny.rstudio.com
#
options(warn = -1)
# ==== 1) Install & Load Required Packages ====================================

# For each needed package, install if missing, then load quietly.
# 'type = "source"' may be needed on certain systems.

pkgs_required <- c(
  "DT", "xts", "rgl", "zoo", "shiny", "dplyr", "ggplot2", "spsComps", "janitor",
  "dygraphs", "lubridate", "data.table", "colourpicker", "RColorBrewer",
  "shinyvalidate", "shinycssloaders", "shinyWidgets", "shinythemes",
  "shinyjs", "htmltools", "leaflet", "tidyr", "Rcpp", "assertthat",
  "fst", "plotly", "isotree", "leaflet.extras", "sf", "leafgl",
  "jsonlite", "shinyAce", "signal", "RcppRoll", "leafem",
  "raster", "MASS", "terra", "stats", "ks", "KernSmooth", "shinyscreenshot"
)

for (pkg in pkgs_required) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, type = "source")
    suppressMessages(library(pkg, character.only = TRUE))
  } else {
    suppressMessages(library(pkg, character.only = TRUE))
  }
}

# ==== 2) Verify Packages Are Installed ========================================

# Confirm that all required packages truly installed on the system.
# If not, throw an error listing which are missing.

installed_pkgs <- row.names(installed.packages())
missing_pkgs   <- setdiff(pkgs_required, installed_pkgs)
if (length(missing_pkgs) > 0) {
  stop(
    "The following required packages are not installed: ",
    paste(missing_pkgs, collapse = ", ")
  )
}
# ==== 3) Global Shiny Options ================================================

# Increase the maximum size of uploaded files to 5000 MB (5 GB).
options(shiny.maxRequestSize = 5000 * 1024^2)

################################################################################
##                                FUNCTIONS                                    ##
################################################################################

# ==== 4) General Utility Functions ===========================================

# Add transparency to color(s).
addalpha <- function(colors, alpha = 1.0) {
  # Convert color(s) to RGBA, modify alpha channel, then convert back to hex string.
  r <- col2rgb(colors, alpha = TRUE)
  r[4, ] <- alpha * 255
  r <- r / 255
  rgb(r[1, ], r[2, ], r[3, ], r[4, ])
}

# Check if an object is POSIXct type.
is.POSIXct <- function(x) inherits(x, "POSIXct")

# Ensures milliseconds are displayed in dygraph legend date/time formatting.
CustomValueFormat <- '
  function (ms) {
    var d = new Date(ms);
    return Dygraph.zeropad(d.getDate()) + "/" +
           Dygraph.zeropad(d.getMonth() + 1) + "/" +
           Dygraph.zeropad(d.getFullYear()) + " " +
           Dygraph.zeropad(d.getHours()) + ":" +
           Dygraph.zeropad(d.getMinutes()) + ":" +
           Dygraph.zeropad(d.getSeconds()) + "." +
           Dygraph.zeropad(d.getMilliseconds());
  }
'

# ==== 5) Dygraphs Plugins =====================================================

# Plugin: reset zoom button for a dygraph.
dyUnzoom <- function(dygraph) {
  dyPlugin(
    dygraph   = dygraph,
    name      = "Unzoom",
    path      = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}

# Plugin: crosshair for a dygraph (both axes or single axis).
dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph   = dygraph,
    name      = "Crosshair",
    path      = system.file("plugins/crosshair.js", package = "dygraphs"),
    options   = list(direction = match.arg(direction))
  )
}


# ==== 6) JavaScript Helpers for Sliders & PickerInputs ========================

# JavaScript code for log-scale sliders in Shiny using Ion.RangeSlider
JS.logify <- "
  function logifySlider (sliderId, sci = false) {
    $('#' + sliderId).data('ionRangeSlider').update({
      'prettify': function (num) {
        return (Math.round((Math.pow(10, num) + Number.EPSILON) * 1000) / 1000);
      }
    });
  }
"

JS.onload <- "
  $(document).ready(function() {
    setTimeout(function() {
      logifySlider('log_slider', sci = false);
    }, 5);
  });
"

# JavaScript for customizing PickerInputs (re-ordering, adding tips).
js.advice <- '
function customizePickerInput(id, order, tips) {
  if (order) {
    $("#" + id).on("change.bs.select loaded.bs.select", function(event) {
      $(this).find("option:selected").prependTo(this);
    });
  }
  if (tips) {
    $("#" + id).on("shown.bs.select", function() {
      var $lis = $($(this).data("selectpicker").selectpicker.current.elements);
      $lis.each(function(i) {
        $(this).attr("title", tips[i]);
      });
    });
  }
}

$(function() {
  customizePickerInput("optionsdateformat", true, [
    "Day of the month (decimal number)",
    "Month (abbreviated)",
    "Month (full name)",
    "Month (decimal number)",
    "Year (4 digit)",
    "Year (2 digit)"
  ]);

  customizePickerInput("optionsdatesep", true, [
    "Dash", "Dot", "Slash", "Comma", "Colon", "Space", "No separator"
  ]);

  customizePickerInput("optionstimeformat", true, [
    "Decimal hours (0-23)", "Decimal minutes (0-59)",
    "Decimal seconds (0-59)", "Infra-seconds (> 1Hz? e.g., 0.000)"
  ]);

  customizePickerInput("optionstimesep", true, [
    "Dash", "Dot", "Slash", "Comma", "Colon", "Space", "No separator"
  ]);
});
'

# ==== 7) Spatial / GPS Utility Functions =====================================

# Haversine distance (in meters) between two lat/lon points.
disty <- function(long1, lat1, long2, lat2) {
  # Convert degrees to radians.
  long1 <- long1 * pi / 180
  long2 <- long2 * pi / 180
  lat1  <- lat1  * pi / 180
  lat2  <- lat2  * pi / 180
  
  # Haversine formula
  a <- sin((lat2 - lat1) / 2)^2 +
    cos(lat1) * cos(lat2) *
    sin((long2 - long1) / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- 6378137 * c  # Earth radius ~6,378,137 m
  d
}

# Compute bearing between two lat/lon points (returns degrees, -180 to +180).
beary <- function(long1, lat1, long2, lat2) {
  # Convert degrees to radians.
  long1 <- long1 * pi / 180
  long2 <- long2 * pi / 180
  lat1  <- lat1  * pi / 180
  lat2  <- lat2  * pi / 180
  
  # Great-circle bearing
  a <- sin(long2 - long1) * cos(lat2)
  b <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(long2 - long1)
  c <- (atan2(a, b) / pi) * 180
  c  # degrees in [-180, +180]
}

# Helper to apply a separator between multiple components (used for date/time).
apply_separator <- function(components, sepr) {
  if (length(components) > 1) {
    paste(components, collapse = sepr)
  } else {
    components
  }
}

# Example function that shows a warning if Timestamps are not correct.
my_long_func13 <- function(){
  warning("Something is wrong with the Timestamp computed - Ensure correct format.")
}

# ==== 8) C++ Time-Step Interval Standardization ===============================

# Compiled C++ function for subsetting time series data by a fixed step size.
cppFunction('
IntegerVector getTimeSteppedIndicesC(NumericVector ctimes,
                                     double step,
                                     bool keepFirst = true,
                                     bool keepLast  = true) {
 int n = ctimes.size();
  if(n < 2) {
    IntegerVector trivial(n);
    for(int i = 0; i < n; i++) {
      trivial[i] = i + 1; // 1-based indexing
    }
    return trivial;
  }
  
  std::vector<int> retained;
  int currentIndex = 0;  // start at the first fix
  if(keepFirst) {
    retained.push_back(0); // using 0-based indices here; well add 1 later.
            }
double current_time = ctimes[currentIndex];

while(true) {
  double target = current_time + step;
  int j = currentIndex + 1;
  
  // Advance j until we reach a time that is >= target, or run off the end.
  while(j < n && ctimes[j] < target) {
    j++;
  }
  
  if(j >= n) break; // no more points available
  
  // j is now the first index with time >= target.
  // Check whether the point before j (if available) is closer to target.
  int candidate;
  if(j - 1 == currentIndex) {
    candidate = j;  // no choice, we must pick j.
  } else {
    double diff_prev = target - ctimes[j - 1];
    double diff_j    = ctimes[j] - target;
    if(diff_prev <= diff_j) {
      candidate = j - 1;
    } else {
      candidate = j;
    }
  }
  
  retained.push_back(candidate);
  // Reset the clock from the newly retained fix:
    current_time = ctimes[candidate];
  currentIndex = candidate;
}

// Optionally keep the last fix if not already retained.
if(keepLast && retained.empty() == false && retained.back() != n - 1) {
  retained.push_back(n - 1);
}

// Sort and remove duplicates (in case the same fix was selected more than once)
std::sort(retained.begin(), retained.end());
retained.erase(std::unique(retained.begin(), retained.end()), retained.end());

// Convert to 1-based indexing for R.
IntegerVector out(retained.size());
for (size_t i = 0; i < retained.size(); i++) {
  out[i] = retained[i] + 1;
}
return out;
}
')

# Wrapper function in R that calls the compiled function above.
getTimeSteppedDataFast <- function(df,
                                   stepSeconds = 60,
                                   latCol = "GPS_Latitude",
                                   lonCol = "GPS_Longitude",
                                   keepFirst = TRUE,
                                   keepLast  = TRUE) {
  # Filter out rows with missing coords and sort by Timestamp
  df <- df %>%
    dplyr::filter(!is.na(.data[[lonCol]]) & !is.na(.data[[latCol]])) %>%
    dplyr::arrange(Timestamp)
  
  if (nrow(df) < 2) return(df)
  
  # Convert Timestamps to numeric offset from the first fix
  firstT  <- df$Timestamp[1]
  ctimes  <- as.numeric(df$Timestamp - firstT)
  
  # Get indices from the C++ function
  idx_retained <- getTimeSteppedIndicesC(ctimes, stepSeconds, keepFirst, keepLast)
  
  # Subset original data
  df_stepped <- df[idx_retained, , drop = FALSE]
  df_stepped
}

# ==== 9) Point clicker handling & Speed Calculation =============================

# Example functions used in user threshold expression building, or to
# calculate speed from lat/lon/time difference, etc.

calc_speed <- function(lon, lat, time) {
  dist_vec <- c(
    NA,
    disty(
      lon[-1], lat[-1],
      lon[-length(lon)], lat[-length(lat)]
    )
  )
  dt <- c(NA, diff(as.numeric(time)))
  dist_vec / dt  # speed in m/s
}

# Extend polyline leaflet options (used in "draw" features).
mydrawPolylineOptions <- function(
    allowIntersection = TRUE,
    drawError = list(color = "#b00b00", timeout = 2500),
    guidelineDistance = 20,
    metric = TRUE,
    feet   = FALSE,
    zIndexOffset = 2000,
    shapeOptions = drawShapeOptions(fill = FALSE),
    repeatMode   = FALSE
) {
  leaflet::filterNULL(list(
    allowIntersection = allowIntersection,
    drawError         = drawError,
    guidelineDistance = guidelineDistance,
    metric            = metric,
    feet              = feet,
    zIndexOffset      = zIndexOffset,
    shapeOptions      = shapeOptions,
    repeatMode        = repeatMode
  ))
}

# ==== 10) Track Smoothing Calculation =============================

# Wrapper for rolling medians to handle NAs
apply_median_smoothing <- function(x, window, subset) {
  # x: numeric vector to smooth.
  # window: smoothing window (must be an odd number).
  # subset: logical vector (same length as x) indicating which positions should be smoothed.
  
  if (window %% 2 != 1) {
    stop("The 'window' parameter must be an odd number.")
  }
  if (length(x) != length(subset)) {
    stop("Length of x and subset must be the same.")
  }
  
  # Identify the indices where both the subset condition is TRUE and x is not NA.
  idx <- which(subset & !is.na(x))
  if (length(idx) < window) return(x)  # Not enough eligible values
  
  # Extract the eligible (non-NA) values as a contiguous vector.
  y <- x[idx]
  
  # Apply the running median to y.
  y_smoothed <- runmed(y, k = window, endrule = "keep")
  
  # Create a copy of x and replace the eligible positions with the smoothed values.
  x_smoothed <- x
  x_smoothed[idx] <- y_smoothed
  
  return(x_smoothed)
}

# Wrapper for rolling medians for 1 Hz data
apply_median_smoothing_1Hz <- function(x, window, subset) {
  # x: numeric vector to smooth.
  # window: smoothing window (must be odd).
  # subset: logical vector (same length as x) indicating which values to smooth.
  
  if (window %% 2 != 1) {
    stop("The 'window' parameter must be an odd number.")
  }
  if (length(x) != length(subset)) {
    stop("Length of x and subset must be the same.")
  }
  
  result <- x  # start with original vector
  
  # Use rle on the subset indicator to find contiguous segments where subset == TRUE.
  r <- rle(subset)
  lengths <- r$lengths
  values  <- r$values
  ends    <- cumsum(lengths)
  starts  <- c(1, head(ends, -1) + 1)
  
  # For each contiguous segment where the value is TRUE, apply the median filter.
  for (i in seq_along(values)) {
    if (values[i]) {
      seg_idx <- seq(from = starts[i], to = ends[i])
      # Only smooth if the segment length is at least the window size.
      if (length(seg_idx) >= window) {
        seg_x <- x[seg_idx]
        seg_smoothed <- runmed(seg_x, k = window, endrule = "keep")
        result[seg_idx] <- seg_smoothed
      } else {
        result[seg_idx] <- x[seg_idx]
      }
    }
  }
  return(result)
}

# Wrapper for sgolayfilt to handle NAs
apply_sgolay_smoothing <- function(x, p, n, subset) {
  # x: numeric vector to smooth.
  # p: polynomial order.
  # n: window length (must be odd).
  # subset: logical vector (same length as x) indicating which positions should be smoothed.
  
  if (n %% 2 != 1) {
    stop("The window length 'n' must be an odd number.")
  }
  if (length(x) != length(subset)) {
    stop("Length of x and subset must be the same.")
  }
  
  # Identify the indices eligible for smoothing.
  idx <- which(subset & !is.na(x))
  if (length(idx) < n) return(x)  # Not enough eligible values
  
  # Extract the eligible values.
  y <- x[idx]
  
  # Apply the Savitzky-Golay filter on y.
  y_smoothed <- signal::sgolayfilt(y, p = p, n = n)
  
  # Replace the eligible positions in the original vector with the smoothed values.
  x_smoothed <- x
  x_smoothed[idx] <- y_smoothed
  
  return(x_smoothed)
}

# Wrapper for sgolayfilt for 1 Hz data
apply_sgolay_smoothing_1Hz <- function(x, p, n, subset) {
  # x: numeric vector to smooth.
  # p: polynomial order.
  # n: window length (must be odd).
  # subset: logical vector (same length as x) indicating which positions should be smoothed.
  
  if (n %% 2 != 1) {
    stop("The window length 'n' must be an odd number.")
  }
  if (length(x) != length(subset)) {
    stop("Length of x and subset must be the same.")
  }
  
  result <- x  # start with the original vector
  
  # Use rle on the subset indicator to determine contiguous segments to smooth.
  r <- rle(subset)
  seg_lengths <- r$lengths
  seg_values  <- r$values
  seg_ends    <- cumsum(seg_lengths)
  seg_starts  <- c(1, head(seg_ends, -1) + 1)
  
  # For each contiguous segment where subset is TRUE, apply SG smoothing.
  for (i in seq_along(seg_values)) {
    if (seg_values[i]) {
      seg_idx <- seq(from = seg_starts[i], to = seg_ends[i])
      # Only smooth if the segment has at least n eligible points.
      if (length(seg_idx) >= n) {
        seg_x <- x[seg_idx]
        seg_smoothed <- signal::sgolayfilt(seg_x, p = p, n = n)
        result[seg_idx] <- seg_smoothed
      } else {
        # Optionally, you could choose to leave the short segment unchanged:
        result[seg_idx] <- x[seg_idx]
      }
    }
  }
  
  return(result)
}

# Kalman filter
apply_kalman_filter_complex <- function(data,
                                        procNoise,         # base process noise (scalar)
                                        measNoise_pos,     # measurement noise for position (scalar)
                                        measNoise_speed,   # measurement noise for speed (scalar)
                                        KF_scaleFactor,    # scaling factor: factor by which VeDBA increases process noise and also used to convert mean_VeDBA_interval to a speed measurement
                                        initialCov = diag(1, 4),  # initial covariance matrix (4x4)
                                        useVeDBA = FALSE) {
  # data: a data frame sorted by time (if not, we sort it below) that must have at least:
  #   Timestamp, GPS_Longitude_Filtered, GPS_Latitude_Filtered.
  # If useVeDBA==TRUE, it should also have mean_VeDBA_interval.
  #
  # The state vector is defined as:
  #    x = [longitude, latitude, v_long, v_lat]^T.
  #
  # The dynamics assume a constant-velocity model.
  #
  # For measurements:
  #   If useVeDBA is FALSE (or if mean_VeDBA_interval is NA), we use:
  #     z = [longitude, latitude]^T   with H = [1 0 0 0; 0 1 0 0].
  #   Otherwise, we use:
  #     z = [longitude, latitude, speed]^T,
  #     where speed is computed as KF_scaleFactor * mean_VeDBA_interval.
  #     In that case the measurement function is:
  #         h(x) = [x[1], x[2], sqrt(v_long^2 + v_lat^2)]^T,
  #     and its Jacobian is computed accordingly.
  #
  # The function also handles missing measurements by performing only a prediction.
  
  # Sort data in time (if not already)
  data <- data[order(data$Timestamp), ]
  n <- nrow(data)
  if(n < 1) return(data)
  
  # Convert timestamps to seconds relative to the first measurement.
  times <- as.numeric(difftime(data$Timestamp, data$Timestamp[1], units = "secs"))
  
  # Find the first row that has valid position (for initialization).
  first_valid <- which(!is.na(data$GPS_Longitude_Filtered) & !is.na(data$GPS_Latitude_Filtered))[1]
  if (is.na(first_valid)) stop("No valid GPS fix found for filter initialization.")
  
  # Initialize the state vector using the first valid fix and assume zero velocity.
  x <- c(data$GPS_Longitude_Filtered[first_valid],
         data$GPS_Latitude_Filtered[first_valid],
         0, 0)
  
  # Set the initial state covariance.
  P <- initialCov
  
  # Prepare a matrix to hold the filtered state at each step.
  x_filtered <- matrix(NA, nrow = n, ncol = 4)
  x_filtered[first_valid, ] <- x
  
  # Loop over time steps starting from the row after first_valid.
  for (i in (first_valid+1):n) {
    dt <- times[i] - times[i-1]
    if (dt <= 0) dt <- 1e-3  # avoid zero dt
    
    # --- Prediction step ---
    F <- matrix(c(1, 0, dt, 0,
                  0, 1, 0, dt,
                  0, 0, 1,  0,
                  0, 0, 0,  1), nrow = 4, byrow = TRUE)
    
    # Process noise covariance from a constant-acceleration model:
    Q_base <- matrix(c((dt^4)/4, 0, (dt^3)/2, 0,
                       0, (dt^4)/4, 0, (dt^3)/2,
                       (dt^3)/2, 0, dt^2, 0,
                       0, (dt^3)/2, 0, dt^2), nrow = 4, byrow = TRUE)
    
    # If using VeDBA, scale process noise by a factor computed from mean_VeDBA_interval.
    if (useVeDBA && "mean_VeDBA_interval" %in% names(data)) {
      v_val <- data$mean_VeDBA_interval[i]
      if (is.na(v_val)) {
        factor <- 1
      } else {
        # For example, above a threshold the process noise increases linearly.
        threshold <- 0.1
        factor <- ifelse(v_val > threshold, 1 + KF_scaleFactor * (v_val - threshold), 1)
      }
    } else {
      factor <- 1
    }
    Q <- procNoise * factor * Q_base
    
    # Predicted state and covariance.
    x_pred <- F %*% x
    P_pred <- F %*% P %*% t(F) + Q
    
    # --- Measurement update ---
    pos_available <- !is.na(data$GPS_Longitude_Filtered[i]) && !is.na(data$GPS_Latitude_Filtered[i])
    
    if (!pos_available) {
      # No measurement available at this time: only perform prediction.
      x <- x_pred
      P <- P_pred
    } else {
      # Construct the measurement vector.
      z_pos <- c(data$GPS_Longitude_Filtered[i],
                 data$GPS_Latitude_Filtered[i])
      
      if (!useVeDBA || is.na(data$mean_VeDBA_interval[i])) {
        # Use only position measurement.
        z <- z_pos
        H <- matrix(c(1, 0, 0, 0,
                      0, 1, 0, 0), nrow = 2, byrow = TRUE)
        R <- diag(c(measNoise_pos, measNoise_pos))
        
        y <- z - H %*% x_pred
        S <- H %*% P_pred %*% t(H) + R
        K <- P_pred %*% t(H) %*% solve(S)
        
        x <- x_pred + K %*% y
        P <- (diag(4) - K %*% H) %*% P_pred
      } else {
        # Use extended measurement update with speed.
        # Measured speed is computed as KF_scaleFactor * mean_VeDBA_interval.
        z_speed <- KF_scaleFactor * data$mean_VeDBA_interval[i]
        z <- c(z_pos, z_speed)
        
        # Define nonlinear measurement function h(x) = [position; speed] with speed = sqrt(v_long^2 + v_lat^2).
        h <- function(x) {
          pos <- x[1:2]
          vel <- x[3:4]
          speed <- sqrt(sum(vel^2))
          c(pos, speed)
        }
        h_x_pred <- h(x_pred)
        
        # Jacobian of h at x_pred.
        speed_pred <- sqrt(x_pred[3]^2 + x_pred[4]^2)
        if (speed_pred < 1e-6) speed_pred <- 1e-6  # avoid division by zero
        H_nl <- matrix(c(1, 0, 0, 0,
                         0, 1, 0, 0,
                         0, 0, x_pred[3]/speed_pred, x_pred[4]/speed_pred),
                       nrow = 3, byrow = TRUE)
        
        # Use separate measurement noise for speed.
        R <- diag(c(measNoise_pos, measNoise_pos, measNoise_speed))
        
        y <- z - h_x_pred
        S <- H_nl %*% P_pred %*% t(H_nl) + R
        K <- P_pred %*% t(H_nl) %*% solve(S)
        
        x <- x_pred + K %*% y
        P <- (diag(4) - K %*% H_nl) %*% P_pred
      }
    }
    
    x_filtered[i, ] <- x
  }
  
  # Now, update the data frame.
  # For each row, if the original measurement was available (i.e. non-NA),
  # then replace the filtered fix with the filtered state; otherwise leave as NA.
  data$GPS_Longitude_Filtered <- ifelse(!is.na(data$GPS_Longitude_Filtered),
                                        x_filtered[, 1],
                                        NA)
  data$GPS_Latitude_Filtered  <- ifelse(!is.na(data$GPS_Latitude_Filtered),
                                        x_filtered[, 2],
                                        NA)
  
  return(data)
}

# C++ rolling median
optimized_roll_median <- function(x, window, fill = NA, align = "center") {
  # RcppRoll::roll_median returns a vector of the same length as x.
  RcppRoll::roll_median(x, n = window, fill = fill, align = align)
}

# Generating Grid Coordinates
ji <- function(xy, origin = c(0,0), cellsize = c(20, 20)) {
  # xy is an Nx2 matrix of coordinates
  # cellsize is something like c(20,20) for 20m
  # origin typically c(0,0), or you can let origin = c(minx, miny)
  # to anchor the grid at a known corner.
  
  t(apply(xy, 1, function(z) {
    floor((z - origin) / cellsize) * cellsize + (cellsize/2 + origin)
  }))
}

################################################################################
##                             SHINY APP START                                 ##
################################################################################

# ==== 10) Launch Shiny App ====================================================

shinyApp(
  ui = navbarPage(
    strong("WildPulse EcoMove Analytics"),
    tabPanel("Home", uiOutput("page1"), icon = icon("home")),
    tabPanel("Data Import", uiOutput("page2"), icon = icon("file-import")),
    tabPanel("GPS Processing", uiOutput("page3"), icon = icon("cogs")),
    tabPanel("GPS Analysis", uiOutput("page4"), icon = icon("magnifying-glass-chart")),
    tabPanel("Data Export", uiOutput("page5"), icon = icon("download")),
    tabPanel("User Notes", uiOutput("page6"), icon = icon("folder-open")),
    #tabPanel("App Manual", uiOutput("page7"), icon = icon("info")),
    
    # Apply a Bootstrap theme
    theme = shinythemes::shinytheme("cerulean")  # 'cerulean' bootstrap theme
  ),
  
  # ==== B) The Server: Render Each Page's UI Elements ========================
  shinyServer(function(input, output, session) {
    
    
    # -------------------------------------------------------------------------
    # 1) HOME PAGE
    # -------------------------------------------------------------------------
    output$page1 <- renderUI({
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            # Logo
            fluidRow(
              column(
                width = 12, align = "center",
                img(
                  src    = "WILDPULSE.png", 
                  height = 350, width = "100%"
                )
              )
            ),
            
            # About the app
            h2("About this app"),
            p("This app acts as an R interface to pre-process, analyze, and visualize GPS datasets."),
            br(), 
            
            # RStudio mention
            img(src = "rstudio.png", height = 60, width = 180),
            br(),
            "Shiny is a product of ",
            span("RStudio", style = "color:blue"),
            hr(),
            
            # Collaborating institutions
            h3("Collaborating Institutions:", align = "left"),
            fluidRow(
              column(
                width = 12, align = "left",
                img(height = 80, width = "55%",  src = "Swansea lab for animal movement.png"),
                br(), br(),
                img(height = 70, width = "100%", src = "Max Plank Institute for animal behaviour.png")
              )
            ),
            hr(),

            # README link
            #h5(
            #  "Click ",
             # a("README", target = "_blank", href = "README.pdf"),
             # " for information on input data formats."
            #),
           # br(),
            
            # Current time display
            h4(textOutput("currentTime"))
          ),
          
          mainPanel(
            # Features
            # About and Features Section
            fluidRow(
              column(12,
                     wellPanel(
                       h1("Welcome to WildPulse EcoMove Analytics", align = "center"),
                       p("This app is a comprehensive solution for wildlife tracking and movement ecology. "
                         ,"It integrates advanced data import, preprocessing, and interactive visualizations to pre-process and gain early insights from GPS datasets."),
                       h3("Key Features"),
                       tags$ul(
                         tags$li("Seamless Data Import: Load CSV, RDS, and FST files with automated column mapping."),
                         tags$li("Dynamic Preprocessing: Adjust time zones, filter data, and rediscretize GPS tracks to create consistent intervals."),
                         tags$li("Interactive Visualizations: Explore your data with responsive dygraphs, plotly charts, and high-performance leaflet maps (with WebGL options)."),
                         tags$li("Advanced Analytics: Use custom thresholds, expression builders, and isolation forest anomaly detection to flag GPS anomalies. Also integrate VeDBA with GPS for anomaly detection"),
                         tags$li("Reproducible Workflow: Document your analysis using built-in R console and export/import functionalities for scripts and notes.")
                       ),
                       p("Start by using the quick start buttons or navigating to the appropriate tabs above. Enjoy a streamlined, interactive experience that accelerates your research workflow!")
                     )
              )
            ),
            
            br(),
            # Two columns: image + video
            fluidRow(
              column(
                width = 6,
                br(), br(), br(),
                img(src = "DR track tropicbird.2.png", height = 350, width = 500)
              ),
              column(
                width = 6,
                tags$video(
                  id        = "Oryx.movements.mp4",
                  type      = "video/mp4",
                  src       = "Oryx.movements.mp4",
                  autoplay  = TRUE,
                  controls  = TRUE,
                  width     = "420px",
                  height    = "420px"
                )
              )
            ),
            
            # Footer with developer info and current time
            fluidRow(
              column(12,
                     hr(),
                     div(style = "text-align:center; font-size:12px; color:gray;",
                         "Developed by Richard Micheal Gunner | ",
                         HTML("Email: <a href='mailto:rgunner@ab.mpg.de'>rgunner@ab.mpg.de</a> | "),
                         HTML("ResearchGate: <a href='https://www.researchgate.net/profile/Richard-Gunner-2' target='_blank'>Richard Gunner</a> | "),
                         HTML("ORCID: <a href='https://orcid.org/0000-0002-2054-9944' target='_blank'>0000-0002-2054-9944</a>")
                     ),
                     h4(textOutput("currentTime"), style = "text-align:center;")
              )
            ),
          )
        )
      )
    })
    
    # Show current time in home page
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      paste("Current time:", Sys.time())
    })
    
    # -------------------------------------------------------------------------
    # 2) DATA IMPORT / DOWNLOAD PAGE
    # -------------------------------------------------------------------------
    
    output$page2 <- renderUI({
      fluidPage(
        # Title
        titlePanel(
          h1(
            strong("Import data"), 
            align = "center", 
            style = "background-color:lightgreen; color:black; font-size:30px;"
          )
        ),
        
        # Loading message
        tags$style(type="text/css", "
          #loadmessage {
            position: fluid;
            top: 0px;
            left: 0px;
            width: 100%;
            padding: 5px 0px 5px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            background-color: #CCFF66;
            z-index: 60;
          }
        "),
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id="loadmessage")
        ),
        
        # Table font sizes
        tags$style(HTML("
          .dataTables_wrapper .dataTables_length,
          .dataTables_wrapper .dataTables_filter,
          .dataTables_wrapper .dataTables_info,
          .dataTables_wrapper .dataTables_paginate,
          table.dataTable tr {
            font-size: 8pt !important;
          }
        ")),
        
        sidebarLayout(
          # --- Left Sidebar ---
          sidebarPanel(
            width = 3,
            h3("Load data", align = "center"),
            
            # File input
            fileInput(
              "file1", "Choose File",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv", ".rds", ".fst"
              )
            ),
            
            # CSV parsing controls
            fluidRow(
              column(
                width = 4,
                radioButtons("sep", "Separator",
                             choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                             selected = ",")
              ),
              column(
                width = 4,
                radioButtons("quote", "Quote",
                             choices = c("None" = "", "Double" = '"', "Single" = "'"),
                             selected = '"')
              ),
              column(
                width = 4,
                checkboxInput("header", "Header", TRUE)
              )
            ),
            
            # Meta-data for study
            textInput("words",    strong("Study Name/Location"),   value = "Enter Study Name/Location"),
            textInput("id",       strong("ID"),                    value = "Enter Individual Name"),
            textInput("species",  strong("Species"),               value = "Enter Species Name"),
            
            actionButton("submit_new_word", "Save User Written Inputs",
                         style = "margin-top:10px; font-size:110%;", align = "center"),
            br(),
            
            # Additional selections
            selectInput("gender", "Gender:",
                        choices = c("Male", "Female", "Unknown", "Unspecified"),
                        selected = "Unspecified"),
            selectInput("deployment_num", "Deployment Number:",
                        choices = 1:10, selected = 1),
            br(),
            
            # JavaScript enhancements
            tags$head(tags$script(HTML(js.advice))),
            
            # Date/Time format pickers
            fluidRow(
              column(
                width = 6,
                pickerInput(
                  inputId = "optionsdateformat",
                  label   = "Date format",
                  choices = c("%Y", "%y", "%B", "%m", "%d", "%b"),
                  multiple= TRUE,
                  options = list(`live-search` = TRUE),
                  selected= c("%Y", "%m", "%d")
                )
              ),
              column(
                width = 6,
                pickerInput(
                  inputId = "optionsdatesep",
                  label   = "Date format separator",
                  choices = c('-', '.', '/', ',', ':', ' ', ''),
                  options = list(`live-search` = TRUE, multiple = FALSE),
                  selected= '-'
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                pickerInput(
                  inputId = "optionstimeformat",
                  label   = "Time format",
                  choices = c("%H", "%M", "%S", "%OS"),
                  multiple= TRUE,
                  options = list(`live-search` = TRUE),
                  selected= c("%H", "%M", "%OS")
                )
              ),
              column(
                width = 6,
                pickerInput(
                  inputId = "optionstimesep",
                  label   = "Time format separator",
                  choices = c('-', '.', '/', ',', ':', ' ', ''),
                  options = list(`live-search` = TRUE, multiple = FALSE),
                  selected= ':'
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                wellPanel(
                  h4("Selected Timestamp format", align = "center"),
                  textOutput("selectedOptions")
                )
              ),
              column(
                width = 6,
                wellPanel(
                  h4("Resultant Timestamp format", align = "center"),
                  textOutput("datetimeexample")
                )
              )
            ),
            
            # Table display options
            h3("Table options", align = "center"),
            fluidRow(
              column(
                width = 6,
                numericInput("num.digits", "Number of Digits", value = 5, min = 0, max = 10)
              ),
              column(
                width = 6,
                radioButtons("fixedWidth", "Table format",
                             choices = c("Wide format", "Long format"),
                             selected = "Long format")
              )
            ),
            fluidRow(
              column(
                width = 8,
                selectInput("tableHeight", "Table Height (Only for 'Long format')",
                            choices  = c("200px", "400px", "600px", "800px", "1000px",
                                         "1200px", "1400px", "1600px", "1800px", "2000px"),
                            selected = "600px")
              ),
              column(
                width = 4,
                checkboxInput("scroller", "Long format scroller?", value = TRUE)
              )
            ),
            radioButtons("disp", "Display",
                         choices  = c("All" = "all", "Undersample" = "undersample"),
                         selected = "all"),
            numericInput("unders", "Undersample every 'n' rows to view:", value = 400),
            
            # Short instructions
            h3(strong("To do:"), align = "center", style = "color: darkblue; font-size:30px;"),
            p("(1) Import your data file via the Browse button. Accepted formats: .csv, .txt, .fst, .rds."),
            p("(2) Provide relevant metadata above, then click 'Save User Written Inputs'."),
            p("(3) Assign column names in the 'Variable Selection' tab. If no relevant variable, use 'Non-applicable'."),
            p("(4) Choose the correct sequence (and separators) for your 'Timestamp' column."),
            p("(5) Click 'Apply' to load data. You can then adjust timezones and subsets, then proceed to 'Data Processing'.")
          ),
          
          # --- Right Main Panel ---
          mainPanel(
            actionButton("do", strong("Apply"), 
                         style = "width:100%; text-align:center; font-size:18px;"),
            
            tabsetPanel(
              id   = "tabsetPanelID",
              type = "tabs",
              
              # Sub-Tab: (1) Imported data
              tabPanel(
                "(1) Imported data",
                tabsetPanel(
                  # Sub-sub-tab: Variable Selection
                  tabPanel(
                    "Variable Selection",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h3("Variable Selection"),
                          uiOutput("Timestamp"),
                          uiOutput("GPS_Longitude"),
                          uiOutput("GPS_Latitude"),
                          uiOutput("IMU_Mean_VeDBA"),
                          uiOutput("Marked_Events"),
                          tags$style(type='text/css',
                                     ".selectize-input { font-size:14px; line-height:20px;}
                                      .selectize-dropdown { font-size:16px; line-height:12px; }"
                          ),
                          tags$head(
                            tags$style(HTML("label { font-size:100%; margin-bottom:2px;}"))
                          )
                        ),
                        mainPanel(
                          width = 9,
                          h4("Raw Data"),
                          withSpinner(DT::dataTableOutput("contents"))
                        )
                      )
                    )
                  ),
                  
                  # Sub-sub-tab: Summary
                  tabPanel(
                    "Summary",
                    tags$div(
                      style = "text-align:center;",
                      h4(strong("Descriptive Stats"), style = "color:#555; font-size:14px;")
                    ),
                    verbatimTextOutput("summary")
                  )
                )
              ),
              
              # Sub-Tab: (2) Convert Timezones / Subset Data
              tabPanel(
                "(2) Convert Timezones / Subset Data",
                tabsetPanel(
                  # Sub-sub-tab: Filter Data
                  tabPanel(
                    "Filter Data",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("Convert Timezones"),
                        numericInput("timezone_offset", "Timezone Offset (hours):", value = 0, min = -24, max = 24, step = 1),
                        actionButton("apply_timezone", "Convert Timezone", style = "margin-top:10px; font-size:110%;"),
                        br(),
                        p("Enter offset (e.g. -5 for UTC-5) to convert GPS time to local time."),
                        
                        # Subset data by time
                        h3("Subset Data by Timestamp"),
                        dateInput("exclude_date_start", "Exclude From (Date):", value = Sys.Date()),
                        timeInput("exclude_time_start", "Exclude From (Time):", value = strptime("00:00:00", "%H:%M:%S")),
                        dateInput("exclude_date_end",   "Exclude To (Date):",   value = Sys.Date()),
                        timeInput("exclude_time_end",   "Exclude To (Time):",   value = strptime("23:59:59", "%H:%M:%S")),
                        
                        actionButton("add_exclusion",   "Add Exclusion Period", style = "margin-top:10px; font-size:110%;"),
                        h4("Exclusion Periods"),
                        DT::dataTableOutput("exclusion_table"),
                        
                        actionButton("apply_exclusion", "Apply Exclusions", style = "margin-top:10px; font-size:110%;"),
                        actionButton("reset_data",      "Reset Data",        style = "margin-top:10px; font-size:110%; color:red;")
                      ),
                      mainPanel(
                        h4("Filtered Data"),
                        withSpinner(DT::dataTableOutput("filtered_data"))
                      )
                    )
                  ),
                  
                  # Sub-sub-tab: Summary
                  tabPanel(
                    "Summary",
                    tags$div(
                      style = "text-align:center;",
                      h4(strong("Descriptive Stats"), style = "color:#555; font-size:14px;")
                    ),
                    verbatimTextOutput("summary.filt")
                  )
                )
              ),
              
              # Sub-Tab: (3) Inspection Plots
              tabPanel(
                "(3) Inspection Plots",
                tabsetPanel(
                  # Sub-sub-tab: GPS and IMU Sampling Rates
                  tabPanel(
                    "GPS and IMU Sampling Rates",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("Sampling Rate Plots"),
                        checkboxGroupInput("plot_components", "Select Data to Plot:",
                                           choices = c("GPS Fixes" = "gps", "IMU Bursts" = "imu"),
                                           selected = c("gps", "imu")),
                        checkboxInput("normalize", "Normalize Time Differences", value = FALSE)
                      ),
                      mainPanel(
                        dygraphOutput("sampling_rate_dygraph", height = "600px") %>% withSpinner(),
                        plotlyOutput("timeDiffPlots", height = "600px")          %>% withSpinner(),
                        plotlyOutput("burstDurationPlots", height = "600px")     %>% withSpinner()
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    })
    
    ###############################################################################
    ###############################################################################
    ###############################################################################
    ##          MAIN REACTIVE VALUES, DATA FRAMES, AND TABLE OPTIONS             ##
    ###############################################################################
    
    # === A) Reactive Values ===
    v <- reactiveValues()
    
    # For storing selected columns (not used frequently, but kept for reference)
    selectedColumns <- reactiveVal(NULL)
    
    # Font size used in data tables (if relevant)
    font.size <- "10pt"
    
    # === B) Data Table Format Options ===
    # These reactiveValues store DT options (wide vs. long format, with or without scroller).
    
    ## 1) Wide format
    rv1 <- reactiveValues(
      options = list(
        stateSave       = TRUE,
        orderClasses    = TRUE,
        stateDuration   = -1,
        lengthMenu      = c(20, 50, 100, 250, 500, 1000, 2300, 5000, 10000),
        colReorder      = TRUE,
        search          = list(regex = TRUE),
        fixedHeader     = TRUE,
        searching       = TRUE,
        autoWidth       = TRUE,
        class           = 'cell-border stripe',
        rownames        = FALSE,
        fixedColumns    = list(leftColumns = 1, rightColumns = 0)
      )
    )
    
    ## 2) Long format (standard scrolling)
    rv2 <- reactiveValues(
      options = list(
        stateSave       = TRUE,
        orderClasses    = TRUE,
        stateDuration   = -1,
        lengthMenu      = c(20, 50, 100, 250, 500, 1000, 2300, 5000, 10000),
        colReorder      = TRUE,
        search          = list(regex = TRUE),
        fixedHeader     = FALSE,
        scrollY         = "600px",
        searching       = TRUE,
        scrollX         = TRUE,
        class           = 'cell-border stripe',
        rownames        = FALSE,
        scroller        = FALSE,
        fixedColumns    = list(leftColumns = 1, rightColumns = 0)
      )
    )
    
    ## 3) Long format with infinite scroller
    rv3 <- reactiveValues(
      options = list(
        stateSave       = TRUE,
        orderClasses    = TRUE,
        stateDuration   = -1,
        colReorder      = TRUE,
        search          = list(regex = TRUE),
        fixedHeader     = FALSE,
        scrollY         = "600px",
        searching       = TRUE,
        scrollX         = TRUE,
        class           = 'cell-border stripe',
        rownames        = FALSE,
        scroller        = TRUE,
        fixedColumns    = list(leftColumns = 1, rightColumns = 0)
      )
    )
    
    # === C) Other Reactive Values ===
    
    # For storing the chosen date/time format
    resultdatetime <- reactiveVal(NULL)
    
    # For storing the data’s column names
    columnNames <- reactiveVal(NULL)
    
    # For user notes or text input from the user
    user_notes <- reactiveVal("")
    
    # For storing basic user input
    v$words_submitted <- ""  # e.g. study name
    v$species         <- ""  # e.g. species name
    v$id              <- ""  # e.g. individual ID
    
    # Check that data is loaded properly
    v$data.chk <- FALSE
    
    # For sync issues with numeric/time inputs
    v$now  <- Sys.time()
    v$when <- Sys.time()
    
    # Table digit precision
    v$num.digits <- 5
    
    # GPS map reactive values (line width, point size, scale bar)
    v$GPS_LW       <- 1   # Raw GPS
    v$GPS_PS       <- 2.2 # Raw GPS
    v$GPS_PS_proc  <- 2.2 # Processed GPS
    v$GPS_LW_proc  <- 1   # Processed GPS
    v$scalebarwidth<- 200 # Scale bar width
    
    # Exclusion periods for filtering data by timestamp
    exclusions <- reactiveVal(
      data.frame(Start = character(), End = character(), stringsAsFactors = FALSE)
    )
    
    # Original timestamp reference
    v$original_timestamp <- NULL
    v$orig_time_offset   <- 0  # track time offset changes (e.g. from timezone updates)
    
    # User thresholds for GPS filtering
    v$burstLength     <- 1
    v$fixDropout      <- 3600
    v$StandTimeInt    <- 240
    v$Anglespeed      <- 5
    v$Anglethresh     <- 150
    v$GPSaccuracy     <- 25
    v$IFconf          <- 0.99
    v$isosamplesize   <- 256
    
    options(digits.secs = 3)
    
    # Post filter track smoothing
    v$Med_window <- 3
    v$SG_window <- 5
    v$SG_order <- 3
    v$gpsFilteredOriginal <- NULL # Store original GPS Processed filtered fixes
    
    # Grid cell residency time summation parameters
    v$dropoutThreshold <- 3600
    v$residGridSize <- 50 
    v$maxTimeCap <- 3600
    v$maxVistsCap <- 200
    v$maxSpeedCap <- 1
    v$maxVeDCap <- 1
    
    # Heat map parameters
    v$blur <- 5
    v$maxPI <- 5 
    v$intensity <- 5 
    v$radius <- 15
    # KD heat map parameters
    v$gridsize <- 50
    v$opacity_kd <- 0.8
    # KD expontnetial time weighted 
    v$gridsizeKDW <- 0.001
    v$maxTimeCapKDW <- 3600
 
    # === D) Main Data Reactives ===
    
    # DF        : the main “renamed / reorganized” data frame
    # filtered_data : the user-filtered data
    DF            <- reactiveVal(data.frame())
    filtered_data <- reactiveVal(NULL)
    
    # gpsProcessedData : The data after GPS/IMU processing
    gpsProcessedData <- reactiveVal(data.frame())
    
    isoForestResults  <- reactiveVal(NULL)
    userThreshResults <- reactiveVal(NULL)
    
    # For expression building
    currentExpression   <- reactiveVal("ifelse(")
    expressionHistory   <- reactiveVal(list("ifelse("))
    savedExpressions    <- reactiveVal(list())
    
    # For storing map bounding box
    mapCentered  <- reactiveVal(FALSE)
    boundsStored <- reactiveVal(NULL)
    filteredMapData <- reactiveVal(data.frame())
    mapCentered2  <- reactiveVal(FALSE)
    boundsStored2 <- reactiveVal(NULL)
    
    # Marker clicking in Leaflet
    selectedPointRow <- reactiveVal(NULL)
    
    # Manual verdict overrides table
    overrides <- reactiveVal(
      data.frame(
        Observations = numeric(0),
        Verdict_user = character(0),
        stringsAsFactors = FALSE
      )
    )
    
    # For VeDBA ~ GPS speed merges
    combinedData  <- reactiveVal(NULL)
    allSelectedObs<- reactiveVal(character(0))
    
    # GPS anlaysis data frame
    GPSanalysis <- reactiveVal(data.frame())  # starts empty
    
    # A helper reactive that stores your final “grid-based” SpatRasters for each metric
    gridRasters <- reactiveVal(NULL)
    
    ###############################################################################################################
    ###############################################################################################################
    
    ###############################################################################
    ##                  DATA IMPORT OBSERVERS & LOGIC                           ##
    ###############################################################################
    
    # myData() : raw loaded data from user’s file input
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      
      # Determine file type by extension
      file_ext <- tools::file_ext(inFile$name)
      
      # Load data accordingly
      if (file_ext == "csv") {
        df <- fread(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      } else if (file_ext == "rds") {
        df <- as.data.table(readRDS(inFile$datapath))
      } else if (file_ext == "fst") {
        df <- as.data.table(fst::read_fst(inFile$datapath))
      } else {
        showNotification("Unsupported file type.", type = "error")
        return(NULL)
      }
      
      df <- df %>% clean_names()
      df
    })
    
    
    # === E) Observers for user input text fields ===
    
    observeEvent(input$submit_new_word, {
      v$words_submitted <- input$words
      v$species         <- input$species
      v$id              <- input$id
      
      updateTextInput(session, "words",   value = v$words_submitted)
      updateTextInput(session, "species", value = v$species)
      updateTextInput(session, "id",      value = v$id)
    }, ignoreNULL = FALSE)
    
    
    # Validate num.digits
    observeEvent(input$num.digits, {
      i <- InputValidator$new()
      i$add_rule("num.digits", sv_required("Number must be provided"))
      i$add_rule("num.digits", sv_gte(0))
      i$add_rule("num.digits", sv_lte(10))
      i$enable()
      req(i$is_valid())
      
      v$num.digits <- input$num.digits
    }, ignoreNULL = FALSE)
    
    observeEvent(input$num.digits2, {
      i <- InputValidator$new()
      i$add_rule("num.digits2", sv_required("Number must be provided"))
      i$add_rule("num.digits2", sv_gte(0))
      i$add_rule("num.digits2", sv_lte(10))
      i$enable()
      req(i$is_valid())
      
      v$num.digits2 <- input$num.digits2
    }, ignoreNULL = FALSE)
    
    
    ###############################################################################
    ##     RENDERING THE “CONTENTS” DATA TABLE (Initial Imported Data)           ##
    ###############################################################################
    output$contents <- DT::renderDataTable({
    
      req(input$file1)
      
      # Decide which DT options to use
      if (input$fixedWidth == "Long format") {
        scroller <- input$scroller
        if (scroller) {
          datatable_options <- rv3$options
        } else {
          datatable_options <- rv2$options
        }
        datatable_options$scrollY <- input$tableHeight
      } else if (input$fixedWidth == "Wide format") {
        datatable_options <- rv1$options
      }
      
      # Show all data or undersample
      if (input$disp == "all") {
        DT::datatable(
          myData(), 
          options = datatable_options,
          selection = 'single',
          filter    = 'top',
          extensions= c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
          rownames  = FALSE
        ) %>%
          formatRound(
            columns = which(sapply(myData(), is.numeric)),
            digits  = v$num.digits
          ) %>%
          formatStyle(
            columns = 1:ncol(myData()),
            'text-align' = 'center'
          )
      } else if (input$disp == "undersample") {
        i <- InputValidator$new()
        i$add_rule("unders", sv_required("Number must be provided"))
        i$add_rule("unders", sv_gte(1))
        i$enable()
        req(i$is_valid())
        
        v$undersample <- as.integer(input$unders)
        DT::datatable(
          myData() %>% dplyr::slice(seq(1, dplyr::n(), by = v$undersample)),
          options    = datatable_options,
          selection  = 'single',
          filter     = 'top',
          extensions = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
          rownames   = FALSE
        ) %>%
          formatRound(
            columns = which(sapply(myData(), is.numeric)),
            digits  = v$num.digits
          ) %>%
          formatStyle(
            columns = 1:ncol(myData()),
            'text-align' = 'center'
          )
      }
    })
    
    ###############################################################################
    ##        UI OUTPUT FOR TIMESTAMP / GPS / ACCEL COLUMNS SELECTION            ##
    ###############################################################################
    output$Timestamp <- renderUI({
      selectizeInput(
        'Timestamp', 'Timestamp:',
        choices = c("", "Not_applicable", colnames(myData())),
        options = list(
          create       = TRUE,
          placeholder  = 'Please select',
          onInitialize = I('function() { this.setValue(""); }')
        ),
        multiple = FALSE
      )
    })
    
    output$GPS_Longitude <- renderUI({
      selectizeInput(
        'GPS_Longitude', 'GPS_Longitude:',
        choices = c("", "Not_applicable", colnames(myData())),
        options = list(
          create       = TRUE,
          placeholder  = 'Please select',
          onInitialize = I('function() { this.setValue(""); }')
        ),
        multiple = FALSE
      )
    })
    
    output$GPS_Latitude <- renderUI({
      selectizeInput(
        'GPS_Latitude', 'GPS_Latitude:',
        choices = c("", "Not_applicable", colnames(myData())),
        options = list(
          create       = TRUE,
          placeholder  = 'Please select',
          onInitialize = I('function() { this.setValue(""); }')
        ),
        multiple = FALSE
      )
    })
    
    output$IMU_Mean_VeDBA <- renderUI({
      selectizeInput(
        'IMU_Mean_VeDBA', 'IMU_Mean_VeDBA:',
        choices = c("", "Not_applicable", colnames(myData())),
        options = list(
          create       = TRUE,
          placeholder  = 'Please select',
          onInitialize = I('function() { this.setValue(""); }')
        ),
        multiple = FALSE
      )
    })
    
    output$Marked_Events <- renderUI({
      selectizeInput(
        'Marked_Events', 'Marked_Events:',
        choices = c("", "Not_applicable", colnames(myData())),
        options = list(
          create       = TRUE,
          placeholder  = 'Please select',
          onInitialize = I('function() { this.setValue(""); }')
        ),
        multiple = FALSE
      )
    })
    
    ###############################################################################
    ##       DATE/TIME FORMAT OUTPUTS (SELECTED OPTIONS + EXAMPLE PREVIEW)        ##
    ###############################################################################
    output$selectedOptions <- renderText({
      # Combine date/time formats and separators
      date_format <- input$optionsdateformat
      date_sep    <- input$optionsdatesep
      time_format <- input$optionstimeformat
      time_sep    <- input$optionstimesep
      
      # Build final format string
      formatted_date <- apply_separator(date_format, date_sep)
      formatted_time <- apply_separator(time_format, time_sep)
      result         <- paste(formatted_date, formatted_time)
      
      # Store in reactiveVal
      resultdatetime(result)
      result
    })
    
    output$datetimeexample <- renderText({
      req(input$optionsdateformat, input$optionstimeformat)
      
      # Example lookup
      format_lookup <- list(
        "%d" = "06",
        "%b" = "Jan",
        "%B" = "January",
        "%m" = "01",
        "%Y" = "1995",
        "%y" = "95",
        "%H" = "16",
        "%M" = "30",
        "%S" = "15",
        "%OS"= "15.275"
      )
      
      # Retrieve user picks
      date_format <- input$optionsdateformat
      date_sep    <- input$optionsdatesep
      time_format <- input$optionstimeformat
      time_sep    <- input$optionstimesep
      
      # Build each piece
      formatted_date <- apply_separator(date_format, date_sep)
      formatted_time <- apply_separator(time_format, time_sep)
      
      # Extract specifiers
      date_specifiers <- gsub("[^%a-zA-Z]", "", strsplit(formatted_date, date_sep)[[1]])
      time_specifiers <- gsub("[^%a-zA-Z]", "", strsplit(formatted_time, time_sep)[[1]])
      
      # Map to example values
      example_date <- sapply(date_specifiers, function(x) {
        ifelse(x %in% names(format_lookup), format_lookup[[x]], "")
      })
      example_time <- sapply(time_specifiers, function(x) {
        ifelse(x %in% names(format_lookup), format_lookup[[x]], "")
      })
      
      # Combine
      example_date   <- paste(example_date, collapse = date_sep)
      example_time   <- paste(example_time, collapse = time_sep)
      example_datetime <- paste(example_date, example_time)
      example_datetime
    })
    
    
    ###############################################################################
    ##           MAIN REACTIVE DATA FRAME & COLUMN RENAME LOGIC                  ##
    ###############################################################################
    
    # Keep a master DF() and a user-filtered version
    # If DF changes, we re-initialize filtered_data if needed.
    observe({
      req(DF())
      if (is.null(filtered_data())) {
        filtered_data(DF())  # Initialize with the original data
      }
    })
    
    # Observe user-chosen columns, rename accordingly in DF
    observe({
      req(input$file1, myData())
      
      DataNew1         <- myData()
      Timestamp        <- input$Timestamp
      GPS_Longitude    <- input$GPS_Longitude
      GPS_Latitude     <- input$GPS_Latitude
      IMU_Mean_VeDBA   <- input$IMU_Mean_VeDBA
      Marked_Events    <- input$Marked_Events
      
      # Rename columns if user chose them
      if (!is.null(Timestamp))
        colnames(DataNew1)[colnames(DataNew1) == Timestamp] <- "Timestamp"
      if (!is.null(GPS_Longitude))
        colnames(DataNew1)[colnames(DataNew1) == GPS_Longitude] <- "GPS_Longitude"
      if (!is.null(GPS_Latitude))
        colnames(DataNew1)[colnames(DataNew1) == GPS_Latitude] <- "GPS_Latitude"
      if (!is.null(IMU_Mean_VeDBA))
        colnames(DataNew1)[colnames(DataNew1) == IMU_Mean_VeDBA] <- "IMU_Mean_VeDBA"
      if (!is.null(Marked_Events))
        colnames(DataNew1)[colnames(DataNew1) == Marked_Events] <- "Marked_Events"
      
      # Keep only relevant columns
      relevant_cols <- c("Timestamp", "GPS_Longitude", "GPS_Latitude", 
                         "IMU_Mean_VeDBA", "Marked_Events")
      DataNew1 <- DataNew1[, names(DataNew1) %in% relevant_cols, with = FALSE]
      
      # Convert numeric columns
      numeric_columns <- c("GPS_Longitude", "GPS_Latitude", "IMU_Mean_VeDBA")
      for (col in numeric_columns) {
        if (col %in% colnames(DataNew1)) {
          DataNew1[[col]] <- as.numeric(DataNew1[[col]])
        }
      }
      
      # Keep original Timestamp in case of errors
      v$original_timestamp <- DataNew1$Timestamp
      
      DF(DataNew1)
    })
    
    # “Apply” button logic
    observeEvent(input$do, {
      # Reset exclusions so they are fresh
      exclusions(data.frame(Start = character(), End = character(), stringsAsFactors = FALSE))
      
      req(input$file1, myData(), nrow(DF()) > 0, ncol(DF()) > 0)
      
      isolate({
        tryCatch({
          DataNew1 <- DF()
          DataNew1$Timestamp <- v$original_timestamp  # restore original
          
          # Attempt to parse as.POSIXct
          DataNew1$Timestamp <- tryCatch(
            as.POSIXct(strptime(DataNew1$Timestamp, format = resultdatetime())),
            error = function(e) {
              showNotification(
                "Error: Failed to parse Timestamp. Check format in resultdatetime().",
                type = "error"
              )
              return(v$original_timestamp)
            }
          )
          
          # If conversion fails
          if (all(is.na(DataNew1$Timestamp))) {
            showNotification(
              "Error: Unable to process Timestamp. Ensure format matches resultdatetime().",
              type = "error"
            )
            return()
          }
          
          # Validate GPS columns
          if (!"GPS_Longitude" %in% colnames(DataNew1) ||
              !"GPS_Latitude" %in% colnames(DataNew1)) {
            showNotification(
              "Error: Must select GPS Longitude and Latitude columns.",
              type = "error"
            )
            return()
          }
          
          if (!all(is.numeric(DataNew1$GPS_Longitude), 
                   is.numeric(DataNew1$GPS_Latitude))) {
            showNotification(
              "Error: GPS Longitude & Latitude must be numeric.",
              type = "error"
            )
            return()
          }
          
          if (any(DataNew1$GPS_Longitude < -180 | DataNew1$GPS_Longitude > 180, 
                  na.rm = TRUE)) {
            showNotification("Error: Longitude must be between -180 and 180.", type = "error")
            return()
          }
          
          if (any(DataNew1$GPS_Latitude < -90 | DataNew1$GPS_Latitude > 90, 
                  na.rm = TRUE)) {
            showNotification("Error: Latitude must be between -90 and 90.", type = "error")
            return()
          }
          
          # Standardize Timestamp display
          DataNew1$Timestamp <- format(DataNew1$Timestamp, "%Y-%m-%d %H:%M:%OS")
          DataNew1$Timestamp <- as.POSIXct(DataNew1$Timestamp, format = "%Y-%m-%d %H:%M:%OS")
          DataNew1 <- DataNew1 %>% dplyr::filter(!is.na(Timestamp))
          
          # Add metadata
          DataNew1 <- DataNew1 %>%
            mutate(
              Gender            = input$gender,
              Deployment_Number = input$deployment_num,
              Seconds           = cumsum(c(0, diff(as.numeric(Timestamp)))),
              Observations      = seq_len(nrow(DataNew1))
            )
          
          DataNew1$Dataset_name <- v$words_submitted
          DataNew1$Species      <- v$species
          DataNew1$ID           <- v$id
          
          # Convert to factors
          DataNew1$ID              <- as.factor(DataNew1$ID)
          DataNew1$Species         <- as.factor(DataNew1$Species)
          DataNew1$Gender          <- as.factor(DataNew1$Gender)
          DataNew1$Deployment_Number <- as.numeric(DataNew1$Deployment_Number)
          
          # Marked_Events as factor
          if ("Marked_Events" %in% colnames(DataNew1)) {
            DataNew1$Marked_Events <- ifelse(DataNew1$Marked_Events == 0, NA, DataNew1$Marked_Events)
            DataNew1$Marked_Events <- as.factor(DataNew1$Marked_Events)
          }
          
          # Reorder columns if present
          x <- c(
            "Observations", "Dataset_name", "ID", "Species", "Gender", "Deployment_Number",
            "Timestamp", "Seconds", "GPS_Longitude", "GPS_Latitude", "IMU_Mean_VeDBA", "Marked_Events"
          )
          DataNew1 <- DataNew1[, x[x %in% names(DataNew1)], with = FALSE]
          
          # Update reactive
          DF(DataNew1)
          filtered_data(DataNew1)
          
        }, error = function(e) {
          showNotification("Error: Unable to process data. Check inputs & try again.", type = "error")
        })
        
        # Re-render “contents” with updated DF
        output$contents <- DT::renderDataTable({
          if (input$fixedWidth == "Long format") {
            scroller <- input$scroller
            if (scroller) {
              datatable_options <- rv3$options
            } else {
              datatable_options <- rv2$options
            }
            datatable_options$scrollY <- input$tableHeight
          } else {
            datatable_options <- rv1$options
          }
          
          if (exists("DF") && "Timestamp" %in% colnames(DF())) {
            display_data            <- DF()
            display_data$Timestamp  <- format(display_data$Timestamp, "%Y-%m-%d %H:%M:%OS")
            
            # show all vs. undersample
            if (input$disp == "all") {
              DT::datatable(
                display_data,
                options     = datatable_options,
                selection   = 'single',
                filter      = 'top',
                extensions  = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
                rownames    = FALSE
              ) %>%
                formatRound(
                  columns = which(sapply(display_data, is.numeric)),
                  digits  = v$num.digits
                ) %>%
                formatStyle(
                  columns = 1:ncol(display_data),
                  'text-align' = 'center'
                )
            } else if (input$disp == "undersample") {
              i <- InputValidator$new()
              i$add_rule("unders", sv_required("Number must be provided"))
              i$add_rule("unders", sv_gte(1))
              i$enable()
              req(i$is_valid())
              
              v$undersample <- as.integer(input$unders)
              
              DT::datatable(
                display_data %>% dplyr::slice(seq(1, dplyr::n(), by = v$undersample)),
                options     = datatable_options,
                selection   = 'single',
                filter      = 'top',
                extensions  = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
                rownames    = FALSE
              ) %>%
                formatRound(
                  columns = which(sapply(display_data, is.numeric)),
                  digits  = v$num.digits
                ) %>%
                formatStyle(
                  columns = 1:ncol(display_data),
                  'text-align' = 'center'
                )
            }
          }
        })
        
        # Summary for entire DF
        output$summary <- renderPrint({
          summary(DF())
        })
        
        ###########################################################################
        ##             TIME STAMP EXCLUSIONS / TIMEZONE OBSERVERS                ##
        ###########################################################################
        
        # Convert timezone
        observeEvent(input$apply_timezone, {
          req(filtered_data())
          
          isolate({
            data <- filtered_data()
            
            if ("Timestamp" %in% colnames(data) && is.POSIXct(data$Timestamp)) {
              # offset in seconds
              offset_seconds <- input$timezone_offset * 3600
              data$Timestamp <- data$Timestamp - v$orig_time_offset
              data$Timestamp <- data$Timestamp + offset_seconds
              v$orig_time_offset <- offset_seconds
              filtered_data(data)
              showNotification(
                paste("Timezone offset of", input$timezone_offset, "hours applied."),
                type = "message"
              )
            } else {
              showNotification("Error: Timestamp missing or not POSIXct.", type = "error")
            }
          })
        })
        
        # Add an exclusion period
        observeEvent(input$add_exclusion, {
          req(input$exclude_date_start, input$exclude_time_start,
              input$exclude_date_end,   input$exclude_time_end)
          
          # Pad “:00” if needed
          start_time <- ifelse(
            nchar(input$exclude_time_start) == 5,
            paste0(input$exclude_time_start, ":00"),
            input$exclude_time_start
          )
          end_time <- ifelse(
            nchar(input$exclude_time_end) == 5,
            paste0(input$exclude_time_end, ":00"),
            input$exclude_time_end
          )
          
          start_timestamp <- paste(input$exclude_date_start, start_time)
          end_timestamp   <- paste(input$exclude_date_end,   end_time)
          
          formatted_start <- tryCatch(
            as.POSIXct(strptime(start_timestamp, "%Y-%m-%d %H:%M:%S")),
            error = function(e) {
              showNotification("Invalid start timestamp. Check format.", type = "error")
              NULL
            }
          )
          formatted_end <- tryCatch(
            as.POSIXct(strptime(end_timestamp, "%Y-%m-%d %H:%M:%S")),
            error = function(e) {
              showNotification("Invalid end timestamp. Check format.", type = "error")
              NULL
            }
          )
          
          req(!is.null(formatted_start), !is.null(formatted_end))
          
          # Insert if not a duplicate
          existing_exclusions <- exclusions()
          is_duplicate <- any(
            existing_exclusions$Start == formatted_start &
              existing_exclusions$End   == formatted_end
          )
          if (!is_duplicate) {
            new_exclusion <- data.frame(
              Start = formatted_start,
              End   = formatted_end,
              stringsAsFactors = FALSE
            )
            exclusions(rbind(existing_exclusions, new_exclusion))
          }
        })
        
        # Display the exclusion table
        output$exclusion_table <- DT::renderDataTable({
          exclusions_df <- exclusions()
          DT::datatable(exclusions_df, options = list(pageLength = 5), rownames = FALSE) %>%
            formatDate("Start", method = "toLocaleString") %>%
            formatDate("End",   method = "toLocaleString")
        })
        
        # Apply Exclusions
        observeEvent(input$apply_exclusion, {
          req(filtered_data(), exclusions())
          isolate({
            data <- filtered_data()
            for (i in seq_len(nrow(exclusions()))) {
              start <- as.POSIXct(exclusions()$Start[i])
              end   <- as.POSIXct(exclusions()$End[i])
              data  <- data %>% dplyr::filter(!(Timestamp >= start & Timestamp <= end))
            }
            filtered_data(data)
          })
        })
        
        # Show the filtered data in a table
        output$filtered_data <- DT::renderDataTable({
          req(filtered_data())
          
          display_data <- filtered_data()
          display_data$Timestamp <- format(display_data$Timestamp, "%Y-%m-%d %H:%M:%OS")
          
          if (input$fixedWidth == "Long format") {
            scroller <- input$scroller
            if (scroller) {
              datatable_options <- rv3$options
            } else {
              datatable_options <- rv2$options
            }
            datatable_options$scrollY <- input$tableHeight
          } else if (input$fixedWidth == "Wide format") {
            datatable_options <- rv1$options
          }
          
          isolate({
            if (input$disp == "all") {
              DT::datatable(
                display_data,
                options     = datatable_options,
                selection   = 'single',
                filter      = 'top',
                extensions  = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
                rownames    = FALSE
              ) %>%
                formatRound(
                  columns = which(sapply(display_data, is.numeric)),
                  digits  = v$num.digits
                ) %>%
                formatStyle(
                  columns = 1:ncol(display_data),
                  'text-align' = 'center'
                )
            } else if (input$disp == "undersample") {
              i <- InputValidator$new()
              i$add_rule("unders", sv_required("Number must be provided"))
              i$add_rule("unders", sv_gte(1))
              i$enable()
              req(i$is_valid())
              
              v$undersample <- as.integer(input$unders)
              DT::datatable(
                display_data %>% dplyr::slice(seq(1, dplyr::n(), by = v$undersample)),
                options     = datatable_options,
                selection   = 'single',
                filter      = 'top',
                extensions  = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller"),
                rownames    = FALSE
              ) %>%
                formatRound(
                  columns = which(sapply(display_data, is.numeric)),
                  digits  = v$num.digits
                ) %>%
                formatStyle(
                  columns = 1:ncol(display_data),
                  'text-align' = 'center'
                )
            }
          })
        })
        
        # Summary of the filtered data
        output$summary.filt <- renderPrint({
          summary(filtered_data())
        })
        
        # Reset data
        observeEvent(input$reset_data, {
          req(DF())
          filtered_data(DF())
          exclusions(data.frame(Start = character(), End = character(), stringsAsFactors = FALSE))
          v$orig_time_offset <- 0
        })
        
  
        # === Summary Inspection Plots Tab ===
        
        # Render sampling rate plots with dygraphs
        output$sampling_rate_dygraph <- renderDygraph({
          req(filtered_data())
          data <- filtered_data()
          
          if (nrow(data) == 0) {
            showNotification("No data available for plotting. Check filters or data input.", type = "warning")
            return(NULL)
          }
          
          # Initialize plot components based on user selection
          selected_components <- input$plot_components # e.g., "gps", "imu"
          if (is.null(selected_components) || length(selected_components) == 0) {
            showNotification("Please select at least one component (GPS Fixes or IMU Bursts) for plotting.", type = "warning")
            return(NULL)
          }
          
          # Subset data for GPS and IMU as needed
          gps_data <- NULL
          imu_data <- NULL
          
          if ("gps" %in% selected_components) {
            if ("GPS_Longitude" %in% colnames(data) && "GPS_Latitude" %in% colnames(data)) {
              gps_data <- data %>%
                dplyr::filter(!is.na(GPS_Longitude) & !is.na(GPS_Latitude)) %>%
                mutate(time_diff = c(NA, diff(as.numeric(Timestamp)))) %>%
                dplyr::select(Timestamp, time_diff)
              
              if (input$normalize) {
                max_gps <- max(gps_data$time_diff, na.rm = TRUE)
                gps_data$time_diff <- gps_data$time_diff / max_gps
              }
            } else {
              showNotification("GPS data is missing or not properly selected. Skipping GPS Fixes.", type = "warning")
            }
          }
          
          if ("imu" %in% selected_components) {
            if ("IMU_Mean_VeDBA" %in% colnames(data)) {
              imu_data <- data %>%
                dplyr::filter(!is.na(IMU_Mean_VeDBA)) %>%
                mutate(time_diff = c(NA, diff(as.numeric(Timestamp)))) %>%
                dplyr::select(Timestamp, time_diff)
              
              if (input$normalize) {
                max_imu <- max(imu_data$time_diff, na.rm = TRUE)
                imu_data$time_diff <- imu_data$time_diff / max_imu
              }
            } else {
              showNotification("IMU data is missing or not properly selected. Skipping IMU Bursts.", type = "warning")
            }
          }
          
          # Merge data for dygraph only if selected
          merged_data <- data.frame(Timestamp = data$Timestamp) # Start with empty data frame
          if (!is.null(gps_data)) {
            merged_data <- merge(merged_data, gps_data %>% dplyr::rename(gps_time_diff = time_diff), by = "Timestamp", all = TRUE)
          }
          if (!is.null(imu_data)) {
            merged_data <- merge(merged_data, imu_data %>% dplyr::rename(imu_time_diff = time_diff), by = "Timestamp", all = TRUE)
          }
          
          # Ensure merged_data has valid rows
          if (nrow(merged_data) == 0 || all(is.na(merged_data))) {
            showNotification("No valid data available for plotting after filtering. Check inputs.", type = "warning")
            return(NULL)
          }
          
          # Convert to xts object for dygraph
          dygraph_columns <- intersect(colnames(merged_data), c("gps_time_diff", "imu_time_diff"))
          dygraph_data <- xts::xts(merged_data[, dygraph_columns, drop = FALSE], order.by = merged_data$Timestamp)
          
          # Create dygraph
          dg <- dygraph(dygraph_data, main = "Sampling Rates Over Time") %>%
            dyAxis("x", 
                   ticker = "Dygraph.dateTicker", 
                   label = "Timestamp", 
                   valueFormatter = JS(CustomValueFormat),
                   axisLabelWidth = 80, 
                   labelWidth = 30, 
                   labelHeight = 30,
                   axisLabelFontSize = 14, 
                   drawGrid = FALSE
            ) %>%
            dyAxis("y", label = ifelse(input$normalize, "Normalized Time Difference", "Time Difference (s)"),
                   axisLabelWidth = 80, 
                   labelWidth = 30, 
                   axisLabelFontSize = 14, 
                   drawGrid = FALSE) %>%
            dyOptions(strokeWidth = 1.2, 
                      drawPoints = TRUE, 
                      pointSize = 2, 
                      useDataTimezone = TRUE,
                      connectSeparatedPoints = TRUE) %>%
            dyRangeSelector(retainDateWindow = TRUE) %>% 
            dyUnzoom()
          
          # Dynamically add dySeries for available components
          if ("gps_time_diff" %in% dygraph_columns) {
            dg <- dg %>% dySeries("gps_time_diff", label = "GPS Fixes", color = "blue")
          }
          if ("imu_time_diff" %in% dygraph_columns) {
            dg <- dg %>% dySeries("imu_time_diff", label = "IMU Bursts", color = "green")
          }
          dg
        })
        
        ## Time Differences by 30-Minute Bins
        
        output$timeDiffPlots <- renderPlotly({
          req(filtered_data())
          data <- filtered_data()
          
          if (nrow(data) == 0) {
            showNotification("No data available for plotting. Check filters or data input.", type = "warning")
            return(NULL)
          }
          
          # Initialize plot components based on user selection
          selected_components <- input$plot_components # e.g., "gps", "imu"
          if (is.null(selected_components) || length(selected_components) == 0) {
            showNotification("Please select at least one component (GPS Fixes or IMU Bursts) for plotting.", type = "warning")
            return(NULL)
          }
          
          imu_plot <- NULL
          gps_plot <- NULL
          
          if ("gps" %in% selected_components) {
            
            # Filter valid GPS data
            gps_data <- data %>%
              dplyr::filter(!is.na(GPS_Longitude) & !is.na(GPS_Latitude)) %>%
              mutate(
                time_diff = c(NA, diff(as.numeric(Timestamp))),
                hour_decimal = lubridate::hour(Timestamp) + lubridate::minute(Timestamp) / 60
              )
            
            
            # Aggregate GPS and IMU time differences by 30-minute bins
            gps_summary <- gps_data %>%
              mutate(hour_bin = floor(hour_decimal * 2) / 2) %>%  # 30-minute bins
              group_by(hour_bin) %>%
              summarize(
                mean_time_diff = mean(time_diff, na.rm = TRUE),
                sd_time_diff = sd(time_diff, na.rm = TRUE),
                .groups = "drop"
              )
            
            # Create interactive plotly plots
            gps_plot <- plot_ly(gps_summary, x = ~hour_bin) %>%
              add_trace(y = ~mean_time_diff, type = "scatter", mode = "lines+markers",
                        error_y = list(array = gps_summary$sd_time_diff, color = "blue"),
                        name = "GPS Mean ± SD", line = list(color = "blue")) %>%
              layout(title = "GPS Time Differences Over 30-Minute Bins",
                     xaxis = list(title = "Hour of Day (30-minute bins)"),
                     yaxis = list(title = "Time Difference (s)"))
          }
          
          if ("imu" %in% selected_components) {
            # Filter valid IMU data (if available)
            imu_data <- NULL
            if ("IMU_Mean_VeDBA" %in% colnames(data)) {
              imu_data <- data %>%
                dplyr::filter(!is.na(IMU_Mean_VeDBA)) %>%
                mutate(
                  time_diff = c(NA, diff(as.numeric(Timestamp))),
                  hour_decimal = lubridate::hour(Timestamp) + lubridate::minute(Timestamp) / 60
                )
            }
            
            imu_summary <- NULL
            if (!is.null(imu_data)) {
              imu_summary <- imu_data %>%
                mutate(hour_bin = floor(hour_decimal * 2) / 2) %>%  # 30-minute bins
                group_by(hour_bin) %>%
                summarize(
                  mean_time_diff = mean(time_diff, na.rm = TRUE),
                  sd_time_diff = sd(time_diff, na.rm = TRUE),
                  .groups = "drop"
                )
            }
            
            
            if (!is.null(imu_summary)) {
              imu_plot <- plot_ly(imu_summary, x = ~hour_bin) %>%
                add_trace(y = ~mean_time_diff, type = "scatter", mode = "lines+markers",
                          error_y = list(array = imu_summary$sd_time_diff, color = "green"),
                          name = "IMU Mean ± SD", line = list(color = "green")) %>%
                layout(title = "IMU Time Differences Over 30-Minute Bins",
                       xaxis = list(title = "Hour of Day (30-minute bins)"),
                       yaxis = list(title = "Time Difference (s)"))
            }
          }
          
          # Combine the two plots if IMU data exists
          if (!is.null(imu_plot) & !is.null(gps_plot)) {
            subplot(gps_plot, imu_plot, nrows = 2, shareX = TRUE, titleX = TRUE, margin = 0.05)
          } else if(is.null(imu_plot) & !is.null(gps_plot)) {
            gps_plot
          } else if(!is.null(imu_plot) & is.null(gps_plot)) {
            imu_plot
          }
        })
        
        # GPS Burst Duration (<2s)
        
        output$burstDurationPlots <- renderPlotly({
          req(filtered_data())
          data <- filtered_data()
          
          if (nrow(data) == 0) {
            showNotification("No data available for plotting. Check filters or data input.", type = "warning")
            return(NULL)
          }
          
          # Initialize plot components based on user selection
          selected_components <- input$plot_components # e.g., "gps", "imu"
          if (is.null(selected_components) || length(selected_components) == 0) {
            showNotification("Please select at least one component (GPS Fixes or IMU Bursts) for plotting.", type = "warning")
            return(NULL)
          }
          
          if ("gps" %in% selected_components) {
            # Filter valid GPS data
            gps_data <- data %>%
              dplyr::filter(!is.na(GPS_Longitude) & !is.na(GPS_Latitude)) %>%
              mutate(
                time_diff = c(NA, diff(as.numeric(Timestamp)))  # Calculate time differences
              )
            
            # Assign bursts: a new burst starts when time_diff > 2 seconds
            gps_data <- gps_data %>%
              mutate(
                burst_group = cumsum(if_else(is.na(time_diff) | time_diff > 2, 1, 0))  # Burst group
              )
            
            # Calculate burst durations (max time - min time per burst group)
            burst_summary <- gps_data %>%
              group_by(burst_group) %>%
              summarize(
                burst_duration = as.numeric(max(Timestamp) - min(Timestamp), units = "secs"),  # Duration in seconds
                hour_decimal = lubridate::hour(min(Timestamp)) + lubridate::minute(min(Timestamp)) / 60,  # Hour for binning
                .groups = "drop"
              )
            
            # Bin burst durations by 30-minute intervals
            burst_summary <- burst_summary %>%
              mutate(hour_bin = floor(hour_decimal * 2) / 2) %>%  # 30-minute bins
              group_by(hour_bin) %>%
              summarize(
                mean_burst_duration = mean(burst_duration, na.rm = TRUE),
                sd_burst_duration = sd(burst_duration, na.rm = TRUE),
                .groups = "drop"
              )
            
            # Check if there is any data to plot
            if (nrow(burst_summary) == 0) {
              showNotification("No valid GPS bursts detected. Adjust the parameters or input data.", type = "warning")
              return(NULL)
            }
            
            # Create interactive Plotly plot
            plot_ly(burst_summary, x = ~hour_bin) %>%
              add_trace(y = ~mean_burst_duration, type = "scatter", mode = "lines+markers",
                        error_y = list(array = burst_summary$sd_burst_duration, color = "red"),
                        name = "GPS Burst Duration Mean ± SD", line = list(color = "red")) %>%
              layout(
                title = "GPS Burst Durations (<2 s) Over 30-Minute Bins",
                xaxis = list(title = "Hour of Day (30-minute bins)"),
                yaxis = list(title = "Mean GPS Burst Duration (s)")
              )
          }
        })
        
        #################################################################
        
        # Final check that we can move on
        if (exists("filtered_data") && "Timestamp" %in% colnames(filtered_data()) &&
            nrow(filtered_data()) > 0 && ncol(filtered_data()) > 0) {
          v$data.chk <- TRUE
        } else {
          v$data.chk <- FALSE
        }
      })
      
      ### End of page 2 ###
      
    })

    ###############################################################################
    ##     WARNING MESSAGE IF data.chk == FALSE (No data loaded / invalid)       ##
    ###############################################################################
    output$warning_message <- renderUI({
      if (v$data.chk == FALSE) {
        HTML("
      <div style='font-weight:bold; color:red; background-color:#ffcccc; padding:10px;'>
        Please correct inputs on the previous page to render the table and continue with data processing.
      </div>
    ")
      }
    })
    
    ################################################################################
    ##                 GPS PROCESSING PAGE (page3) - UI Rendering                 ##
    ################################################################################
    
    output$page3 <- renderUI({
      fluidPage(
        # === 1) Page Title ===
        titlePanel(
          h1(
            strong("GPS Processing"),
            align = "center",
            style = "background-color:lightgreen; color:black; font-size:30px;"
          )
        ),
        
        # === 2) Loading Message Style & Conditional Panel ===
        tags$style(type="text/css", "
      #loadmessage {
        position: fluid;
        top: 0px;
        left: 0px;
        width: 100%;
        padding: 5px 0px 5px 0px;
        text-align: center;
        font-weight: bold;
        font-size: 100%;
        color: #000000;
        background-color: #CCFF66;
        z-index: 60;
      }
    "),
        tags$style(HTML("
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate,
      table.dataTable tr {
        font-size: 10pt !important;
      }
    ")),
        
        # A small JS snippet to convert area to km² for geometry tools
        tags$script(HTML("
      L.GeometryUtil.readableArea = function (area, isMetric, precision) {
        var km2 = area / 1000000;
        var output = (Math.round(km2 * 100) / 100).toLocaleString() + ' km²';
        return output;
      };
    ")),
        tags$style(HTML("
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate,
      table.dataTable tr {
        font-size: 8pt !important;
      }
    ")),
        
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        ),
        
        # Warning if data_chk is false (no valid data loaded)
        conditionalPanel(
          condition = "output.data_chk == false",
          uiOutput("warning_message")
        ),
        
        # === 3) Main Layout: 2 sidebars (left + right) + main center (?)
        # Actually, by the code, we have:
        # Left sidebar of width=3: sub-tab panels for GPS settings
        # Main panel of width=6: main results
        # Right sidebar of width=3: advanced sub-tab panels (User thresholds, IF config, etc.)
        
        # Left Sidebar: GPS processing & map options
        sidebarPanel(
          width = 3,
          tabsetPanel(
            id   = "tabsetPanelID2",
            type = "tabs",
            
            # --- (A) GPS Processing Options ---
            tabPanel("Processing Options",
                     h3("GPS Processing Parameters", align="center"),
                     
                     tags$div(
                       "1) Select GPS Burst Length and Burst Processing Method",
                       style = "font-size:14px; color:white; background-color:#007bff;
                     text-align:center; padding:5px; font-weight:bold;"
                     ),
                     # Burst length input
                     h5("(Burst lengths > this supplied value will be regarded as 1Hz sampling)", align="center"),
                     numericInput("burstLength", "GPS Burst Length (seconds):",
                                  value = 1, min=1, max=60, step=1),
                     
                     # Burst processing method
                     radioButtons("burstMethod", "Burst Processing Method:",
                                  choices  = c("None" = "none", "Mean" = "mean",
                                               "Median" = "median", "Last" = "last"),
                                  selected = "none"
                     ),
                     # Fix drop out
                     numericInput("fixDropout", "Fix Drop Out Window (seconds):",
                                  value=3600, min=1, step=1),
                     
                     tags$div(
                       "2) Standardise GPS Sampling Interval?",
                       style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     # Standardize time interval
                     radioButtons("StandTimeIntLog", "Standardize Fix Intervals?",
                                  choices  = c("No"="no", "Yes"="yes"),
                                  selected = "no"
                     ),
                     numericInput("StandTimeInt", "Standardize Fix Intervals (seconds):",
                                  value=240, min=1, step=1),
                     radioButtons("StandTimeIntUniv", "Standardize Fix Intervals Across...",
                                  choices  = c("All Data"="all", "Just 1 Hz Periods"="Hz",
                                               "All Data Excl. 1 Hz"="exc.1Hz"),
                                  selected = "all"
                     ),
                     
                     tags$div(
                       "3) Mean VeDBA for Preceding GPS Intervals?",
                       style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     radioButtons("VeDInt", "Mean VeDBA for Intervals Leading Up to Each GPS Fix",
                                  choices  = c("Yes"="yes", "No"="no"),
                                  selected = "yes"
                     ),
                     tags$div(
                       "4) Post-Smooth Processed Track?",
                       style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     
                     fluidRow(
                       column(width = 7,
                     radioButtons("TrackSmoothing", "Smoothing Method",
                                  choices  = c("No Smoothing" = "No_Smooth", 
                                               "Median Smoothing" = "Med_window", 
                                               "Savitzky-Golay Smoothing" = "SG_Smooth",
                                               "Kalman Filter Smoothing" = "KF_Smooth"),
                                  selected = "No_Smooth")
                       ),
                     h5("Kalman filter not properly functional yet"),
                       column(width = 5,
                              checkboxInput("SmoothOnly1Hz", "1Hz Only", value = FALSE)
                       )
                     ),
                     br(),
                     conditionalPanel(
                       condition = "input.TrackSmoothing == 'Med_window'",
                       fluidRow(
                         column(width = 6,
                                numericInput("Med_window", "Median Smoothing Window (odd):",
                                             value = 5, min = 3, step = 2)
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.TrackSmoothing == 'SG_Smooth'",
                       fluidRow(
                         column(width = 6,
                                numericInput("SG_order", "SG Filter Order (odd):",
                                             value = 3, min = 1, step = 2)
                         ),
                         column(width = 6,
                                numericInput("SG_window", "SG Window Length (odd):",
                                             value = 5, min = 3, step = 2)
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.TrackSmoothing == 'KF_Smooth'",
                       h3("Kalman Filter Smoothing Options", align = "center"),
                       fluidRow(
                         column(width = 4,
                                numericInput("KF_procNoise", "Process Noise:", 
                                             value = 0.1, min = 0.001, step = 0.01)
                         ),
                         column(width = 4,
                                numericInput("KF_measNoise_pos", "Measurement Noise (Position):", 
                                             value = 0.5, min = 0.001, step = 0.01)
                         ),
                         column(width = 4,
                                numericInput("KF_measNoise_speed", "Measurement Noise (Speed):", 
                                             value = 0.2, min = 0.001, step = 0.01)
                         )
                       ),
                       fluidRow(
                         column(width = 6,
                                numericInput("KF_scaleFactor", "KF Scale Factor (VeDBA):", 
                                             value = 1.0, min = 0.1, step = 0.1)
                         ),
                         column(width = 6,
                                checkboxInput("KF_includeVeDBA", "Include mean_VeDBA in KF?", value = FALSE)
                         )
                       )
                     )
            ),
            
            # --- (B) Leaflet Map Options ---
            tabPanel("Leaflet Map Options",
                     h3("Leaflet Map Display Options", align="center"),
                     
                     fluidRow(
                       column(
                         width=6,
                         # Map type selection
                         radioButtons("mapType", "Select Map Type:",
                                      choices  = c("Satellite"="Esri.WorldImagery",
                                                   "OpenStreetMap"="OpenStreetMap"),
                                      selected = "OpenStreetMap"
                         )
                       ),
                       column(
                         width=6,
                         # Scale bar width
                         numericInput("scalebarwidth", "Scale Bar Width (m):",
                                      value=200, min=5, max=1000, step=1
                         )
                       )
                     ),
                     
                     fluidRow(
                       # Raw GPS point size
                       column(
                         width=6,
                         numericInput("GPS_PS", "Raw GPS Point Size:",
                                      value=3, min=0, max=80, step=0.1
                         )
                       ),
                       # Raw GPS point color
                       column(
                         width=6,
                         colourpicker::colourInput(
                           "pointColor", "Raw GPS Point Color:",
                           value="cyan"
                         )
                       ),
                       
                       # Raw GPS line width
                       column(
                         width=6,
                         numericInput("GPS_LW", "Raw GPS Line width:",
                                      value=1, min=0, max=10, step=0.1
                         )
                       ),
                       # Raw GPS line color
                       column(
                         width=6,
                         colourpicker::colourInput(
                           "lineColor", "Raw GPS Line Color:",
                           value="cyan"
                         )
                       ),
                       
                       # Processed GPS point size
                       column(
                         width=6,
                         numericInput("GPS_PS_proc", "Processed GPS Point Size:",
                                      value=3, min=0, max=80, step=0.1
                         )
                       ),
                       # Processed GPS point color
                       column(
                         width=6,
                         colourpicker::colourInput(
                           "pointColor_proc", "Processed GPS Point Color:",
                           value="purple"
                         )
                       ),
                       
                       # Processed GPS line width
                       column(
                         width=6,
                         numericInput("GPS_LW_proc", "Processed Line width:",
                                      value=1, min=0, max=10, step=0.1
                         )
                       ),
                       # Processed GPS line color
                       column(
                         width=6,
                         colourpicker::colourInput(
                           "lineColor_proc", "Processed GPS Line Color:",
                           value="purple"
                         )
                       ),
                       
                       # Display modes
                       checkboxGroupInput(
                         "display_modes", "Display Modes:",
                         choices  = c("Show GPS Points"="points",
                                      "Show Connecting Lines"="lines",
                                      "Show Clusters (only for leaflet circle markers)"="clusters",
                                      "Enable Popup Info on GPS Point Click"="popups"),
                         selected = c("points","lines")
                       ),
                       
                       # Radio for popups on raw vs. processed
                       radioButtons("popuploc", "Show Popups on...",
                                    choices  = c("Raw Fixes"="raw", "Processed Fixes"="proc"),
                                    selected = "proc"
                       )
                     ),
                     
                     fluidRow(
                       h5("(WebGl (leafgl) rendering is much faster, but only precise to a few metres."),
                       h5("Base leaflet rendering is more precise, but slower for large data.)"),
                       
                       column(
                         width=6,
                         radioButtons("pointrendering", "Point Rendering Method",
                                      choices  = c("Base Leaflet Circle Markers"="leaflet_point",
                                                   "WebGl Points (leafgl)"="leafgl_point"),
                                      selected = "leaflet_point"
                         )
                       ),
                       column(
                         width=6,
                         radioButtons("linerendering", "Line Rendering Method",
                                      choices  = c("Base Leaflet Poly Lines"="leaflet_line",
                                                   "WebGl Lines (leafgl)"="leafgl_line"),
                                      selected = "leaflet_line"
                         )
                       )
                     ),

            ),
            
            # --- (C) VeDBA ~ GPS Speed Options ---
            tabPanel("DBA~Speed Options",
                     h3("VeDBA~GPS Speed Options", align="center"),
                     fluidRow(
                       column(width=12,
                              pickerInput(
                                inputId="DBA_Sp_Vars",
                                label  ="Select Variable(s) to plot:",
                                choices= c("speed_raw","speed_filt","IMU_Mean_VeDBA","mean_VeDBA_interval"),
                                multiple=TRUE,
                                selected="speed_raw",
                                options = list(`actions-box`=TRUE, `live-search`=TRUE)
                              )
                       ),
                       column(width=6,
                              numericInput("speedStepSeconds_R","Speed Step [Raw Fixes] (s):",
                                           value=1, min=1, max=3600, step=1
                              )
                       ),
                       column(width=6,
                              numericInput("speedStepSeconds_P","Speed Step [Processed Fixes] (s):",
                                           value=1, min=1, max=3600, step=1
                              )
                       ),
                       column(width=6,
                              colourpicker::colourInput("color_speed_raw","Color for speed_raw", value="orange")
                       ),
                       column(width=6,
                              colourpicker::colourInput("color_speed_filt","Color for speed_filt", value="red")
                       ),
                       column(width=6,
                              colourpicker::colourInput("color_vedba_raw","Color for IMU_Mean_VeDBA", value="cyan")
                       ),
                       column(width=6,
                              colourpicker::colourInput("color_vedba_proc","Color for mean_VeDBA_interval", value="blue")
                       ),
                       column(width=12,
                              selectInput("eventPalette", "Marked Event Color Palette:",
                                          choices = c("Set1","Set2","Dark2","Paired","Accent","Spectral","RdYlBu"),
                                          selected= "Set1"
                              )
                       ),
                       column(width=12,
                              sliderInput("eventAlpha","Marked Event Alpha:", min=0, max=1, value=0.3, step=0.05)
                       )
                     )
            ),
        
          )
        ),
        
        # === 4) Main Panel for Displaying Outputs ===
        mainPanel(
          width=6,
          actionButton("GPSprocessing", "Process GPS Data (must press before screening/plotting)",
                       style="width:100%; margin-top:20px; font-size:16px;"
          ),
          tabsetPanel(
            id   = "gpsResultsTabset",
            type = "tabs",
            
            # ~ (A) Table Tab
            tabPanel("Table",
                     withSpinner(DT::dataTableOutput("gpsDataTable"))
            ),
            
            # ~ (B) Leaflet Map Tab
            tabPanel("Leaflet Map",
                     
                     h4("Time Filters", align="center"),
                     fluidRow(
                       
                       column(
                         width=3,
                         pickerInput(
                           "selectedYears", "Select Years:",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`="{0} years selected",
                             `none-selected-text`="No years selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       ),
                       column(
                         width=3,
                         pickerInput(
                           "selectedMonths", "Select Months:",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`="{0} months selected",
                             `none-selected-text`="No months selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       ),
                       
                       column(
                         width=3,
                         pickerInput(
                           "selectedDays", "Select Days:",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`="{0} days selected",
                             `none-selected-text`="No days selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       ),
                       column(
                         width=3,
                         pickerInput(
                           "selectedHours", "Select Hours:",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`="{0} hours selected",
                             `none-selected-text`="No hours selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       )
                     ),
                     fluidRow(
                       # Additional time-based pickers
                       column(
                         width=3,
                         pickerInput(
                           inputId = "selectedDayCount",
                           label   = "Select Day Counter(s):",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`= "{0} days selected",
                             `none-selected-text`="No day counters selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       ),
                       column(
                         width=9,
                         # Timestamp range filter
                         uiOutput("timestampSliderUI")
                       )
                     ),
                     fluidRow(
                       # Additional time-based pickers
                       column(width=9,
                     actionButton("refreshMap", "Plot/Refresh Map",
                                  style="width:100%; margin-top:20px; font-size:16px;"
                     )),
                     column(width=3,
                            actionButton("downloadMap", "Download Map Image")
                     )
                     ),
                     fluidRow(
                       column(12,
                              withSpinner(leafletOutput("gpsMap", height="650px"))
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(12,
                              h4("Selected Point Row:"),
                              DT::dataTableOutput("selectedPointTable")
                       ),
                       column(8,
                              radioButtons("manualVerdict","Change verdict:",
                                           choices  = c("Anomalous","Not Anomalous"),
                                           selected = "Not Anomalous"
                              )
                       ),
                       column(4,
                              actionButton("applyManualVerdict","Apply New Verdict")
                       ),
                       column(12, h4("Modify/Reapply Verdict Changes:")),
                       column(6,
                              actionButton("removeOverride", "Remove selected override",
                                           style="background-color:red; color:red;"
                              )
                       ),
                       column(6,
                              actionButton("reApplyManual", "Re-Apply Manual Verdicts",
                                           style="background-color:green; color:green;"
                              )
                       )
                     ),
                     column(12,
                            h4("Current Overrides (Shared with VeDBA ~ GPS Speed)"),
                            DT::dataTableOutput("overridesTable")
                     ),
                     br(),
                     column(6,
                            fileInput("import_overrides", "Import Overrides (CSV)")
                     ),
                     column(6,
                            downloadButton("export_overrides", "Export Overrides (CSV)")
                     ),
                     uiOutput("glDataScript"),
                     tags$head(
                       tags$style(HTML("
              #gpsMap canvas {
                cursor:pointer !important;
              }
            "))
                     )
            ),
            
            # ~ (C) VeDBA ~ GPS Speed Tab
            tabPanel("VeDBA ~ GPS Speed",
                     actionButton("refreshDBA_Sp_plot", "Plot/Refresh Graphs",
                                  style="width:100%; margin-top:20px; font-size:16px;"
                     ),
                     fluidRow(
                       column(width=12,
                              withSpinner(
                                dygraphs::dygraphOutput("vedbaSpeedDygraph", height="500px")
                              )
                       ),
                       br(),
                       column(width=12,
                              br(),
                              h4("(click, box highlight or lasso to select data points)", align="center")
                       ),
                       column(width=6,
                              withSpinner(
                                plotlyOutput("vedbaScatter", height="300px")
                              )
                       ),
                       column(width=6,
                              withSpinner(
                                plotlyOutput("latLonPlot", height="300px")
                              )
                       ),
                       hr(),
                       column(12,
                              h4("Selected Point Row:"),
                              DT::dataTableOutput("selectedPointTable2")
                       ),
                       column(8,
                              radioButtons("manualVerdict2","Change verdict:",
                                           choices=c("Anomalous","Not Anomalous")
                              )
                       ),
                       column(4,
                              actionButton("applyManualVerdict2","Apply New Verdict")
                       ),
                       column(12,
                              h4("Modify/Reapply Verdict Changes:")
                       ),
                       column(6,
                              actionButton("removeOverride2", "Remove selected override (Plotly)",
                                           style="background-color:red; color:red;"
                              )
                       ),
                       column(6,
                              actionButton("reApplyManual2", "Re-Apply Manual Verdicts (Plotly)",
                                           style="background-color:green; color:green;"
                              )
                       ),
                       column(width=12,
                              h4("Current Overrides (Shared with Leaflet)"),
                              DT::dataTableOutput("overridesTable2")
                       ),
                       br(),
                       column(6,
                              fileInput("import_overrides", "Import Overrides (CSV)")
                       ),
                       column(6,
                              downloadButton("export_overrides2", "Export Overrides (CSV)")
                       )
                     )
            ),
            
            # ~ (D) Inspection Histograms
            tabPanel("Inspection Histograms",
                     withSpinner(
                       plotlyOutput("gpsHistograms", height="800px")
                     )
            ),
            
            # ~ (E) Summary Tab
            tabPanel("Summary",
                     fluidRow(
                       tags$div(
                         style="text-align:center;",
                         h4(strong("Quantile Values"), style="color:#555; font-size:14px;")
                       ),
                       column(6,
                              selectInput(
                                "col_for_quantile", "Select numeric column:",
                                choices=NULL
                              )
                       ),
                       column(6,
                              numericInput(
                                "quantile_value", "Quantile (0-1):",
                                min=0, max=1, value=0.99, step=0.001
                              )
                       ),
                       column(12, verbatimTextOutput("quantile_result"))
                     ),
                     tags$div(
                       style="text-align:center;",
                       h4(strong("Descriptive Stats"), style="color:#555; font-size:14px;")
                     ),
                     verbatimTextOutput("summary.proc")
            )
          )
        ),
        
        # === 5) Right Sidebar: Additional Tab Panels (User Thresholds, Expression Builder, IF, R Console)
        sidebarPanel(
          width = 3,
          tabsetPanel(
            id   = "tabsetPanelID3",
            type = "tabs",
            
            # --- (A) User Thresholds
            tabPanel("User Thresholds",
                     h3("GPS Screening: User Thresholds", align="center"),
                     h5("(To Be Used Within Expression builder)", align="center"),
                     
                     tags$div("1) Angle and Speed/Distance Thresholds Between 3 Consecutive Fixes",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("Anglespeed", "Approach/Departure Speeds (m/s):",
                                  value=5, min=0.1, step=0.1
                     ),
                     numericInput("Anglethresh","Absolute Turning Angle (°):",
                                  value=150, min=1, max=180, step=1
                     ),
                     
                     tags$div("2) Calculate distance (m) between raw fixes and the median equivalent",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("Window_length","Rolling Median Distance Window:",
                                  value=5, min=3, step=1
                     ),
                     numericInput("distance_thresh","Median Distance Threshold (m):",
                                  value=100, min=1, step=1
                     ),
                     
                     tags$div("3) GPS error radius (m) under stationary conditions",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("GPSaccuracy","GPS Error Radius (m):",
                                  value=25, min=1, step=1
                     ),
                     
                     tags$div("4) Maximum travel speed (m/s)",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("max_speed","Maximum Speed (m/s):",
                                  value=5, min=0, step=0.1
                     )
            ),
            
            # --- (B) Expression Builder
            tabPanel("Expression Builder",
                     h3("Expression Builder: User Thresholds", align="center"),
                     tags$div(
                       "1) Build 'ifelse()' Statement",
                       style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     # Expression Preview + Validation
                     h4("Preview"),
                     verbatimTextOutput("expressionPreview"),
                     uiOutput("validationResult"),
                     
                     pickerInput(
                       "exprVars","Select Data Variable:",
                       choices = c("Outgoing_speed","Incoming_speed","Ang_vertex",
                                   "Dist_circular","Dist_from_median","mean_VeDBA_interval"),
                       multiple=FALSE,
                       selected="Outgoing_speed",
                       options = list(`actions-box`=TRUE, `live-search`=TRUE)
                     ),
                     actionButton("applyVar","Apply Variable"),
                     br(),
                     
                     # Logical operators
                     fluidRow(
                       tags$div(style="text-align:center;",
                                h4(strong("Select operator(s)"),
                                   style="color:#555; font-size:14px;")),
                       column(2, actionButton("LEFTBRACKET",  " (")),
                       column(2, actionButton("RIGHTBRACKET", " )")),
                       column(2, actionButton("AND",          " & ")),
                       column(2, actionButton("OR",           " | ")),
                       column(2, actionButton("EQUAL",        " == ")),
                       column(2, actionButton("NOTEQUAL",     " != ")),
                       column(2, actionButton("GREATERTHAN",  " > ")),
                       column(2, actionButton("LESSTHAN",     " < ")),
                       column(2, actionButton("GREATERTHANEQUAL", " >= ")),
                       column(2, actionButton("LESSTHANEQUAL",   " <= ")),
                       column(2, actionButton("ISNA",            " is.na("))
                     ),
                     numericInput("quantileValue","Quantile Value (0-1):",value=0.25,min=0,max=1,step=0.01),
                     actionButton("ADD_QUANTILE","Add quantile()"),
                     
                     numericInput("numericValue","Absolute Value:",value=0,min=0,step=0.1),
                     actionButton("ADD_VALUE","Add Absolute Value"),
                     br(),br(),
                     
                     pickerInput(
                       "userThreh","Select Pre-set User Threshold:",
                       choices=c("Approach/Departure Speeds","Absolute Turning Angle",
                                 "Median Distance","GPS Error Radius","Maximum Speed"),
                       multiple=FALSE,selected="Outgoing_speed",
                       options=list(`actions-box`=TRUE, `live-search`=TRUE)
                     ),
                     actionButton("applyThresh","Apply User Threshold"),
                     br(),
                     
                     # Add outcomes
                     h4("Add Outcome"),
                     actionButton("ADD_ANOMALOUS","Add 'Anomalous'",
                                  style="background-color:red; color:red;"
                     ),
                     actionButton("ADD_NOT_ANOMALOUS","Add 'Not Anomalous'",
                                  style="background-color:green; color:green;"
                     ),
                     
                     # Undo + Reset
                     h4("Modify Expression"),
                     actionButton("UNDO","Undo Last Step",
                                  style="background-color:orange; color:orange;"
                     ),
                     actionButton("RESET","Reset Expression",
                                  style="background-color:red; color:red;"
                     ),
                     br(),br(),
                     
                     # Save / List / Delete / Export / Import
                     tags$div("2) Manage Expressions",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     h4("Saved Expressions"),
                     textInput("expressionName","Expression Name:", placeholder="Enter a name"),
                     actionButton("SAVE_EXPRESSION","Save Expression",
                                  style="background-color:green; color:green;"
                     ),
                     actionButton("DELETE_SELECTED_EXPRESSIONS","Delete Selected Expressions",
                                  style="background-color:red; color:red;"
                     ),
                     br(),
                     uiOutput("expressionSelector"),
                     verbatimTextOutput("savedExpressions"),
                     fileInput("importExpressions","Import Expressions", accept=".json"),
                     downloadButton("exportExpressions","Export Expressions"),
                     actionButton("RUN_SELECTED_EXPRESSIONS","Run Selected Saved Expressions",
                                  style="width:100%; margin-top:20px; font-size:16px;"
                     )
            ),
            
            # --- (C) Isolation Forest Thresholds
            tabPanel("Isolation Forest Thresholds",
                     h3("GPS Screening: IF Thresholds", align="center"),
                     tags$div("Isolation Forest (IF)?",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("IFconf","IF Confidence level (0-1):", 0.99, min=0.01, max=1, step=0.01),
                     numericInput("isosamplesize","IF sample size:",256,min=50,step=1),
                     
                     tags$div("Features To Use in the Isolation Forest Computation",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     pickerInput(
                       "isoVars","Select Variables for Isolation Forest:",
                       choices=c("Outgoing_speed","Incoming_speed","Ang_vertex",
                                 "Dist_circular","Dist_from_median","mean_VeDBA_interval"),
                       multiple=TRUE,
                       selected=c("Outgoing_speed","Incoming_speed","Ang_vertex",
                                  "Dist_circular","Dist_from_median","mean_VeDBA_interval"),
                       options=list(`actions-box`=TRUE, `live-search`=TRUE)
                     ),
                     
                     tags$div("Advanced IF Functionality",
                              style="font-size:14px; color:white; background-color:#007bff;
                   text-align:center; padding:5px; font-weight:bold;"
                     ),
                     numericInput("ntry","ntry:",20,min=1,max=500,step=1),
                     numericInput("ntrees","ntrees:",1000,min=10,max=10000,step=1),
                     numericInput("ndim","ndim:",3,min=1,max=5,step=1),
                     numericInput("min_gain","min_gain:",0.25,min=0,max=1,step=0.01),
                     numericInput("prob_pick_pooled_gain","prob_pick_pooled_gain:",0.75,min=0,max=1,step=0.01),
                     checkboxInput("penalize_range","penalize_range?",value=TRUE),
                     actionButton("runIsoForest","Run Isolation Forest",
                                  style="width:100%; margin-top:20px; font-size:16px;"
                     ),
                     tags$a(
                       "View isotree Documentation",
                       href="https://www.rdocumentation.org/packages/isotree/versions/0.6.1-1/topics/isolation.forest",
                       target="_blank"
                     )
            ),
            
            # --- (D) R Console
            tabPanel("R Console",
                     h3("User Script Editor", align="center"),
                     
                     shinyAce::aceEditor(
                       outputId="userScript",
                       mode="r",
                       theme="chrome",
                       height="500px",
                       value="# Example script:\n\n# 'df' refers to the main GPS processed data frame copy\n# Write any transformations here.\n# NOTE: do not remove the 'combined_verdict' column. Modify 'Verdict_user' if needed.\n\n# e.g.\n# df$Verdict_user <- ifelse(...some condition..., 'Anomalous', df$Verdict_user)\n"
                     ),
                     
                     fluidRow(
                       column(12,
                              actionButton("runScript","Run User Script",
                                           style="width:100%; margin-top:20px; font-size:16px;"
                              )
                       ),
                       column(12,
                              actionButton("resetScript","Reset Editor to Example",
                                           style="width:100%; margin-top:20px; font-size:16px;"
                              )
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(12,
                              fileInput("importScript","Import Script (.R or .txt)")
                       ),
                       column(12,
                              downloadButton("exportScript","Export Script")
                       )
                     ),
                     
                     h4("Console Output / Errors:"),
                     verbatimTextOutput("userScriptOutput"),
                     br(),
                     tags$div(
                       style="font-size:12px; color:#555;",
                       "Note: Attempting to remove or rename columns is blocked.",
                       "Use this console for custom transformations, e.g. changing 'Verdict_user' or 'Marked_Events'."
                     )
            )
          ) # end tabsetPanel (id=tabsetPanelID3)
        ) # end sidebarPanel (width=3)
      ) # end fluidPage
    })
    
    ################################################################################
    ##               GPS PROCESSING SERVER LOGIC / OUTPUT OBSERVERS               ##
    ################################################################################
    
    # === A) INPUT VALIDATIONS (Line Width, Point Size, Thresholds) ===============
    
    # 1) GPS Line Width (Raw)
    observeEvent(input$GPS_LW, {
      i <- InputValidator$new()
      i$add_rule("GPS_LW", sv_required("Number must be provided"))
      i$add_rule("GPS_LW", sv_gte(0))
      i$add_rule("GPS_LW", sv_lte(10))
      i$enable()
      req(i$is_valid())
      v$GPS_LW <- input$GPS_LW
    }, ignoreNULL = FALSE)
    
    observeEvent(input$GPS_LW2, {
      i <- InputValidator$new()
      i$add_rule("GPS_LW2", sv_required("Number must be provided"))
      i$add_rule("GPS_LW2", sv_gte(0))
      i$add_rule("GPS_LW2", sv_lte(10))
      i$enable()
      req(i$is_valid())
      v$GPS_LW2 <- input$GPS_LW2
    }, ignoreNULL = FALSE)
    
    # 2) GPS Point Size (Raw)
    observeEvent(input$GPS_PS, {
      i <- InputValidator$new()
      i$add_rule("GPS_PS", sv_required("Number must be provided"))
      i$add_rule("GPS_PS", sv_gte(0))
      i$add_rule("GPS_PS", sv_lte(80))
      i$enable()
      req(i$is_valid())
      v$GPS_PS <- input$GPS_PS
    }, ignoreNULL = FALSE)
    
    observeEvent(input$GPS_PS2, {
      i <- InputValidator$new()
      i$add_rule("GPS_PS2", sv_required("Number must be provided"))
      i$add_rule("GPS_PS2", sv_gte(0))
      i$add_rule("GPS_PS2", sv_lte(80))
      i$enable()
      req(i$is_valid())
      v$GPS_PS2 <- input$GPS_PS2
    }, ignoreNULL = FALSE)
    
    # 3) GPS Line Width (Processed)
    observeEvent(input$GPS_LW_proc, {
      i <- InputValidator$new()
      i$add_rule("GPS_LW_proc", sv_required("Number must be provided"))
      i$add_rule("GPS_LW_proc", sv_gte(0))
      i$add_rule("GPS_LW_proc", sv_lte(10))
      i$enable()
      req(i$is_valid())
      v$GPS_LW_proc <- input$GPS_LW_proc
    }, ignoreNULL = FALSE)
    
    # 4) GPS Point Size (Processed)
    observeEvent(input$GPS_PS_proc, {
      i <- InputValidator$new()
      i$add_rule("GPS_PS_proc", sv_required("Number must be provided"))
      i$add_rule("GPS_PS_proc", sv_gte(0))
      i$add_rule("GPS_PS_proc", sv_lte(80))
      i$enable()
      req(i$is_valid())
      v$GPS_PS_proc <- input$GPS_PS_proc
    }, ignoreNULL = FALSE)
    
    # 5) Scale Bar Width
    observeEvent(input$scalebarwidth, {
      i <- InputValidator$new()
      i$add_rule("scalebarwidth", sv_required("Number must be provided"))
      i$add_rule("scalebarwidth", sv_gte(5))
      i$add_rule("scalebarwidth", sv_lte(1000))
      i$enable()
      req(i$is_valid())
      v$scalebarwidth <- input$scalebarwidth
    }, ignoreNULL = FALSE)
    
    observeEvent(input$scalebarwidth2, {
      i <- InputValidator$new()
      i$add_rule("scalebarwidth2", sv_required("Number must be provided"))
      i$add_rule("scalebarwidth2", sv_gte(5))
      i$add_rule("scalebarwidth2", sv_lte(1000))
      i$enable()
      req(i$is_valid())
      v$scalebarwidth2 <- input$scalebarwidth2
    }, ignoreNULL = FALSE)
    
    # === B) THRESHOLD & PARAMETER INPUTS =========================================
    
    # 1) Burst Length
    observeEvent(input$burstLength, {
      i <- InputValidator$new()
      i$add_rule("burstLength", sv_required("Number must be provided"))
      i$add_rule("burstLength", sv_gte(1))
      i$add_rule("burstLength", sv_lte(60))
      i$enable()
      req(i$is_valid())
      v$burstLength <- input$burstLength
    }, ignoreNULL = FALSE)
    
    # 2) Fix Drop Out
    observeEvent(input$fixDropout, {
      i <- InputValidator$new()
      i$add_rule("fixDropout", sv_required("Number must be provided"))
      i$add_rule("fixDropout", sv_gte(1))
      i$add_rule("fixDropout", sv_lte(100000))
      i$enable()
      req(i$is_valid())
      v$fixDropout <- input$fixDropout
    }, ignoreNULL = FALSE)
    
    # 3) Standardize Time Interval
    observeEvent(input$StandTimeInt, {
      i <- InputValidator$new()
      i$add_rule("StandTimeInt", sv_required("Number must be provided"))
      i$add_rule("StandTimeInt", sv_gte(1))
      i$add_rule("StandTimeInt", sv_lte(1000000))
      i$enable()
      req(i$is_valid())
      v$StandTimeInt <- input$StandTimeInt
    }, ignoreNULL = FALSE)
    
    # 4) Approach/Departure Speeds
    observeEvent(input$Anglespeed, {
      i <- InputValidator$new()
      i$add_rule("Anglespeed", sv_required("Number must be provided"))
      i$add_rule("Anglespeed", sv_gte(0.1))
      i$add_rule("Anglespeed", sv_lte(30))
      i$enable()
      req(i$is_valid())
      v$Anglespeed <- input$Anglespeed
    }, ignoreNULL = FALSE)
    
    # 5) Absolute Turning Angle (°)
    observeEvent(input$Anglethresh, {
      i <- InputValidator$new()
      i$add_rule("Anglethresh", sv_required("Number must be provided"))
      i$add_rule("Anglethresh", sv_gte(1))
      i$add_rule("Anglethresh", sv_lte(180))
      i$enable()
      req(i$is_valid())
      v$Anglethresh <- input$Anglethresh
    }, ignoreNULL = FALSE)
    
    # 6) Rolling Median Distance Window
    observeEvent(input$Window_length, {
      i <- InputValidator$new()
      i$add_rule("Window_length", sv_required("Number must be provided"))
      i$add_rule("Window_length", sv_gte(3))
      i$add_rule("Window_length", sv_lte(1000))
      i$enable()
      req(i$is_valid())
      v$Window_length <- input$Window_length
    }, ignoreNULL = FALSE)
    
    # 7) Median Distance Threshold
    observeEvent(input$distance_thresh, {
      i <- InputValidator$new()
      i$add_rule("distance_thresh", sv_required("Number must be provided"))
      i$add_rule("distance_thresh", sv_gte(1))
      i$add_rule("distance_thresh", sv_lte(10000))
      i$enable()
      req(i$is_valid())
      v$distance_thresh <- input$distance_thresh
    }, ignoreNULL = FALSE)
    
    # 8) GPS Error Radius
    observeEvent(input$GPSaccuracy, {
      i <- InputValidator$new()
      i$add_rule("GPSaccuracy", sv_required("Number must be provided"))
      i$add_rule("GPSaccuracy", sv_gte(1))
      i$add_rule("GPSaccuracy", sv_lte(10000))
      i$enable()
      req(i$is_valid())
      v$GPSaccuracy <- input$GPSaccuracy
    }, ignoreNULL = FALSE)
    
    # 9) Maximum Speed
    observeEvent(input$max_speed, {
      i <- InputValidator$new()
      i$add_rule("max_speed", sv_required("Number must be provided"))
      i$add_rule("max_speed", sv_gte(0))
      i$add_rule("max_speed", sv_lte(10000))
      i$enable()
      req(i$is_valid())
      v$max_speed <- input$max_speed
    }, ignoreNULL = FALSE)
    
    # 10) IF Confidence & Sample Size
    observeEvent(input$IFconf, {
      i <- InputValidator$new()
      i$add_rule("IFconf", sv_required("Number must be provided"))
      i$add_rule("IFconf", sv_gte(0.01))
      i$add_rule("IFconf", sv_lte(1))
      i$enable()
      req(i$is_valid())
      v$IFconf <- input$IFconf
    }, ignoreNULL = FALSE)
    
    observeEvent(input$isosamplesize, {
      i <- InputValidator$new()
      i$add_rule("isosamplesize", sv_required("Number must be provided"))
      i$add_rule("isosamplesize", sv_gte(50))
      i$add_rule("isosamplesize", sv_lte(10000))
      i$enable()
      req(i$is_valid())
      v$isosamplesize <- input$isosamplesize
    }, ignoreNULL = FALSE)
    
    # 11) Isolation Forest Advanced
    observeEvent(input$ntry, {
      i <- InputValidator$new()
      i$add_rule("ntry", sv_required("Number must be provided"))
      i$add_rule("ntry", sv_gte(1))
      i$add_rule("ntry", sv_lte(500))
      i$enable()
      req(i$is_valid())
      v$ntry <- input$ntry
    }, ignoreNULL = FALSE)
    
    observeEvent(input$ntrees, {
      i <- InputValidator$new()
      i$add_rule("ntrees", sv_required("Number must be provided"))
      i$add_rule("ntrees", sv_gte(10))
      i$add_rule("ntrees", sv_lte(10000))
      i$enable()
      req(i$is_valid())
      v$ntrees <- input$ntrees
    }, ignoreNULL = FALSE)
    
    observeEvent(input$ndim, {
      i <- InputValidator$new()
      i$add_rule("ndim", sv_required("Number must be provided"))
      i$add_rule("ndim", sv_gte(1))
      i$add_rule("ndim", sv_lte(5))
      i$enable()
      req(i$is_valid())
      v$ndim <- input$ndim
    }, ignoreNULL = FALSE)
    
    observeEvent(input$min_gain, {
      i <- InputValidator$new()
      i$add_rule("min_gain", sv_required("Number must be provided"))
      i$add_rule("min_gain", sv_gte(0))
      i$add_rule("min_gain", sv_lte(1))
      i$enable()
      req(i$is_valid())
      v$min_gain <- input$min_gain
    }, ignoreNULL = FALSE)
    
    observeEvent(input$prob_pick_pooled_gain, {
      i <- InputValidator$new()
      i$add_rule("prob_pick_pooled_gain", sv_required("Number must be provided"))
      i$add_rule("prob_pick_pooled_gain", sv_gte(0))
      i$add_rule("prob_pick_pooled_gain", sv_lte(1))
      i$enable()
      req(i$is_valid())
      v$prob_pick_pooled_gain <- input$prob_pick_pooled_gain
    }, ignoreNULL = FALSE)
    
    
    # === C) QUANTILE SUMMARY ======================================================
    
    observeEvent(input$quantile_value, {
      i <- InputValidator$new()
      i$add_rule("quantile_value", sv_required("Number must be provided"))
      i$add_rule("quantile_value", sv_gte(0))
      i$add_rule("quantile_value", sv_lte(1))
      i$enable()
      req(i$is_valid())
      v$quantile_value <- input$quantile_value
    }, ignoreNULL = FALSE)
    
    
    # === D) GPS SPEED - RAW & PROCESSED ==========================================
    
    # Raw
    observeEvent(input$speedStepSeconds_R, {
      i <- InputValidator$new()
      i$add_rule("speedStepSeconds_R", sv_required("Number must be provided"))
      i$add_rule("speedStepSeconds_R", sv_gte(1))
      i$add_rule("speedStepSeconds_R", sv_lte(100000))
      i$enable()
      req(i$is_valid())
      v$speedStepSeconds_R <- input$speedStepSeconds_R
    }, ignoreNULL = FALSE)
    
    # Processed
    observeEvent(input$speedStepSeconds_P, {
      i <- InputValidator$new()
      i$add_rule("speedStepSeconds_P", sv_required("Number must be provided"))
      i$add_rule("speedStepSeconds_P", sv_gte(1))
      i$add_rule("speedStepSeconds_P", sv_lte(100000))
      i$enable()
      req(i$is_valid())
      v$speedStepSeconds_P <- input$speedStepSeconds_P
    }, ignoreNULL = FALSE)
    
    # === E) Apply Smoothing ==========================================
    
    # Median smoothing
    observeEvent(input$Med_window, {
      i <- InputValidator$new()
      i$add_rule("Med_window", sv_required("Number must be provided"))
      i$add_rule("Med_window", sv_gte(3))
      i$add_rule("Med_window", function(x) {
        if (x %% 2 != 1){ "Median Smoothing Window must be an odd number" }
      })
      i$enable()
      req(i$is_valid())
      v$Med_window <- input$Med_window
    }, ignoreNULL = FALSE)
    
    # Savitzky-Golay Smoothing
    observeEvent(input$SG_Smooth, {
      i <- InputValidator$new()
      i$add_rule("SG_Smooth", sv_required("Number must be provided"))
      i$add_rule("SG_Smooth", sv_gte(3))
      i$add_rule("SG_Smooth", function(x) {
        if (x %% 2 != 1){ "Savitzky-Golay Window Length must be an odd number" }
      })
      i$enable()
      req(i$is_valid())
      v$SG_Smooth <- input$SG_Smooth
    }, ignoreNULL = FALSE)
    
    # SG filter
    observeEvent(input$SG_order, {
      i <- InputValidator$new()
      i$add_rule("SG_order", sv_required("Number must be provided"))
      i$add_rule("SG_order", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$SG_order <- input$SG_order
    }, ignoreNULL = FALSE)
    

    # === F) Time Residency values ===========================================
   # Fix drop out
    observeEvent(input$dropoutThreshold, {
      i <- InputValidator$new()
      i$add_rule("dropoutThreshold", sv_required("Number must be provided"))
      i$add_rule("dropoutThreshold", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$dropoutThreshold <- input$dropoutThreshold
    }, ignoreNULL = FALSE)
    
    # Grid size
    observeEvent(input$residGridSize, {
      i <- InputValidator$new()
      i$add_rule("residGridSize", sv_required("Number must be provided"))
      i$add_rule("residGridSize", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$residGridSize <- input$residGridSize
    }, ignoreNULL = FALSE)
    
    # Max time cap
    observeEvent(input$maxTimeCap, {
      i <- InputValidator$new()
      i$add_rule("maxTimeCap", sv_required("Number must be provided"))
      i$add_rule("maxTimeCap", sv_gte(0))
      i$enable()
      req(i$is_valid())
      v$maxTimeCap <- input$maxTimeCap
    }, ignoreNULL = FALSE)

    # max revists cap
    observeEvent(input$maxVistsCap, {
      i <- InputValidator$new()
      i$add_rule("maxVistsCap", sv_required("Number must be provided"))
      i$add_rule("maxVistsCap", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$maxVistsCap <- input$maxVistsCap
    }, ignoreNULL = FALSE)
    
    # max speed cap 
    observeEvent(input$maxSpeedCap, {
      i <- InputValidator$new()
      i$add_rule("maxSpeedCap", sv_required("Number must be provided"))
      i$add_rule("maxSpeedCap", sv_gte(0))
      i$enable()
      req(i$is_valid())
      v$maxSpeedCap <- input$maxSpeedCap
    }, ignoreNULL = FALSE)
    
    # max VeDBA cap 
    observeEvent(input$maxVeDCap, {
      i <- InputValidator$new()
      i$add_rule("maxVeDCap", sv_required("Number must be provided"))
      i$add_rule("maxVeDCap", sv_gte(0))
      i$enable()
      req(i$is_valid())
      v$maxVeDCap <- input$maxVeDCap
    }, ignoreNULL = FALSE)
    
    # radius - leaflet heat map 
    observeEvent(input$radius, {
      i <- InputValidator$new()
      i$add_rule("radius", sv_required("Number must be provided"))
      i$add_rule("radius", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$radius <- input$radius
    }, ignoreNULL = FALSE)
    
    # blur - leaflet heat map 
    observeEvent(input$radius, {
      i <- InputValidator$new()
      i$add_rule("blur", sv_required("Number must be provided"))
      i$add_rule("blur", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$blur <- input$blur
    }, ignoreNULL = FALSE)
    
    # max point intensity - leaflet heat map 
    observeEvent(input$maxPI, {
      i <- InputValidator$new()
      i$add_rule("maxPI", sv_required("Number must be provided"))
      i$add_rule("maxPI", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$maxPI <- input$maxPI
    }, ignoreNULL = FALSE)
    
    # Intensity - leaflet heat map 
    observeEvent(input$intensity, {
      i <- InputValidator$new()
      i$add_rule("intensity", sv_required("Number must be provided"))
      i$add_rule("intensity", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$intensity <- input$intensity
    }, ignoreNULL = FALSE)
    
    # KD heat map parameters
    # grid size
    observeEvent(input$gridsize, {
      i <- InputValidator$new()
      i$add_rule("gridsize", sv_required("Number must be provided"))
      i$add_rule("gridsize", sv_gte(1))
      i$enable()
      req(i$is_valid())
      v$gridsize <- input$gridsize
    }, ignoreNULL = FALSE)
    
    # opacity_kd
    observeEvent(input$opacity_kd, {
      i <- InputValidator$new()
      i$add_rule("opacity_kd", sv_required("Number must be provided"))
      i$add_rule("opacity_kd", sv_gte(0))
      i$add_rule("opacity_kd", sv_lte(1))
      i$enable()
      req(i$is_valid())
      v$opacity_kd <- input$opacity_kd
    }, ignoreNULL = FALSE)
    
    # Time-weighted
    # Grid size cells spatial
    observeEvent(input$gridsizeKDW, {
      i <- InputValidator$new()
      i$add_rule("gridsizeKDW", sv_required("Number must be provided"))
      i$add_rule("gridsizeKDW", sv_gte(0))
      i$enable()
      req(i$is_valid())
      v$gridsizeKDW <- input$gridsizeKDW
    }, ignoreNULL = FALSE)
    # Time max KD weighting
    observeEvent(input$maxTimeCapKDW , {
      i <- InputValidator$new()
      i$add_rule("maxTimeCapKDW ", sv_required("Number must be provided"))
      i$add_rule("maxTimeCapKDW ", sv_gte(0))
      i$enable()
      req(i$is_valid())
      v$maxTimeCapKDW <- input$maxTimeCapKDW
    }, ignoreNULL = FALSE)
    
    
    # === G) MAIN GPS PROCESSING ACTION ===========================================
    observeEvent(input$GPSprocessing, {
      # Check if we have valid data loaded + data.chk == TRUE
      req(v$data.chk, filtered_data(), nrow(filtered_data()) > 0, ncol(filtered_data()) > 0)
      
      # 1) Get the data from filtered_data()
      df <- filtered_data()
      
      # 2) Validate required columns exist
      if (!all(c("GPS_Longitude","GPS_Latitude") %in% names(df))) {
        spsComps::shinyCatch({
          stop("Data must contain 'GPS_Longitude' and 'GPS_Latitude' columns", call. = FALSE)
        }, blocking_level="error",
        prefix="WildPulse EcoMove Analytics",
        position="bottom-right"
        )
      }
      
      # Check longitude range
      if (min(df$GPS_Longitude, na.rm=TRUE) < -180 || max(df$GPS_Longitude, na.rm=TRUE) > 180) {
        spsComps::shinyCatch({
          stop("Longitude values must be between -180 and 180 degrees", call. = FALSE)
        }, blocking_level="error",
        prefix="WildPulse EcoMove Analytics",
        position="bottom-right"
        )
      }
      
      # Check latitude range
      if (min(df$GPS_Latitude, na.rm=TRUE) < -90 || max(df$GPS_Latitude, na.rm=TRUE) > 90) {
        spsComps::shinyCatch({
          stop("Latitude values must be between -90 and 90 degrees", call. = FALSE)
        }, blocking_level="error",
        prefix="WildPulse EcoMove Analytics",
        position="bottom-right"
        )
      }
      
      # 3) Replace missing coords of 0 with NA
      df$GPS_Longitude <- ifelse(df$GPS_Longitude == 0, NA, df$GPS_Longitude)
      df$GPS_Latitude  <- ifelse(df$GPS_Latitude  == 0, NA, df$GPS_Latitude)
      
      # Remove rows with missing coords
      df <- df %>% dplyr::filter(!is.na(GPS_Longitude), !is.na(GPS_Latitude))
      
      # Initialize processed columns as duplicates of original
      df$GPS_Longitude_Filtered <- df$GPS_Longitude
      df$GPS_Latitude_Filtered  <- df$GPS_Latitude
      
      # 4) Basic ordering + grouping logic
      df <- df %>%
        arrange(Timestamp) %>%
        mutate(
          Year          = year(Timestamp),
          Month         = month(Timestamp),
          Day           = day(Timestamp),
          Hour          = hour(Timestamp),
          Time_diff     = c(0, as.numeric(diff(Timestamp))),
          Fix_number    = cumsum(c(TRUE, Time_diff[-1] > v$burstLength)),
          Window_group  = cumsum(c(TRUE, Time_diff[-1] >= v$fixDropout)),
          date_only     = as.Date(Timestamp),
          # day_count increments each time date changes
          day_count     = cumsum(c(TRUE, diff(as.integer(date_only)) != 0))
        ) %>%
        group_by(Fix_number) %>%
        mutate(
          orig_burst_length = n(),
          process_burst     = (orig_burst_length <= v$burstLength),
          GPS_SamplingMethod= ifelse(orig_burst_length > v$burstLength, "1Hz", "Variable")
        ) %>%
        ungroup()
      
      # 5) Apply burst processing method
      if (input$burstMethod == "last") {
        df <- df %>%
          group_by(Fix_number) %>%
          mutate(
            GPS_Latitude_Filtered = if_else(
              process_burst & row_number() == n(),
              last(GPS_Latitude),
              if_else(process_burst, NA_real_, GPS_Latitude_Filtered)
            ),
            GPS_Longitude_Filtered = if_else(
              process_burst & row_number() == n(),
              last(GPS_Longitude),
              if_else(process_burst, NA_real_, GPS_Longitude_Filtered)
            )
          ) %>%
          ungroup()
        
      } else if (input$burstMethod %in% c("mean","median")) {
        
        df <- df %>%
          group_by(Fix_number) %>%
          mutate(
            middle_index         = ceiling(n()/2),
            GPS_Latitude_Filtered = if_else(
              process_burst & row_number() == middle_index,
              if (input$burstMethod=="mean") mean(GPS_Latitude) else median(GPS_Latitude),
              if_else(process_burst, NA_real_, GPS_Latitude_Filtered)
            ),
            GPS_Longitude_Filtered = if_else(
              process_burst & row_number() == middle_index,
              if (input$burstMethod=="mean") mean(GPS_Longitude) else median(GPS_Longitude),
              if_else(process_burst, NA_real_, GPS_Longitude_Filtered)
            )
          ) %>%
          ungroup() %>%
          dplyr::select(-middle_index)
        
      } else if (input$burstMethod == "none") {
        
        df$GPS_Longitude_Filtered <- df$GPS_Longitude
        df$GPS_Latitude_Filtered  <- df$GPS_Latitude
      }
      
      # 6) Standardize GPS intervals if requested
      if (input$StandTimeIntLog == "yes") {
        
        StandTimeInt <- v$StandTimeInt
        
        # (a) "all" or "exc.1Hz"
        if (input$StandTimeIntUniv %in% c("all","exc.1Hz")) {
          
          if (input$StandTimeIntUniv == "all") {
            df_filtered <- df %>%
              dplyr::filter(!is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered))
          } else {  # exc.1Hz
            df_filtered <- df %>%
              dplyr::filter(!is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered) &
                     GPS_SamplingMethod=="Variable")
          }
          
          stepped <- getTimeSteppedDataFast(
            df_filtered,
            stepSeconds = StandTimeInt,
            latCol      = "GPS_Latitude_Filtered",
            lonCol      = "GPS_Longitude_Filtered",
            keepFirst   = TRUE,
            keepLast    = TRUE
          )
          
          # Merge the stepped fixes back
          df <- df %>%
            left_join(
              stepped %>% dplyr::select(Timestamp, GPS_Longitude_Filtered, GPS_Latitude_Filtered),
              by="Timestamp", suffix=c("","_new")
            ) %>%
            mutate(
              GPS_Longitude_Filtered = ifelse(!is.na(GPS_Longitude_Filtered_new),
                                              GPS_Longitude_Filtered_new, NA),
              GPS_Latitude_Filtered  = ifelse(!is.na(GPS_Latitude_Filtered_new),
                                              GPS_Latitude_Filtered_new, NA)
            ) %>%
            dplyr::select(-GPS_Longitude_Filtered_new, -GPS_Latitude_Filtered_new)
          
          # For "exc.1Hz", restore the original 1Hz coords
          if (input$StandTimeIntUniv == "exc.1Hz") {
            df <- df %>%
              mutate(
                GPS_Longitude_Filtered = ifelse(
                  GPS_SamplingMethod=="1Hz" & !is.na(GPS_Longitude),
                  GPS_Longitude, GPS_Longitude_Filtered
                ),
                GPS_Latitude_Filtered = ifelse(
                  GPS_SamplingMethod=="1Hz" & !is.na(GPS_Latitude),
                  GPS_Latitude, GPS_Latitude_Filtered
                )
              )
          }
        }
        
        # (b) "Hz" only
        if (input$StandTimeIntUniv == "Hz") {
          df_filtered <- df %>%
            dplyr::filter(!is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered) &
                   GPS_SamplingMethod == "1Hz") %>%
            group_by(Fix_number) %>%
            mutate(
              cumulative_time      = cumsum(c(0, diff(as.numeric(Timestamp)))),
              interval_marker      = floor(cumulative_time / StandTimeInt),
              time_diff_to_interval= abs(cumulative_time - interval_marker * StandTimeInt)
            ) %>%
            arrange(interval_marker, time_diff_to_interval) %>%
            group_by(Fix_number, interval_marker) %>%
            slice_head(n=1) %>%
            ungroup()
        
          # Retain original coordinates for non-1 Hz groups
          df <- df %>%
            mutate(
              GPS_Longitude_Filtered = ifelse(
                GPS_SamplingMethod == "Variable", GPS_Longitude_Filtered, NA
              ),
              GPS_Latitude_Filtered = ifelse(
                GPS_SamplingMethod == "Variable", GPS_Latitude_Filtered, NA
              )
            ) 
          
          # Merge stepped data
          df <- df %>%
            left_join(
              df_filtered %>% dplyr::select(Timestamp, GPS_Longitude_Filtered, GPS_Latitude_Filtered),
              by="Timestamp", suffix=c("","_new")
            ) %>%
            mutate(
              GPS_Longitude_Filtered = ifelse(!is.na(GPS_Longitude_Filtered_new),
                                              GPS_Longitude_Filtered_new, GPS_Longitude_Filtered),
              GPS_Latitude_Filtered  = ifelse(!is.na(GPS_Latitude_Filtered_new),
                                              GPS_Latitude_Filtered_new, GPS_Latitude_Filtered)
            ) %>%
            dplyr::select(-GPS_Longitude_Filtered_new, -GPS_Latitude_Filtered_new)
        }
        
        # Recompute Fix_number to reflect new data
        df <- df %>%
          mutate(Fix_number = cumsum(!is.na(GPS_Longitude_Filtered)))
      }
      
      # 7) If IMU_Mean_VeDBA was present in the original, remove it now (?)
      if ("IMU_Mean_VeDBA" %in% colnames(df)) {
        df <- df %>% dplyr::select(-IMU_Mean_VeDBA)
      }
      
      # 8) Additional VeDBA logic if user selected "VeDInt == yes" + IMU data present
      if (input$VeDInt=="yes" && "IMU_Mean_VeDBA" %in% colnames(filtered_data())) {
        imu_data <- filtered_data() %>%
          dplyr::filter(!is.na(IMU_Mean_VeDBA))
        
        if (nrow(imu_data)==0) {
          spsComps::shinyCatch({
            stop("IMU data is missing or not properly selected", call.=FALSE)
          }, blocking_level="error",
          prefix="WildPulse EcoMove Analytics",
          position="bottom-right")
        }
        
        ACC_TS <- imu_data$Timestamp
        VeDBA  <- imu_data$IMU_Mean_VeDBA
        
        # Create an "acc_df" to insert into df
        acc_df <- data.frame(matrix(NA, nrow=length(ACC_TS), ncol=ncol(df)))
        colnames(acc_df) <- colnames(df)
        
        acc_df$Timestamp <- ACC_TS
        acc_df$VeDBA     <- VeDBA
        
        # Combine + reorder
        df <- bind_rows(df, acc_df) %>%
          arrange(Timestamp)
        
        # fix_group: increments for each valid fix
        df <- df %>%
          mutate(
            fix_group = cumsum(!is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered))
          )
        
        # Reverse for backward interval computations
        df_reversed <- df %>%
          arrange(desc(Timestamp)) %>%
          mutate(
            fix_group = cumsum(!is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered))
          ) %>%
          group_by(fix_group) %>%
          mutate(
            mean_VeDBA_interval = ifelse(
              !is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered),
              mean(VeDBA, na.rm=TRUE), NA
            ),
            sd_VeDBA_interval = ifelse(
              !is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered),
              sd(VeDBA, na.rm=TRUE), NA
            ),
            num_VeDBA_bursts = ifelse(
              !is.na(GPS_Longitude_Filtered) & !is.na(GPS_Latitude_Filtered),
              sum(!is.na(VeDBA)), NA
            )
          ) %>%
          ungroup() %>%
          arrange(Timestamp) %>%
          dplyr::select(-fix_group)
        
        # Remove rows that are pure IMU only (have no GPS data?)
        df <- subset(df_reversed, !is.na(Observations))
        df$VeDBA <- NULL  # remove that temporary column
      } else {
        # If no IMU data or user didn't request it
        showNotification("IMU data is missing or not properly selected", type="warning")
      }
      
      # ----------------------------------------------------------------------------------
      # Track Smoothing
      # ----------------------------------------------------------------------------------
      # store a copy as the unsmoothed baseline:
        v$gpsFilteredOriginal <- df
      
      # Apply Smoothing Using the Original Data
        # Create a logical mask indicating which fixes are eligible:
        # If the user opts for "only 1Hz", then smooth only rows where GPS_SamplingMethod is "1Hz".
        # Otherwise, set all rows as eligible.
      # --- (8) Build the smoothing mask:
      if (isTRUE(input$SmoothOnly1Hz)) {
        smooth_mask <- !is.na(df$GPS_Longitude_Filtered) & (df$GPS_SamplingMethod == "1Hz")
      } else {
        smooth_mask <- !is.na(df$GPS_Longitude_Filtered)
      }
        
      # --- (9) Always start from the unsmoothed baseline:
      data_smoothed <- v$gpsFilteredOriginal
      
      # --- (10) Apply the chosen smoothing method:
      if (input$TrackSmoothing == "Med_window") {
        # Check that there are enough eligible points
        if (length(na.omit(data_smoothed$GPS_Longitude_Filtered)) < input$Med_window) {
          showNotification("Smoothing window too large.", type = "warning")
          return(NULL)
        }
        
        if (isTRUE(input$SmoothOnly1Hz)) {
        data_smoothed$GPS_Longitude_Filtered <- apply_median_smoothing_1Hz(
          data_smoothed$GPS_Longitude_Filtered, 
          window = input$Med_window, 
          subset = smooth_mask
        )
        data_smoothed$GPS_Latitude_Filtered  <- apply_median_smoothing_1Hz(
          data_smoothed$GPS_Latitude_Filtered, 
          window = input$Med_window, 
          subset = smooth_mask
        )
        } else {
          data_smoothed$GPS_Longitude_Filtered <- apply_median_smoothing(
            data_smoothed$GPS_Longitude_Filtered, 
            window = input$Med_window, 
            subset = smooth_mask
          )
          data_smoothed$GPS_Latitude_Filtered  <- apply_median_smoothing(
            data_smoothed$GPS_Latitude_Filtered, 
            window = input$Med_window, 
            subset = smooth_mask
          )
        }
          
      } else if (input$TrackSmoothing == "SG_Smooth") {
        if (length(na.omit(data_smoothed$GPS_Longitude_Filtered)) < input$SG_window) {
          showNotification("Smoothing window too large.", type = "warning")
          return(NULL)
        }
        if (input$SG_order >= input$SG_window) {
          showNotification("SG window length must be larger than the polynomial order.", type = "warning")
          return(NULL)
        }
        if (isTRUE(input$SmoothOnly1Hz)) {
        data_smoothed$GPS_Longitude_Filtered <- apply_sgolay_smoothing_1Hz(
          data_smoothed$GPS_Longitude_Filtered, 
          p = input$SG_order, 
          n = input$SG_window, 
          subset = smooth_mask
        )
        data_smoothed$GPS_Latitude_Filtered  <- apply_sgolay_smoothing_1Hz(
          data_smoothed$GPS_Latitude_Filtered, 
          p = input$SG_order, 
          n = input$SG_window, 
          subset = smooth_mask
        )
        } else {
          data_smoothed$GPS_Longitude_Filtered <- apply_sgolay_smoothing(
            data_smoothed$GPS_Longitude_Filtered, 
            p = input$SG_order, 
            n = input$SG_window, 
            subset = smooth_mask
          )
          data_smoothed$GPS_Latitude_Filtered  <- apply_sgolay_smoothing(
            data_smoothed$GPS_Latitude_Filtered, 
            p = input$SG_order, 
            n = input$SG_window, 
            subset = smooth_mask
          )
        }
      } else if (input$TrackSmoothing == "KF_Smooth") {
        data_smoothed <- apply_kalman_filter_complex(
          data_smoothed,
          procNoise = input$KF_procNoise,
          measNoise_pos = input$KF_measNoise_pos,
          measNoise_speed = input$KF_measNoise_speed,
          KF_scaleFactor = input$KF_scaleFactor,
          initialCov = diag(1,4),  # or input$KF_initialCov if provided
          useVeDBA = input$KF_includeVeDBA
        )
        data_smoothed  # now contains updated smoothed GPS_Longitude_Filtered and GPS_Latitude_Filtered
      }
      
      # --- (11) Update the processed data with the newly smoothed values:
      gpsProcessedData(data_smoothed)
      
      # --- (12) Now, for the subsequent calculations, work on the smoothed data:
      df <- data_smoothed
      
      # (Proceed with computing speeds, turning angles, etc.)
      df$Time_diff_filtered <- NA
      df$Ang_vertex         <- NA
      df$Outgoing_speed     <- NA
      df$Incoming_speed     <- NA
      df$Dist_circular      <- NA
      df$Dist_from_median   <- NA
      
      # Prepare verdicts
      df$Verdict_user <- NA
      df$Verdict_IF   <- NA
      
      df_subset <- df %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered)) %>%
        dplyr::rename(Longitude = GPS_Longitude_Filtered, Latitude = GPS_Latitude_Filtered) %>%
        mutate(Time_diff = c(0, as.numeric(difftime(Timestamp, dplyr::lag(Timestamp), units="secs")[-1])),
               Longitude.lag  = dplyr::lag(Longitude, 1, default=NA),
               Latitude.lag   = dplyr::lag(Latitude, 1, default=NA),
               Longitude.lead = dplyr::lead(Longitude, 1, default=NA),
               Latitude.lead  = dplyr::lead(Latitude, 1, default=NA),
               Ang.lag  = beary(Longitude, Latitude, Longitude.lag, Latitude.lag),
               Ang.lead = beary(Longitude, Latitude, Longitude.lead, Latitude.lead),
               Ang.lag  = ifelse(Ang.lag < 0, Ang.lag + 360, Ang.lag),
               Ang.lead = ifelse(Ang.lead < 0, Ang.lead + 360, Ang.lead),
               Ang.vertex = abs((Ang.lead - Ang.lag + 360) %% 360 - 180),
               Dist.lag   = disty(Longitude, Latitude, Longitude.lag, Latitude.lag),
               Dist.lead  = disty(Longitude, Latitude, Longitude.lead, Latitude.lead),
               Time.diff.lag  = as.numeric(difftime(Timestamp, dplyr::lag(Timestamp), units="secs")),
               Time.diff.lead = dplyr::lead(Time.diff.lag, 1, default=NA),
               Outgoing.speed = Dist.lag / Time.diff.lag,
               Incoming.speed = Dist.lead / Time.diff.lead,
               Dist_circular  = disty(Longitude.lag, Latitude.lag, Longitude.lead, Latitude.lead)
        )
      
      
      # Rolling median
      df_subset <- df_subset %>%
        group_by(Window_group) %>%
        mutate(Window_group_length = max(sequence(rle(Window_group)$lengths))) %>%
        ungroup()
      
      # If group length < user Window_length, shorten
      df_subset$Window_group_length <- ifelse(
        df_subset$Window_group_length >= v$Window_length,
        v$Window_length, df_subset$Window_group_length
      )
      
      # rolling median calc
      df_subset <- df_subset %>%
        group_by(Window_group) %>%
        mutate(win = first(Window_group_length),
               Longitude_med = if (first(win) < 3) NA else optimized_roll_median(Longitude, window = first(win)),
               Latitude_med  = if (first(win) < 3) NA else optimized_roll_median(Latitude, window = first(win))
        ) %>%
        ungroup() %>%
        tidyr::fill(Longitude_med, .direction = "updown") %>%
        tidyr::fill(Latitude_med, .direction = "updown")
      
      df_subset$Dist_from_median <- with(
        df_subset,
        disty(Longitude, Latitude, Longitude_med, Latitude_med)
      )
      
      # Merge these computed columns back
      df$Time_diff_filtered[!is.na(df$GPS_Latitude_Filtered)] <- df_subset$Time_diff
      df$Ang_vertex[!is.na(df$GPS_Latitude_Filtered)]         <- df_subset$Ang.vertex
      df$Outgoing_speed[!is.na(df$GPS_Latitude_Filtered)]     <- df_subset$Outgoing.speed
      df$Incoming_speed[!is.na(df$GPS_Latitude_Filtered)]     <- df_subset$Incoming.speed
      df$Dist_circular[!is.na(df$GPS_Latitude_Filtered)]      <- df_subset$Dist_circular
      df$Dist_from_median[!is.na(df$GPS_Latitude_Filtered)]   <- df_subset$Dist_from_median
      
      # Replace NaN with NA
      df <- df %>%
        mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
      
  
      # 10) Finally update the gpsProcessedData
      gpsProcessedData(df)
      
    })
    
    ################################################################################
    ##                        ISOLATION FOREST & EXPRESSION BUILDER               ##
    ################################################################################
    
    # === A) ISOLATION FOREST CONFIG & EXECUTION ==================================
    
    observe({
      req(gpsProcessedData())  # Ensure main processed data exists
      
      # Only show user the subset of possible variables that exist in gpsProcessedData
      available_vars <- c(
        "Outgoing_speed", "Incoming_speed", "Ang_vertex", 
        "Dist_circular", "Dist_from_median", "mean_VeDBA_interval"
      )
      current_vars <- intersect(available_vars, colnames(gpsProcessedData()))
      
      updatePickerInput(session, "isoVars", choices=current_vars, selected=current_vars)
    })
    
    observeEvent(input$runIsoForest, {
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)  # Must have valid data
      
      # 1) Filter for valid GPS coords
      df <- gpsProcessedData() %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered))
      
      # 2) User-selected variables
      user_selected_vars <- input$isoVars
      available_vars     <- intersect(user_selected_vars, names(df))
      
      if (length(available_vars)<1) {
        showNotification(
          "No valid variables selected for Isolation Forest. Please adjust your selection.",
          type="error"
        )
        return(NULL)
      }
      
      # 3) Subset data, dropping columns with only NA
      df_sub_data <- df[, available_vars, drop=FALSE] %>%
        select_if(~any(!is.na(.)))
      
      if (ncol(df_sub_data) < 1) {
        showNotification("Not enough variables for Isolation Forest. Please check your input.", type="error")
        return(NULL)
      }
      
      if (v$ndim > length(available_vars)) {
        showNotification("'ndim' is larger than number of valid variables selected. Adjust selection.",
                         type="error"
        )
        return(NULL)
      }
      
      # 4) Build Isolation Forest model
      iforest_model <- isotree::isolation.forest(
        df_sub_data,
        ntry             = v$ntry,
        ndim             = v$ndim,
        ntrees           = v$ntrees,
        sample_size      = min(nrow(df), v$isosamplesize),
        min_gain         = v$min_gain,
        prob_pick_pooled_gain = v$prob_pick_pooled_gain,
        standardize_data = TRUE,
        missing_action   = "impute",
        penalize_range   = input$penalize_range
      )
      
      # 5) Predict anomaly scores
      anomaly_scores <- predict(iforest_model, df_sub_data, type="score")
      
      # 6) Append anomaly scores + set verdict
      df$IF_anomaly_score <- anomaly_scores
      cutoff <- quantile(anomaly_scores, probs=v$IFconf, na.rm=TRUE)
      df$Verdict_IF <- ifelse(anomaly_scores >= cutoff, "Anomalous", "Not Anomalous")
      
      # 7) Merge new IF verdict into main data
      processed <- gpsProcessedData()
      
      merged <- processed %>%
        dplyr::select(-any_of("Verdict_IF")) %>%
        left_join(df[, c("Observations","Verdict_IF")], by="Observations") %>%
        dplyr::relocate(Verdict_IF, .after="Verdict_user")
      
      gpsProcessedData(merged)  # forcibly update main data
    })
    
    
    ################################################################################
    ##                        B) EXPRESSION BUILDER LOGIC                         ##
    ################################################################################
    
    observe({
      req(gpsProcessedData())  # Ensure data is present
      
      # Offer only columns that actually exist
      available_vars <- c(
        "Outgoing_speed","Incoming_speed","Ang_vertex",
        "Dist_circular","Dist_from_median","mean_VeDBA_interval"
      )
      current_vars <- intersect(available_vars, colnames(gpsProcessedData()))
      
      updatePickerInput(session, "exprVars", choices=current_vars, selected=current_vars)
    })
    
    appendToExpression <- function(content) {
      expr <- currentExpression()
      expr <- paste0(expr, content)
      currentExpression(expr)
      
      # Track expression history
      history <- expressionHistory()
      expressionHistory(append(history, expr))
    }
   
    # --- 1)  'applyVar' button: Insert selected variable name into expression
    observeEvent(input$applyVar, {
      appendToExpression(input$exprVars)
    })
    
    # --- 2)  'applyThresh' button: Insert user-chosen threshold (e.g. max_speed)
    observeEvent(input$applyThresh, {
      
      thresholdMapping <- list(
        "Approach/Departure Speeds" = v$Anglespeed,
        "Absolute Turning Angle"     = v$Anglethresh,
        "Median Distance"            = v$distance_thresh,
        "GPS Error Radius"           = v$GPSaccuracy,
        "Maximum Speed"              = v$max_speed
      )
      
      selectedThreshold <- input$userThreh
      if (!is.null(selectedThreshold) && selectedThreshold %in% names(thresholdMapping)) {
        selectedValue <- as.numeric(thresholdMapping[[selectedThreshold]])
        appendToExpression(selectedValue)
      } else {
        showNotification("Invalid threshold selected.", type="error")
      }
    })
    
    # --- 3)  Insert operators
    observeEvent(input$LEFTBRACKET,     { appendToExpression(" (") })
    observeEvent(input$RIGHTBRACKET,    { appendToExpression(" )") })
    observeEvent(input$AND,            { appendToExpression(" & ") })
    observeEvent(input$OR,             { appendToExpression(" | ") })
    observeEvent(input$EQUAL,          { appendToExpression(" == ") })
    observeEvent(input$NOTEQUAL,       { appendToExpression(" != ") })
    observeEvent(input$GREATERTHAN,    { appendToExpression(" > ") })
    observeEvent(input$LESSTHAN,       { appendToExpression(" < ") })
    observeEvent(input$GREATERTHANEQUAL, { appendToExpression(" >= ") })
    observeEvent(input$LESSTHANEQUAL,    { appendToExpression(" <= ") })
    observeEvent(input$ISNA,           { appendToExpression(" is.na(") })
    
    # --- 4)  Insert quantile() call
    observeEvent(input$ADD_QUANTILE, {
      q_expr <- paste0("quantile(", input$exprVars, ", ",
                       input$quantileValue, ", na.rm=TRUE)")
      appendToExpression(q_expr)
    })
    
    # --- 5)  Insert numeric value
    observeEvent(input$ADD_VALUE, {
      numeric_val <- input$numericValue
      appendToExpression(numeric_val)
    })
    
    # --- 6)  Insert outcomes
    observeEvent(input$ADD_ANOMALOUS, {
      appendToExpression(", 'Anomalous', Verdict_user)")
    })
    observeEvent(input$ADD_NOT_ANOMALOUS, {
      appendToExpression(", 'Not Anomalous', Verdict_user)")
    })
    
    # --- 7)  Undo last step
    observeEvent(input$UNDO, {
      histy <- expressionHistory()
      if (length(histy) > 1) {
        histy <- histy[-length(histy)]   # drop last
        currentExpression(histy[[length(histy)]])
        expressionHistory(histy)
      }
    })
    
    # --- 8)  Reset expression
    observeEvent(input$RESET, {
      currentExpression("ifelse(")
      expressionHistory(list("ifelse("))
    })
    
    # --- 9)  Preview expression
    output$expressionPreview <- renderText({
      paste0("df$Verdict_user <- with(df, ", currentExpression(), ")")
    })
    
    # --- 10) Save expression under a named slot
    observeEvent(input$SAVE_EXPRESSION, {
      nm <- input$expressionName
      if (nm=="" || is.null(nm)) {
        showNotification("Please provide a name for the expression.", type="error")
        return()
      }
      expr <- paste0("df$Verdict_user <- with(df, ", currentExpression(), ")")
      
      saved_list <- savedExpressions()
      saved_list[[nm]] <- expr
      savedExpressions(saved_list)
    })
    
    # --- 11) Display saved expressions
    output$savedExpressions <- renderText({
      exprs <- savedExpressions()
      paste(names(exprs), ":", unlist(exprs), collapse="\n")
    })
    
    # --- 12) Validate expression
    output$validationResult <- renderUI({
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      df <- gpsProcessedData()
      
      # Construct code snippet
      expr_code <- paste0("df$Verdict_user <- with(df, ", currentExpression(), ")")
      
      tryCatch({
        eval(parse(text=expr_code), envir=list(data=df))
        HTML("<span style='color:green;'>Expression is valid.</span>")
      }, error=function(e) {
        HTML(paste("<span style='color:red;'>Invalid expression:</span>", e$message))
      })
    })
    
    # --- 13) Expression Selector
    output$expressionSelector <- renderUI({
      expr_list <- savedExpressions()
      if (length(expr_list)==0) return(NULL)
      
      checkboxGroupInput(
        "selectedExpressions","Select Expressions to Run:",
        choices=names(expr_list),
        selected=names(expr_list)
      )
    })
    
    # --- 14) Delete selected expressions
    observeEvent(input$DELETE_SELECTED_EXPRESSIONS, {
      sel <- input$selectedExpressions
      if (!is.null(sel)) {
        current <- savedExpressions()
        current <- current[!names(current) %in% sel]
        savedExpressions(current)
      }
    })
    
    # --- 15) Run selected expressions
    observeEvent(input$RUN_SELECTED_EXPRESSIONS, {
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      saved_list <- savedExpressions()
      
      if (length(saved_list)==0) {
        return(NULL)
      }
      
      # We'll subset the existing data
      df <- gpsProcessedData() %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered))
      
      sel <- input$selectedExpressions
      
      # If no expressions selected
      if (is.null(sel) || length(sel)==0) {
        showNotification("No expressions selected to run.", type="warning")
        return(NULL)
      }
      
      # Reset to default if no expressions are selected
      df$Verdict_user <- NA
      
      # Apply each expression in turn
      for (nm in sel) {
        expr_code <- saved_list[[nm]]
        tryCatch({
          df$Verdict_user <- eval(parse(text=paste0("with(df, ", expr_code, ")")))
        }, error=function(e) {
          showNotification(paste("Error in evaluating expression", nm, ":", e$message), type="error")
        })
      }
      
      # Store the results in userThreshResults
      df_reduced <- df[, c("Observations","Verdict_user")]
      userThreshResults(df_reduced)
      
      # Merge into main dataset
      processed <- gpsProcessedData()
      merged <- processed %>%
        dplyr::select(-any_of("Verdict_user")) %>%
        left_join(df_reduced, by="Observations") %>%
        dplyr::relocate(Verdict_user, .before="Verdict_IF")
      
      gpsProcessedData(merged)
    })
    
    
    ################################################################################
    ##              C) RENDER PROCESSED GPS TABLE (gpsDataTable)                  ##
    ################################################################################
    
    output$gpsDataTable <- DT::renderDataTable({
      # If no processed data or empty, show message
      if (is.null(gpsProcessedData()) || nrow(gpsProcessedData())==0) {
        return(DT::datatable(data.frame(Message="Need to process GPS data for table to render")))
      }
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      df_processed <- gpsProcessedData()
      
      # Datatable configuration
      scroller <- input$scroller
      if (scroller) {
        datatable_options <- rv3$options
      } else {
        datatable_options <- rv2$options
      }
      datatable_options$scrollY <- input$tableHeight
      
      # Undersample vs full display
      if (input$disp=="all") {
        DT::datatable(
          df_processed,
          options     = datatable_options,
          selection   ='single',
          filter      ='top',
          extensions  = c("ColReorder","FixedColumns","FixedHeader","Scroller"),
          rownames    =FALSE
        ) %>%
          formatRound(columns = c(which(sapply(df_processed, is.numeric))), v$num.digits) %>%
          formatStyle(columns = c(1:ncol(df_processed)), 'text-align'='center') %>%
          formatDate("Timestamp", method='toLocaleString')
        
      } else if (input$disp=="undersample") {
        
        i <- InputValidator$new()
        i$add_rule("unders", sv_required("Number must be provided"))
        i$add_rule("unders", sv_gte(1))
        i$enable()
        req(i$is_valid())
        
        v$undersample <- as.integer(input$unders)
        
        # Slice every nth row
        display_data <- df_processed %>%
          dplyr::slice(seq(1, n(), by=v$undersample))
        
        DT::datatable(
          display_data,
          options     = datatable_options,
          selection   ='single',
          filter      ='top',
          extensions  = c("ColReorder","FixedColumns","FixedHeader","Scroller"),
          rownames    =FALSE
        ) %>%
          formatRound(columns=which(sapply(display_data, is.numeric)), v$num.digits) %>%
          formatStyle(columns=c(1:ncol(df_processed)), 'text-align'='center') %>%
          formatDate("Timestamp", method='toLocaleString')
      }
    })
    
    
    ################################################################################
    ##                   D) INSPECTION HISTOGRAMS (Plotly)                         ##
    ################################################################################
    
    output$gpsHistograms <- renderPlotly({
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      df <- gpsProcessedData()
      
      # Columns we want histograms for
      available_cols <- c(
        "Ang_vertex","Outgoing_speed","Dist_circular",
        "Dist_from_median","mean_VeDBA_interval"
      )
      # Only keep columns that exist and are not all NA
      plot_cols <- intersect(available_cols, names(df))
      plot_cols <- plot_cols[sapply(plot_cols, function(col) any(!is.na(df[[col]])))]
      
      if (length(plot_cols)==0) {
        showNotification("No valid data available for histograms.", type="warning")
        return(NULL)
      }
      
      # Define quantile lines
      quantile_lines <- list(
        "1%"  = list(color="red", dash="dash"),
        "5%"  = list(color="green", dash="dash"),
        "95%" = list(color="green", dash="dash"),
        "99%" = list(color="red", dash="dash")
      )
      
      # Create separate histogram plots for each variable
      hist_plots <- lapply(plot_cols, function(col) {
        data_vec   <- df[[col]]
        quant_vals <- quantile(data_vec, probs=c(0.01,0.05,0.95,0.99), na.rm=TRUE)
        
        # Create histogram bin edges (e.g. Freedman–Diaconis)
        hist_data <- hist(data_vec, breaks="FD", plot=FALSE)
        bins      <- hist_data$breaks
        
        # Build base histogram
        p <- plot_ly(
          x       = data_vec,
          type    = "histogram",
          name    = col,
          marker  = list(color="blue", line=list(width=1, color="black")),
          autobinx= FALSE,
          xbins   = list(start=min(bins), end=max(bins), size=diff(bins)[1])
        )
        
        # Add each quantile line
        for (q_name in names(quantile_lines)) {
          q_val   <- quant_vals[which(names(quantile_lines)==q_name)]
          q_style <- quantile_lines[[q_name]]
          p <- p %>%
            add_lines(
              x = c(q_val,q_val),
              y = c(0, max(hist_data$counts)),
              name = q_name,
              line = list(color=q_style$color, dash=q_style$dash),
              showlegend=FALSE
            )
        }
        
        # Layout adjustments
        p <- p %>%
          layout(
            title = paste("Histogram of", col),
            xaxis = list(
              title = paste(col,"(x-axis)"),
              range = c(0, max(data_vec, na.rm=TRUE)*1.1)
            ),
            yaxis   = list(title="Frequency"),
            bargap  = 0.05,
            showlegend=FALSE
          )
        
        p
      })
      
      # Combine them in a grid
      combined_plot <- subplot(
        hist_plots,
        nrows = ceiling(sqrt(length(hist_plots))),
        margin=0.05
      )
      
      # Single legend for quantiles
      combined_plot %>%
        layout(
          title="Interactive Histograms with Quantiles",
          showlegend=FALSE,
          legend=list(
            orientation="h",
            x=0.5,
            y=-0.1,
            xanchor="center",
            yanchor="top"
          ),
          annotations=list(
            list(
              text=paste0("<b>Legend:</b><br>",
                          "<span style='color:red;'>1%, 99%</span>: Red Dash Lines<br>",
                          "<span style='color:green;'>5%, 95%</span>: Green Dash Lines"),
              showarrow=FALSE,
              xref="paper",
              yref="paper",
              x=0.5,
              y=-0.15,
              font=list(size=12),
              align="center"
            )
          )
        )
    })
    
    
    ################################################################################
    ##            E) SUMMARY STATISTICS & QUANTILE SELECTION LOGIC                ##
    ################################################################################
    
    # 1) Summaries (Text-based) for processed data
    output$summary.proc <- renderPrint({
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      summary(gpsProcessedData())
    })
    
    # 2) Update numeric column choice whenever gpsProcessedData changes
    observe({
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      df <- gpsProcessedData()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      if (length(numeric_cols)==0) numeric_cols <- character(0)
      
      old_sel <- isolate(input$col_for_quantile)
      new_sel <- ""
      if (length(numeric_cols)>0) {
        if (!is.null(old_sel) && old_sel %in% numeric_cols) {
          new_sel <- old_sel
        } else {
          new_sel <- numeric_cols[1]
        }
      }
      updateSelectInput(
        session, "col_for_quantile",
        choices = numeric_cols,
        selected= new_sel
      )
    })
    
    # 3) Recompute & display quantile based on user-chosen column & quantile_value
    observe({
      req(gpsProcessedData())
      req(input$col_for_quantile, input$col_for_quantile != "")
      req(v$quantile_value)
      
      df <- gpsProcessedData()
      numeric_vec <- df[[ input$col_for_quantile ]]
      
      q_val <- quantile(numeric_vec, probs=v$quantile_value, na.rm=TRUE)
      
      output$quantile_result <- renderPrint({
        cat("Quantile", v$quantile_value, "of",
            input$col_for_quantile, "=", q_val)
      })
      
    })
    
    ################################################################################
    ##                    GPS PROCESSING MAP: TIME FILTERS & RENDERING            ##
    ################################################################################
    
    # === A) TIME-RELATED FILTERS (SLIDERS & PICKERS) =============================
    
    observe({
      # We need a valid processed dataset
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      # 1) Extract global min & max Timestamp
      min_timestamp <- min(gpsProcessedData()$Timestamp, na.rm=TRUE)
      max_timestamp <- max(gpsProcessedData()$Timestamp, na.rm=TRUE)
      
      # 2) Current slider input (if any)
      curr_slider <- isolate(input$timestampRange)
      
      # 3) Decide new default or clamp
      if (is.null(curr_slider) || length(curr_slider)!=2) {
        # If no user selection, default to entire range
        new_val <- c(min_timestamp, max_timestamp)
      } else {
        # Ensure user’s previously chosen range is within the new min/max
        new_min <- max(curr_slider[1], min_timestamp)
        new_max <- min(curr_slider[2], max_timestamp)
        new_val <- c(new_min, new_max)
      }
      
      # 4) Re-render the slider with updated range
      output$timestampSliderUI <- renderUI({
        sliderInput(
          "timestampRange","Select Timestamp Range:",
          min=min_timestamp, max=max_timestamp,
          value=new_val,
          timeFormat="%Y-%m-%d %H:%M:%S",
          width = "90%",
          step = 0.1
        )
      })
      
      # === Hour filter ===
      hours_available <- sort(unique(gpsProcessedData()$Hour))
      curr_sel_h      <- isolate(input$selectedHours)
      if (is.null(curr_sel_h) || length(curr_sel_h)==0) {
        new_sel_h <- hours_available
      } else {
        new_sel_h <- intersect(curr_sel_h, hours_available)
      }
      updatePickerInput(session,"selectedHours", choices=hours_available, selected=new_sel_h)
      
      # === Day filter ===
      days_available <- sort(unique(gpsProcessedData()$Day))
      curr_sel_d     <- isolate(input$selectedDays)
      if (is.null(curr_sel_d) || length(curr_sel_d)==0) {
        new_sel_d <- days_available
      } else {
        new_sel_d <- intersect(curr_sel_d, days_available)
      }
      updatePickerInput(session, "selectedDays", choices=days_available, selected=new_sel_d)
      
      # === Month filter ===
      months_available <- sort(unique(gpsProcessedData()$Month))
      month_names      <- month.name[months_available]
      names(months_available) <- month_names
      curr_sel_m       <- isolate(input$selectedMonths)
      if (is.null(curr_sel_m) || length(curr_sel_m)==0) {
        new_sel_m <- months_available
      } else {
        new_sel_m <- intersect(curr_sel_m, months_available)
      }
      updatePickerInput(session, "selectedMonths",
                        choices=months_available,
                        selected=new_sel_m)
      
      # === Year filter ===
      years_available <- sort(unique(gpsProcessedData()$Year))
      curr_sel_y      <- isolate(input$selectedYears)
      if (is.null(curr_sel_y) || length(curr_sel_y)==0) {
        new_sel_y <- years_available
      } else {
        new_sel_y <- intersect(curr_sel_y, years_available)
      }
      updatePickerInput(session,"selectedYears",
                        choices=years_available,
                        selected=new_sel_y)
      
      # === Day Count filter ===
      day_count_available <- sort(unique(gpsProcessedData()$day_count))
      curr_sel_dc         <- isolate(input$selectedDayCount)
      if (is.null(curr_sel_dc) || length(curr_sel_dc)==0) {
        new_sel_dc <- day_count_available
      } else {
        new_sel_dc <- intersect(curr_sel_dc, day_count_available)
      }
      updatePickerInput(session, "selectedDayCount",
                        choices=day_count_available,
                        selected=new_sel_dc)
    })
    
    
    # === B) SYNC combined_verdict IF/USER each time gpsProcessedData() changes ===
    
    observeEvent(gpsProcessedData(), {
      df <- gpsProcessedData()
      
      # If no or empty df, do nothing
      if (is.null(df) || nrow(df)==0) return()
      
      # Ensure needed columns exist; if missing, create them
      if (!all(c("Verdict_IF","Verdict_user") %in% names(df))) {
        if (!"Verdict_user"      %in% names(df)) df$Verdict_user     <- NA
        if (!"Verdict_IF"        %in% names(df)) df$Verdict_IF       <- NA
        if (!"combined_verdict"  %in% names(df)) df$combined_verdict <- NA
        
        gpsProcessedData(df)
        # Possibly triggers re-run of this observer. So we do `return()` now:
        return()
      }
      
      # Compare old vs new to avoid reassign loops
      old_combined <- df$combined_verdict
      old_user     <- df$Verdict_user
      old_IF       <- df$Verdict_IF
      
      # Recompute combined_verdict
      df$combined_verdict <- dplyr::case_when(
        # If both user & IF exist
        !is.na(df$Verdict_IF) & !is.na(df$Verdict_user) ~ dplyr::case_when(
          df$Verdict_user=="Not Anomalous" & df$Verdict_IF=="Not Anomalous" ~ "Not Anomalous",
          df$Verdict_user=="Anomalous"     & df$Verdict_IF=="Anomalous"     ~ "Anomalous",
          df$Verdict_user=="Anomalous"     & df$Verdict_IF=="Not Anomalous" ~ "Anomalous",
          df$Verdict_user=="Not Anomalous" & df$Verdict_IF=="Anomalous"     ~ "Not Anomalous",
          TRUE ~ NA
        ),
        # If only user
        !is.na(df$Verdict_user) & is.na(df$Verdict_IF) ~ dplyr::case_when(
          df$Verdict_user=="Not Anomalous" ~ "Not Anomalous",
          df$Verdict_user=="Anomalous"     ~ "Anomalous",
          TRUE ~ NA
        ),
        # If only IF
        is.na(df$Verdict_user) & !is.na(df$Verdict_IF) ~ dplyr::case_when(
          df$Verdict_IF=="Not Anomalous" ~ "Not Anomalous",
          df$Verdict_IF=="Anomalous"     ~ "Anomalous",
          TRUE ~ NA
        ),
        # Otherwise
        TRUE ~ NA
      )
      
      # If anything changed, reassign
      if (!identical(old_combined, df$combined_verdict) ||
          !identical(old_user, df$Verdict_user)         ||
          !identical(old_IF,   df$Verdict_IF)) {
        gpsProcessedData(df)
      }
    })
    
    
    ################################################################################
    ##               C) LEAFLET MAP: RENDER BASE MAP & REFRESH MAP                 ##
    ################################################################################
    
    # Renders the base map only once
    output$gpsMap <- renderLeaflet({
      
      # 1) Basic Leaflet map
      map <- leaflet(options=leafletOptions(maxZoom=22, preferCanvas=TRUE)) %>%
        addProviderTiles(
          providers[[ input$mapType ]],
          options=providerTileOptions(
            updateWhenZooming=FALSE,
            updateWhenIdle=FALSE
          )
        ) %>%
        addScaleBar(
          position="bottomleft",
          options=scaleBarOptions(
            maxWidth = v$scalebarwidth,
            metric   = TRUE,
            imperial = FALSE
          )
        ) %>%
        addDrawToolbar(
          targetGroup='draw',
          polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
          editOptions     = editToolbarOptions(selectedPathOptions=selectedPathOptions()),
          circleOptions   = drawCircleOptions(
            shapeOptions=drawShapeOptions(),
            showRadius =TRUE,
            metric     =TRUE,
            feet       =FALSE,
            repeatMode =FALSE
          ),
          rectangleOptions = drawRectangleOptions(
            shapeOptions=drawShapeOptions(),
            showArea    =TRUE,
            metric      =TRUE,
            repeatMode  =FALSE
          ),
          polygonOptions = drawPolygonOptions(
            shapeOptions=drawShapeOptions(),
            showArea    =TRUE,
            metric      =TRUE,
            repeatMode  =FALSE
          )
        ) %>%
        addMeasure(primaryLengthUnit="meters", secondaryLengthUnit="kilometers") %>%
        addFullscreenControl(position="topright", pseudoFullscreen=TRUE)
      
      # 2) If we have a stored bounding box, fit the map to that
      if (!is.null(boundsStored())) {
        bb <- boundsStored()  # c(minLon, minLat, maxLon, maxLat)
        map <- map %>%
          fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
      
      # 3) If using leafgl points, attach custom JS for pixel-based nearest click
      # (only if we already have data in filteredMapData())
      if (input$pointrendering=="leafgl_point" && nrow(filteredMapData())>0) {
        map <- map %>% htmlwidgets::onRender(
          sprintf("
        function(el, x) {
          var map = this; 
          var glData = %s;
          if (!glData || glData.length===0) return;
          map.on('click', function(e) {
            var latC = e.latlng.lat;
            var lngC = e.latlng.lng;
            if (typeof glData !== 'undefined') {
              var nearest = findNearestPointPixel(glData, latC, lngC, map);
              if (nearest) {
                Shiny.onInputChange('leafgl_point_clicked',{
                  obs:nearest.obs,
                  ts:new Date().getTime()
                });
              }
            }
          });
          function findNearestPointPixel(dataArray, latClicked, lngClicked, mapObj) {
            var clickedLayerPt = mapObj.latLngToLayerPoint([latClicked,lngClicked]);
            var minDist=Infinity, best=null;
            for (var i=0; i<dataArray.length; i++) {
              var ptLat=dataArray[i].lat;
              var ptLng=dataArray[i].lng;
              var ptLayer= mapObj.latLngToLayerPoint([ptLat,ptLng]);
              var dx=ptLayer.x - clickedLayerPt.x;
              var dy=ptLayer.y - clickedLayerPt.y;
              var distPx= Math.sqrt(dx*dx + dy*dy);
              var pixelThreshold=20;
              if (distPx<minDist) {
                minDist=distPx;
                best=dataArray[i];
              }
            }
            if (minDist<20) return best;
            return null;
          }
        }",
                  isolate(glDataJSON())  # pass the JSON from R
          )
        )
      }
      
      map
    })
    
    # Refresh or re-plot the map
    observeEvent(input$refreshMap, {
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      # 1) Filter data by user’s time filters
      gps_data <- gpsProcessedData()
      
      if (!is.null(input$timestampRange) && length(input$timestampRange)==2) {
        gps_data <- gps_data %>%
          dplyr::filter(
            Timestamp>=input$timestampRange[1],
            Timestamp<=input$timestampRange[2],
            Hour     %in% input$selectedHours,
            Day      %in% input$selectedDays,
            Month    %in% input$selectedMonths,
            Year     %in% input$selectedYears,
            day_count%in% input$selectedDayCount
          )
      } else {
        # No slider input, so apply everything else
        gps_data <- gps_data %>%
          dplyr::filter(
            Hour     %in% input$selectedHours,
            Day      %in% input$selectedDays,
            Month    %in% input$selectedMonths,
            Year     %in% input$selectedYears,
            day_count %in% input$selectedDayCount
          )
      }
      
      # 2) Update filteredMapData for leafgl or other usage
      filteredMapData(
        gps_data %>%
          dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered))
      )
      
      # If no data left, notify
      if (nrow(gps_data)==0) {
        showNotification("No data available for the selected filters.", type="warning")
        return(NULL)
      }
      
      # Clear old layers
      leafletProxy("gpsMap", session) %>%
        clearGroup("Raw GPS") %>%
        clearGroup("Processed GPS")
      
      # 3) Split raw vs processed
      df_raw <- gps_data %>%
        dplyr::filter(!is.na(GPS_Longitude), !is.na(GPS_Latitude))
      df_processed <- gps_data %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered)) %>%
        mutate(
          Time_diff_filtered = c(0, as.numeric(difftime(Timestamp, dplyr::lag(Timestamp), units = "secs"))[-1])
        )
      
      # If map not centered yet, set bounding box
      if (!mapCentered()) {
        if (nrow(df_processed)>0) {
          minLon <- min(df_processed$GPS_Longitude_Filtered)
          maxLon <- max(df_processed$GPS_Longitude_Filtered)
          minLat <- min(df_processed$GPS_Latitude_Filtered)
          maxLat <- max(df_processed$GPS_Latitude_Filtered)
          
          boundsStored(c(minLon,minLat,maxLon,maxLat))
          
          leafletProxy("gpsMap") %>%
            fitBounds(minLon,minLat, maxLon,maxLat)
        }
        mapCentered(TRUE)
      }
      
      # 4) Color palette for processed data
      df_processed$colorCol <- dplyr::case_when(
        df_processed$combined_verdict=="Anomalous"     ~ "red",
        df_processed$combined_verdict=="Not Anomalous" ~ "green",
        TRUE                                           ~ input$pointColor_proc
      )
      
      # 5) If using 'leafgl_point'
      if(input$pointrendering == "leafgl_point"){
        if ("popups" %in% input$display_modes) {
          label_map <- c(
            "Observations"           = "Observation: ",
            "Timestamp"              = "Timestamp: ",
            "Time_diff"              = "Time difference (s): ",
            "GPS_Longitude"          = "Longitude: ",
            "GPS_Latitude"           = "Latitude: ",
            "GPS_Longitude_Filtered" = "Longitude: ",
            "GPS_Latitude_Filtered"  = "Latitude: ",
            "mean_VeDBA_interval"         = "VeDBA (g): ",
            "combined_verdict"       = "Verdict: ",
            "Marked_Events"          = "Marked Events: "
          )
          
          if (input$popuploc == "raw") {
            x <- c("Observations","Timestamp","Time_diff",
                   "GPS_Longitude","GPS_Latitude","mean_VeDBA_interval",
                   "Marked_Events")
            common_cols_raw <- intersect(x, names(df_raw))
            popup <- df_raw %>%
              dplyr::select(all_of(common_cols_raw))
            
            # Round certain columns if present
            if ("Time_diff" %in% common_cols_raw) {
              popup$Time_diff <- round(popup$Time_diff, 3)
            }
            if ("GPS_Longitude" %in% common_cols_raw) {
              popup$GPS_Longitude <- round(popup$GPS_Longitude, 5)
            }
            if ("GPS_Latitude" %in% common_cols_raw) {
              popup$GPS_Latitude <- round(popup$GPS_Latitude, 5)
            }
            if ("mean_VeDBA_interval" %in% common_cols_raw) {
              popup$mean_VeDBA_interval <- round(popup$mean_VeDBA_interval, 3)
            }
            
            # rename them
            old_names <- names(popup)
            new_names <- label_map[old_names]
            keep_idx  <- !is.na(new_names)
            data.table::setnames(popup, old = old_names[keep_idx], new = new_names[keep_idx])
            
          } else if (input$popuploc == "proc") {
            x <- c("Observations","Timestamp","Time_diff",
                   "GPS_Longitude_Filtered","GPS_Latitude_Filtered",
                   "mean_VeDBA_interval","combined_verdict","Marked_Events")
            common_cols_proc <- intersect(x, names(df_processed))
            popup <- df_processed %>%
              dplyr::select(all_of(common_cols_proc))
            
            # Round certain columns if present
            if ("Time_diff" %in% common_cols_proc) {
              popup$Time_diff <- round(popup$Time_diff, 3)
            }
            if ("GPS_Longitude_Filtered" %in% common_cols_proc) {
              popup$GPS_Longitude_Filtered <- round(popup$GPS_Longitude_Filtered, 5)
            }
            if ("GPS_Latitude_Filtered" %in% common_cols_proc) {
              popup$GPS_Latitude_Filtered <- round(popup$GPS_Latitude_Filtered, 5)
            }
            if ("mean_VeDBA_interval" %in% common_cols_proc) {
              popup$mean_VeDBA_interval <- round(popup$mean_VeDBA_interval, 3)
            }
            
            # rename them
            old_names <- names(popup)
            new_names <- label_map[old_names]
            keep_idx  <- !is.na(new_names)
            data.table::setnames(popup, old = old_names[keep_idx], new = new_names[keep_idx])
          }
        } else {
          popup <- NULL
        }
      }
      
      ############################# Show GPS Lines - raw data #############################
      if ("lines" %in% input$display_modes) {
        #### Leaflet
        if(input$linerendering == "leaflet_line"){
          # Raw GPS
          leafletProxy("gpsMap") %>%
            addPolylines(
              data = df_raw,  
              lng = ~GPS_Longitude,
              lat = ~GPS_Latitude,
              color = input$lineColor,
              weight = v$GPS_LW,
              opacity = 0.7,
              group = "Raw GPS",
            )
          
          # Processed GPS
          leafletProxy("gpsMap") %>%
            addPolylines(
              data = df_processed,  
              lng = ~GPS_Longitude_Filtered,
              lat = ~GPS_Latitude_Filtered,
              color = input$lineColor_proc,
              weight = v$GPS_LW_proc,
              opacity = 0.7,
              group = "Processed GPS",
            )
          #### Leafgl
        } else if (input$linerendering == "leafgl_line") {
          # Raw GPS
          df_line_raw <- df_raw %>%  #Convert to one linestring
            arrange(Timestamp) %>%
            st_as_sf(coords = c("GPS_Longitude", "GPS_Latitude"), crs = 4326) %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("LINESTRING")
          
          leafletProxy("gpsMap") %>%
            addGlPolylines(
              data = df_line_raw,  
              color = input$lineColor,
              weight = v$GPS_LW,
              group = "Raw GPS",
              digits = 12
            )
          
          # Processed GPS
          df_line_proc <- df_processed %>%  #Convert to one linestring
            arrange(Timestamp) %>%
            st_as_sf(coords = c("GPS_Longitude_Filtered", "GPS_Latitude_Filtered"), crs = 4326) %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("LINESTRING")
          
          leafletProxy("gpsMap") %>%
            addGlPolylines(
              data = df_line_proc,  
              color = input$lineColor_proc,
              weight = v$GPS_LW_proc,
              group = "Processed GPS",
              digits = 12
            )
        }
      }
      
      ############################# Show GPS Points - raw data #############################
      if ("points" %in% input$display_modes) {
        # Add raw GPS overlay 
        # Leaflet
        if(input$pointrendering == "leaflet_point"){
          # Raw GPS
          leafletProxy("gpsMap") %>%
            addCircleMarkers(
              data = df_raw,  
              lng = ~GPS_Longitude,
              lat = ~GPS_Latitude,
              layerId = ~Observations,  # <---- Key
              fillColor = input$pointColor,  # Map Color column
              color = "black",  # Border color
              radius = v$GPS_PS,
              stroke = TRUE,
              fillOpacity = 0.7,
              weight = v$GPS_PS*.2,
              group = "Raw GPS",
              popup = if ("popups" %in% input$display_modes) {
                if (input$popuploc == "raw") {
                  ~paste(
                    "<strong>Observation:</strong> ", Observations, "<br>",
                    "<strong>Timestamp:</strong> ", as.POSIXct(Timestamp, origin = "1970-01-01"), "<br>",
                    "<strong>Time difference (s):</strong> ", round(Time_diff, 3), "<br>",
                    "<strong>Longitude:</strong> ", round(GPS_Longitude, 5), "<br>",
                    "<strong>Latitude:</strong> ", round(GPS_Latitude, 5), "<br>",
                    if ("mean_VeDBA_interval" %in% colnames(df_raw)) {
                      paste("<strong>VeDBA (g):</strong> ", round(mean_VeDBA_interval, 3), "<br>")
                    } else {
                      ""
                    },
                    if ("Marked_Events" %in% colnames(df_raw)) {
                      paste("<strong>Marked Events:</strong> ", Marked_Events, "<br>")
                    } else {
                      ""
                    }
                  )
                }
              } else NULL,
              clusterOptions = if ("clusters" %in% input$display_modes) {
                markerClusterOptions()
              }
            )

          # Processed GPS
          leafletProxy("gpsMap") %>%
            addCircleMarkers(
              data = df_processed,  
              lng = ~GPS_Longitude_Filtered,
              lat = ~GPS_Latitude_Filtered,
              layerId = ~Observations,  # <---- Key
              color = "black",  # Border color
              fillColor = ~ if (!all(is.na(df_processed$combined_verdict))) {
                colorCol
              } else {
                # fallback color if no anomaly columns at all
                input$pointColor_proc
              },
              weight = v$GPS_PS_proc*.2,
              radius = v$GPS_PS_proc,
              stroke = TRUE,
              fillOpacity = 0.7,
              group = "Processed GPS",
              popup = if ("popups" %in% input$display_modes) {
                if (input$popuploc == "proc") {
                  ~paste(
                    "<strong>Observation:</strong> ", Observations, "<br>",
                    "<strong>Timestamp:</strong> ", as.POSIXct(Timestamp, origin = "1970-01-01"), "<br>",
                    "<strong>Time difference (s):</strong> ", round(Time_diff_filtered, 3), "<br>",
                    "<strong>Longitude:</strong> ", round(GPS_Longitude_Filtered, 5), "<br>",
                    "<strong>Latitude:</strong> ", round(GPS_Latitude_Filtered, 5), "<br>",
                    if ("mean_VeDBA_interval" %in% colnames(df_processed)) {
                      paste("<strong>VeDBA (g):</strong> ", round(mean_VeDBA_interval, 3), "<br>")
                    } else {
                      ""
                    },
                    if ("Marked_Events" %in% colnames(df_processed)) {
                      paste("<strong>Marked Events:</strong> ", Marked_Events, "<br>")
                    } else {
                      ""
                    },
                    if (exists("combined_verdict")) { 
                      paste("<strong>Verdict:</strong> ", combined_verdict)
                    }
                  )
                }
              }  else NULL,
              clusterOptions = if ("clusters" %in% input$display_modes) {
                markerClusterOptions()
              }
            )
          
          ###  Leafgl
        } else if (input$pointrendering == "leafgl_point") {
          
          # Raw GPS
          df_raw_sf <- sf::st_as_sf( # Convert to sf:
            df_raw,
            coords = c("GPS_Longitude", "GPS_Latitude"),
            crs    = 4326
          )
          
          leafletProxy("gpsMap") %>%
            addGlPoints(
              data = df_raw_sf,  
              fillColor = input$pointColor,  
              radius = v$GPS_PS,
              fillOpacity = 0.7,
              group = "Raw GPS",
              popup = popup,
              digits = 12
            )
          
          # Add processed GPS overlay
          df_proc_sf <- st_as_sf(         # Convert to sf:
            df_processed,
            coords = c("GPS_Longitude_Filtered", "GPS_Latitude_Filtered"),
            crs    = 4326
          )
          # Processed GPS
          leafletProxy("gpsMap") %>%
            addGlPoints(
              data = df_proc_sf,  
              fillColor = ~colorCol,
              radius = v$GPS_PS_proc,
              fillOpacity = 0.7,
              group = "Processed GPS",
              popup = popup,
              digits = 12
            )
        }
      }
      
      # Add layers control / hide
      leafletProxy("gpsMap") %>%
        addLayersControl(
          overlayGroups = c("Raw GPS", "Processed GPS"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Raw GPS"))
      
      
    })
    
    ################################################################################
    ##           GPS MAP MARKER CLICK / OVERRIDES / LEAFGL POINT CLICK            ##
    ################################################################################
    
    # -- 1) OBSERVE MARKER CLICKS ON THE LEAFLET MAP -- 
    observeEvent(input$gpsMap_marker_click, {
      click <- input$gpsMap_marker_click
      # 'click$id' is the layerId assigned (e.g., 'Observations' column) when markers are added.
      # 'click$group' might be "Raw GPS" or "Processed GPS," depending on which group was clicked.
      
      # If the click is null or doesn't have an ID, do nothing:
      if (is.null(click$id)) return(NULL)
      
      # Convert the ID (layerId) from string to numeric if Observations is numeric:
      obsID <- as.numeric(click$id)
      
      # Retrieve the row from the gpsProcessedData() dataset matching obsID
      df  <- gpsProcessedData()
      row <- df[df$Observations == obsID, , drop=FALSE]
      
      # If exactly one row is found, store it in selectedPointRow(), else store NULL
      if (nrow(row) == 1) {
        selectedPointRow(row)
      } else {
        selectedPointRow(NULL)
      }
    })
    
    
    # -- 2) APPLY MANUAL VERDICT (Leaflet Tab) --
    observeEvent(input$applyManualVerdict, {
      # This requires we have a valid selectedPointRow()
      req(selectedPointRow())
      
      newVerdict <- input$manualVerdict  # e.g. "Anomalous" / "Not Anomalous"
      df        <- gpsProcessedData()
      
      # The 'Observations' ID of the currently selected row:
      obsID <- selectedPointRow()$Observations
      idx   <- which(df$Observations == obsID)
      
      # 1) Update the main data (gpsProcessedData)
      if (length(idx) == 1) {
        df$Verdict_user[idx]     <- newVerdict
        df$combined_verdict[idx] <- newVerdict
        gpsProcessedData(df)     # Save changes back
      }
      
      # 2) Also store it in the 'overrides' table
      current <- overrides()
      if (obsID %in% current$Observations) {
        # If it already exists, update the verdict
        current$Verdict_user[current$Observations == obsID] <- newVerdict
      } else {
        # Otherwise add a new row
        new_row <- data.frame(
          Observations = obsID,
          Verdict_user = newVerdict,
          stringsAsFactors = FALSE
        )
        current <- rbind(current, new_row)
      }
      overrides(current)  # Save the updated overrides table
      
      # Optionally refresh selectedPointRow() from the updated main data
      updatedRow <- df[df$Observations == obsID, , drop=FALSE]
      selectedPointRow(updatedRow)
    })
    
    
    # -- 3) REMOVE OVERRIDE ROW(S) (Leaflet Tab) --
    observeEvent(input$removeOverride, {
      selected <- input$overridesTable_rows_selected
      if (!is.null(selected) && length(selected) > 0) {
        # Remove these rows from the overrides data frame
        current    <- overrides()
        new_current<- current[-selected, ]
        overrides(new_current)
      }
    })
    
    
    # -- 4) RE-APPLY ALL OVERRIDES (Leaflet Tab) --
    observeEvent(input$reApplyManual, {
      # Merges the overrides back into gpsProcessedData
      
      # Function to apply overrides (used in Leaflet or Plotly override tables).
      applyAllOverrides <- function() {
        df   <- gpsProcessedData()
        over <- overrides()
        
        if (nrow(df) == 0 || nrow(over) == 0) return(NULL)
        
        for (i in seq_len(nrow(over))) {
          obsID   <- over$Observations[i]
          newVerd <- over$Verdict_user[i]
          idx     <- which(df$Observations == obsID)
          if (length(idx) == 1) {
            df$Verdict_user[idx]     <- newVerd
            df$combined_verdict[idx] <- newVerd
          }
        }
        gpsProcessedData(df)
      }
      
      applyAllOverrides()
      showNotification("Manual verdicts have been re-applied to the dataset.", type="message")
    })
    
    
    # -- 5) RENDER SELECTED POINT DATA TABLE --
    output$selectedPointTable <- DT::renderDataTable({
      # If no row is currently selected, show a simple message
      if (is.null(selectedPointRow())) {
        return(DT::datatable(data.frame(Message="No selections yet")))
      }
      
      # Otherwise, show the single row data in a small table
      df <- selectedPointRow()
      
      # Format the Timestamp nicely
      df$Timestamp <- format(df$Timestamp, "%Y-%m-%d %H:%M:%OS")
      
      DT::datatable(
        df,
        options=list(
          dom='t',
          paging=FALSE,        # no pagination
          scrollX=TRUE,
          colReorder=TRUE,
          class='cell-border stripe',
          fixedColumns=list(leftColumns=1, rightColumns=0)
        ),
        rownames=TRUE
      ) %>%
        DT::formatRound(columns=which(sapply(df, is.numeric)), 5) %>%
        formatStyle(columns=1:ncol(df), 'text-align'='center')
    })
    
    
    # -- 6) DISPLAY THE OVERRIDES TABLE --
    output$overridesTable <- DT::renderDataTable({
      df_over <- overrides()
      
      if (nrow(df_over) == 0) {
        # If no rows in overrides yet, show a simple message
        return(DT::datatable(data.frame(Message="No overrides yet")))
      }
      
      DT::datatable(
        df_over,
        options=list(
          stateSave=TRUE,
          orderClasses=TRUE,
          stateDuration=-1,
          colReorder=TRUE,
          fixedHeader=FALSE,
          scrollY="400px",
          scrollX=TRUE,
          class='cell-border stripe',
          rownames=TRUE,
          scroller=TRUE,
          fixedColumns=list(leftColumns=1, rightColumns=0)
        ),
        extensions=c("ColReorder","FixedColumns","FixedHeader","Scroller")
      ) %>%
        formatStyle(columns=1:ncol(df_over), 'text-align'='center')
    })
    
    
    ################################################################################
    ##               LEAFGL POINT CLICK LOGIC FOR WEBGL RENDERING                 ##
    ################################################################################
    
    # JSON data for leafgl point plotting/click-detection
    glDataJSON <- reactive({
      req(filteredMapData())
      
      df <- filteredMapData()
      # We'll subset only the columns needed by the JS
      subdf <- df[, c("Observations","GPS_Latitude_Filtered","GPS_Longitude_Filtered")]
      # rename for convenience (obs, lat, lng)
      names(subdf) <- c("obs","lat","lng")
      
      # Convert data.frame to JSON (in row format)
      jsonlite::toJSON(subdf, dataframe="rows", auto_unbox=TRUE)
    })
    
    
    # Observing the custom 'leafgl_point_clicked' input (pixel-based nearest)
    observeEvent(input$leafgl_point_clicked, {
      req(input$leafgl_point_clicked)
      print(input$leafgl_point_clicked)
      
      # user’s clicked observation ID
      obsID <- input$leafgl_point_clicked$obs
      
      # retrieve that row from the main dataset
      df  <- gpsProcessedData()
      row <- df[df$Observations==obsID, , drop=FALSE]
      
      # store it in selectedPointRow, same as the standard marker click approach
      selectedPointRow(row)
    })
    
    
    ################################################################################
    ##             VEDBA ~ GPS SPEED DYGRAPH AND PLOTTING (TIME SERIES)           ##
    ################################################################################
    
    # pickColor helper: chooses the correct color for each variable
    pickColor <- function(varName) {
      if (varName=="speed_raw")           return(input$color_speed_raw)
      if (varName=="speed_filt")          return(input$color_speed_filt)
      if (varName=="IMU_Mean_VeDBA")      return(input$color_vedba_raw)
      if (varName=="mean_VeDBA_interval") return(input$color_vedba_proc)
      # Fallback if none match
      "black"
    }
    
    # 1) Build a combined dataset for VeDBA vs. GPS Speed overlay
    observeEvent(c(input$refreshDBA_Sp_plot, input$applyManualVerdict, input$applyManualVerdict2), {
      # Must have a valid 'filtered_data' and 'gpsProcessedData'
      req(filtered_data(), nrow(filtered_data())>0, ncol(filtered_data())>0)
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
      
      df_proc <- gpsProcessedData()
      
      # (a) Step the unfiltered data by v$speedStepSeconds_R
      df_unf_stepped <- getTimeSteppedDataFast(
        df_proc,
        stepSeconds = v$speedStepSeconds_R,
        latCol = "GPS_Latitude",
        lonCol = "GPS_Longitude",
        keepFirst = TRUE,
        keepLast  = TRUE
      ) %>%
        mutate(
          speed_raw  = calc_speed(.data[["GPS_Longitude"]], 
                                  .data[["GPS_Latitude"]], Timestamp),
          gps_raw_pts= ifelse(!is.na(GPS_Longitude)&!is.na(GPS_Latitude), 0.01, NA)
        )
      
      # (b) Step the filtered data by v$speedStepSeconds_P
      df_filt_stepped <- getTimeSteppedDataFast(
        df_proc,
        stepSeconds = v$speedStepSeconds_P,
        latCol = "GPS_Latitude_Filtered",
        lonCol = "GPS_Longitude_Filtered",
        keepFirst = TRUE,
        keepLast  = TRUE
      ) %>%
        mutate(
          speed_filt = calc_speed(.data[["GPS_Longitude_Filtered"]],
                                  .data[["GPS_Latitude_Filtered"]], Timestamp)
        )
      
      # (c) Merge unfiltered & filtered stepping data
      merged <- df_unf_stepped %>%
        full_join(df_filt_stepped, by="Timestamp")
      
      # (d) If we have IMU_Mean_VeDBA column in the original (filtered_data), add it
      if ("IMU_Mean_VeDBA" %in% colnames(filtered_data())) {
        df_raw_imu <- filtered_data() %>%
          dplyr::filter(!is.na(IMU_Mean_VeDBA)) %>%
          dplyr::select(Timestamp, IMU_Mean_VeDBA) %>%
          arrange(Timestamp)
        merged <- merged %>%
          full_join(df_raw_imu, by="Timestamp")
      }
      
      # (e) The mean_VeDBA_interval from df_proc, if present
      if ("mean_VeDBA_interval" %in% colnames(df_proc)) {
        df_proc_imu <- df_proc %>%
          dplyr::filter(!is.na(mean_VeDBA_interval)) %>%
          dplyr::select(Timestamp, mean_VeDBA_interval) %>%
          arrange(Timestamp)
        merged <- merged %>%
          full_join(df_proc_imu, by="Timestamp")
      }
      
      # (f) Marked_Events if present in either filtered_data or df_proc
      if ("Marked_Events" %in% colnames(filtered_data())) {
        if ("IMU_Mean_VeDBA" %in% colnames(filtered_data())) {
          df_marked <- filtered_data() %>%
            dplyr::select(Timestamp, Marked_Events) %>%
            arrange(Timestamp)
        } else {
          df_marked <- df_proc %>%
            dplyr::select(Timestamp, Marked_Events) %>%
            arrange(Timestamp)
        }
        merged <- merged %>%
          full_join(df_marked, by="Timestamp")
      }
      
      # Also Observations
      if ("IMU_Mean_VeDBA" %in% colnames(filtered_data())) {
        df_obs <- filtered_data() %>%
          dplyr::select(Timestamp, Observations) %>%
          arrange(Timestamp)
      } else {
        df_obs <- df_proc %>%
          dplyr::select(Timestamp, Observations) %>%
          arrange(Timestamp)
      }
      merged <- merged %>%
        full_join(df_obs, by="Timestamp")
      
      # Also combined_verdict from df_proc
      df_cv <- df_proc %>%
        dplyr::select(Timestamp, combined_verdict) %>%
        arrange(Timestamp)
      merged <- merged %>%
        full_join(df_cv, by="Timestamp")
      
      # (g) Sort by time for cleanliness
      merged <- merged %>% arrange(Timestamp)
      
      # (h) Store in a reactiveVal for plot usage
      combinedData(merged)
    })
    
    
    # 2) Render the main Dygraph for VeDBA vs. GPS Speed
    observeEvent(input$refreshDBA_Sp_plot, {
    output$vedbaSpeedDygraph <- dygraphs::renderDygraph({
      req(isolate(combinedData()))
      df <- isolate(combinedData())
      
      if (is.null(df) || nrow(df)<2) {
        showNotification("Not enough data to plot VeDBA~GPS speed.", type="warning")
        return(NULL)
      }
      
      # A) Collect user-chosen columns
      correctedVars <- isolate(input$DBA_Sp_Vars)
      # Optionally ensure we include gps_raw_pts
      correctedVars <- union(correctedVars, "gps_raw_pts")
      
      # B) Build final columns & convert to XTS
      desiredCols <- c("Timestamp", correctedVars)
      desiredCols <- intersect(desiredCols, names(df))
      
      if (length(desiredCols) <2) {
        showNotification("No valid columns selected for plotting.", type="warning")
        return(NULL)
      }
      
      # Build XTS object
      df_xts <- xts(
        df[, desiredCols[desiredCols!="Timestamp"], drop=FALSE],
        order.by=df$Timestamp
      )
      
      # C) Identify speed & VeDBA columns
      speedVars <- intersect(names(df_xts), c("speed_raw","speed_filt"))
      veDBAVars <- intersect(names(df_xts), c("IMU_Mean_VeDBA","mean_VeDBA_interval"))
      
      haveSpeed <- (length(speedVars)>0)
      haveVeDBA <- (length(veDBAVars)>0)
      
      # D) Create the base Dygraph
      dg <- dygraph(df_xts, main="VeDBA & GPS Speed Over Time") %>%
        dyRangeSelector(height=30, retainDateWindow=TRUE) %>%
        dyOptions(
          connectSeparatedPoints=TRUE,
          drawPoints=FALSE,
          strokeWidth=1.5,
          useDataTimezone=TRUE,
          sigFigs=3,
          axisLineWidth=1.5,
          rightGap=90,
          drawGrid=FALSE
        ) %>%
        dyAxis("x",
               valueFormatter=JS(CustomValueFormat),
               ticker="Dygraph.dateTicker",
               label="Timestamp",
               axisLabelWidth=80,
               labelWidth=30,
               labelHeight=30,
               axisLabelFontSize=14,
               drawGrid=FALSE
        ) %>%
        dyLegend(show="onmouseover", width=200, hideOnMouseOut=TRUE, labelsSeparateLines=TRUE) %>%
        dyUnzoom() %>%
        dyCallbacks(
          drawCallback=JS("
        function(g, is_initial) {
          if (!is_initial) {
            var range = g.xAxisRange();
            Shiny.setInputValue('dygraph_range',{
              min: range[0], max: range[1]
            }, {priority:'event'});
          }
        }
      ")
        )
      
      # E) Single axis vs. dual axis (Speed vs. VeDBA)
      if (haveSpeed && haveVeDBA) {
        # 2 axes
        dg <- dg %>%
          dyAxis("y", label="Speed (m/s)", independentTicks=TRUE, drawGrid=FALSE) %>%
          dyAxis("y2",label="VeDBA", independentTicks=TRUE, drawGrid=FALSE)
        
        # Plot speed on y
        for (sp in speedVars) {
          dg <- dg %>% dySeries(sp, axis="y", color=pickColor(sp))
        }
        # Plot VeDBA on y2
        for (vb in veDBAVars) {
          dg <- dg %>% dySeries(vb, axis="y2", color=pickColor(vb))
        }
        
      } else {
        # Single axis for everything
        dg <- dg %>%
          dyAxis("y", label="Value", independentTicks=TRUE, drawGrid=FALSE)
        
        # Add each column
        for (colname in colnames(df_xts)) {
          dg <- dg %>% dySeries(colname, color=pickColor(colname))
        }
      }
      
      # F) If "gps_raw_pts" exists, plot them as small black points
      if ("gps_raw_pts" %in% colnames(df_xts)) {
        dg <- dg %>%
          dySeries(
            "gps_raw_pts",
            label="Raw GPS Fixes",
            axis= if(haveSpeed && haveVeDBA) "y" else "y",
            color="black",
            drawPoints=TRUE,
            pointSize=2,
            strokeWidth=0,
            stepPlot=FALSE
          )
      }
      
      # G) Marked Events with shading / events if present
      if ("Marked_Events" %in% names(df)) {
        events_col <- df$Marked_Events
        
        # 1) Drop NAs
        events_col_noNA <- events_col[!is.na(events_col)]
        
        # 2) Optionally exclude "0"
        events_col_noNA <- events_col_noNA[events_col_noNA!="0"]
        events_col_noNA <- events_col_noNA[events_col_noNA!=0]
        
        unique_events <- sort(unique(events_col_noNA))
        
        if (length(unique_events)==0) {
          showNotification("No non-zero Marked_Events found; skipping event shading", type="message")
        } else {
          # Build color palette
          event_labels_map <- setNames(as.character(unique_events), unique_events)
          
          n_events <- length(unique_events)
          max_colors <- brewer.pal.info[input$eventPalette, "maxcolors"]
          if (n_events <= max_colors) {
            base_cols <- brewer.pal(n_events, input$eventPalette)
          } else {
            base_cols <- colorRampPalette(brewer.pal(max_colors, input$eventPalette))(n_events)
          }
          
          # alpha
          alpha_cols <- sapply(base_cols, function(cc) addalpha(cc, alpha=input$eventAlpha))
          event_colors_map <- setNames(alpha_cols, unique_events)
          
          # For each event, find contiguous runs
          for (ev in unique_events) {
            ev_idxs <- which(df$Marked_Events==ev)
            if (length(ev_idxs)==0) next
            
            diffs     <- diff(ev_idxs)
            rle_event <- rle(diffs==1)
            lens      <- rle_event$lengths
            vals      <- rle_event$values
            
            start_idx <- ev_idxs[1]
            shading_blocks <- list()
            
            # group consecutive indices
            for (i in seq_along(lens)) {
              if (!vals[i]) {
                # end of a contiguous block
                end_idx <- ev_idxs[ sum(lens[1:i]) ]
                shading_blocks[[length(shading_blocks)+1]] <- c(start_idx, end_idx)
                if (i<length(lens)) {
                  start_idx <- ev_idxs[ sum(lens[1:i]) +1 ]
                }
              }
            }
            # if the last val was TRUE, close out
            if (length(vals)>0 && vals[length(vals)]) {
              end_idx <- ev_idxs[length(ev_idxs)]
              shading_blocks[[length(shading_blocks)+1]] <- c(start_idx, end_idx)
            }
            
            # apply shading
            for (sb in shading_blocks) {
              from_time <- df$Timestamp[sb[1]]
              to_time   <- df$Timestamp[sb[2]]
              
              dg <- dg %>%
                dyShading(from=from_time, to=to_time, color=event_colors_map[[as.character(ev)]]) %>%
                dyEvent(
                  x=from_time,
                  label=event_labels_map[[as.character(ev)]],
                  labelLoc="top",
                  color="grey",
                  strokePattern="dashed"
                )
            }
          }
        }
      }
      
      # Return final dygraph
      dg
    })
    })
    ################################################################################
    ##             VEDBA ~ GPS SPEED CORRELATION PLOT                             ##
    ################################################################################

    # (A) Initialize placeholders to store min/max times from the dygraph
    v$dy_min_time <- NULL
    v$dy_max_time <- NULL
    
    # (B) Observe dygraph zoom range changes
    observeEvent(input$dygraph_range, {
      # input$dygraph_range has $min, $max in milliseconds since 1970
      req(input$dygraph_range$min, input$dygraph_range$max)
      
      # Convert from ms to POSIXct
      new_min <- as.POSIXct(input$dygraph_range$min / 1000, origin="1970-01-01")
      new_max <- as.POSIXct(input$dygraph_range$max / 1000, origin="1970-01-01")
      
      v$dy_min_time <- new_min
      v$dy_max_time <- new_max
    })
    
    # (C) Reactive subset based on the dygraph's zoom range
    df_for_plotly <- reactive({
      req(combinedData())         # Must have the main data
      req(v$dy_min_time, v$dy_max_time)
      
      # Filter by that time range
      combinedData() %>%
        dplyr::filter(Timestamp >= v$dy_min_time,
                      Timestamp <= v$dy_max_time)
    })
    
    # ----------------------------------------------------------------------------------
    # (1) VeDBA ~ GPS speed correlation plot
    # ----------------------------------------------------------------------------------
    output$vedbaScatter <- renderPlotly({
      df_model <- df_for_plotly()
      req(nrow(df_model) > 0)
      
      # Only proceed if 'mean_VeDBA_interval' column exists
      if ("mean_VeDBA_interval" %in% colnames(df_model)) {
        
        # Transform any NA combined_verdict to "NoVerdict" for consistent color mapping
        df_model <- df_model %>%
          mutate(verdict_col = ifelse(is.na(combined_verdict),
                                      "NoVerdict",
                                      combined_verdict))%>%
          dplyr::filter(!is.na(speed_filt), 
                        !is.na(mean_VeDBA_interval))
        
        # Compute linear model for Speed vs mean_VeDBA_interval

        req(nrow(df_model) > 0)
        
        df_model_sub = subset(df_model, df_model$verdict_col != "Anomalous")
        mod           <- lm(mean_VeDBA_interval ~ speed_filt, data = df_model_sub)
        df_model_sub$fit_line <- predict(mod)
        cor_val       <- cor(df_model_sub$speed_filt,
                             df_model_sub$mean_VeDBA_interval,
                             use="complete.obs")
        
        # Plotly scatter + regression line
        plot_ly(
          data   = df_model,
          x      = ~speed_filt,
          y      = ~mean_VeDBA_interval,
          key    = ~Observations,         # attach row IDs
          source = "myVedbaPlot",
          type   = 'scatter',
          mode   = 'markers',
          marker = list(size=5),
          color  = ~verdict_col,
          colors = c("Anomalous"     = "red",
                     "Not Anomalous" = "green",
                     "NoVerdict"     = input$pointColor_proc),
          name = 'Data'
        ) %>%
          add_trace(
            data = df_model_sub,
            x      = ~speed_filt,
            y      = ~fit_line,
            mode   = 'lines',
            line   = list(color='black', width=2),
            name   = 'LM Fit',
            inherit= FALSE
          ) %>%
          layout(
            title = paste("VeDBA vs Speed (r =", round(cor_val,3), ")"),
            xaxis = list(title="Speed (m/s)"),
            yaxis = list(title="Mean VeDBA")
          )
        
      } else {
        return(NULL)
      }
    })
    
    # ----------------------------------------------------------------------------------
    # (2) Lat-Lon Plot with Projected Coordinates
    # ----------------------------------------------------------------------------------
    
    # Subset the processed GPS data for the dygraph's time window
    df_for_latlon <- reactive({
      req(gpsProcessedData(), v$dy_min_time, v$dy_max_time)
      
      gpsProcessedData() %>%
        dplyr::filter(Timestamp >= v$dy_min_time,
                      Timestamp <= v$dy_max_time)
    })
    
    # Convert lat-lon to an sf object, then transform to local meters (using aeqd)
    projectedData <- reactive({
      req(df_for_latlon())
      
      plot_data <- df_for_latlon()
      # Filter out rows with NA lat-lon
      plot_data <- plot_data %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered),
                      !is.na(GPS_Latitude_Filtered))
      if (nrow(plot_data) < 1) return(NULL)
      
      sf_data <- sf::st_as_sf(
        plot_data,
        coords = c("GPS_Longitude_Filtered", "GPS_Latitude_Filtered"),
        crs    = 4326,
        remove = FALSE
      )
      
      if (nrow(sf_data) < 2) {
        # If only 1 row, transform is not that meaningful
        return(sf_data %>%
                 sf::st_drop_geometry() %>%
                 dplyr::mutate(X_m = NA_real_, Y_m = NA_real_))
      }
      
      bbox        <- sf::st_bbox(sf_data)
      center_lon  <- (bbox$xmin + bbox$xmax) / 2
      center_lat  <- (bbox$ymin + bbox$ymax) / 2
      
      local_crs   <- paste0(
        "+proj=aeqd +lat_0=", center_lat,
        " +lon_0=", center_lon,
        " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
      )
      
      sf_data_m   <- sf::st_transform(sf_data, crs = local_crs)
      coords_m    <- sf::st_coordinates(sf_data_m)
      
      out         <- sf::st_drop_geometry(sf_data_m)
      out$X_m     <- coords_m[,1]
      out$Y_m     <- coords_m[,2]
      
      out
    })
    
    # Plotly output for lat-lon in local meters
    output$latLonPlot <- renderPlotly({
      dat_m <- projectedData()
      req(dat_m, nrow(dat_m) > 0)
      
      # Color by verdict
      dat_m <- dat_m %>%
        mutate(verdict_col = ifelse(is.na(combined_verdict),
                                    "NoVerdict",
                                    combined_verdict))
      
      fig <- plot_ly(
        data   = dat_m,
        x      = ~X_m,
        y      = ~Y_m,
        type   = 'scatter',
        mode   = 'markers',
        color  = ~as.factor(verdict_col),
        colors = c("Anomalous"     = "red",
                   "Not Anomalous" = "green",
                   "NoVerdict"     = input$pointColor_proc),
        text       = ~paste("Timestamp:", Timestamp),
        hoverinfo  = 'text',
        marker     = list(size=6),
        name       = "Points",
        key        = ~Observations,
        source     = "myLatLonPlot"
      ) %>%
        layout(
          title = "Track in the Selected Time Window (Meters)",
          xaxis = list(title = "X (m)"),
          yaxis = list(title = "Y (m)")
        )
      
      # Add a line trace over the same data
      fig %>%
        add_trace(
          data   = dat_m,
          x      = ~X_m,
          y      = ~Y_m,
          type   = 'scatter',
          mode   = 'lines',
          line   = list(color = 'blue', width = 0.6),
          group  = -1,
          hoverinfo = 'none',
          name       = "Path",
          showlegend = FALSE,
          inherit    = FALSE
        )
    })
    
    # ----------------------------------------------------------------------------------
    # (3) Selections and verdict overrides
    # ----------------------------------------------------------------------------------
    # allSelectedObs() is presumably a reactiveVal that stores the selected "Observations"
    
    # Capture single-click or lasso selections from latLonPlot
    observeEvent(event_data("plotly_click", source="myLatLonPlot"), {
      ed <- event_data("plotly_click", source="myLatLonPlot")
      req(ed)
      obsClicked <- as.character(ed$key)
      allSelectedObs(obsClicked)
    })
    
    observeEvent(event_data("plotly_selected", source="myLatLonPlot"), {
      ed <- event_data("plotly_selected", source="myLatLonPlot")
      req(ed)
      obsVector <- as.character(ed$key)
      allSelectedObs(obsVector)
    })
    
    # Similarly for the vedbaScatter plot
    observeEvent(event_data("plotly_click", source="myVedbaPlot"), {
      ed <- event_data("plotly_click", source="myVedbaPlot")
      req(ed)
      obsClicked <- as.character(ed$key)
      allSelectedObs(obsClicked)
    })
    
    observeEvent(event_data("plotly_selected", source="myVedbaPlot"), {
      ed <- event_data("plotly_selected", source="myVedbaPlot")
      req(ed)
      obsVector <- as.character(ed$key)
      allSelectedObs(obsVector)
    })
    
    # ----------------------------------------------------------------------------------
    # (3A) Show selected rows in a DT table
    # ----------------------------------------------------------------------------------
    selectedRows2 <- reactive({
      req(gpsProcessedData())
      sel <- allSelectedObs()
      if (length(sel) == 0) return(data.frame())
      
      # Convert Observations if numeric
      obsIDs_num <- as.numeric(sel)
      
      data <- gpsProcessedData()
      data[data$Observations %in% obsIDs_num, ]
    })
    
    output$selectedPointTable2 <- DT::renderDataTable({
      df_tab <- selectedRows2()
      
      if (nrow(df_tab) == 0) {
        return(
          DT::datatable(
            data.frame(Message="No selections yet")
          )
        )
      }
      
      # Safely format Timestamp
      df_tab$Timestamp <- format(df_tab$Timestamp, "%Y-%m-%d %H:%M:%OS")
      
      DT::datatable(
        df_tab,
        options = list(
          dom        = 't',      # table only
          paging     = FALSE,
          scrollX    = TRUE,
          colReorder = TRUE,
          class      = 'cell-border stripe',
          fixedColumns = list(leftColumns = 1, rightColumns = 0)
        ),
        rownames = TRUE
      ) %>%
        formatStyle(columns = c(1:ncol(df_tab)), 'text-align' = 'center') %>%
        DT::formatRound(columns = which(sapply(df_tab, is.numeric)), 5)
    })
    
    # Table of all overrides
    output$overridesTable2 <- DT::renderDataTable({
      df_over <- overrides()
      if (nrow(df_over) == 0) {
        return(
          DT::datatable(data.frame(Message="No overrides yet"))
        )
      }
      DT::datatable(
        df_over,
        options = list(
          stateSave     = TRUE,
          orderClasses  = TRUE,
          stateDuration = -1,
          colReorder    = TRUE,
          fixedHeader   = FALSE,
          scrollY       = "400px",
          scrollX       = TRUE,
          class         = 'cell-border stripe',
          scroller      = TRUE,
          rownames      = TRUE
        ),
        extensions = c("ColReorder", "FixedColumns", "FixedHeader", "Scroller")
      ) %>%
        formatStyle(columns = c(1:ncol(df_over)), 'text-align' = 'center')
    })
    
    # ----------------------------------------------------------------------------------
    # (3B) Apply verdict to the selected set
    # ----------------------------------------------------------------------------------
    observeEvent(input$applyManualVerdict2, {
      req(allSelectedObs())
      
      # Which Observations are selected?
      sel <- allSelectedObs()
      if (length(sel) == 0) {
        showNotification("No points selected! Please select points first.", type="warning")
        return(NULL)
      }
      
      newVerdict <- input$manualVerdict2
      bigDF <- gpsProcessedData()
      
      sel_num <- as.numeric(sel)
      idx <- which(bigDF$Observations %in% sel_num)
      
      if (length(idx) == 0) {
        showNotification("No matching points found in gpsProcessedData. Check Observations column!", type="error")
        return(NULL)
      }
      
      # Update user and combined verdict
      bigDF$Verdict_user[idx]    <- newVerdict
      bigDF$combined_verdict[idx] <- newVerdict
      
      # Overwrite gpsProcessedData with updated data
      gpsProcessedData(bigDF)
      
      # Also store them in overrides()
      currentOver <- overrides()
      for (obs in sel_num) {
        if (obs %in% currentOver$Observations) {
          currentOver$Verdict_user[currentOver$Observations == obs] <- newVerdict
        } else {
          newrow <- data.frame(
            Observations = obs,
            Verdict_user = newVerdict,
            stringsAsFactors = FALSE
          )
          currentOver <- rbind(currentOver, newrow)
        }
      }
      overrides(currentOver)
      
      showNotification(
        paste("Applied verdict", newVerdict, "to", length(sel), "points."),
        type="message"
      )
    })
    
    # Remove an override from the table
    observeEvent(input$removeOverride2, {
      sel2 <- input$overridesTable2_rows_selected
      if (!is.null(sel2) && length(sel2) > 0) {
        current <- overrides()
        # remove selected rows from 'overrides'
        new_current <- current[-sel2, ]
        overrides(new_current)
      }
    })
    
    # Re-apply overrides to the main dataset
    observeEvent(input$reApplyManual2, {
      
      # Function to apply overrides (used in Leaflet or Plotly override tables).
      applyAllOverrides <- function() {
        df   <- gpsProcessedData()
        over <- overrides()
        
        if (nrow(df) == 0 || nrow(over) == 0) return(NULL)
        
        for (i in seq_len(nrow(over))) {
          obsID   <- over$Observations[i]
          newVerd <- over$Verdict_user[i]
          idx     <- which(df$Observations == obsID)
          if (length(idx) == 1) {
            df$Verdict_user[idx]     <- newVerd
            df$combined_verdict[idx] <- newVerd
          }
        }
        gpsProcessedData(df)
      }
      
      applyAllOverrides()
      showNotification(
        "Manual verdicts have been re-applied to the dataset (from Speed tab).",
        type = "message"
      )
    })
    
    # ----------------------------------------------------------------------------------
    # (3C) Exporting overrides to CSV (two buttons on different tabs)
    # ----------------------------------------------------------------------------------
    output$export_overrides <- downloadHandler(
      filename = function() {
        paste0("GPS_Overrides_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df_over <- overrides()
        if (nrow(df_over) == 0) {
          write.csv(
            data.frame(Observations = numeric(0), Verdict_user = character(0)),
            file,
            row.names = FALSE
          )
        } else {
          write.csv(df_over, file, row.names = FALSE)
        }
      }
    )
    
    output$export_overrides2 <- downloadHandler(
      filename = function() {
        paste0("GPS_Overrides_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df_over <- overrides()
        if (nrow(df_over) == 0) {
          write.csv(
            data.frame(Observations = numeric(0), Verdict_user = character(0)),
            file,
            row.names = FALSE
          )
        } else {
          write.csv(df_over, file, row.names = FALSE)
        }
      }
    )
    
    # ----------------------------------------------------------------------------------
    # (3D) Import overrides from CSV
    # ----------------------------------------------------------------------------------
    observeEvent(input$import_overrides, {
      req(input$import_overrides)
      
      imported_path <- input$import_overrides$datapath
      df_imported <- tryCatch(
        read.csv(imported_path, stringsAsFactors = FALSE),
        error = function(e) {
          showNotification(paste("Error reading overrides CSV:", e$message), type="error")
          return(NULL)
        }
      )
      
      if (is.null(df_imported) || nrow(df_imported) == 0) {
        showNotification("No data found in imported CSV", type="error")
        return(NULL)
      }
      
      needed_cols <- c("Observations", "Verdict_user")
      if (!all(needed_cols %in% names(df_imported))) {
        showNotification(
          "Import failed: CSV must have columns 'Observations' and 'Verdict_user'.",
          type="error"
        )
        return(NULL)
      }
      
      overrides(df_imported)
      showNotification("Overrides imported successfully!", type="message")
    })
    
    # ----------------------------------------------------------------------------------
    # (4) R console: Let the user run custom R code that modifies the main dataset
    # ----------------------------------------------------------------------------------
    
    # (4A) Provide an example script on reset
    observeEvent(input$resetScript, {
      shinyAce::updateAceEditor(
        session,
        editorId = "userScript",
        value = paste(
          "# Example script:\n",
          "# 'df' refers to the main GPS processed data frame copy\n",
          "# Modify 'df$Verdict_user' for user-defined overrides.\n",
          "# Avoid changing 'combined_verdict' directly.\n",
          "#\n",
          "# e.g.\n",
          "# df$Verdict_user <- ifelse(\n",
          "#   df$Verdict_user == 'Not Anomalous' & !is.na(df$Ang_vertex) & df$Ang_vertex >= 175 &\n",
          "#   !is.na(df$Dist_circular) & df$Dist_circular <= quantile(df$Dist_circular, 0.1, na.rm = TRUE),\n",
          "#   'Anomalous', df$Verdict_user\n",
          "# )\n",
          sep=""
        )
      )
    })
    
    # (4B) Run the user script
    observeEvent(input$runScript, {
      req(gpsProcessedData())
      
      df_local <- gpsProcessedData()
      code     <- input$userScript
      
      # Evaluate user code in a temporary environment with 'df' referencing df_local
      temp_env <- new.env(parent = globalenv())
      assign("df", df_local, envir = temp_env)
      
      # Capture output
      outputStr <- capture.output(
        res <- tryCatch({
          eval(parse(text = code), envir = temp_env)
          
          # After successful run, retrieve "df" 
          new_df <- get("df", envir = temp_env)
          
          # Forbid renaming or removing existing columns
          if (!all(names(df_local) %in% names(new_df))) {
            stop("Renaming or removing existing columns is not allowed. All old columns must remain.")
          }
          
          # Update main data
          gpsProcessedData(new_df)
          
          cat("Script ran successfully!\n")
          cat("Rows:", nrow(new_df), "Columns:", ncol(new_df), "\n")
          cat("User-defined transformations applied.\n")
          
        }, error = function(e) {
          cat("Error:\n", e$message, "\n")
        }, warning = function(w) {
          cat("Warning:\n", w$message, "\n")
        })
      )
      
      # Display output or errors in "userScriptOutput"
      output$userScriptOutput <- renderText({
        paste(outputStr, collapse = "\n")
      })
    })
    
    # (4C) Export the user’s script
    output$exportScript <- downloadHandler(
      filename = function() {
        paste0("UserScript_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        cat(input$userScript, file = file, sep = "\n")
      }
    )
    
    # (4D) Import a script
    observeEvent(input$importScript, {
      req(input$importScript)
      script_lines <- readLines(input$importScript$datapath, warn = FALSE)
      script_text  <- paste(script_lines, collapse = "\n")
      
      # Place this in the editor
      shinyAce::updateAceEditor(session, "userScript", value = script_text)
      
      output$userScriptOutput <- renderText({
        paste0("Imported script with ", length(script_lines), " lines.")
      })
    })
    
   
    
    ################################################################################################################################
    ################################################################################################################################
    # ----------------------------------------------------------------------------------
    # (4) GPS ANALYSIS PAGE
    # ----------------------------------------------------------------------------------
    
    output$page4 <- renderUI({
      fluidPage(
        
        # === 1) Page Title ===
        titlePanel(
          h1(
            strong("GPS Analysis"),
            align = "center",
            style = "background-color:lightgreen; color:black; font-size:30px;"
          )
        ),
        
        # === 2) Loading Message Style & Conditional Panel ===
        tags$style(type="text/css", "
      #loadmessage {
        position: fluid;
        top: 0px;
        left: 0px;
        width: 100%;
        padding: 5px 0px 5px 0px;
        text-align: center;
        font-weight: bold;
        font-size: 100%;
        color: #000000;
        background-color: #CCFF66;
        z-index: 60;
      }
    "),
        tags$style(HTML("
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate,
      table.dataTable tr {
        font-size: 10pt !important;
      }
    ")),
        
        # A small JS snippet to convert area to km² for geometry tools
        tags$script(HTML("
      L.GeometryUtil.readableArea = function (area, isMetric, precision) {
        var km2 = area / 1000000;
        var output = (Math.round(km2 * 100) / 100).toLocaleString() + ' km²';
        return output;
      };
    ")),
        tags$style(HTML("
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate,
      table.dataTable tr {
        font-size: 8pt !important;
      }
    ")),
        
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id = "loadmessage")
        ),
        
        # Warning if data_chk is false (no valid data loaded)
        conditionalPanel(
          condition = "output.data_chk == false",
          uiOutput("warning_message")
        ),
        
        sidebarLayout(
          sidebarPanel(
            width = 4,
            div(class = "sidebar-taskbar",
                column(width = 12,
                       actionButton("GPSanalysis", "Press Once to Generate Filtered GPS Dataframe",
                                    style="width:100%; margin-top:0px; margin-bottom:10px; font-size:16px;"
                       )),
                navlistPanel(
                  
                  # --- (A) Table display options ---
                  tabPanel(HTML("<i class='fa fa-table'></i> Table View"), value = "table",
                           h3("Table options", align = "center"),
                           fluidRow(
                             column(
                               width = 6,
                               numericInput("num.digits2", "Number of Digits", value = 5, min = 0, max = 10)
                             ),
                             column(
                               width = 6,
                               radioButtons("fixedWidth2", "Table format",
                                            choices = c("Wide format", "Long format"),
                                            selected = "Long format")
                             )
                           ),
                           fluidRow(
                             column(
                               width = 8,
                               selectInput("tableHeight2", "Table Height (Only for 'Long format')",
                                           choices  = c("200px", "400px", "600px", "800px", "1000px",
                                                        "1200px", "1400px", "1600px", "1800px", "2000px"),
                                           selected = "600px")
                             ),
                             column(
                               width = 4,
                               checkboxInput("scroller2", "Long format scroller?", value = TRUE)
                             )
                           ),
                           radioButtons("disp2", "Display",
                                        choices  = c("All" = "all", "Undersample" = "undersample"),
                                        selected = "all"),
                           numericInput("unders2", "Undersample every 'n' rows to view:", value = 400)
                           ),
                  
                  
                  tabPanel(HTML("<i class='fa fa-map'></i> Leaflet Map"), value = "map",
                           
                           # --- (B) Leaflet Map Options ---
                           tabPanel("Leaflet Map Options",
                                    h3("Leaflet Map Display Options", align="center"),
                                    
                                    fluidRow(
                                      column(
                                        width=6,
                                        # Map type selection
                                        radioButtons("mapType2", "Select Map Type:",
                                                     choices  = c("Satellite"="Esri.WorldImagery",
                                                                  "OpenStreetMap"="OpenStreetMap"),
                                                     selected = "OpenStreetMap"
                                        )
                                      ),
                                      column(
                                        width=6,
                                        # Scale bar width
                                        numericInput("scalebarwidth2", "Scale Bar Width (m):",
                                                     value=200, min=5, max=1000, step=1
                                        )
                                      )
                                    ),
                                    
                                    fluidRow(
                                      # Raw GPS point size
                                      column(
                                        width=6,
                                        numericInput("GPS_PS2", "GPS Point Size:",
                                                     value=3, min=0, max=80, step=0.1
                                        )
                                      ),
                                      # Raw GPS point color
                                      column(
                                        width=6,
                                        colourpicker::colourInput(
                                          "pointColor2", "GPS Point Color:",
                                          value="cyan"
                                        )
                                      ),
                                      
                                      # Raw GPS line width
                                      column(
                                        width=6,
                                        numericInput("GPS_LW2", "GPS Line width:",
                                                     value=1, min=0, max=10, step=0.1
                                        )
                                      ),
                                      # Raw GPS line color
                                      column(
                                        width=6,
                                        colourpicker::colourInput(
                                          "lineColor2", "GPS Line Color:",
                                          value="cyan"
                                        )
                                      ),
                                      # Display modes
                                      checkboxGroupInput(
                                        "display_modes2", "Display Modes:",
                                        choices  = c("Show GPS Points"="points",
                                                     "Show Connecting Lines"="lines",
                                                     "Show Clusters (only for leaflet circle markers)"="clusters",
                                                     "Enable Popup Info on GPS Point Click"="popups"),
                                        selected = c("points","lines")
                                      ),
                                    ),
                                    
                                    fluidRow(
                                      h5("(WebGl (leafgl) rendering is much faster, but only precise to a few metres."),
                                      h5("Base leaflet rendering is more precise, but slower for large data.)"),
                                      
                                      column(
                                        width=6,
                                        radioButtons("pointrendering2", "Point Rendering Method",
                                                     choices  = c("Base Leaflet Circle Markers"="leaflet_point",
                                                                  "WebGl Points (leafgl)"="leafgl_point"),
                                                     selected = "leaflet_point"
                                        )
                                      ),
                                      column(
                                        width=6,
                                        radioButtons("linerendering2", "Line Rendering Method",
                                                     choices  = c("Base Leaflet Poly Lines"="leaflet_line",
                                                                  "WebGl Lines (leafgl)"="leafgl_line"),
                                                     selected = "leaflet_line"
                                        )
                                      ),
                                      column(12,
                                      # Possibly a choice of color palettes
                                      selectInput(
                                        inputId = "gridColorPalette",
                                        label   = "Color Palette for Raster Layers:",
                                        choices = c("Default" = "default",
                                                    "Viridis" = "viridis",
                                                    "Plasma"  = "plasma",
                                                    "Spectral (RColorBrewer)" = "Spectral"),
                                        selected = "default"
                                      )
                                    )
                                    ),
                                    fluidRow(
                                    h4("Point Density Heat Map?", align="center"),
                                    column(6,
                                    radioButtons("heatmaptype", "Heat Map Type",
                                                 choices  = c("Leaflet" = "leafletmap",
                                                              "Kernel Density"= "KD"),
                                                 selected = "leafletmap"
                                    )
                                    ),
                                    column(6,
                                    checkboxInput("heatmap", "Add Heat Map?", FALSE)
                                    )
                                    ),
                                    fluidRow(
                                    conditionalPanel(
                                      condition = "input.heatmaptype == 'leafletmap'",
                                      
                                      column(
                                        width=12,
                                        h4("Leaflet Heatmap Settings")
                                      ),
                                    column(
                                      width=6,
                                    numericInput("blur", "blur:",
                                                 value=5, min=1, step=1
                                    )
                                    ),
                                    column(
                                      width=6,
                                      numericInput("maxPI", "Max Point Intensity:",
                                                   value=5, min=1, step=1
                                      )
                                    ),
                                    column(
                                      width=6,
                                      numericInput("radius", "Radius:",
                                                   value=15, min=1, step=1
                                      )
                                    ),
                                    column(
                                      width=6,
                                      numericInput("intensity", "Intensity:",
                                                   value=5, min=1, step=1
                                      )
                                    )
                                    )
                                    ),
                                    fluidRow(
                                    conditionalPanel(
                                      condition = "input.heatmaptype == 'KD'",
                                     
                                      column(
                                        width=12,
                                        h4("Kernel Density Heatmap Settings")
                                      ),
                                      
                                    column(
                                      width=12,
                                      numericInput("opacity_kd", "Opacity:",
                                                   value=0.8, min=0, max=1, step=0.05
                                      )
                                    ),
                                    column(12,
                                    checkboxInput("useTimeWeight", "KDE with Time Weighting?", FALSE)
                                    ),
                                    # Conditional panel for time-weighted inputs
                                    conditionalPanel(
                                      condition = "input.useTimeWeight == true",
                                      column(6,
                                      numericInput("maxTimeCapKDW", "Max Time Cap (s)", value = 3600, min = 1, step = 1)
                                      ),
                                      column(6,
                                      numericInput("gridsizeKDW", "Grid Cell Size (spatial units)", value = 0.001, min = 0, step = 0.00001)
                                      )
                                    ),
                                    # Conditional panel for standard (non-time-weighted) inputs
                                    conditionalPanel(
                                      condition = "input.useTimeWeight == false",
                                    column(
                                      width=12,
                                      radioButtons("BW", "Bandwidth Selector:",
                                                   choices  = c("bw.nrd0"="bw.nrd0",
                                                                "bw.nrd"="bw.nrd",
                                                                "bw.ucv" = "bw.ucv",
                                                                "bw.bcv" = "bw.bcv",
                                                                "bw.SJ" = "bw.SJ"), inline = TRUE,
                                                   selected = "bw.nrd0"
                                      )
                                    ),
                                    column(
                                      width=6,
                                      numericInput("gridsize", "Grid Size:",
                                                   value=50, min=1, step=1
                                      )
                                    )
                                    )
                                    )
                                    )

                           )
                           ),
                  
                  tabPanel(HTML("<i class='fa fa-sliders'></i> Residency Time/Revisitation Analysis"), value = "residency",
                           # Map type selection
                           radioButtons("utilizationType", "Approach for Estimating/Visualising 'Residency Time':",
                                        choices  = c("Grid-Based Summation" = "grids",
                                                     "Radius-Based 'Time in Circle'" = "circles",
                                                     "T-LoCoH (Time-Local Convex Hull)"),
                                        selected = "grids"
                           ),
                           conditionalPanel(
                             condition = "input.utilizationType == 'grids'",
                             h4("Grid-Based Summation Settings"),
                             
                             # Grid cell size in meters & anchoring coords?
                             fluidRow(
                             column(6,
                                    numericInput(
                                      inputId = "residGridSize",
                                      label   = "Grid Cell Size (m):",
                                      value   = 50, 
                                      min     = 1, 
                                      step    = 1
                                    )
                             ), 
                             column(6,
                                    checkboxInput("Anchor", "Anchor Origin?", value = FALSE))
                             ),
                             # Drop-out threshold in seconds (S)
                             numericInput(
                               inputId = "dropoutThreshold",
                               label   = "Drop-out Threshold (s):",
                               value   = 3600,
                               min     = 1,
                               step    = 1
                             ),
                          
                             h5("Cumulated Time (s)", align="center"),
                             
                             # Maximum capping for final color scale (if any):
                             numericInput(
                               inputId = "maxTimeCap",
                               label   = "Time Cap (s):",
                               value   = 3600,
                               min     = 0,
                               step    = 1
                             ),
                             h5("Revists (n)", align="center"),
                             numericInput(
                               inputId = "maxVistsCap",
                               label   = "Revisits Cap (n):",
                               value   = 200,
                               min     = 1,
                               step    = 1
                             ),
                             
                             h5("Mean GPS speed (m/s)", align="center"),
                             numericInput(
                               inputId = "maxSpeedCap",
                               label   = "Speed Cap (m/s):",
                               value   = 1,
                               min     = 0,
                               step    = 0.05
                               ),
                             h5("Mean VeDBA (g)", align="center"),
                             numericInput(
                               inputId = "maxVeDCap",
                               label   = "VeDBA Cap (g):",
                               value   = 1,
                               min     = 0,
                               step    = 0.01
                             ),
                             
                             # Whether to show final result in Leaflet or in a ggplot
                             # (some users might want either/both)
                             radioButtons("metricType", "Grid Metric to Display on Leaflet Map",
                                          choices  = c("None" = "Non",
                                                       "Residency Time (s)" = "resT",
                                                       "Revisits (n)" = "revs",
                                                       "Mean GPS speed (m/s)" = "GPSsp",
                                                       "Mean VeDBA (g)" = "VeD"),
                                          selected = "Non"
                             ),
                             checkboxInput("interpolatePaths", "Distribute Times Over Path Segments?", FALSE),
                             h5("Revisits counting is not yet supported with line interpolation."),
                             h5("If revisits is plotted with line interpolation, then just point density is considered"),
                             actionButton("calcResid", "Compute Grid Metrics")
                           )
                           
                  ),
                  
                  tabPanel(HTML("<i class='fa fa-chart-line'></i> Brownian Bridge Movement Model (dBBMM)"), value = "dBBMM",
                           numericInput(
                             inputId = "raster",
                             label   = "Raster:",
                             value   = 0.25,
                             step    = 1
                           ),
                           numericInput(
                             inputId = "location.error",
                             label   = "Location Error (m):",
                             value   = 20,
                             step    = 1
                           ),
                           numericInput(
                             inputId = "margin",
                             label   = "Margin:",
                             value   = 9,
                             step    = 1
                           ),
                           numericInput(
                             inputId = "window.size",
                             label   = "Window Size:",
                             value   = 10,
                             step    = 1
                           ),
                           numericInput(
                             inputId = "ext",
                             label   = "Extent:",
                             value   = 1.1,
                             step    = 0.01
                           ),
                           numericInput(
                             inputId = "time.step",
                             label   = "Time step (mins):",
                             value   = 1,
                             step    = 0.001
                           )
                           
                           ),
                  
                  id = "analysis_nav"
                )
            )
          ),
          mainPanel(
            width = 8,
            tabsetPanel(
              id = "analysis_tabs",
              tabPanel("Table",  withSpinner(DT::dataTableOutput("analysisTable"))),
              tabPanel("Leaflet Map", 
                       
                       h4("Time Filters", align="center"),
                       fluidRow(
                         
                         column(
                           width=3,
                           pickerInput(
                             "selectedYears2", "Select Years:",
                             choices = NULL,
                             options = list(
                               `actions-box`=TRUE,
                               `selected-text-format`="count > 3",
                               `count-selected-text`="{0} years selected",
                               `none-selected-text`="No years selected",
                               `live-search`=TRUE
                             ),
                             multiple=TRUE
                           )
                         ),
                         column(
                           width=3,
                           pickerInput(
                             "selectedMonths2", "Select Months:",
                             choices = NULL,
                             options = list(
                               `actions-box`=TRUE,
                               `selected-text-format`="count > 3",
                               `count-selected-text`="{0} months selected",
                               `none-selected-text`="No months selected",
                               `live-search`=TRUE
                             ),
                             multiple=TRUE
                           )
                         ),
                         
                         column(
                           width=3,
                           pickerInput(
                             "selectedDays2", "Select Days:",
                             choices = NULL,
                             options = list(
                               `actions-box`=TRUE,
                               `selected-text-format`="count > 3",
                               `count-selected-text`="{0} days selected",
                               `none-selected-text`="No days selected",
                               `live-search`=TRUE
                             ),
                             multiple=TRUE
                           )
                         ),
                         column(
                           width=3,
                           pickerInput(
                             "selectedHours2", "Select Hours:",
                             choices = NULL,
                             options = list(
                               `actions-box`=TRUE,
                               `selected-text-format`="count > 3",
                               `count-selected-text`="{0} hours selected",
                               `none-selected-text`="No hours selected",
                               `live-search`=TRUE
                             ),
                             multiple=TRUE
                           )
                         )
                       ),
                       fluidRow(
                       # Additional time-based pickers
                       column(
                         width=3,
                         pickerInput(
                           inputId = "selectedDayCount2",
                           label   = "Select Day Counter(s):",
                           choices = NULL,
                           options = list(
                             `actions-box`=TRUE,
                             `selected-text-format`="count > 3",
                             `count-selected-text`= "{0} days selected",
                             `none-selected-text`="No day counters selected",
                             `live-search`=TRUE
                           ),
                           multiple=TRUE
                         )
                       ),
                       column(
                         width=9,
                         # Timestamp range filter
                         uiOutput("timestampSliderUI2")
                       )
                       ),
                       fluidRow(
                         # Additional time-based pickers
                         column(width=9,
                       actionButton("refreshMap2", "Plot/Refresh Map",
                                    style="width:100%; margin-top:20px; font-size:16px;"
                       )),
                       column(width=3,
                       actionButton("downloadMap2", "Download Map Image")
                       )
                       ),
                       fluidRow(
                         column(12,
                                withSpinner(leafletOutput("analysisMap", height="650px"))
                         )
                       )
              ),
              tabPanel("ggPlots", 
                       tabsetPanel(
                         tabPanel("Plot 1", plotOutput("ggPlot1")),
                         tabPanel("Plot 2", plotOutput("ggPlot2"))
                       )
              )
            )
          )
        )
      )
    })
    
    
    ##########################################################################################################################################
    ##########################################################################################################################################
    ##########################################################################################################################################
    # GPS analysis server side
    observeEvent(input$GPSanalysis, {
      # 1) We get the main processed data from gpsProcessedData()
      req(gpsProcessedData(), nrow(gpsProcessedData())>0, ncol(gpsProcessedData())>0)
    
      df_full <- gpsProcessedData()
    
      # 2) Subset conditions
      #   - Keep only rows with non-NA "GPS_Longitude_Filtered" & "GPS_Latitude_Filtered"
      #   - Keep rows where combined_verdict is NA or "Not Anomalous" 
      #       (i.e., remove "Anomalous" rows)
      
      df_sub <- df_full %>%
        dplyr::filter(
          !is.na(GPS_Longitude_Filtered), 
          !is.na(GPS_Latitude_Filtered),
          is.na(combined_verdict) | combined_verdict == "Not Anomalous"
        )
    
      # 3) Remove redundant columns. For example:
      #    - GPS_Longitude, GPS_Latitude, Time_diff, Dist_from_median
      #    - (You can remove other columns as well, or keep whichever you want)
      
      columns_to_drop <- c("GPS_Longitude", "GPS_Latitude", "Time_diff_filtered", "Dist_from_median")
      df_sub <- df_sub %>% dplyr::select(-dplyr::any_of(columns_to_drop))
    
      # 4) Recalculate base metrics with retained fixes
      df_sub <- df_sub %>%
        dplyr::filter(!is.na(GPS_Longitude_Filtered), !is.na(GPS_Latitude_Filtered)) %>%
        dplyr::rename(Longitude = GPS_Longitude_Filtered, Latitude = GPS_Latitude_Filtered) %>%
        mutate(Time_diff = c(0, as.numeric(difftime(Timestamp, dplyr::lag(Timestamp), units="secs")[-1])),
               Longitude.lag  = dplyr::lag(Longitude, 1, default=NA),
               Latitude.lag   = dplyr::lag(Latitude, 1, default=NA),
               Longitude.lead = dplyr::lead(Longitude, 1, default=NA),
               Latitude.lead  = dplyr::lead(Latitude, 1, default=NA),
               Ang.lag  = beary(Longitude, Latitude, Longitude.lag, Latitude.lag),
               Ang.lead = beary(Longitude, Latitude, Longitude.lead, Latitude.lead),
               Ang.lag  = ifelse(Ang.lag < 0, Ang.lag + 360, Ang.lag),
               Ang.lead = ifelse(Ang.lead < 0, Ang.lead + 360, Ang.lead),
               Ang_vertex = abs((Ang.lead - Ang.lag + 360) %% 360 - 180),
               Dist.lag   = disty(Longitude, Latitude, Longitude.lag, Latitude.lag),
               Dist.lead  = disty(Longitude, Latitude, Longitude.lead, Latitude.lead),
               Time.diff.lag  = as.numeric(difftime(Timestamp, dplyr::lag(Timestamp), units="secs")),
               Time.diff.lead = dplyr::lead(Time.diff.lag, 1, default=NA),
               Outgoing_speed = Dist.lag / Time.diff.lag,
               Incoming_speed = Dist.lead / Time.diff.lead,
               Dist_circular  = disty(Longitude.lag, Latitude.lag, Longitude.lead, Latitude.lead)
        ) %>% 
        dplyr::select(-Longitude.lag, -Latitude.lag, -Longitude.lead, -Latitude.lead, -Ang.lag, -Ang.lead, -Dist.lag, -Dist.lead, -Time.diff.lag, -Time.diff.lead)
    
      # Recaluate mean VeDBA between retained fixes
      if (input$VeDInt=="yes" && "IMU_Mean_VeDBA" %in% colnames(filtered_data())) {
        imu_data <- filtered_data() %>%
          dplyr::filter(!is.na(IMU_Mean_VeDBA))
        
        if (nrow(imu_data)==0) {
          spsComps::shinyCatch({
            stop("IMU data is missing or not properly selected", call.=FALSE)
          }, blocking_level="error",
          prefix="WildPulse EcoMove Analytics",
          position="bottom-right")
        }
        
        ACC_TS <- imu_data$Timestamp
        VeDBA  <- imu_data$IMU_Mean_VeDBA
        
        # Create an "acc_df" to insert into df_sub
        acc_df <- data.frame(matrix(NA, nrow=length(ACC_TS), ncol=ncol(df_sub)))
        colnames(acc_df) <- colnames(df_sub)
        
        acc_df$Timestamp <- ACC_TS
        acc_df$VeDBA     <- VeDBA
        
        # Combine + reorder
        df_sub <- bind_rows(df_sub, acc_df) %>%
          arrange(Timestamp)
        
        # fix_group: increments for each valid fix
        df_sub <- df_sub %>%
          mutate(
            fix_group = cumsum(!is.na(Longitude) & !is.na(Latitude))
          )
        
        # Reverse for backward interval computations
        df_reversed <- df_sub %>%
          arrange(desc(Timestamp)) %>%
          mutate(
            fix_group = cumsum(!is.na(Longitude) & !is.na(Latitude))
          ) %>%
          group_by(fix_group) %>%
          mutate(
            mean_VeDBA_interval = ifelse(
              !is.na(Longitude) & !is.na(Latitude),
              mean(VeDBA, na.rm=TRUE), NA
            ),
            sd_VeDBA_interval = ifelse(
              !is.na(Longitude) & !is.na(Latitude),
              sd(VeDBA, na.rm=TRUE), NA
            ),
            num_VeDBA_bursts = ifelse(
              !is.na(Longitude) & !is.na(Latitude),
              sum(!is.na(VeDBA)), NA
            )
          ) %>%
          ungroup() %>%
          arrange(Timestamp) %>%
          dplyr::select(-fix_group)
        
        # Remove rows that are pure IMU only (have no GPS data?)
        df_sub <- subset(df_reversed, !is.na(Observations))
        df_sub$VeDBA <- NULL  # remove that temporary column
      }
      
      #######################################################################
      # 5) Update the reactiveVal
      GPSanalysis(df_sub)
    
      #  Show a notification
      showNotification("GPS Analysis subset created successfully! Metrics recalulated from retained fixes", type = "message")
      
    })
    
    # GPS analysis table
    output$analysisTable <- DT::renderDataTable({
      # If no processed data or empty, show message
      if (is.null(GPSanalysis()) || nrow(GPSanalysis())==0) {
        return(DT::datatable(data.frame(Message="No valid rows in analysis subset")))
      }
      req(GPSanalysis(), nrow(GPSanalysis())>0, ncol(GPSanalysis())>0)
      
      df <- GPSanalysis()  # The subset we created
      
      # Decide which DT options to use
      if (input$fixedWidth2 == "Long format") {
        scroller <- input$scroller2
        if (scroller) {
          datatable_options <- rv3$options
        } else {
          datatable_options <- rv2$options
        }
        datatable_options$scrollY <- input$tableHeight2
      } else if (input$fixedWidth2 == "Wide format") {
        datatable_options <- rv1$options
      }
      
      # Undersample vs full display
      if (input$disp2=="all") {
        DT::datatable(
          df,
          options     = datatable_options,
          selection   ='single',
          filter      ='top',
          extensions  = c("ColReorder","FixedColumns","FixedHeader","Scroller"),
          rownames    =FALSE
        ) %>%
          formatRound(columns = c(which(sapply(df, is.numeric))), v$num.digits2) %>%
          formatStyle(columns = c(1:ncol(df)), 'text-align'='center') %>%
          formatDate("Timestamp", method='toLocaleString')
        
      } else if (input$disp2 =="undersample") {
        
        i <- InputValidator$new()
        i$add_rule("unders2", sv_required("Number must be provided"))
        i$add_rule("unders2", sv_gte(1))
        i$enable()
        req(i$is_valid())
        
        v$undersample2 <- as.integer(input$unders2)
        
        # Slice every nth row
        display_data <- df %>%
          dplyr::slice(seq(1, n(), by=v$undersample2))
        
        DT::datatable(
          display_data,
          options     = datatable_options,
          selection   ='single',
          filter      ='top',
          extensions  = c("ColReorder","FixedColumns","FixedHeader","Scroller"),
          rownames    =FALSE
        ) %>%
          formatRound(columns=which(sapply(display_data, is.numeric)), v$num.digits2) %>%
          formatStyle(columns=c(1:ncol(df)), 'text-align'='center') %>%
          formatDate("Timestamp", method='toLocaleString')
      }
    })
    
    ################################################################################
    ##                    Residency Time calculation                             ##
    ################################################################################
    
    ################################################################################################################################
    ################################################################################################################################
    # Server-Side Outline for Creating the Grid Res time Summation
    # --------------------------------------------
    # 2) Observe the "Compute Grid Metrics" button
    # --------------------------------------------
    observeEvent(input$calcResid, {
      req(GPSanalysis(), nrow(GPSanalysis())>0, ncol(GPSanalysis())>0)
      # Step A: Get the data
      df_raw <- GPSanalysis()
      
      # Time filter
      if (!is.null(input$timestampRange2) && length(input$timestampRange2)==2) {
        df_raw <- df_raw %>%
          dplyr::filter(
            Timestamp>=input$timestampRange2[1],
            Timestamp<=input$timestampRange2[2],
            Hour     %in% input$selectedHours2,
            Day      %in% input$selectedDays2,
            Month    %in% input$selectedMonths2,
            Year     %in% input$selectedYears2,
            day_count%in% input$selectedDayCount2
          )
      } else {
        # No slider input, so apply everything else
        df_raw <- df_raw %>%
          dplyr::filter(
            Hour     %in% input$selectedHours2,
            Day      %in% input$selectedDays2,
            Month    %in% input$selectedMonths2,
            Year     %in% input$selectedYears2,
            day_count %in% input$selectedDayCount2
          )
      }
      
      # If no data left, notify
      if (nrow(df_raw)==0) {
        showNotification("No data available for the selected filters.", type="warning")
        return(NULL)
      }
      
      # Step B: Decide bounding box, do the projection
      sf_raw <- st_as_sf(df_raw, coords = c("Longitude","Latitude"), crs = 4326)
      # bounding box center for AEQD
      bbox <- st_bbox(sf_raw)
      center_lon <- (bbox$xmin + bbox$xmax)/2
      center_lat <- (bbox$ymin + bbox$ymax)/2
      
      local_crs <- sprintf(
        "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
        center_lat, center_lon
      )
      
      sf_proj <- st_transform(sf_raw, local_crs)
      
      # Add X_m, Y_m columns to df for convenience
      coords_proj <- st_coordinates(sf_proj)
      df <- df_raw
      df$X_m <- coords_proj[,1]
      df$Y_m <- coords_proj[,2]
   
      # Step C: If the user wants "interpolationPaths" == TRUE,
      # we do the path-based approach. If not, do the fix-based approach.
      if (isTRUE(input$interpolatePaths)) {
        # (1) Sort by time, build consecutive line segments
        df <- df[order(df$Timestamp), ]
        df <- df[!is.na(df$X_m) & !is.na(df$Y_m), ]
        
        df$X_m_lead <- dplyr::lead(df$X_m)
        df$Y_m_lead <- dplyr::lead(df$Y_m)
        df$Timestamp_lead <- dplyr::lead(df$Timestamp)
        df$Time_diff_lead <- as.numeric(difftime(df$Timestamp_lead, df$Timestamp, units="secs"))
        
        # If user doesn't have mean_VeDBA_interval:
        has_vedba <- "mean_VeDBA_interval" %in% colnames(df)
        if (has_vedba) {
          df$vedba_avg <- df$mean_VeDBA_interval
        } else {
          df$vedba_avg <- NA_real_
        }
        
        df_lines <- df[!is.na(df$X_m_lead), ]
        
        line_list <- lapply(seq_len(nrow(df_lines)), function(i) {
          st_linestring( matrix(c(
            df_lines$X_m[i],      df_lines$Y_m[i],
            df_lines$X_m_lead[i], df_lines$Y_m_lead[i]
          ), ncol=2, byrow=TRUE) )
        })
        sfc_lines <- st_sfc(line_list, crs=local_crs)
        sf_lines <- st_sf(
          geometry     = sfc_lines,
          time_segment = df_lines$Time_diff_lead,
          speed_avg    = df_lines$Outgoing_speed,
          vedba_avg    = df_lines$vedba_avg,
          do_revisit   = 1
        )
        # Add the full_length_m column
        sf_lines$full_length_m <- as.numeric(st_length(sf_lines$geometry))
        
        # (2) Build the polygon grid
        cell_size <- input$residGridSize
        bb_proj   <- st_bbox(sf_proj)
        if (isTRUE(input$Anchor)) {
          xmin_aligned <- floor(bb_proj["xmin"] / cell_size) * cell_size
          ymin_aligned <- floor(bb_proj["ymin"] / cell_size) * cell_size
          xmax_aligned <- ceiling(bb_proj["xmax"] / cell_size) * cell_size
          ymax_aligned <- ceiling(bb_proj["ymax"] / cell_size) * cell_size
        } else {
          xmin_aligned <- bb_proj["xmin"]
          ymin_aligned <- bb_proj["ymin"]
          xmax_aligned <- bb_proj["xmax"]
          ymax_aligned <- bb_proj["ymax"]
        }
        r_template <- rast(
          xmin=xmin_aligned, xmax=xmax_aligned,
          ymin=ymin_aligned, ymax=ymax_aligned,
          resolution=cell_size,
          crs=st_crs(local_crs)$wkt
        )
        spat_poly <- as.polygons(r_template)
        sf_grid   <- st_as_sf(spat_poly)
        sf_grid$cell_id <- seq_len(nrow(sf_grid))
        
        # (3) st_intersection
        line_poly <- st_intersection(
          st_make_valid(sf_grid),
          st_make_valid(sf_lines)
        )
        
        # (4) For each partial line in line_poly, compute fraction & partial sums
        line_poly$seg_length_m <- as.numeric(st_length(line_poly))
        line_poly$fraction <- line_poly$seg_length_m / line_poly$full_length_m
        line_poly$fraction[line_poly$fraction > 1] <- 1
        line_poly$time_in_cell <- line_poly$time_segment * line_poly$fraction
        line_poly$speed_sum    <- line_poly$speed_avg  * line_poly$time_in_cell
        line_poly$vedba_sum    <- line_poly$vedba_avg  * line_poly$time_in_cell
        
        # (5) Summarise *after* dropping geometry
        cell_summary <- line_poly %>%
          st_drop_geometry() %>%
          group_by(cell_id) %>%
          summarise(
            sum_time    = sum(time_in_cell, na.rm=TRUE),
            sum_speed   = sum(speed_sum,    na.rm=TRUE),
            sum_vedba   = sum(vedba_sum,    na.rm=TRUE),
            sum_revisits= sum(do_revisit,   na.rm=TRUE),
            sum_weights = sum(time_in_cell, na.rm=TRUE)
          ) %>%
          ungroup()
        
        cell_summary$mean_speed <- ifelse(
          cell_summary$sum_weights > 0,
          cell_summary$sum_speed / cell_summary$sum_weights,
          NA
        )
        cell_summary$mean_vedba <- ifelse(
          cell_summary$sum_weights > 0,
          cell_summary$sum_vedba / cell_summary$sum_weights,
          NA
        )
        
        # (6) Join with sf_grid
        sf_grid_metrics <- sf_grid %>%
          left_join(cell_summary, by="cell_id")
        
        # (7) clamp the metrics
        sf_grid_metrics$sum_time    <- pmin(sf_grid_metrics$sum_time,    input$maxTimeCap)
        sf_grid_metrics$mean_speed  <- pmin(sf_grid_metrics$mean_speed,  input$maxSpeedCap)
        sf_grid_metrics$mean_vedba  <- pmin(sf_grid_metrics$mean_vedba,  input$maxVeDCap)
        sf_grid_metrics$sum_revisits<- pmin(sf_grid_metrics$sum_revisits,input$maxVistsCap)
        
        # (8) rasterize
        makeSpatRaster <- function(sf_polygons, value_col) {
          # Convert to terra vect
          v_spat <- vect(sf_polygons)
          # Use the same r_template we made above
          # Then rasterize
          r_out <- raster::rasterize(
            x     = v_spat,
            y     = r_template,
            field = value_col,
            fun   = "max"   # or "sum", or "mean"—but typically we store the final value in the polygon
          )
          # Reproject to lat-lon if we want
          r_out_4326 <- project(r_out, "EPSG:4326", method="near")
          r_out_4326
        }
        
        r_time    <- makeSpatRaster(sf_grid_metrics, "sum_time")
        r_speed   <- makeSpatRaster(sf_grid_metrics, "mean_speed")
        r_vedba   <- makeSpatRaster(sf_grid_metrics, "mean_vedba")
        r_revis   <- makeSpatRaster(sf_grid_metrics, "sum_revisits")
        
        # Store them in a list
        res_list <- list(
          resT = r_time,
          GPSsp= r_speed,
          VeD  = r_vedba,
          revs = r_revis
        )
        
        gridRasters(res_list)
        
      } else {
        # --------------------------------------------
        # 2D) The simpler fix-based approach
        # --------------------------------------------
        # Exactly your old code: each fix belongs to 1 cell, we do group_by cell -> aggregator
        # This is shorter, so I'll do it quickly:
        
        cell_size <- input$residGridSize
        # bounding box
        bb_proj <- st_bbox(sf_proj)
        if (isTRUE(input$Anchor)) {
          xmin_aligned <- floor(bb_proj["xmin"] / cell_size) * cell_size
          ymin_aligned <- floor(bb_proj["ymin"] / cell_size) * cell_size
          xmax_aligned <- ceiling(bb_proj["xmax"] / cell_size) * cell_size
          ymax_aligned <- ceiling(bb_proj["ymax"] / cell_size) * cell_size
        } else {
          xmin_aligned <- bb_proj["xmin"]
          ymin_aligned <- bb_proj["ymin"]
          xmax_aligned <- bb_proj["xmax"]
          ymax_aligned <- bb_proj["ymax"]
        }
        origin <- c(xmin_aligned, ymin_aligned)
        
        # Use your `ji()` function to center
        coords_mat <- cbind(df$X_m, df$Y_m)
        snapped    <- ji(coords_mat, origin=origin, cellsize=c(cell_size, cell_size))
        df$cell_x <- snapped[,1]
        df$cell_y <- snapped[,2]
        df$cell_id <- paste(df$cell_x, df$cell_y, sep="_")
        
        # If time_diff > dropoutThreshold => set to 0 or NA
        df$res_time <- ifelse(df$Time_diff > input$dropoutThreshold, 0, df$Time_diff)
        df$Outgoing_speed <- ifelse(df$Outgoing_speed > input$dropoutThreshold, 0, df$Outgoing_speed)
        
        # Revisit counting:
        df$revisit_bool <- FALSE  # default
        for (i in 2:nrow(df)) {
          if (!is.na(df$cell_id[i]) && !is.na(df$cell_id[i-1])) {
            if (df$cell_id[i] == df$cell_id[i-1]) {
              # same cell => not a new entry
            } else {
              # we just 'entered' a new cell => revisit
              df$revisit_bool[i] <- TRUE
            }
          }
        }
        
        # Summaries
        df$used_vedba <- if ("mean_VeDBA_interval" %in% colnames(df)) {
          df$mean_VeDBA_interval
        } else {
          NA_real_
        }
        
        fix_summary <- df %>%
          group_by(cell_id) %>%
          summarise(
            sum_time   = sum(res_time, na.rm=TRUE),
            sum_speed  = sum(Outgoing_speed, na.rm=TRUE),
            c_speed    = sum(!is.na(Outgoing_speed)),
            sum_vedba = sum(used_vedba, na.rm=TRUE),
            c_vedba   = sum(!is.na(used_vedba)),
            revisits = sum(revisit_bool, na.rm=TRUE)
          ) %>%
          ungroup()
        
        fix_summary$mean_speed <- ifelse(
          fix_summary$c_speed > 0,
          fix_summary$sum_speed / fix_summary$c_speed, NA
        )
        fix_summary$mean_vedba <- ifelse(
          fix_summary$c_vedba > 0,
          fix_summary$sum_vedba / fix_summary$c_vedba, NA
        )
        # revisit: if you want a better logic, you do another approach
        
        fix_summary$sum_time    <- pmin(fix_summary$sum_time,   input$maxTimeCap)
        fix_summary$mean_speed  <- pmin(fix_summary$mean_speed, input$maxSpeedCap)
        fix_summary$mean_vedba  <- pmin(fix_summary$mean_vedba, input$maxVeDCap)
        fix_summary$revisits   <- pmin(fix_summary$revisits,  input$maxVistsCap)
        
        # Build a polygon grid as before
        r_template <- rast(
          xmin = xmin_aligned, xmax = xmax_aligned,
          ymin = ymin_aligned, ymax = ymax_aligned,
          resolution = cell_size,
          crs = st_crs(local_crs)$wkt
        )
        # Initialize with NA
        values(r_template) <- NA_real_
        
        # We'll build a data frame of all possible cell centers:
        # we do the same xseq,yseq approach
        xseq <- seq(xmin_aligned + cell_size/2, xmax_aligned - cell_size/2, by=cell_size)
        yseq <- seq(ymin_aligned + cell_size/2, ymax_aligned - cell_size/2, by=cell_size)
        df_full <- expand.grid(cx=xseq, cy=yseq)
        df_full$cell_id <- paste(df_full$cx, df_full$cy, sep="_")
        
        # left_join the fix_summary
        df_full <- left_join(df_full, fix_summary, by="cell_id")
        
        # Turn it into squares
        sf_centers <- st_as_sf(
          df_full,
          coords = c("cx","cy"),
          crs = local_crs
        )
        half_cell <- cell_size/2
        make_square <- function(pt, half) {
          if (anyNA(pt)) return(st_polygon())
          x0 <- pt[1] - half
          x1 <- pt[1] + half
          y0 <- pt[2] - half
          y1 <- pt[2] + half
          st_polygon(list(matrix(
            c(x0,y0, x1,y0, x1,y1, x0,y1, x0,y0),
            ncol=2, byrow=TRUE
          )))
        }
        squares_list <- apply(st_coordinates(sf_centers), 1, make_square, half_cell)
        sfc_polys    <- st_sfc(squares_list, crs=local_crs)
        sf_poly      <- st_set_geometry(sf_centers, sfc_polys)
        
        # Now we can do the same approach: build a set of SpatRasters for each metric
        # We'll write a quick helper:
        do_rasterize <- function(sf_in, colname) {
          v_spat <- vect(sf_in)
          r_out  <- raster::rasterize(
            x     = v_spat,
            y     = r_template,
            field = colname,
            fun   = "max"
          )
          r_wgs84 <- project(r_out, "EPSG:4326", method="near")
          r_wgs84
        }
        
        r_time <- do_rasterize(sf_poly, "sum_time")
        r_sp   <- do_rasterize(sf_poly, "mean_speed")
        r_vd   <- do_rasterize(sf_poly, "mean_vedba")
        r_rev  <- do_rasterize(sf_poly, "revisits")
        
        # store them:
        res_list <- list(
          resT = r_time,
          GPSsp= r_sp,
          VeD  = r_vd,
          revs = r_rev
        )
        gridRasters(res_list)
      } # end if-else (interpolatePaths)
      
      # Done. Now gridRasters() is set to a list of 4 SpatRasters: resT, GPSsp, VeD, revs.
      # Next step is for your Leaflet code to pick the appropriate layer based on input$metricType.
      showNotification("Grid metrics computed!", type="message")
    })
        
    ################################################################################
    ##                    GPS PROCESSING MAP: TIME FILTERS & RENDERING            ##
    ################################################################################
    
    # === A) TIME-RELATED FILTERS (SLIDERS & PICKERS) =============================
    
    observe({
      # We need a valid processed dataset
      req(GPSanalysis(), nrow(GPSanalysis())>0, ncol(GPSanalysis())>0)
      
      # 1) Extract global min & max Timestamp
      min_timestamp <- min(GPSanalysis()$Timestamp, na.rm=TRUE)
      max_timestamp <- max(GPSanalysis()$Timestamp, na.rm=TRUE)
      
      # 2) Current slider input (if any)
      curr_slider <- isolate(input$timestampRange2)
      
      # 3) Decide new default or clamp
      if (is.null(curr_slider) || length(curr_slider)!=2) {
        # If no user selection, default to entire range
        new_val <- c(min_timestamp, max_timestamp)
      } else {
        # Ensure user’s previously chosen range is within the new min/max
        new_min <- max(curr_slider[1], min_timestamp)
        new_max <- min(curr_slider[2], max_timestamp)
        new_val <- c(new_min, new_max)
      }
      
      # 4) Re-render the slider with updated range
      output$timestampSliderUI2 <- renderUI({
        sliderInput(
          "timestampRange2","Select Timestamp Range:",
          min=min_timestamp, max=max_timestamp,
          value=new_val,
          timeFormat="%Y-%m-%d %H:%M:%S",
          width = "90%",
          step = 0.1
        )
      })
      
      # === Hour filter ===
      hours_available <- sort(unique(GPSanalysis()$Hour))
      curr_sel_h      <- isolate(input$selectedHours2)
      if (is.null(curr_sel_h) || length(curr_sel_h)==0) {
        new_sel_h <- hours_available
      } else {
        new_sel_h <- intersect(curr_sel_h, hours_available)
      }
      updatePickerInput(session,"selectedHours2", choices=hours_available, selected=new_sel_h)
      
      # === Day filter ===
      days_available <- sort(unique(GPSanalysis()$Day))
      curr_sel_d     <- isolate(input$selectedDays2)
      if (is.null(curr_sel_d) || length(curr_sel_d)==0) {
        new_sel_d <- days_available
      } else {
        new_sel_d <- intersect(curr_sel_d, days_available)
      }
      updatePickerInput(session, "selectedDays2", choices=days_available, selected=new_sel_d)
      
      # === Month filter ===
      months_available <- sort(unique(GPSanalysis()$Month))
      month_names      <- month.name[months_available]
      names(months_available) <- month_names
      curr_sel_m       <- isolate(input$selectedMonths2)
      if (is.null(curr_sel_m) || length(curr_sel_m)==0) {
        new_sel_m <- months_available
      } else {
        new_sel_m <- intersect(curr_sel_m, months_available)
      }
      updatePickerInput(session, "selectedMonths2",
                        choices=months_available,
                        selected=new_sel_m)
      
      # === Year filter ===
      years_available <- sort(unique(GPSanalysis()$Year))
      curr_sel_y      <- isolate(input$selectedYears2)
      if (is.null(curr_sel_y) || length(curr_sel_y)==0) {
        new_sel_y <- years_available
      } else {
        new_sel_y <- intersect(curr_sel_y, years_available)
      }
      updatePickerInput(session,"selectedYears2",
                        choices=years_available,
                        selected=new_sel_y)
      
      # === Day Count filter ===
      day_count_available <- sort(unique(GPSanalysis()$day_count))
      curr_sel_dc         <- isolate(input$selectedDayCount2)
      if (is.null(curr_sel_dc) || length(curr_sel_dc)==0) {
        new_sel_dc <- day_count_available
      } else {
        new_sel_dc <- intersect(curr_sel_dc, day_count_available)
      }
      updatePickerInput(session, "selectedDayCount2",
                        choices=day_count_available,
                        selected=new_sel_dc)
    })
    
    ################################################################################
    ##               C) LEAFLET MAP: RENDER BASE MAP & REFRESH MAP                 ##
    ################################################################################
    
    # Renders the base map only once
    output$analysisMap <- renderLeaflet({
      
      # 1) Basic Leaflet map
      map <- leaflet(options=leafletOptions(maxZoom=22, preferCanvas=TRUE)) %>%
        addProviderTiles(
          providers[[ input$mapType2 ]],
          options=providerTileOptions(
            updateWhenZooming=FALSE,
            updateWhenIdle=FALSE
          )
        ) %>%
        addScaleBar(
          position="bottomleft",
          options=scaleBarOptions(
            maxWidth = v$scalebarwidth2,
            metric   = TRUE,
            imperial = FALSE
          )
        ) %>%
        addDrawToolbar(
          targetGroup='draw',
          polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
          editOptions     = editToolbarOptions(selectedPathOptions=selectedPathOptions()),
          circleOptions   = drawCircleOptions(
            shapeOptions=drawShapeOptions(),
            showRadius =TRUE,
            metric     =TRUE,
            feet       =FALSE,
            repeatMode =FALSE
          ),
          rectangleOptions = drawRectangleOptions(
            shapeOptions=drawShapeOptions(),
            showArea    =TRUE,
            metric      =TRUE,
            repeatMode  =FALSE
          ),
          polygonOptions = drawPolygonOptions(
            shapeOptions=drawShapeOptions(),
            showArea    =TRUE,
            metric      =TRUE,
            repeatMode  =FALSE
          )
        ) %>%
        addMeasure(primaryLengthUnit="meters", secondaryLengthUnit="kilometers") %>%
        addFullscreenControl(position="topright", pseudoFullscreen=TRUE)
      
      # 2) If we have a stored bounding box, fit the map to that
      if (!is.null(boundsStored2())) {
        bb <- boundsStored2()  # c(minLon, minLat, maxLon, maxLat)
        map <- map %>%
          fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
      map
      
    })
    
    # Refresh or re-plot the map
    observeEvent(input$refreshMap2, {
      req(GPSanalysis(), nrow(GPSanalysis())>0, ncol(GPSanalysis())>0)
      
      # 1) Filter data by user’s time filters
      gps_data <- GPSanalysis()
      
      # 2) Define “custom” default palette if user picks “default”:
      my_default_colors <- c(
        "white", "lightblue", "cyan", "green", "yellowgreen",
        "yellow", "orange", "darkorange", "red", "darkred"
      )
      
      # 3) Construct the ‘palette’ depending on user selection:
      pal <- NULL
      chosen <- input$gridColorPalette
      
      if (!is.null(input$timestampRange2) && length(input$timestampRange2)==2) {
        gps_data <- gps_data %>%
          dplyr::filter(
            Timestamp>=input$timestampRange2[1],
            Timestamp<=input$timestampRange2[2],
            Hour     %in% input$selectedHours2,
            Day      %in% input$selectedDays2,
            Month    %in% input$selectedMonths2,
            Year     %in% input$selectedYears2,
            day_count%in% input$selectedDayCount2
          )
      } else {
        # No slider input, so apply everything else
        gps_data <- gps_data %>%
          dplyr::filter(
            Hour     %in% input$selectedHours2,
            Day      %in% input$selectedDays2,
            Month    %in% input$selectedMonths2,
            Year     %in% input$selectedYears2,
            day_count %in% input$selectedDayCount2
          )
      }
      
      # If no data left, notify
      if (nrow(gps_data)==0) {
        showNotification("No data available for the selected filters.", type="warning")
        return(NULL)
      }
      
      # Clear old layers
      leafletProxy("analysisMap", session) %>%
        clearGroup("GPS") %>%
        clearGroup("GridMetric") %>%
        clearGroup("KDHeat") %>%
        removeControl("GridMetricLegend") %>%
        removeControl("KDHeatLegend")

        
      # If map not centered yet, set bounding box
      if (!mapCentered2()) {
        if (nrow(gps_data)>0) {
          minLon <- min(gps_data$Longitude)
          maxLon <- max(gps_data$Longitude)
          minLat <- min(gps_data$Latitude)
          maxLat <- max(gps_data$Latitude)
          
          boundsStored2(c(minLon,minLat,maxLon,maxLat))
          
          leafletProxy("analysisMap") %>%
            fitBounds(minLon,minLat, maxLon,maxLat)
        }
        mapCentered2(TRUE)
      }
      
      # 4) If using 'leafgl_point'
      if(input$pointrendering2 == "leafgl_point"){
        if ("popups" %in% input$display_modes2) {
          label_map <- c(
            "Observations"           = "Observation: ",
            "Timestamp"              = "Timestamp: ",
            "Time_diff"              = "Time difference (s): ",
            "Longitude" = "Longitude: ",
            "Latitude"  = "Latitude: ",
            "mean_VeDBA_interval"         = "VeDBA (g): ",
            "Marked_Events"          = "Marked Events: "
          )
          
          x <- c("Observations","Timestamp","Time_diff",
                 "Longitude","Latitude","mean_VeDBA_interval",
                 "Marked_Events")
          common_cols_raw <- intersect(x, names(gps_data))
          popup <- gps_data %>%
            dplyr::select(all_of(common_cols_raw))
          
          # Round certain columns if present
          if ("Time_diff" %in% common_cols_raw) {
            popup$Time_diff <- round(popup$Time_diff, 3)
          }
          if ("Longitude" %in% common_cols_raw) {
            popup$Longitude <- round(popup$Longitude, 5)
          }
          if ("Latitude" %in% common_cols_raw) {
            popup$Latitude <- round(popup$Latitude, 5)
          }
          if ("mean_VeDBA_interval" %in% common_cols_raw) {
            popup$mean_VeDBA_interval <- round(popup$mean_VeDBA_interval, 3)
          }
          
          # rename them
          old_names <- names(popup)
          new_names <- label_map[old_names]
          keep_idx  <- !is.na(new_names)
          data.table::setnames(popup, old = old_names[keep_idx], new = new_names[keep_idx])
          
        } else {
          popup <- NULL
        }
      }
      
      # 5) Heat map?
      if(input$heatmap == TRUE){
        if(input$heatmaptype == "leafletmap"){
        leafletProxy("analysisMap") %>%
          addHeatmap(
            data = gps_data,  
            lng = ~Longitude,
            lat = ~Latitude,
            blur= v$blur, 
            max = v$maxPI, 
            intensity = v$intensity,
            group = "GPS",
            radius = v$radius)
      } else{
        ## Create kernel density output
        gps_data = as.data.frame(gps_data)
        
        if(input$useTimeWeight != TRUE){
        # Kernel desntiy estimate
        if(input$BW == "bw.nrd0"){
          bw = c(stats::bw.nrd0(gps_data$Longitude), stats::bw.nrd0(gps_data$Latitude))
        } else if(input$BW == "bw.nrd"){
          bw = c(stats::bw.nrd(gps_data$Longitude), stats::bw.nrd(gps_data$Latitude))
        } else if(input$BW == "bw.ucv"){
          bw = c(stats::bw.ucv(gps_data$Longitude), stats::bw.ucv(gps_data$Latitude))
        } else if(input$BW == "bw.bcv"){
          bw = c(stats::bw.bcv(gps_data$Longitude), stats::bw.bcv(gps_data$Latitude))
        } else {
          bw = c(stats::bw.SJ(gps_data$Longitude), stats::bw.SJ(gps_data$Latitude))
        }
        kde2D <- KernSmooth::bkde2D(gps_data[, c('Longitude', 'Latitude')],
                      bandwidth = bw, gridsize = c(v$gridsize, v$gridsize))
        # Create Raster from Kernel Density output
        KernelDensityRaster <- raster::raster(list(x=kde2D$x1 ,y=kde2D$x2 ,z = kde2D$fhat))
        #set low density cells as NA so we can make them transparent with the colorNumeric function
        KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA
        
        #create pal function for coloring the raster
        if (chosen == "default") {
          palRaster <- colorNumeric(my_default_colors, domain = KernelDensityRaster@data@values, na.color = "transparent")
        } else if (chosen == "viridis") {
          palRaster <- colorNumeric("viridis", domain = KernelDensityRaster@data@values, na.color = "transparent")
        } else if (chosen == "plasma") {
          palRaster <- colorNumeric("plasma", domain = KernelDensityRaster@data@values, na.color = "transparent")
        } else if (chosen == "Spectral") {
          palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")
        }
        
        ## Leaflet map with raster
        leafletProxy("analysisMap") %>%
          addRasterImage(KernelDensityRaster, 
                         colors = palRaster, 
                         group    = "KDHeat",
                         layerId  = "KDHeatLayer",
                         opacity = v$opacity_kd) %>%
          leaflet::addLegend(pal = palRaster, 
                    values = KernelDensityRaster@data@values, 
                    title = "Kernel Density of Points",
                    layerId  = "KDHeatLegend")
        } else if(input$useTimeWeight){
          
          # Weighted SD and IQR for X
          wx <- gps_data$Longitude; wy <- gps_data$Latitude  # coordinates of filtered points
          time_diffs_capped <- pmin(gps_data$Time_diff, v$maxTimeCapKDW)
          # exponential decay
          raw_wts <- exp(- time_diffs_capped / v$maxTimeCapKDW)
          # Normalize weights
          sum_of_raw <- sum(raw_wts)
          wts <- raw_wts / sum_of_raw
          n_eff <- sum_of_raw
          # Weighted mean and variance for X
          mu_x <- sum(wts * wx) / n_eff
          sd_x <- sqrt(sum(wts * (wx - mu_x)^2) / n_eff)
          # Weighted IQR for X (approximate via weighted quantiles)
          q1_x <- quantile(wx, 0.25, type=7, weights = wts, na.rm = TRUE)  # using a weights-capable quantile function
          q3_x <- quantile(wx, 0.75, type=7, weights = wts, na.rm = TRUE)
          IQR_x <- q3_x - q1_x
          # Silverman bandwidth for X
          bw_x <- 0.9 * min(sd_x, IQR_x/1.34) * (n_eff^(-1/5))
          # Repeat for Y
          mu_y <- sum(wts * wy) / n_eff
          sd_y <- sqrt(sum(wts * (wy - mu_y)^2) / n_eff)
          q1_y <- quantile(wy, 0.25, type=7, weights = wts, na.rm = TRUE)
          q3_y <- quantile(wy, 0.75, type=7, weights = wts, na.rm = TRUE)
          IQR_y <- q3_y - q1_y
          bw_y <- 0.9 * min(sd_y, IQR_y/1.34) * (n_eff^(-1/5))
          bandwidth <- c(bw_x, bw_y)
          # Determine spatial range based on filtered data
          x_range <- range(wx)
          y_range <- range(wy)
          # Compute grid vectors based on grid cell size
          x_seq <- seq(x_range[1], x_range[2], by = v$gridsizeKDW)
          y_seq <- seq(y_range[1], y_range[2], by = v$gridsizeKDW)
          grid_size <- c(length(x_seq), length(y_seq))
          # Perform weighted 2D KDE with ks::kde
          fhat <- kde(x = cbind(wx, wy), 
                      w = wts, 
                      H = diag(bandwidth^2),    # bandwidth matrix using bw for X and Y
                      xmin = c(x_range[1], y_range[1]), 
                      xmax = c(x_range[2], y_range[2]), 
                      gridsize = grid_size)
          # Extract results
          dens_mat <- fhat$estimate        # 2D density estimate matrix
          x_grid <- fhat$eval.points[[1]]  # grid coordinates for x
          y_grid <- fhat$eval.points[[2]]  # grid coordinates for y
          # Create raster from density matrix, with appropriate extent
          density_list <- list(x = x_grid, y = y_grid, z = dens_mat)
          
          # Create raster from density matrix, with appropriate extent
          dens_raster <- raster::raster(density_list) 
          crs(dens_raster) <- sp::CRS("+init=EPSG:4326")
          #set low density cells as NA so we can make them transparent with the colorNumeric function
          dens_raster@data@values[which(dens_raster@data@values < 1)] <- NA
          #create pal function for coloring the raster
          if (chosen == "default") {
            palRaster <- colorNumeric(my_default_colors, domain = values(dens_raster), na.color = "transparent")
          } else if (chosen == "viridis") {
            palRaster <- colorNumeric("viridis", domain = values(dens_raster), na.color = "transparent")
          } else if (chosen == "plasma") {
            palRaster <- colorNumeric("plasma", domain = values(dens_raster), na.color = "transparent")
          } else if (chosen == "Spectral") {
            palRaster <- colorNumeric("Spectral", domain = values(dens_raster), na.color = "transparent")
          }
          
          ## Leaflet map with raster
          leafletProxy("analysisMap") %>%
            addRasterImage(dens_raster, 
                           colors = palRaster, 
                           group    = "KDHeat",
                           layerId  = "KDHeatLayer",
                           project = FALSE,
                           opacity = v$opacity_kd) %>%
            leaflet::addLegend(pal = palRaster, 
                      values = dens_raster@data@values, 
                      title = "Time-Weighted Kernel Density of Points",
                      layerId  = "KDHeatLegend")
        }
      }
        
        
      }
      
      ############################# 6) Show GPS Lines - raw data #############################
      if ("lines" %in% input$display_modes2) {
        #### Leaflet
        if(input$linerendering2 == "leaflet_line"){
          leafletProxy("analysisMap") %>%
            addPolylines(
              data = gps_data,  
              lng = ~Longitude,
              lat = ~Latitude,
              color = input$lineColor2,
              weight = v$GPS_LW2,
              opacity = 0.7,
              group = "GPS"
            )
          
          #### Leafgl
        } else if (input$linerendering2 == "leafgl_line") {
          df_line_raw <- gps_data %>%  #Convert to one linestring
            arrange(Timestamp) %>%
            st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("LINESTRING")
          
          leafletProxy("analysisMap") %>%
            addGlPolylines(
              data = df_line_raw,  
              color = input$lineColor2,
              weight = v$GPS_LW2,
              digits = 12,
              group = "GPS"
            )
        }
      }
      
      ############################# 7) Show GPS Points - raw data #############################
      if ("points" %in% input$display_modes2) {
        # Add raw GPS overlay 
        # Leaflet
        if(input$pointrendering2 == "leaflet_point"){
          leafletProxy("analysisMap") %>%
            addCircleMarkers(
              data = gps_data,  
              lng = ~Longitude,
              lat = ~Latitude,
              fillColor = input$pointColor2,  # Map Color column
              color = "black",  # Border color
              radius = v$GPS_PS2,
              stroke = TRUE,
              fillOpacity = 0.7,
              weight = v$GPS_PS2*.2,
              group = "GPS",
              popup = if ("popups" %in% input$display_modes2) {
                ~paste(
                  "<strong>Observation:</strong> ", Observations, "<br>",
                  "<strong>Timestamp:</strong> ", as.POSIXct(Timestamp, origin = "1970-01-01"), "<br>",
                  "<strong>Time difference (s):</strong> ", round(Time_diff, 3), "<br>",
                  "<strong>Longitude:</strong> ", round(Longitude, 5), "<br>",
                  "<strong>Latitude:</strong> ", round(Latitude, 5), "<br>",
                  if ("mean_VeDBA_interval" %in% colnames(gps_data)) {
                    paste("<strong>VeDBA (g):</strong> ", round(mean_VeDBA_interval, 3), "<br>")
                  } else {
                    ""
                  },
                  if ("Marked_Events" %in% colnames(gps_data)) {
                    paste("<strong>Marked Events:</strong> ", Marked_Events, "<br>")
                  } else {
                    ""
                  }
                )
                
              } else NULL,
              clusterOptions = if ("clusters" %in% input$display_modes2) {
                markerClusterOptions()
              }
            )
          
          ###  Leafgl
        } else if (input$pointrendering2 == "leafgl_point") {
          
          # Raw GPS
          df_raw_sf <- sf::st_as_sf( # Convert to sf:
            gps_data,
            coords = c("Longitude", "Latitude"),
            crs    = 4326
          )
          
          leafletProxy("analysisMap") %>%
            addGlPoints(
              data = df_raw_sf,  
              fillColor = input$pointColor2,  
              radius = v$GPS_PS2,
              fillOpacity = 0.7,
              popup = popup,
              digits = 12,
              group = "GPS"
            )
        }
      }
      
      ######## 8) Grid summation residency time ########
      if (input$metricType != "Non") {
      req(gridRasters(), length(gridRasters()) > 0)
          metric_selected <- input$metricType
          sp_list <- gridRasters()
          r_to_show <- sp_list[[ metric_selected ]]
          req(r_to_show)
          # define color scale
          rng <- range(values(r_to_show), na.rm=TRUE)
       
        if (chosen == "default") {
          pal <- colorNumeric(
            palette  = my_default_colors,
            domain   = rng,
            na.color = "transparent"
          )
        } else if (chosen == "viridis") {
          pal <- colorNumeric(
            palette  = "viridis",
            domain   = rng,
            na.color = "transparent"
          )
        } else if (chosen == "plasma") {
          pal <- colorNumeric(
            palette  = "plasma",
            domain   = rng,
            na.color = "transparent"
          )
        } else if (chosen == "Spectral") {
          # For brewer palettes, you can do, e.g.:
          # colorNumeric("Spectral", domain = rng, na.color = "transparent")
          # or colorBrewer-based approach:
          pal <- colorNumeric(
            palette  = "Spectral",
            domain   = rng,
            na.color = "transparent"
          )
        }
        
        # Add the raster to Leaflet
        leafletProxy("analysisMap") %>%
          addRasterImage(
            x        = r_to_show,
            group    = "GridMetric",
            layerId  = "GridMetricLayer",
            colors   = pal,
            project  = FALSE,
            opacity  = 0.7
          ) %>%
          # add a legend
          leaflet::addLegend(
            position = "topright",
            pal      = pal,
            values   = rng,
            title    = paste("Metric:", metric_selected),
            layerId  = "GridMetricLegend"
          )
    }
    
    })
    
    # Download leaflet map plots
    #GPS preprocessing map
    observeEvent(input$downloadMap, {
      shinyscreenshot::screenshot(id = "gpsMap")
    })
    observeEvent(input$downloadMap2, {
      shinyscreenshot::screenshot(id = "analysisMap")
    })

    
    ################################################################################################################################
    ################################################################################################################################
    # ----------------------------------------------------------------------------------
    # (5) Data Export tab
    # ----------------------------------------------------------------------------------
    
    # Render UI for Data Export tab
    output$page5 <- renderUI({
  
      tagList(
        h4("Export GPS Analysis Data"),
        p("Select a file, file format and then click the button to download."),
        radioButtons(
          inputId = "datasetExport",
          label   = "Choose Dataset:",
          choices = c("Processed GPS Data (Data Table in 'GPS Analysis' Tab)" = "processed", "Raw GPS Data (Data Table in 'GPS Processing' Tab)" = "raw"),
          selected = "processed",
          inline = TRUE
        ),
        radioButtons(
          inputId = "exportFormat",
          label   = "File Format:",
          choices = c("CSV", "TXT", "RDS", "FST"),
          selected = "CSV",
          inline   = TRUE
        ),
        downloadButton(
          outputId = "downloadData",
          label    = "Download"
        )
      )
    })
    
      # The download handler for exporting data
      output$downloadData <- downloadHandler(
        filename = function() {
          # Choose base file name based on dataset selection
          base <- if (input$datasetExport == "processed") "GPSanalysis" else "gpsProcessedData"
          # Append file extension based on selected format
          switch(input$exportFormat,
                 "CSV" = "data_export.csv",
                 "TXT" = "data_export.txt",
                 "RDS" = "data_export.rds",
                 "FST" = "data_export.fst"
          )
        },
        content = function(file) {
          # Get the data
          # Select dataset based on user's choice
          data_to_export <- if (input$datasetExport == "processed") {
            GPSanalysis()
          } else {
            gpsProcessedData()
          }
          
          # Ensure the dataset is valid
          req(data_to_export, 
              nrow(data_to_export) > 0, 
              ncol(data_to_export) > 0)
          
          # Export based on selected format
          switch(input$exportFormat,
                 "CSV" = {
                   write.csv(data_to_export, file, row.names = FALSE, quote = FALSE)
                 },
                 "TXT" = {
                   write.table(data_to_export, file, sep = "\t", 
                               row.names = FALSE, quote = FALSE)
                 },
                 "RDS" = {
                   saveRDS(data_to_export, file = file)
                 },
                 "FST" = {
                   # For FST, ensure library(fst) is installed/loaded
                   write_fst(data_to_export, path = file)
                 }
          )
        }
      )

    
    ################################################################################################################################
    ################################################################################################################################
    # ----------------------------------------------------------------------------------
    # (6) User Notes Page
    # ----------------------------------------------------------------------------------
    output$page6 <- renderUI({
      fluidPage(
        useShinyjs(),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            fileInput("loadNotesFile", "Load Notes (.txt) File", accept = ".txt"),
            actionButton("saveNotes", "Save Notes"),
            br(), br(),
            downloadButton("exportNotes", "Export Notes", disabled = TRUE)
          ),
          mainPanel(
            width = 10,
            textAreaInput("userNotes", "Enter your notes here:", rows = 20, width = '100%'),
            br(),
            uiOutput("savedNotes")
          )
        )
      )
    })
    
    # Observe save notes button
    observeEvent(input$saveNotes, {
      user_notes(input$userNotes)
      
      # Enable or disable export button depending on presence of notes
      if (!is.null(input$userNotes) && input$userNotes != "") {
        shinyjs::enable("exportNotes")
      } else {
        shinyjs::disable("exportNotes")
      }
    })
    
    # Observe file input to load notes
    observeEvent(input$loadNotesFile, {
      if (!is.null(input$loadNotesFile) && input$loadNotesFile$datapath != "") {
        user_notes(readLines(input$loadNotesFile$datapath, warn=FALSE))
      }
    })
    
    # Display saved notes
    output$savedNotes <- renderUI({
      notes <- user_notes()
      if (length(notes) > 0) {
        HTML(
          paste0(
            "<div style='border: 1px solid #ddd; padding: 10px;'>",
            "<b>Saved Notes:</b><br>",
            paste(notes, collapse = "<br>"),
            "</div>"
          )
        )
      } else {
        HTML("<div style='border: 1px solid #ddd; padding: 10px;'><b>Saved Notes:</b></div>")
      }
    })
    
    # Export notes
    output$exportNotes <- downloadHandler(
      filename = function() {
        "user_notes.txt"
      },
      content = function(file) {
        cat(user_notes(), file = file)
      }
    )
     
    ########################   
   ###########################################################
     
     
  }) # END OF SHINY SERVER 
  
) # END OF SHINYAPP

##################################################################################################################################################################################################################################################################################################################################