library(sf)
library(ggplot2)
library(dplyr)

center <- c(0, 0)
a <- 10   # width
b <- 12   # height
n <- 100  

electrodes <- data.frame(
  name = c("FP1", "FP2",
           "F7", "F3", "FZ", "F4", "F8", 
           "FT7", "FC3", "FCZ", "FC4", "FT8", 
           "T3", "C3", "CZ", "C4", "T4", 
           "TP7", "CP3", "CPZ", "CP4", "TP8", 
           "T5", "P3", "PZ", "P4", "T6", 
           "O1", "OZ", "O2"),
  x = c(-3, 3,
        -6.2, -3, 0, 3, 6.2,
        -7, -3, 0, 3, 7,
        -8, -3, 0, 3, 8,
        -8, -3, 0, 3, 8,
        -7, -3, 0, 3, 7,  
        -3.5, 0, 3.5),
  y = c(9.5, 9.5,
        6.5, 6.5, 6.5, 6.5, 6.5,
        3.5, 3, 3.5, 3, 3.5,
        0, 0, 0, 0, 0,
        -3.5, -3, -3.5, -3, -3.5,
        -6, -6.5, -6.5, -6.5,-6,
        -9.5, -10.5, -9.5) # Occipitals
)

electrodes$region <- case_when(
  electrodes$name %in% c("FP1", "FP2", "F7", "F3", "FZ", "F4", "F8") ~ "Frontal",
  electrodes$name %in% c("FC3", "FCZ", "FC4", "C3", "CZ", "C4") ~ "Central",
  electrodes$name %in% c("CP3", "CPZ", "CP4", "P3", "PZ", "P4") ~ "Parietal",
  electrodes$name %in% c("O1", "OZ", "O2") ~ "Occipital",
  electrodes$name %in% c("FT7", "FT8", "T3", "T4", "TP7", "TP8", "T5", "T6") ~ "Temporal",
  TRUE ~ "Other"
)

electrodes_sf <- st_as_sf(electrodes, coords = c("x", "y"), crs = NA)

create_head_boundary <- function(center = c(0, 0), a = 10, b = 12, n = 100){
  theta <- seq(0, 2 * pi, length.out = n)
  x <- center[1] + a * cos(theta)
  y <- center[2] + b * sin(theta)
  head_coords <- cbind(x, y)
  # Ensure the polygon is closed by adding the first point at the end
  head_coords <- rbind(head_coords, head_coords[1, ])
  head_polygon <- st_polygon(list(head_coords))
  return(head_polygon)
}

# Generate the scalp boundary as the head ellipse
scalp_boundary <- create_head_boundary(center, a, b, n)

scalp_sf <- st_sf(geometry = st_sfc(scalp_boundary))

region_lines <- list(
  st_linestring(rbind(c(-a, 8), c(a, 8))),       # Top horizontal line
  st_linestring(rbind(c(-a, 5), c(a, 5))),       # Second horizontal line
  
  st_linestring(rbind(c(-5, 2), c(5, 2))),       # Central upper horizontal line
  st_linestring(rbind(c(-5, -2), c(5, -2))),     # Central lower horizontal line
  
  st_linestring(rbind(c(-5, -5), c(5, -5))),     # Third horizontal line
  st_linestring(rbind(c(-a, -8), c(a, -8))),     # Bottom horizontal line
  
  st_linestring(rbind(c(-5, 5), c(-5, -8))),     # Left vertical line
  st_linestring(rbind(c(5, 5), c(5, -8)))        # Right vertical line
)

region_lines_sf <- st_sf(geometry = st_sfc(region_lines))

region_lines_clipped_sf <- st_intersection(region_lines_sf, scalp_sf)

nose_coords <- matrix(c(
  0, b + 2,   # Tip of the nose
  -1, b,      # Left base of the nose
  1, b,       # Right base of the nose
  0, b + 2    # Close the triangle
), ncol = 2, byrow = TRUE)
nose_polygon <- st_polygon(list(nose_coords))
nose_sf <- st_sf(geometry = st_sfc(nose_polygon))

ear_left_coords <- matrix(c(
  -a, 0 + 2,    # Top point
  -a - 2, 0 + 1,  # Top-left
  -a - 2, 0 - 1,  # Bottom-left
  -a, 0 - 2,    # Bottom point
  -a, 0 + 2     # Close the polygon
), ncol = 2, byrow = TRUE)
ear_left_polygon <- st_polygon(list(ear_left_coords))
ear_left_sf <- st_sf(geometry = st_sfc(ear_left_polygon))

ear_right_coords <- matrix(c(
  a, 0 + 2,    # Top point
  a + 2, 0 + 1,  # Top-right
  a + 2, 0 - 1,  # Bottom-right
  a, 0 - 2,    # Bottom point
  a, 0 + 2     # Close the polygon
), ncol = 2, byrow = TRUE)
ear_right_polygon <- st_polygon(list(ear_right_coords))
ear_right_sf <- st_sf(geometry = st_sfc(ear_right_polygon))

# Compute the Voronoi diagram using only the head boundary
electrode_points <- st_geometry(electrodes_sf)
voronoi_raw <- st_voronoi(electrode_points, envelope = scalp_boundary)

# Extract the Voronoi polygons from the result
voronoi_polygons <- st_collection_extract(voronoi_raw)

# Assign electrode names to the Voronoi polygons
voronoi_sf <- st_sf(name = electrodes$name, region = electrodes$region, geometry = voronoi_polygons)

# Clip the Voronoi polygons to the scalp boundary
voronoi_clipped <- st_intersection(voronoi_sf, scalp_sf)

# Clip the Voronoi polygons to the scalp boundary (for region assignment)
voronoi_clipped_sf <- st_sf(voronoi_clipped)

# Split the scalp boundary by the clipped region lines
region_lines_union <- st_union(region_lines_clipped_sf$geometry)
regions_split <- st_split(scalp_sf$geometry, region_lines_union)
regions_polygons <- st_collection_extract(regions_split, "POLYGON")
regions_sf <- st_sf(geometry = regions_polygons)

regions_sf$region <- NA

for(i in seq_len(nrow(regions_sf))){
  poly <- regions_sf$geometry[i]
  
  electrodes_in_poly <- electrodes_sf[st_within(electrodes_sf, poly, sparse = FALSE), ]
  
  regions_in_poly <- unique(electrodes_in_poly$region)
  
  if(length(regions_in_poly) == 1){
    regions_sf$region[i] <- regions_in_poly
  } else if(length(regions_in_poly) > 1){
    region_counts <- table(electrodes_in_poly$region)
    majority_region <- names(region_counts)[which.max(region_counts)]
    regions_sf$region[i] <- majority_region
  } else {
    centroid <- st_centroid(poly)
    coords <- st_coordinates(centroid)
    x_coord <- coords[,1]
    y_coord <- coords[,2]
    
    if(y_coord > 8){
      regions_sf$region[i] <- "Pre-frontal"
    } else if(y_coord > 5){
      regions_sf$region[i] <- "Frontal"
    } else if(y_coord > 2){
      regions_sf$region[i] <- "Central Frontal"
    } else if(y_coord > -2){
      regions_sf$region[i] <- "Central"
    } else if(y_coord > -6){
      regions_sf$region[i] <- "Parietal"
    } else {
      regions_sf$region[i] <- "Occipital"
    }
    if(x_coord < -6 && y_coord < 6 && y_coord > -6){
      regions_sf$region[i] <- "Temporal Left"
    }
    if(x_coord > 6 && y_coord < 6 && y_coord > -6){
      regions_sf$region[i] <- "Temporal Right"
    }
  }
}

for(i in seq_len(nrow(regions_sf))){
  # Get the i-th polygon
  poly <- regions_sf$geometry[i]
  # Find electrodes within the polygon
  electrodes_in_poly <- electrodes_sf[st_within(electrodes_sf, poly, sparse = FALSE), ]
  # Get unique regions of these electrodes
  regions_in_poly <- unique(electrodes_in_poly$region)
  if(length(regions_in_poly) == 1){
    regions_sf$region[i] <- regions_in_poly
  } else if(length(regions_in_poly) > 1){
    # Assign based on majority
    region_counts <- table(electrodes_in_poly$region)
    majority_region <- names(region_counts)[which.max(region_counts)]
    regions_sf$region[i] <- majority_region
  } else {
    # No electrodes in this polygon
    # Assign based on centroid location (optional)
    centroid <- st_centroid(poly)
    y_coord <- st_coordinates(centroid)[,2]
    if(y_coord > 5){
      regions_sf$region[i] <- "Frontal"
    } else if(y_coord > -5){
      regions_sf$region[i] <- "Central"
    } else if(y_coord > -10){
      regions_sf$region[i] <- "Parietal"
    } else {
      regions_sf$region[i] <- "Occipital"
    }
  }
}

ggplot() +
  geom_sf(data = scalp_sf, fill = NA, color = "black", alpha = 0.1) +
  geom_sf(data = regions_sf, aes(fill = region), color = "black", alpha = 0.3) +

  geom_sf(data = electrodes_sf, color = "white", size = 6) +
  geom_sf_text(data = electrodes_sf, aes(label = name), nudge_y = 0.3, cex = 2) +
  geom_sf(data = region_lines_clipped_sf, color = "black", size = 1.5) +
  
  geom_sf(data = nose_sf, fill = NA, color = "black") +
  geom_sf(data = ear_left_sf, fill = NA, color = "black") +
  geom_sf(data = ear_right_sf, fill = NA, color = "black") +
  theme_minimal() +
  ggtitle("EEG 10-20 System with Region Separations")

