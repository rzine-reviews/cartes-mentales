# Affichage
library(mapsf)

beyrouth <- st_transform(beyrouth, crs ="EPSG: 4326")
mf_map(beyrouth)

yx <- locator(1)
xy <- locator(1)


bbox_coords <- c(yx$x , xy$y , xy$x, yx$y)

bbox_polygon <- st_as_sf(st_sfc(st_polygon(list(matrix(c(
  bbox_coords[1], bbox_coords[2],  
  bbox_coords[3], bbox_coords[2],
  bbox_coords[3], bbox_coords[4],  
  bbox_coords[1], bbox_coords[4],
  bbox_coords[1], bbox_coords[2]   
), ncol = 2, byrow = TRUE))), crs = 4326))

mf_map(bbox_polygon)
mf_map(beyrouth, add = TRUE)

beyrouth_quartier <- st_intersection(x = beyrouth, y = bbox_polygon)
mf_map(beyrouth_quartier)

beyrouth_quartier <- st_transform(beyrouth_quartier, crs ="EPSG:32636")
st_write(beyrouth_quartier, dsn = "beyrouth.gpkg", layer = "quartier", delete_layer = TRUE)


# ----------------------------------------------

library(sf)
bbbox <- st_read("data/beyrouth.gpkg", layer = "quartier", quiet = TRUE)

# Définition d'une bounding box
q <- opq(bbox = st_bbox(st_transform(bbbox, 4326)))

# Extraction des restaurants
req <- add_osm_feature(opq = q, key = 'boundary', value = "administrative")
res <- osmdata_sf(req)

resto_point <- res$osm_lines

mapview(resto_point)


# TEST TO DELETE -PREPARATION ILLUSTRATIOPN ISOLIGNE
library(sf)
mentals_maps_macro_reg <- st_read(dsn = "data/geometries.gpkg", 
                                  layer = "mental_maps", quiet = TRUE)
mentals_maps_macro <- subset(mentals_maps_macro_reg, 
                             E1903_map_rgname_concept_fr %in% c("Caraïbes"))
emprise <- st_bbox(mentals_maps_macro)


# Création grille vectorielle de points (objet sfc)
grid_caraibes <- st_make_grid(x = emprise, 
                              cellsize = 500000,      # Résolution en mètres
                              square = TRUE,          # Forme 
                              what = "centers")       # Grille de points

# Ajout d'un attribut (identifiant) à l'objet sfc (= objet sf)
grid_caraibes <- st_sf(ID = 1:length(grid_caraibes), geom = grid_caraibes)
# Calcul des intersections grille - polygones
result_intersection <- st_intersects(x = grid_caraibes, 
                                     y = mentals_maps_macro, 
                                     sparse = TRUE) 
grid_caraibes$count <- lengths(result_intersection) 

library(mapsf)
mf_map(grid_caraibes)
mf_map(grid_caraibes, col = NA)
mf_label(grid_caraibes, var = "count", lines=FALSE)

# Discrétisation de la part des régions tracées nommées "Caraïbes" (8 classes)
breaks_area <- c(0,2,10,20,30,40,50,72)
library(mapiso)
iso_surface <- mapiso(x = grid_caraibes,
                      var = "count", 
                      breaks = breaks_area)

emprise <- st_bbox(iso_surface[iso_surface$isomin ==2,])

# Création grille vectorielle de points (objet sfc)
grid_caraibes <- st_make_grid(x = emprise, 
                              cellsize = 500000,      # Résolution en mètres
                              square = TRUE,          # Forme 
                              what = "centers")       # Grille de points

# Ajout d'un attribut (identifiant) à l'objet sfc (= objet sf)
grid_caraibes <- st_sf(ID = 1:length(grid_caraibes), geom = grid_caraibes)
# Calcul des intersections grille - polygones
result_intersection <- st_intersects(x = grid_caraibes, 
                                     y = mentals_maps_macro, 
                                     sparse = TRUE) 
grid_caraibes$count <- lengths(result_intersection) 

library(mapsf)
mf_map(grid_caraibes)
mf_map(grid_caraibes, col = NA)
mf_label(grid_caraibes, var = "count", lines=FALSE)

# Discrétisation de la part des régions tracées nommées "Caraïbes" (8 classes)
breaks_area <- c(0,5,10,20,30,40,50,72)

iso_surface <- mapiso(x = grid_caraibes,
                      var = "count", 
                      breaks = breaks_area)

library(mapsf)
mf_map(grid_caraibes)


mf_map(grid_caraibes, col = NA)
mf_label(grid_caraibes, var = "count", lines=FALSE, cex = 1.1)

mf_map(grid_caraibes, col = NA)
mf_label(grid_caraibes, var = "count", lines=FALSE, col="grey70")
mf_map(iso_surface, border = "black", col = NA, add = TRUE)


mf_map(grid_caraibes, col = NA)
mf_label(grid_caraibes, var = "count",col="white", cex = 1.1)
# cartographie des intersections
mf_map(x = iso_surface, 
       var = "isomin", 
       type = "choro", 
       breaks = breaks_area, 
       border = NA, 
       pal = "Rocket",
       alpha = 0.6, 
       leg_pos = c(-5225244,3146972), 
       leg_title = "",
       leg_val_rnd = 0, add=TRUE)
mf_label(grid_caraibes, var = "count",col="white", cex = 1.1)
# cartographie des intersections
mf_map(x = iso_surface, 
       var = "isomin", 
       type = "choro", 
       breaks = breaks_area, 
       border = NA, 
       pal = "Rocket",
       alpha = 0.6, 
       leg_pos = c(-5225244,3146972), 
       leg_title = "",
       leg_val_rnd = 0, add=TRUE)
