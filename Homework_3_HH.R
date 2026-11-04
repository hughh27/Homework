# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset

#Problem 1
#Part A
vehicles_a <- vehicles %>%
  filter(displ <= 5)

plot_1a <- ggplot(vehicles_a, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              color = "#3366FF", linewidth = 1.2) +
  facet_wrap(~ drv) +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

plot_1a
ggsave("plot_1a.png", plot_1a, width = 9, height = 4.5, dpi = 150)

#Second part 
# Because of facet_wrap(~ drv). Faceting splits the data into one subset per
# value of drv (4, f, r) and draws EVERY layer separately in each panel. So
# geom_smooth() gets its own subset in each panel and fits a separate
# regression of hwy on displ for that drive type: 3 panels = 3 lines.
# The color = drv mapping in geom_point() does not cause this. It is local to
# the point layer, so it colors the points and creates the legend, but
# geom_smooth() never sees it. Without the facet (all cars in one panel),
# geom_smooth() would have no grouping variable and would draw just ONE line
# through all the cars. The lines are all the same blue because the smooth
# layer uses a fixed color, not a mapped one.

#Part B

ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  theme_bw()

#Second part 
ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()

# position = "fill" rescales every bar to add up to 1, so each bar shows the share
# of each cylinder count, not the number of cars. All bars are the same height no
# matter how many cars they hold. The count plot shows the real sizes differ:
# 103 four-wheel drive, 106 front-wheel drive, but only 25 rear-wheel drive cars.

#Part C
max_carat <- 3   # smaller table: diamonds of 3 carats or less

gems_small <- gems %>%
  filter(carat <= max_carat)

n_total    <- nrow(gems)
n_retained <- nrow(gems_small)
n_excluded <- n_total - n_retained
cat("Original records:", n_total, "\n")
cat("Retained (carat <=", max_carat, "):", n_retained, "\n")
cat("Excluded (carat >", max_carat, "):", n_excluded, "\n")

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, max_carat)) +
  scale_x_continuous(breaks = seq(0, max_carat, by = 0.5)) +
  labs(title = "Bin width = 0.01", x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, max_carat)) +
  scale_x_continuous(breaks = seq(0, max_carat, by = 0.5)) +
  labs(title = "Bin width = 0.5", x = "Carat", y = "Count") +
  theme_bw()

p_fine
p_coarse
ggsave("p_fine.png", p_fine, width = 7, height = 4, dpi = 150)
ggsave("p_coarse.png", p_coarse, width = 7, height = 4, dpi = 150)

# Part 2

gems %>%
  filter(round(carat, 2) %in% c(0.99, 1.00)) %>%
  count(carat)
#   carat     n
#    0.99    23
#    1.00  1558
# Only 23 diamonds are recorded at 0.99 carat, but 1,558 at exactly 1.00.

# Records retained and excluded (from the cat() output above):
#   Original gems:            53,940 records
#   Retained (carat <= 3):    53,908 records
#   Excluded (carat > 3):         32 records (very large stones in the far
#                                 right tail, which would stretch the x-axis)

# Part 3
# With 0.01-carat bins, the distribution shows sharp spikes at "round"
# weights such as 0.3, 0.5, 0.7, 0.9, 1.0, 1.5 and 2.0 carats, and each
# spike is followed by a gradual decline in counts just above it. Very few
# diamonds sit just below these weights (23 at 0.99 vs. 1,558 at 1.00),
# suggesting stones are cut or recorded to reach the popular sizes. The
# 0.5-carat bins merge all of this into a smooth right-skewed shape, so the
# spikes are invisible.

#Problem 2
data_sources <- tibble(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  publisher = c("City of Chicago Data Portal (Chicago Public Library)",
                "City of Chicago Data Portal"),
  url = c("https://data.cityofchicago.org/Education/Libraries-Locations-Contact-Information-and-Usual-/x8fc-8rcq",
          "https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas/igwz-8jzy"),
  format = c("CSV", "GeoJSON"),
  accessed = c("2026-11-04", "2026-11-04")
)
write_csv(data_sources, "data_sources.csv")

libraries_raw <- read_csv("~/Downloads/Libraries_-_Locations___Contact_Information__and_Usual_Hours_of_Operation.csv", show_col_types = FALSE)
areas <- st_read("~/Downloads/Boundaries - Community Areas.geojson", quiet = TRUE)
nrow(libraries_raw)

libraries <- libraries_raw %>%
  extract(LOCATION, into = c("lat", "lon"),
          regex = "\\(([-0-9.]+),\\s*([-0-9.]+)\\)", convert = TRUE) %>%
  rename(facility_name = BRANCH, address = ADDRESS)

sum(is.na(libraries$lat) | is.na(libraries$lon))
range(libraries$lat)
range(libraries$lon)
sum(duplicated(libraries$facility_name))
sum(duplicated(libraries$address))
sum(duplicated(libraries[, c("lat", "lon")]))

libraries_clean <- libraries %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  distinct(facility_name, address, .keep_all = TRUE) %>%
  mutate(site_id = sprintf("LIB%03d", row_number())) %>%
  select(site_id, facility_name, address, lat, lon)
nrow(libraries_clean)

sites_m <- st_as_sf(libraries_clean, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(26971)
areas_m <- st_transform(areas, 26971)
st_crs(sites_m)$Name
st_crs(sites_m)$units_gdal

# Maps
buffer_500  <- st_union(st_buffer(sites_m, dist = 500))
buffer_1000 <- st_union(st_buffer(sites_m, dist = 1000))

p_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey70") +
  geom_sf(data = sites_m, color = "blue", size = 1.8) +
  labs(title = "Chicago Public Library branches (n = 82)") +
  theme_bw()

p_buffers <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey70") +
  geom_sf(data = buffer_500, fill = "orange", alpha = 0.5) +
  geom_sf(data = sites_m, color = "blue", size = 1) +
  labs(title = "Library branches and combined 500 m buffer") +
  theme_bw()

p_sites
p_buffers

# Areas
buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = round(as.numeric(c(st_area(buffer_500), st_area(buffer_1000))) / 1e6, 2)
)
buffer_summary

# Data checks:
# Facilities are Chicago Public Library branches in the City of Chicago.
# Source is recorded in data_sources.csv; the raw files are not changed.
# Records before cleaning: 82. Records after cleaning: 82. No records removed.
# Coordinates: none missing, all within Chicago (lat 41.66 to 42.01, lon -87.81 to -87.53).
# Duplicates: no repeated names, addresses, or coordinates.
# Each branch has a unique ID from LIB001 to LIB082.
# CRS: EPSG:26971 (NAD83 / Illinois East State Plane). Units: meters.

# Area table:
#   radius_m  union_area_km2
#        500           64.3
#       1000          250.0

# Interpretation:
# Branches are spread fairly evenly, about 1.9 km apart on average, so the 500 m
# buffers barely overlap and together cover only about 11% of the city. Doubling the
# radius to 1,000 m roughly quadruples the covered area to about 42% of the city.
# The biggest gaps are on the far Northwest Side and in some South Side and industrial areas.

#Problem 3
districts <- areas_m %>%
  transmute(district_id = as.integer(area_numbe),
            district_name = community)

nrow(districts)
sum(is.na(districts$district_id))
sum(duplicated(districts$district_id))
sum(st_is_empty(districts))
sum(!st_is_valid(districts))
st_crs(districts) == st_crs(sites_m)

ggplot() +
  geom_sf(data = districts, fill = "grey95", color = "grey60") +
  geom_sf(data = sites_m, color = "blue", size = 1) +
  labs(title = "Library branches over Chicago community areas") +
  theme_bw()

# Matches and summary
site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_matches = lengths(st_intersects(sites_m, districts))
)
write_csv(site_match_audit, "site_match_audit.csv")
table(site_match_audit$n_matches)
site_match_audit %>% filter(n_matches != 1)

assigned <- sites_m %>%
  filter(site_match_audit$n_matches == 1) %>%
  st_join(districts, join = st_intersects)
n_assigned <- nrow(assigned)
n_unassigned <- nrow(sites_m) - n_assigned

district_counts <- districts %>%
  left_join(count(st_drop_geometry(assigned), district_id, name = "n_sites"),
            by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0),
         area_km2 = as.numeric(st_area(geometry)) / 1e6,
         sites_per_km2 = n_sites / area_km2)

all(district_counts$area_km2 > 0)
sum(district_counts$n_sites) == n_assigned
sum(district_counts$n_sites == 0)

district_counts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  arrange(desc(n_sites)) %>%
  print(n = 77)

# Maps
map_caption <- paste0("Source: City of Chicago Data Portal (accessed 2026-11-04).\n",
                      n_unassigned, " sites left unassigned. White = 0 libraries, grey = missing.")
zero_areas <- filter(district_counts, n_sites == 0)
has_sites  <- filter(district_counts, n_sites > 0)

p_count <- ggplot() +
  geom_sf(data = zero_areas, fill = "white", color = "grey60") +
  geom_sf(data = has_sites, aes(fill = n_sites), color = "grey60") +
  scale_fill_viridis_c(name = "Libraries\n(count)", breaks = 1:3, na.value = "grey60") +
  labs(title = "Library branches per community area", caption = map_caption) +
  theme_bw()

p_density <- ggplot() +
  geom_sf(data = zero_areas, fill = "white", color = "grey60") +
  geom_sf(data = has_sites, aes(fill = sites_per_km2), color = "grey60") +
  scale_fill_viridis_c(name = "Libraries\nper sq km", na.value = "grey60") +
  labs(title = "Library branches per square kilometer", caption = map_caption) +
  theme_bw()

p_count
p_density

# Boundary checks:
# 77 community areas, IDs 1 to 77 with names. No missing or duplicate IDs,
# no empty or invalid shapes, so no repairs or removals. Projected to EPSG:26971
# (meters), the same CRS as sites_m. All 77 areas are kept, including those with
# no libraries. Boundaries were last updated 2025-04-22 and libraries 2026-09-17,
# both from the City of Chicago, so they describe the same city geography.

# Matching audit:
# All 82 sites match exactly one area (0 sites with zero matches, 0 with multiple),
# so 82 sites are assigned and 0 are unassigned.
# Area checks: all areas are positive, and n_sites adds up to 82.
# 16 areas have 0 libraries, 45 have 1, 11 have 2, and 5 have 3.

# Interpretation:
# By raw count, the top areas are Portage Park, Humboldt Park, Austin, Near West Side
# and New City, with 3 libraries each. Accounting for area size changes the ranking:
# the highest densities are in small areas such as Douglas (0.47 per sq km), Montclare,
# Armour Square, Woodlawn and Kenwood, which have only 1 or 2 libraries. Austin has 3
# libraries but is large (18.5 sq km), so its density (0.16) is only middling. Large
# areas with no library, like O'Hare (34.5 sq km), look the same as small ones on the
# count map, so the density map gives a fairer comparison.
