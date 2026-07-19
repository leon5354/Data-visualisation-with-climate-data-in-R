# Hurricane track analysis with HURDAT2
#
# Originally a 2023 data-visualisation coursework project. This version
# fixes several bugs in the original draft and documents the dataset source.
# See report/hurricane-analysis.Rmd for the rendered write-up.
#
# Bug fixes vs. the original draft:
#   * `if (y <-90 & y> 90)` parsed as `if ((y <- 90) & (y > 90))` — the
#     latitude-validation branch was dead code. Now `if (y < -90 || y > 90)`.
#   * `findD_city` had a dangling `x = Taipei` reference at the top that
#     would error. Removed.
#   * `install.packages("maps")` mid-script removed (run install.packages
#     separately, not inside an analysis).
#   * Redundant `library(dplyr)` dropped (tidyverse already loads it).
#   * The Port-au-Prince `mutate(distance_to_Prince = findD_city_v2(...))`
#     passed a vector into a function written for scalars. `findD_city_v2`
#     now handles vectorised input cleanly.
#   * The `Hurdat2_summary` loop used `c(notrack, trackpoint[i,])` which
#     produced a malformed mixed-type accumulator. Replaced with `bind_rows`.

library(maps)
library(tidyverse)

# ---- Q1: dataset summaries ------------------------------------------------

load("data/hurdat2_tidy.RData")

# Hurricane Katrina 2005 track.
katrina <- hurrs %>% filter(id == "AL122005")

# Summary helper: total storms, year span, and per-storm track-point counts.
Hurdat2_summary <- function(x) {
  storms_total <- n_distinct(x$id)
  year_covered <- max(x$year) - min(x$year)
  track_points <- x %>% group_by(id) %>% summarise(amount = n(), .groups = "drop")
  list(
    "Number of Storm"      = storms_total,
    "Year Covered"         = year_covered,
    "Number of Track Point" = track_points
  )
}

Hurdat2_summary_year <- function(x, y) {
  x <- x %>% filter(year == y)
  Hurdat2_summary(x)
}

Hurdat2_summary(hurrs)
Hurdat2_summary_year(hurrs, 2010)
Hurdat2_summary(katrina)

# Histogram of track-point counts per storm.
trackpoint <- hurrs %>% group_by(id) %>% summarise(n = n())
ggplot(trackpoint, aes(n)) + geom_histogram(binwidth = 5) +
  labs(x = "Track points per storm", y = "Number of storms") +
  theme_minimal()

# The earliest named (non-UNNAMED) storms tell us when naming began.
hurrs %>% filter(name != "UNNAMED") %>% arrange(year) %>% head()
# Storms are named from 1950 onwards.

# ---- Q2: basemap helper ---------------------------------------------------

hurricaneBasemap <- function() {
  world <- map_data("world")
  ggplot() +
    coord_fixed(xlim = c(-130, 30), ylim = c(0, 90)) +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 color = "gray50", fill = "white")
}

# All recorded tracks coloured by windspeed.
hurricaneBasemap() +
  geom_path(data = hurrs, aes(x = longitude, y = latitude, group = id, color = windspeed),
            na.rm = TRUE) +
  scale_color_distiller(type = "seq", direction = 1, palette = "YlOrRd", na.value = NA)

# Tracks for a single year.
hurricanes_year <- function(x) {
  yearset <- hurrs %>% filter(year == x)
  hurricaneBasemap() +
    geom_path(data = yearset, aes(x = longitude, y = latitude, group = id, color = windspeed),
              na.rm = TRUE) +
    scale_color_distiller(type = "seq", direction = 1, palette = "YlOrRd", na.value = NA)
}
hurricanes_year(1992)

# ---- Q3: great-circle distance to a city ----------------------------------

cities <- world.cities

# Equirectangular approximation of great-circle distance, in km.
findD <- function(xx, yy, x, y) {
  dy <- yy - y
  dx <- (xx - x) * cos((pi / 180) * ((y + yy) / 2))
  111.325 * sqrt(dx^2 + dy^2)
}

# Vectorised distance from a (lon, lat) point to a named city.
findD_city <- function(x, y, city) {
  ref <- cities %>% filter(name == city) %>% slice(1)
  if (nrow(ref) == 0) stop("city not found: ", city)
  findD(ref$long, ref$lat, x, y)
}

# Same as findD_city but with a sanity check on latitude bounds.
findD_city_v2 <- function(x, y, city) {
  if (any(y < -90 | y > 90, na.rm = TRUE)) {
    warning("Latitude outside [-90, 90]: check for swapped lon/lat.")
  }
  findD_city(x, y, city)
}

# Identify storms passing within 100 km of Port-au-Prince.
close <- hurrs %>% mutate(distance_to_Prince = findD_city_v2(longitude, latitude, "Port-au-Prince"))
real_close <- close %>% filter(distance_to_Prince <= 100)
hurrs_prince <- hurrs %>% filter(id %in% unique(real_close$id))

port <- cities %>% filter(name == "Port-au-Prince")
world <- map_data("world")
ggplot() +
  coord_fixed(xlim = c(-100, -25), ylim = c(0, 75)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray50", fill = "white") +
  geom_path(data = hurrs_prince, aes(x = longitude, y = latitude, color = id),
            na.rm = TRUE) +
  geom_point(data = port, aes(x = long, y = lat), size = 3, shape = 23, fill = "darkred") +
  geom_text(data = port, aes(x = long, y = lat + 2, label = name), size = 3) +
  theme(legend.position = "none") +
  labs(title = "Storm tracks within 100 km of Port-au-Prince")

# ---- Q4: temporal patterns ------------------------------------------------

# (a) Distinct storms per year.
storms_per_year <- hurrs %>% group_by(year) %>% summarise(count = n_distinct(id))
ggplot(storms_per_year, aes(year, count)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Distinct storms") +
  theme_minimal()

# (b) Landfall tracks coloured by year.
landed <- hurrs %>% filter(isLandfall)
ggplot() +
  coord_fixed(xlim = c(min(landed$longitude), max(landed$longitude)),
              ylim = c(min(landed$latitude),  max(landed$latitude))) +
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               color = "gray50", fill = "white") +
  geom_path(data = landed, aes(x = longitude, y = latitude, group = id, color = year)) +
  scale_color_distiller(type = "div", direction = 0, palette = "RdYlBu") +
  labs(title = "Landfall tracks by year",
       color = "Year") +
  theme_minimal()

# (c) Peak windspeed per year.
peak_windspeed <- hurrs %>%
  group_by(year) %>%
  filter(!is.na(windspeed)) %>%
  summarise(peak = max(windspeed))
ggplot(peak_windspeed, aes(year, peak)) +
  geom_line() +
  labs(x = NULL, y = "Peak windspeed (kt)") +
  theme_minimal()

# (d) Storm counts by month (the Atlantic hurricane season).
storms_per_month <- hurrs %>% group_by(month) %>% summarise(count = n_distinct(id))
ggplot(storms_per_month, aes(month, count)) +
  geom_col() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = NULL, y = "Distinct storms") +
  theme_minimal()

# (e) Decade helper.
decade <- function(year) {
  stopifnot(is.numeric(year))
  10 * (year %/% 10)
}
hurrs <- hurrs %>% mutate(decade = decade(year))

# (f) Stacked storms-per-year histogram, filled by month.
hurrs_year_month <- hurrs %>% group_by(year, month) %>% distinct(id)
ggplot(hurrs_year_month, aes(x = year, fill = factor(month))) +
  geom_histogram(binwidth = 1) +
  scale_fill_brewer(type = "div", direction = 1, palette = "PRGn") +
  labs(x = NULL, y = "Distinct storms", fill = "Month") +
  theme_minimal()
