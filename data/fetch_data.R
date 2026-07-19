# data/fetch_data.R
# ---------------------------------------------------------------------------
# Download and tidy the NOAA HURDAT2 Atlantic basin hurricane dataset so the
# analysis scripts can load it.
#
# Output: data/hurdat2_tidy.RData  (a single data frame called `hurrs`)
#
# `hurrs` schema (one row per storm observation / track point):
#   id         - storm id, e.g. "AL122005" (basin + number + year)
#   name       - storm name, e.g. "KATRINA" (or "UNNAMED")
#   year       - integer year
#   month      - integer month (1-12)
#   longitude  - numeric, degrees west as a negative number
#   latitude   - numeric, degrees north
#   windspeed  - numeric, sustained wind in knots
#   isLandfall - logical, TRUE if the record identifier is "L"
#
# Source: NOAA National Hurricane Center
#   https://www.nhc.noaa.gov/data/#hurdat
# HURDAT2 format documentation:
#   https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atlantic.pdf
#
# Usage:
#   Rscript data/fetch_data.R
# ---------------------------------------------------------------------------

suppressMessages({ library(dplyr); library(stringr); library(httr) })

HURDAT_INDEX_URL <- "https://www.nhc.noaa.gov/data/hurdat/"
OUT_FILE <- "data/hurdat2_tidy.RData"

parse_hurdat2 <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  # Header rows have exactly 3 comma-separated fields: id, name, n_records.
  # (Because of a trailing comma they contain 3 commas, not 2.)
  # Data rows have 21 fields (20 commas).
  n_commas <- str_count(lines, ",")
  is_header <- n_commas == 3L

  # Carry each header's id/name forward across its data rows.
  header_fields <- strsplit(lines[is_header], ",", fixed = TRUE)
  header_id   <- trimws(vapply(header_fields, `[`, character(1), 1))
  header_name <- trimws(vapply(header_fields, `[`, character(1), 2))

  # group index for every line: header 1 owns rows up to the next header, etc.
  group <- cumsum(is_header)
  line_id   <- header_id[group]
  line_name <- header_name[group]

  data_lines <- lines[!is_header]
  data_id    <- line_id[!is_header]
  data_name  <- line_name[!is_header]

  parts <- strsplit(data_lines, ",", fixed = TRUE)
  # Pad short rows so the column extraction is robust.
  nc <- max(lengths(parts))
  parts <- lapply(parts, function(p) {
    if (length(p) < nc) c(p, rep("", nc - length(p))) else p
  })
  mat <- do.call(rbind, parts)

  date_str <- trimws(mat[, 1])
  year     <- as.integer(substr(date_str, 1, 4))
  month    <- as.integer(substr(date_str, 5, 6))
  record_id <- trimws(mat[, 3])   # "L" means landfall
  status   <- trimws(mat[, 4])
  lat_str  <- trimws(mat[, 5])
  lon_str  <- trimws(mat[, 6])
  wind     <- suppressWarnings(as.numeric(trimws(mat[, 7])))

  latitude <- as.numeric(str_extract(lat_str, "[0-9.]+"))
  south    <- str_detect(lat_str, "S")
  latitude[is.na(latitude)] <- 0
  latitude[south] <- -latitude[south]

  longitude <- as.numeric(str_extract(lon_str, "[0-9.]+"))
  west      <- str_detect(lon_str, "W")
  longitude[is.na(longitude)] <- 0
  longitude[west] <- -longitude[west]

  wind[wind < 0] <- NA_real_   # -999 is the missing-value sentinel

  isLandfall <- record_id == "L"

  data.frame(
    id        = data_id,
    name      = data_name,
    year      = year,
    month     = month,
    longitude = longitude,
    latitude  = latitude,
    windspeed = wind,
    isLandfall = isLandfall,
    stringsAsFactors = FALSE
  )
}

main <- function() {
  # Find the most recent Atlantic HURDAT2 file name from the index page.
  cat("Reading HURDAT2 index...\n")
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  r <- httr::GET(HURDAT_INDEX_URL, httr::user_agent(ua), httr::timeout(30))
  httr::stop_for_status(r)
  index_html <- httr::content(r, as = "text")
  candidates <- str_extract_all(index_html, "hurdat2-1851-[0-9]+-[0-9]+\\.txt")[[1]]
  if (length(candidates) == 0L) stop("Could not find any HURDAT2 Atlantic file in the index.")
  latest <- tail(sort(candidates), 1)
  cat(sprintf("Latest HURDAT2 file: %s\n", latest))

  data_url <- paste0("https://www.nhc.noaa.gov/data/hurdat/", latest)
  cat(sprintf("Downloading from: %s\n", data_url))
  r <- httr::GET(data_url, httr::user_agent(ua), httr::timeout(120))
  httr::stop_for_status(r)
  text <- httr::content(r, "text", encoding = "UTF-8")
  cat(sprintf("Downloaded %d bytes\n", nchar(text)))

  cat("Parsing...\n")
  hurrs <- parse_hurdat2(text)
  cat(sprintf("Parsed %d track points across %d storms (%d-%d)\n",
              nrow(hurrs), length(unique(hurrs$id)),
              min(hurrs$year), max(hurrs$year)))

  save(hurrs, file = OUT_FILE, compress = "xz")
  cat(sprintf("Saved: %s\n", OUT_FILE))
}

if (!interactive()) main()
