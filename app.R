# ============================================================
# HeDPAC Community Health Dashboard – Africa & Caribbean
# CHP Maturity, CHW Program Features, UHC Linkages (8 indicators)
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(stringr)
library(htmltools)
library(ggplot2)
library(rlang)
library(httr)
library(ggtext)
library(jsonlite)
# 🔧 make sure Shiny's validate/need are used, not jsonlite's
validate <- shiny::validate
need     <- shiny::need

# ---------- DATA DIRECTORY + FILE PATHS ----------
data_dir      <- "Data"
uhc8_data_dir <- data_dir  # same folder, clearer name for UHC files

# ---------- CHP COUNTRY CONTEXT FILES ----------
# (used in policy_status(), chw_training(), chw_numbers(), services_package(), chw_remuneration())
file_policy_status <- file.path(data_dir, "Availability of updated CHW policy.xlsx")
file_chw_training  <- file.path(data_dir, "CHWs who received pre-service training and are deployed.xlsx")
file_chw_numbers   <- file.path(data_dir, "NUMBER OF CHWS CURRENTLY DEPLOYED IN THE COUNTRY.xlsx")

# ⚠️ IMPORTANT: this file name must match EXACTLY what is in your Data/ folder
# If rsconnect logs say "Services Delivery Package.xlsx", then use that:
# file_services      <- file.path(data_dir, "Services Delivery Package.xlsx")
file_services      <- file.path(data_dir, "Services Delivery Packages.xlsx")

file_remuneration  <- file.path(data_dir, "Financial Remuneration by country and kind (1).xlsx")

# ---------- POPULATION ----------
population_excel_path <- file.path(data_dir, "Population_Africa_Caribbean_2000_2003.xlsx")

# ---------- UHC INDICATORS (DHS XLSX) ----------
anc_path                   <- file.path(data_dir, "ANC.xlsx")
ari_path                   <- file.path(data_dir, "ARI.xlsx")
chw_policy_path            <- file.path(data_dir, "Availability of updated CHW policy.xlsx")
chw_training_deployed_path <- file.path(data_dir, "CHWs who received pre-service training and are deployed.xlsx")
contraceptive_path         <- file.path(data_dir, "Contraceptive use.xlsx")
diarrhea_path              <- file.path(data_dir, "Diarrhea.xlsx")
fever_path                 <- file.path(data_dir, "Fever.xlsx")
financial_path             <- file.path(data_dir, "Financial Remuneration by country and kind (1).xlsx")
indicator_names_path       <- file.path(data_dir, "indicator and column name.xlsx")
itn_path                   <- file.path(data_dir, "ITN.xlsx")
chws_number_path           <- file.path(data_dir, "NUMBER OF CHWS CURRENTLY DEPLOYED IN THE COUNTRY.xlsx")
postnatal_mothers_path     <- file.path(data_dir, "Postnatal mothers.xlsx")
postnatal_newborns_path    <- file.path(data_dir, "Postnatal newborns.xlsx")

# For consistency, point these helper paths to the same files used elsewhere
services_package_path      <- file_services

# ---------- MATURITY ----------
file_countries_score     <- file.path(data_dir, "Countries_Score.xlsx")
file_category_level      <- file.path(data_dir, "Category_Level.xlsx")
file_component_score     <- file.path(data_dir, "Componet_score.xlsx")
file_category_components <- file.path(data_dir, "Category_Components.xlsx")
file_maturity_bands_new  <- file.path(data_dir, "Maturity_Bands.xlsx")

# ---------- UHC INDICATOR → COLUMN MAPPING ----------
file_indicator_colmap <- file.path(uhc8_data_dir, "indicator and column name.xlsx")

# Your mapping file is **wide**:
#   columns  = ANC, ARI, Fever, ITN, Postnatal mothers, ...
#   rows     = Country / Survey / Headline / Provider1 / Provider2 / ...
# This function turns that into a list:
#   list(
#     "ANC" = list(
#       headline  = "Antenatal Care (ANC)",
#       providers = c("Provider name 1", "Provider name 2", ...)
#     ),
#     ...
#   )
read_indicator_colmap <- function(path, sheet = 1) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  
  # Hard-coded preferred order (MUST be in same order as uhc8_files)
  fixed_keys <- c(
    "ANC",
    "Postnatal mothers",
    "Postnatal newborns",
    "ARI",
    "Fever",
    "ITN",
    "Diarrhea",
    "Contraceptive use"
  )
  
  # How many indicator columns are actually present in the file?
  # Column 1 = row label; indicator columns start at column 2.
  max_available <- ncol(raw) - 1
  
  # Use the minimum between "what we want" and "what exists"
  n_ind <- min(length(fixed_keys), max_available)
  
  if (n_ind <= 0) {
    stop(
      sprintf(
        "indicator mapping file '%s' has no indicator columns (ncol = %d)",
        basename(path), ncol(raw)
      )
    )
  }
  
  # Subset keys to the indicators actually present
  keys <- fixed_keys[seq_len(n_ind)]
  
  # Headlines from row 3, columns 2 to (1 + n_ind)
  headlines_raw <- as.character(raw[3, 2:(1 + n_ind)])
  headlines     <- trimws(headlines_raw)
  
  message("Headlines extracted (", n_ind, "): ", paste(headlines, collapse = " | "))
  
  out_list <- vector("list", n_ind)
  names(out_list) <- keys
  
  for (i in seq_len(n_ind)) {
    key <- keys[i]
    
    # Providers: rows 4+, column = i + 1 (since column 1 is labels)
    col_vals <- as.character(raw[seq(4, nrow(raw)), i + 1])
    col_vals <- col_vals[!is.na(col_vals) & trimws(col_vals) != ""]
    col_vals <- trimws(col_vals)
    
    if (length(col_vals) == 0) {
      message("Warning: No providers found for ", key)
      next
    }
    
    out_list[[key]] <- list(
      headline  = headlines[i] %||% key,
      providers = col_vals
    )
    
    message(
      "  Loaded ", length(col_vals), " providers for ", key,
      ": ", paste(head(col_vals, 5), collapse = " | "), " ..."
    )
  }
  
  message(
    "\nLoaded mapping keys: ", paste(names(out_list), collapse = ", "),
    " (", sum(!vapply(out_list, is.null, logical(1))), "/", n_ind, " non-empty)\n"
  )
  
  out_list
}

# ---------- UHC–CHP LINKAGE FILE LIST (USED BY uhc8_data()) ----------
uhc8_files <- list(
  "Contraceptive use"  = file.path(uhc8_data_dir, "Contraceptive use.xlsx"),
  "ANC"                = file.path(uhc8_data_dir, "ANC.xlsx"),
  "ARI"                = file.path(uhc8_data_dir, "ARI.xlsx"),
  "Diarrhea"           = file.path(uhc8_data_dir, "Diarrhea.xlsx"),
  "Fever"              = file.path(uhc8_data_dir, "Fever.xlsx"),
  "ITN"                = file.path(uhc8_data_dir, "ITN.xlsx"),
  "Postnatal mothers"  = file.path(uhc8_data_dir, "Postnatal mothers.xlsx"),
  "Postnatal newborns" = file.path(uhc8_data_dir, "Postnatal newborns.xlsx")
)

# ---------------------------
# Brand colors
# ---------------------------
hedpac_blue   <- "#004A98"
hedpac_gold   <- "#C2A759"
hedpac_bg     <- "#FFFFFF"
hedpac_text   <- "#222222"

# Maturity band -> color (for bars)  (fallback)
maturity_colors <- c(
  "Non-existent" = "#bf0c1d",
  "Emering"      = "#e89b46",
  "Emerging"     = "#e89b46",
  "Developing"   = "#f2ea55",
  "Established"  = "#2fc16a",
  "Mature"       = "#07682e"
)

# Maturity band -> infoBox color (adminLTE-safe names)
maturity_infobox_colors <- c(
  "Non-existent" = "red",
  "Emerging"     = "orange",
  "Developing"   = "yellow",
  "Established"  = "green",
  "Mature"       = "blue"
)
# ---------------------------
# CHP Maturity category order (for plotting)
# ---------------------------
maturity_category_order <- c(
  "Leadership and Governance",
  "Financing",
  "Workforce",
  "Supplies",
  "Outcomes"
)
# Category colours for component scores (0–5)
category_colors <- c(
  "Leadership and Governance" = "#004A98",  # HeDPAC blue
  "Financing"                 = "#C2A759",  # HeDPAC gold
  "Workforce"                 = "#7FB3D5",  # soft blue
  "Supplies"                  = "#F4B183",  # orange
  "Outcomes"                  = "#70AD47"   # green
)

# ---------------------------
# File paths (EDIT THESE TO MATCH YOUR MACHINE)
# ---------------------------

# ---------------------------
# CHP COUNTRY CONTEXT FILES (All CHP Data Final)
# ---------------------------
#chp_dir <- "C:/Users/ahuda/OneDrive - hedpac.org/Documents/1. HeDPAC/CHW Dashboard/Dashboard Indicator Data 2026/All CHP Data Final"

#file_policy_status <- file.path(chp_dir, "Availability of updated CHW policy.xlsx")
#file_chw_numbers   <- file.path(chp_dir, "NUMBER OF CHWS CURRENTLY DEPLOYED IN THE COUNTRY.xlsx")
#file_services      <- file.path(chp_dir, "Services Delivery Package.xlsx")
#file_remuneration  <- file.path(chp_dir, "Financial Remuneration by country and kind (1).xlsx")

# UHC–CHP linkage indicators (DHS)
#uhc8_files <- list(
  #"Contraceptive use"  = file.path(uhc8_data_dir, "Contraceptive use.xlsx"),
  #"ANC"                = file.path(uhc8_data_dir, "ANC.xlsx"),
  #"ARI"                = file.path(uhc8_data_dir, "ARI.xlsx"),
  #"Diarrhea"           = file.path(uhc8_data_dir, "Diarrhea.xlsx"),
  #"Fever"              = file.path(uhc8_data_dir, "Fever.xlsx"),
  #"ITN"                = file.path(uhc8_data_dir, "ITN.xlsx"),
 # "Postnatal mothers"  = file.path(uhc8_data_dir, "Postnatal mothers.xlsx"),
 # "Postnatal newborns" = file.path(uhc8_data_dir, "Postnatal newborns.xlsx")
#)

uhc8_titles <- c(
  "ANC"                = "Antenatal Care (ANC)",
  "Contraceptive use"  = "Contraceptive Use",
  "ARI"                = "ARI Care Seeking",
  "Diarrhea"           = "Diarrhea Care Seeking",
  "Fever"              = "Fever Care Seeking",
  "ITN"                = "Households with ITN",
  "Postnatal mothers"  = "Mother’s First PNC",
  "Postnatal newborns" = "Newborn's First PNC"
)

# Donut chart labels (above donut)
uhc8_donut_labels <- c(
  "Contraceptive use"  = "Women Using Contraception (%)",
  "ANC"                = "Skilled ANC Coverage (%)",
  "Postnatal mothers"  = "Mother PNC ≤2 Days (%)",
  "Postnatal newborns" = "Newborn PNC ≤2 Days (%)",
  "ARI"                = "ARI Care Seeking (%)",
  "Fever"              = "Fever Care Seeking (%)",
  "Diarrhea"           = "Diarrhea Care Seeking (%)",
  "ITN"                = "ITN Ownership (%)"
)

# Horizontal bar labels (above bar chart)
uhc8_bar_labels <- c(
  "Contraceptive use"  = "Contraceptive Source",
  "ANC"                = "ANC Provider",
  "Postnatal mothers"  = "Mother PNC Provider",
  "Postnatal newborns" = "Newborn PNC Provider",
  "ARI"                = "ARI Care Source",
  "Fever"              = "Fever Care Source",
  "Diarrhea"           = "Diarrhea Care Source",
  "ITN"                = "ITN Source"
)

# Tooltip definitions when hovering on the group title bar
uhc8_definitions <- c(
  "Contraceptive use" =
    "Contraception Use: Percentage of women who use any contraceptive method. Source of modern contraceptive methods.",
  "ANC" =
    "Antenatal care: Percentage receiving antenatal care from a skilled provider. Antenatal care provider.",
  "Postnatal mothers" =
    "Mother's first postnatal check: Percentage of women with a postnatal check during the first 2 days after birth. Type of health provider of mothers' first postnatal checkup.",
  "Postnatal newborns" =
    "Newborn's first postnatal check: Percentage of births with a postnatal check during the first 2 days after birth. Type of health provider of newborn’s first postnatal check.",
  "ARI" =
    "Advice or treatment for children with ARI: Among children under age 5 with symptoms of ARI, percentage for whom advice or treatment was sought. Among children with symptoms of ARI for whom advice or treatment was sought from different sources.",
  "Fever" =
    "Advice or treatment for children with fever: Among children under age 5 with fever, percentage for whom advice or treatment was sought. Among children with fever for whom advice or treatment was sought from different sources.",
  "Diarrhea" =
    "Advice or treatment for children with diarrhea: Among children under age 5 with diarrhea, percentage for whom advice or treatment was sought. Among children with diarrhea for whom advice or treatment was sought from different sources.",
  "ITN" =
    "Household possession of mosquito nets: Percentage of households owning at least one insecticide-treated net (ITN). Source of mosquito nets."
)

# Donut chart tooltips
uhc8_donut_tooltips <- c(
  "Contraceptive use"  = "% of women who use any contraceptive method",
  "ANC"                = "% receiving antenatal care from a skilled provider",
  "Postnatal mothers"  = "% of women with a postnatal check during the first 2 days after birth",
  "Postnatal newborns" = "% of births with a postnatal check during the first 2 days after birth",
  "ARI"                = "Among children under age 5 with symptoms of ARI, % for whom advice or treatment was sought",
  "Fever"              = "Among children under age 5 with fever, % for whom advice or treatment was sought",
  "Diarrhea"           = "Among children under age 5 with diarrhea, % for whom advice or treatment was sought",
  "ITN"                = "% of households owning at least one insecticide-treated net (ITN)"
)

# Bar chart tooltips
uhc8_bar_tooltips <- c(
  "Contraceptive use"  = "Source of modern contraceptive methods",
  "ANC"                = "Antenatal care provider",
  "Postnatal mothers"  = "Type of health provider of mothers' first postnatal checkup",
  "Postnatal newborns" = "Type of health provider of newborn’s first postnatal check",
  "ARI"                = "Among children with symptoms of ARI for whom advice or treatment was sought from",
  "Fever"              = "Among children with fever for whom advice or treatment was sought from",
  "Diarrhea"           = "Among children with diarrhea for whom advice or treatment was sought from",
  "ITN"                = "Source of mosquito nets"
)

# Sheet names for UHC files
uhc8_sheets <- list(
  "Contraceptive use"  = "Contraceptive use",
  "ANC"                = "ANC",
  "ARI"                = "ARI",
  "Diarrhea"           = "Diarrhea",
  "Fever"              = "Fever",
  "ITN"                = "ITN",
  "Postnatal mothers"  = "Postnatal mothers",
  "Postnatal newborns" = "Postnatal newborns"
)

# Indicator-column mapping file
## uhc8_data_dir,
 # "indicator attributie/Incicator and column name.xlsx"
#)

# ---------------------------
# CHP Maturity files (NEW TAB)
# ---------------------------
#maturity_dir <- "C:/Users/ahuda/OneDrive - hedpac.org/Documents/1. HeDPAC/CHW Dashboard/Dashboard Indicator Data 2026/CHP Maturity"

#file_countries_score     <- file.path(maturity_dir, "Countries_Score.xlsx")
#file_category_level      <- file.path(maturity_dir, "Category_Level.xlsx")
#file_component_score     <- file.path(maturity_dir, "Componet_score.xlsx")      # file name is 'Componet_score.xlsx'
#file_category_components <- file.path(maturity_dir, "Category_Components.xlsx")
#file_maturity_bands_new  <- file.path(maturity_dir, "Maturity_Bands.xlsx")

# Small helper to trim *all* column names
trim_colnames <- function(df) {
  names(df) <- stringr::str_trim(names(df))
  df
}
normalize_name <- function(x) {
  x <- as.character(x)
  # Convert curly quotes to straight quotes
  x <- stringr::str_replace_all(x, "[\u2019\u2018`]", "'")
  # Lowercase + trim
  x <- tolower(trimws(x))
  x
}
# ---------------------------
# Maturity band standardization (vectorised)
# ---------------------------
standardize_band <- function(band) {
  if (is.null(band)) return(NA_character_)
  b  <- trimws(as.character(band))
  bl <- tolower(b)
  out <- b
  out[grepl("non",    bl)] <- "Non-existent"
  out[grepl("emerg",  bl)] <- "Emerging"
  out[grepl("develop",bl)] <- "Developing"
  out[grepl("establ", bl)] <- "Established"
  out[grepl("matur",  bl)] <- "Mature"
  out[b == "" | is.na(b)]  <- NA_character_
  out
}

# ---------------------------
# Classify 0–100 score into band using Maturity_Bands.xlsx
# (uses the Score ranges in the bands table)
# ---------------------------
classify_score_to_band <- function(score_value, bands_df) {
  if (is.na(score_value)) return(NA_character_)
  
  tmp <- bands_df %>%
    mutate(
      Score_chr = as.character(Score),
      Score_chr = stringr::str_replace_all(Score_chr, " ", ""),
      min_score = dplyr::case_when(
        grepl("-", Score_chr) ~ suppressWarnings(as.numeric(sub("-.*", "", Score_chr))),
        grepl(">", Score_chr) ~ suppressWarnings(as.numeric(gsub(">", "", Score_chr))),
        TRUE                  ~ suppressWarnings(as.numeric(Score_chr))
      ),
      max_score = dplyr::case_when(
        grepl("-", Score_chr) ~ suppressWarnings(as.numeric(sub(".*-", "", Score_chr))),
        grepl(">", Score_chr) ~ 100,
        TRUE                  ~ suppressWarnings(as.numeric(Score_chr))
      )
    ) %>%
    filter(!is.na(min_score), !is.na(max_score)) %>%
    filter(score_value >= min_score, score_value <= max_score)
  
  if (nrow(tmp) == 0) return(NA_character_)
  standardize_band(tmp$`Maturity band`[1])
}

# ---------------------------
# Region lists
# ---------------------------
caribbean_countries <- c(
  "Antigua and Barbuda","Bahamas","Barbados","Belize","Cuba","Dominica",
  "Dominican Republic","Grenada","Guyana","Haiti","Jamaica",
  "Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
  "Suriname","Trinidad and Tobago"
)

africa_countries <- c(
  "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
  "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
  "Republic of the Congo","Democratic Republic of the Congo","Côte d’Ivoire","Djibouti",
  "Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia","Gabon",
  "The Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
  "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
  "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
  "Senegal","Seychelles","Sierra Leone","Somalia","South Africa",
  "South Sudan","Sudan","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"
)

assign_region <- function(country) {
  ifelse(
    country %in% caribbean_countries, "Caribbean",
    ifelse(country %in% africa_countries, "Africa", NA)
  )
}

# ---------------------------
# World geometry
# ---------------------------
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  st_make_valid()

# ensure we *always* have a name_long column
if (!"name_long" %in% names(world_sf)) {
  world_sf <- world_sf %>% mutate(name_long = name)
} else {
  world_sf <- world_sf %>%
    mutate(
      name_long = dplyr::coalesce(name_long, name, sovereignt)
    )
}
# ============================================================
# Generic helpers
# ============================================================
to_numeric_pct <- function(x) {
  if (is.numeric(x)) return(x)
  s <- as.character(x)
  s <- str_trim(s)
  s[s %in% c("", "NA", "N/A", "NaN", "NULL")] <- NA_character_
  s <- str_replace_all(s, "<\\s*1\\s*%?", "0.5")
  s <- str_replace_all(s, "%", "")
  s <- str_replace_all(s, ",", "")
  suppressWarnings(as.numeric(s))
}

fmt_pct <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (v > 0 && v < 1) return("<1%")
    paste0(round(v, 1), "%")
  }, character(1))
}

# Read indicator sheet and normalize Country column
read_uhc8_sheet <- function(path, sheet) {
  df <- read_excel(path, sheet = sheet)
  if ("country"   %in% names(df) && !"Country" %in% names(df)) df <- rename(df, Country = country)
  if ("Countries" %in% names(df) && !"Country" %in% names(df)) df <- rename(df, Country = Countries)
  df %>%
    mutate(
      Country = as.character(Country),
      Region  = assign_region(Country)
    )
}

# ============================================================
# WHO GHO + Population (Excel) – latest available year for AFRICA
# Used in Country Context info boxes
# ============================================================

africa_iso3 <- c(
  "DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD",
  "COM","COG","COD","CIV","DJI","EGY","GNQ","ERI","ETH","GAB",
  "GMB","GHA","GIN","GNB","KEN","LSO","LBR","LBY","MDG","MWI",
  "MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA","RWA","STP",
  "SEN","SYC","SLE","SOM","ZAF","SSD","SDN","SWZ","TGO","TUN",
  "UGA","TZA","ZMB","ZWE"
)

# ---- Generic WHO GHO helper – latest year per country ----
get_gho_indicator_latest <- function(indicator_code,
                                     start_year  = 2000,
                                     end_year    = 2100,
                                     iso3_filter = NULL) {
  base_url <- "https://ghoapi.azureedge.net/api"
  
  # Filter: country-level, between start/end year
  filter_str <- sprintf(
    "SpatialDimType%%20eq%%20%%27COUNTRY%%27%%20and%%20TimeDim%%20ge%%20%d%%20and%%20TimeDim%%20le%%20%d",
    start_year, end_year
  )
  
  url <- sprintf("%s/%s?$filter=%s", base_url, indicator_code, filter_str)
  
  # Robust GET with catch
  res <- tryCatch(
    httr::GET(url),
    error = function(e) {
      message("WHO API request failed for ", indicator_code, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  
  # If request failed completely
  if (is.null(res)) {
    return(tibble(
      iso3c = character(),
      year  = integer(),
      value = numeric()
    ))
  }
  
  # If HTTP status is not 2xx, bail out gracefully
  if (httr::http_error(res)) {
    message(
      "WHO API HTTP error for ", indicator_code, 
      ": status ", httr::status_code(res)
    )
    return(tibble(
      iso3c = character(),
      year  = integer(),
      value = numeric()
    ))
  }
  
  # Safely get body as text
  txt <- tryCatch(
    httr::content(res, as = "text", encoding = "UTF-8"),
    error = function(e) {
      message("WHO API content() failed for ", indicator_code, ": ", conditionMessage(e))
      return("")
    }
  )
  
  # Coerce to character
  if (!is.character(txt)) {
    txt <- as.character(txt)
  }
  
  # Handle empty / weird cases
  if (length(txt) == 0 || all(is.na(txt))) {
    message("WHO GHO API returned empty body for indicator ", indicator_code)
    return(tibble(
      iso3c = character(),
      year  = integer(),
      value = numeric()
    ))
  }
  
  txt_trim <- trimws(txt[1])
  if (!startsWith(txt_trim, "{") && !startsWith(txt_trim, "[")) {
    message(
      "WHO GHO API did not return JSON for indicator ", indicator_code,
      ". First characters:\n", substr(txt_trim, 1, 200)
    )
    return(tibble(
      iso3c = character(),
      year  = integer(),
      value = numeric()
    ))
  }
  
  # Parse JSON safely
  json <- tryCatch(
    jsonlite::fromJSON(txt_trim),
    error = function(e) {
      message(
        "JSON parse error for indicator ", indicator_code, ": ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )
  
  if (is.null(json) || is.null(json$value) || nrow(json$value) == 0) {
    return(tibble(
      iso3c = character(),
      year  = integer(),
      value = numeric()
    ))
  }
  
  df_full <- as_tibble(json$value) %>%
    dplyr::transmute(
      iso3c = SpatialDim,
      year  = as.integer(TimeDim),
      value = as.numeric(NumericValue)
    ) %>%
    dplyr::filter(!is.na(iso3c), !is.na(year), !is.na(value))
  
  if (!is.null(iso3_filter)) {
    df_full <- df_full %>% dplyr::filter(iso3c %in% iso3_filter)
  }
  
  df_full %>%
    dplyr::group_by(iso3c) %>%
    dplyr::filter(year == max(year, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

# ---- Specific WHO indicators ----

# UHC service coverage index – UHC_INDEX_REPORTED
get_uhc_index_africa <- function(iso3_filter = NULL) {
  get_gho_indicator_latest(
    indicator_code = "UHC_INDEX_REPORTED",
    start_year     = 2000,
    end_year       = 2100,
    iso3_filter    = iso3_filter
  ) %>%
    dplyr::rename(
      year_uhc  = year,
      uhc_index = value
    )
}

# Maternal mortality ratio – MDG_0000000026 (per 100 000 live births)
get_mmr_africa <- function(iso3_filter = NULL) {
  get_gho_indicator_latest(
    indicator_code = "MDG_0000000026",
    start_year     = 2000,
    end_year       = 2100,
    iso3_filter    = iso3_filter
  ) %>%
    dplyr::rename(
      year_mmr     = year,
      mmr_per_100k = value
    )
}

# Under-five mortality – MDG_0000000007 (deaths per 1 000 live births)
get_u5mr_africa <- function(iso3_filter = NULL) {
  get_gho_indicator_latest(
    indicator_code = "MDG_0000000007",
    start_year     = 2000,
    end_year       = 2100,
    iso3_filter    = iso3_filter
  ) %>%
    dplyr::rename(
      year_u5mr     = year,
      u5mr_per_1000 = value
    )
}
# ---- Population Helper Function (Excel → latest year per country) ----
# Excel sheet: Population_latest
# Columns: Country, ISO3, year, population, Region, Income, Latest_year, is_latest
get_wb_population_latest_from_excel <- function(excel_path, iso3_filter = NULL) {
  pop_raw <- readxl::read_excel(excel_path, sheet = "Population_latest")
  
  pop_latest <- pop_raw %>%
    dplyr::mutate(
      Region     = stringr::str_trim(Region),
      year       = as.integer(year),
      population = as.numeric(population)
    ) %>%
    dplyr::filter(is_latest == TRUE) %>%
    dplyr::transmute(
      iso3c      = ISO3,
      country    = Country,
      year_pop   = year,
      population = population,
      Region,
      Income
    )
  
  if (!is.null(iso3_filter)) {
    pop_latest <- pop_latest %>% dplyr::filter(iso3c %in% iso3_filter)
  }
  
  pop_latest
}

# ---- Population + WHO GHO indicators (Africa) ------------------------

# Use the same data_dir defined at the top of your script
population_excel_path <- file.path(data_dir, "Population_Africa_Caribbean_2000_2003.xlsx")

# Population (from Excel)
pop_africa <- get_wb_population_latest_from_excel(
  excel_path  = population_excel_path,
  iso3_filter = africa_iso3
)

# WHO GHO indicators (API helpers)
uhc_africa  <- get_uhc_index_africa(africa_iso3)
mmr_africa  <- get_mmr_africa(africa_iso3)
u5mr_africa <- get_u5mr_africa(africa_iso3)

# Join population + WHO indicators
afr_health_raw <- pop_africa %>%
  dplyr::left_join(uhc_africa,  by = "iso3c") %>%
  dplyr::left_join(mmr_africa,  by = "iso3c") %>%
  dplyr::left_join(u5mr_africa, by = "iso3c")

# ============================================================
# NEW: CHW COUNTRY FEATURES (policy, training, numbers, services, remuneration)
#      + join with WHO/Population for Country Context tab
# ============================================================

# 1) Policy status (map)
policy_df <- readxl::read_excel(file_policy_status, sheet = "Policy") %>%
  trim_colnames() %>%
  rename(
    Country = country,
    iso3c   = ISO_A3
  ) %>%
  mutate(
    Country       = as.character(Country),
    Policy_status = as.factor(Policy_status),
    Region        = assign_region(Country)
  )

# Join WHO/Population with policy to get context data per country
  afr_health_for_join <- afr_health_raw %>%
  transmute(
    Country        = country,
    Region         = assign_region(country),   # 🔥 FIX ADDED HERE
    Population_M   = population / 1e6,
    PopulationYear = year_pop,
    UHC_Index      = uhc_index,
    UHCYear        = year_uhc,
    MMR            = mmr_per_100k,
    MMRYear        = year_mmr,
    U5MR           = u5mr_per_1000,
    U5MRYear       = year_u5mr
  )

# 2) Training (% CHWs who received pre-service training AND deployed)
training_df <- readxl::read_excel(file_chw_training, sheet = "Training") %>%
  trim_colnames() %>%
  mutate(
    Country = as.character(Country)
  )

training_col <- "CHWs who received pre-service training and deployed (%  of total)"

# 3) CHWs numbers
numbers_df <- readxl::read_excel(file_chw_numbers, sheet = "CHWs") %>%
  trim_colnames() %>%
  mutate(
    Country = as.character(Country)
  )
# Assume columns: Country, CHWs_total, CHWs_number_female, CHWs_number_male,
# and Community health workers (CHWs) per 10000 population- 2022 population

# 4) Services – service package per country (wide)
services_df_raw <- readxl::read_excel(file_services, sheet = "service") %>%
  trim_colnames()

# 5) Remuneration – kind of financial remuneration per country (wide)
remuneration_df_raw <- readxl::read_excel(file_remuneration, sheet = "remuneration") %>%
  trim_colnames()

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin   = "blue",
  
  # =======================
  # HEADER
  # =======================
  header = dashboardHeader(
    title = tagList(
      tags$a(
        href   = "https://www.hedpac.org",
        target = "_blank",
        tags$img(
          src    = "hedpac_logo.png",
          height = "40px",
          style  = "margin-right:15px; vertical-align:middle; cursor:pointer;"
        )
      ),
      span(
        "Community Health Program (CHP) Dashboard – Africa & Caribbean",
        style = "font-size:16px; font-weight:bold; color:white; vertical-align:middle;"
      )
    ),
    titleWidth = 760
  ),
  
  # =======================
  # SIDEBAR
  # =======================
  sidebar = dashboardSidebar(
    width = 350,
    div(
      style = sprintf("background-color:%s; height:100%%;", hedpac_blue),
      br(),
      h3("Filters", style = "color:white; margin-left:15px;"),
      hr(style = sprintf("border-top: 2px solid %s; margin: 0 15px 10px 15px;", hedpac_gold)),
      
      div(
        style = "padding: 0 15px 15px 15px; color:white;",
        
        div(
          class = "filter-wrapper",
          
          # -------------------------
          # REGION DROPDOWN
          # -------------------------
          tags$span("Select Region:", class = "sidebar-region-label"),
          selectInput(
            inputId  = "region_select",
            label    = NULL,
            choices  = c("Africa", "Caribbean"),
            selected = "Africa"
          ),
          
          br(), br(),
          
          # -------------------------
          # COUNTRY DROPDOWN
          # -------------------------
          tags$span("Country (for CHW features & UHC):", class = "sidebar-region-label"),
          selectInput("country", label = NULL, choices = NULL),
          
          br(),
          
          # -------------------------
          # DISCLAIMER (HeDPAC GOLD)
          # -------------------------
          tags$details(
            class = "disclaimer-panel",
            style = "
              background-color:#C2A759;          /* HeDPAC GOLD */
              padding:15px; 
              border-radius:8px; 
              border:1px solid #9E8A4F;          /* darker gold border */
              margin-top:10px;
            ",
            
            # Title line (bigger + blue)
            tags$summary(
              HTML("<span style='font-size:16px; font-weight:700; color:#002750;'>Disclaimer and Data Sources</span>")
            ),
            
            div(
              class = "disclaimer-body",
              style = "padding-top:12px; font-size:13px; color:#002750;",
              
              # Note paragraph
              tags$p(HTML(
                "<strong>Note:</strong> This prototype dashboard includes data for 
                 <strong>Africa</strong> and <strong>Caribbean</strong> countries only. 
                 Visualisations are intended for learning, policy dialogue, and advocacy – 
                 they do not represent official performance ratings."
              )),
              
              # Data sources title
              tags$p(style = "font-weight:600; margin-bottom:4px;", "Data Sources:"),
              
              # Data sources list
              tags$ul(
                tags$li(
                  tags$a(
                    href   = 'https://ghoapi.azureedge.net/api',
                    target = '_blank',
                    "WHO Global Health Observatory (GHO) API"
                  )
                ),
                tags$li(
                  tags$a(
                    href   = 'https://dhsprogram.com',
                    target = '_blank',
                    "Demographic and Health Surveys (DHS) Program"
                  )
                ),
                tags$li(
                  tags$a(
                    href   = 'https://communityhealthdelivery.org',
                    target = '_blank',
                    "Community Health Delivery Partnership (CHDP)"
                  )
                ),
                tags$li(
                  tags$a(
                    href   = 'https://www.prochw.org/policy-dashboard',
                    target = '_blank',
                    "ProCHW Policy Dashboard"
                  )
                )
              ),
              
              # UHC indicators paragraph
              tags$p(
                "Data for Universal Health Coverage (UHC) indicators—including maternal mortality ratio, ",
                "under-five mortality, UHC service coverage, and other key service-delivery indicators were accessed ",
                "directly through the WHO Global Health Observatory (GHO) API and complemented by the latest DHS surveys ",
                "where applicable."
              ),
              
              # Interpretation note
              tags$p(
                "Please interpret all results with consideration for each country's survey year, data availability, ",
                "and national context, as reporting cycles vary across countries."
              ),
              
              # Map disclaimer
              tags$p(style = "font-weight:600; margin-bottom:4px;", "Map Disclaimer:"),
              tags$p(
                "The boundaries, names, and designations shown on these maps do not imply any expression ",
                "of opinion on the part of HeDPAC regarding the legal status of any country, territory, or area, ",
                "nor concerning the delimitation of its borders. All geographic information is provided for reference only."
              )
            )
          )
        )
      )
    )
  ),
  
  # =======================
  # BODY
  # =======================
  # BODY
  # =======================
  body = dashboardBody(
    shinybusy::add_busy_spinner(spin = "fading-circle", color = hedpac_gold),
      
      # 🔵 FULL-SCREEN LOADING OVERLAY (HTML)
      div(
        id = "loading-overlay",
        div(class = "loader"),
        div("Loading the HeDPAC Community Health Dashboard…")
      ),
      
      # 🔗 HEAD: INCLUDE loading.css + JS TO HIDE OVERLAY
      tags$head(
        # Link to your custom CSS file in www/
        tags$link(rel = "stylesheet", type = "text/css", href = "loading.css"),
        
        # JS handler: hides the overlay when Shiny is ready
        tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var overlay = document.getElementById('loading-overlay');
        Shiny.addCustomMessageHandler('hide_loading', function(message){
          if (overlay) overlay.style.display = 'none';
        });
      });
    ")),
        
        # ---- GLOBAL LAYOUT + TAB COLORS ----
        tags$style(HTML(sprintf("
      body { background-color:%s; font-family:'Segoe UI', Arial, sans-serif; }
      .skin-blue .main-header .logo,
      .skin-blue .main-header .navbar {
        background-color:%s;
        border-bottom: 3px solid %s;
        min-height: 60px;
      }
      .main-header .navbar .sidebar-toggle { color:white; }
      .skin-blue .main-sidebar {
        background-color:%s;
        height: 100vh;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .content-wrapper, .right-side {
        margin-left:350px !important;
        padding:15px 20px 20px 20px !important;
        background-color:white;
      }
      .content { position: relative; z-index: 1; padding-top: 10px !important; }
      
      /* TAB BAR – DARK BLUE */
      .nav-tabs-custom > .nav-tabs {
        background-color: #004A98 !important;
        border-bottom: 2px solid #C2A759 !important;
        margin-top: 0;
      }
      .nav-tabs-custom > .nav-tabs > li > a {
        background-color: transparent !important;
        color: #ffffff !important;
        font-weight: 500;
        border: none !important;
      }
      .nav-tabs-custom > .nav-tabs > li > a:hover {
        background-color: rgba(255,255,255,0.15) !important;
        color: #ffffff !important;
        border: none !important;
      }
      .nav-tabs-custom > .nav-tabs > li.active > a {
        background-color: #ffffff !important;
        color: #004A98 !important;
        font-weight: 700;
        border-top: 3px solid #C2A759 !important;
        border-left: none !important;
        border-right: none !important;
      }
      
      .hedpac-title-bar {
        background:%s;
        color:white;
        padding:10px 15px;
        border-left:5px solid %s;
        margin-bottom:15px;
      }
      .hedpac-title-bar h3 { margin:0; font-weight:bold; }
      table.dataTable td { white-space: normal !important; word-wrap: break-word; }
    ", hedpac_bg, hedpac_blue, hedpac_gold, hedpac_blue, hedpac_blue, hedpac_gold))),
        
        # ... keep ALL your other tags$style(HTML("...")) blocks as they are ...
    
    # ... keep ALL your other tags$style(HTML("...")) blocks as they are ...)),
      
      # ---- FILTER WRAPPER + REGION DROPDOWN + DISCLAIMER CLASS ----
      tags$style(HTML("
      /* REGION DROPDOWN STYLE */
      #region_select {
        background-color: #004A98 !important;   /* HeDPAC BLUE */
        color: white !important;
        border: 1px solid #C2A759 !important;   /* HeDPAC GOLD */
        border-radius: 6px !important;
        padding: 6px !important;
        font-size: 13px !important;
        width: 100% !important;
      }
      #region_select option {
        background-color: #004A98 !important;
        color: white !important;
      }
      #region_select:hover {
        border-color: #d9c27a !important;
        cursor: pointer;
      }
      #region_select:focus {
        outline: none !important;
        box-shadow: 0 0 6px #C2A759 !important;
        border-color: #C2A759 !important;
      }

      /* Label styling */
      .sidebar-region-label {
        font-weight: 600;
        color: white;
        margin-bottom: 6px;
        display: block;
      }

      /* Filter wrapper card */
      .filter-wrapper {
        background: rgba(0, 0, 0, 0.18);
        border-radius: 10px;
        padding: 12px 14px 16px 14px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.35);
        border-left: 3px solid #C2A759;
      }

      /* Selectize styling for all sidebar selects */
      .skin-blue .main-sidebar .selectize-control.single .selectize-input {
        background-color: #004A98;
        color: #ffffff;
        border-radius: 6px;
        border: 1px solid #C2A759;
        box-shadow: none;
      }
      .skin-blue .main-sidebar .selectize-control.single .selectize-input::after {
        border-color: #ffffff transparent transparent transparent;
      }
      .skin-blue .main-sidebar .selectize-control.single .selectize-input.focus {
        box-shadow: 0 0 4px #C2A759;
        border-color: #C2A759;
      }
    ")),
      tags$style(HTML("
      .disclaimer-body a {
        color: #004A98 !important; 
        text-decoration: underline !important;
        font-weight: 600 !important;
        cursor: pointer !important;
      }
      .disclaimer-body a:hover {
        color: #002750 !important;
        text-decoration: underline !important;
      }
    ")),
      tags$style(HTML("
  /* TUNE INFOBOX ICON + TEXT LAYOUT */
  .info-box-icon {
    height: 40px !important;
    width: 40px !important;
    line-height: 40px !important;
    font-size: 20px !important;  /* smaller icon */
  }
  .info-box-content {
    margin-left: 48px !important;   /* give more room to text */
    padding: 4px 8px !important;
  }
  .info-box-text {
    white-space: normal !important; /* allow wrapping */
    font-size: 12px !important;
    line-height: 1.3 !important;
  }
  .info-box-number {
    font-size: 16px !important;
    font-weight: 700 !important;
  }
")),
      
      
      # ---- INDICATOR CARDS ----
      tags$style(HTML("
      .indicator-card {
        background-color: #e7e7f3;
        border-radius: 12px;
        padding: 8px 10px 10px 10px;
        margin-bottom: 16px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.12);
      }
      .indicator-header {
        background-color: #050544;
        color: white;
        padding: 6px 10px;
        margin: -8px -10px 8px -10px;
        border-radius: 12px 12px 0 0;
        font-weight: 700;
        font-size: 15px;
        line-height: 1.2;
      }
    ")),
      tags$style(HTML("
  /* TUNE INFOBOX ICON + TEXT LAYOUT */
  .info-box-icon {
    height: 40px !important;
    width: 40px !important;
    line-height: 40px !important;
    font-size: 20px !important;  /* smaller icon */
  }
  .info-box-content {
    margin-left: 48px !important;   /* give more room to text */
    padding: 4px 8px !important;
  }
  .info-box-text {
    white-space: normal !important; /* allow wrapping */
    font-size: 12px !important;
    line-height: 1.3 !important;
  }
  .info-box-number {
    font-size: 16px !important;
    font-weight: 700 !important;
  }
")),
    # ---- MOBILE RESPONSIVE TWEAKS ----
    tags$style(HTML("
        /* Mobile layout adjustments (phones & small tablets) */
        @media (max-width: 991px) {

          /* Make main content full-width under header */
          .content-wrapper, .right-side {
            margin-left: 0 !important;
            padding: 10px 8px 40px 8px !important;
          }

          /* Sidebar should not reserve 350px on mobile */
          .main-sidebar {
            width: 100% !important;
            position: relative !important;
            height: auto !important;
          }

          /* Header title a bit smaller */
          .main-header .logo,
          .main-header .navbar .navbar-brand {
            font-size: 12px !important;
            white-space: normal !important;
          }

          /* Tabs: tighter spacing, smaller text */
          .nav-tabs-custom > .nav-tabs > li > a {
            padding: 6px 8px !important;
            font-size: 12px !important;
          }

          /* Info boxes: more compact */
          .info-box {
            min-height: 70px !important;
          }
          .info-box-text {
            font-size: 11px !important;
          }
          .info-box-number {
            font-size: 14px !important;
          }

          /* Boxes: less vertical space between them */
          .box {
            margin-bottom: 10px !important;
          }

          /* Large H2 in Overview: scale down */
          h2 {
            font-size: 20px !important;
          }
        }
      "))
    ),
    
    fluidRow(
      column(
        width = 12,
        tabBox(
          id = "main_tabs",
          width = 12,
          
          # 1) OVERVIEW – intro only
          tabPanel(
            "Overview",
            
            # Main title
            h2(
              "Community Health Programs (CHP) Dashboard",
              style = sprintf("color:%s; font-weight:bold; margin-top:10px;", hedpac_blue)
            ),
            br(),
            
            # Quick-glance info cards
            fluidRow(
              column(
                width = 4,
                div(
                  style = "border-radius:8px; padding:12px 14px; background-color:#F4F7FB; border-left:4px solid #004A98; height:100%;",
                  p(
                    style = "font-size:13px; text-transform:uppercase; letter-spacing:0.5px; color:#6B7280; margin-bottom:4px;",
                    "What is it?"
                  ),
                  p(
                    style = "font-size:14px; color:#111827; margin-bottom:0;",
                    "An integrated decision-support platform for community health programs across Africa and the Caribbean."
                  )
                )
              ),
              column(
                width = 4,
                div(
                  style = "border-radius:8px; padding:12px 14px; background-color:#FDF8EE; border-left:4px solid #C2A759; height:100%;",
                  p(
                    style = "font-size:13px; text-transform:uppercase; letter-spacing:0.5px; color:#92400E; margin-bottom:4px;",
                    "What does it track?"
                  ),
                  p(
                    style = "font-size:14px; color:#111827; margin-bottom:0;",
                    "CHP maturity, CHP features, contribution to essential service coverage, and UHC."
                  )
                )
              ),
              column(
                width = 4,
                div(
                  style = "border-radius:8px; padding:12px 14px; background-color:#ECFDF5; border-left:4px solid #047857; height:100%;",
                  p(
                    style = "font-size:13px; text-transform:uppercase; letter-spacing:0.5px; color:#047857; margin-bottom:4px;",
                    "Who is it for?"
                  ),
                  p(
                    style = "font-size:14px; color:#111827; margin-bottom:0;",
                    "Policymakers, program managers, and partners working to strengthen community health systems."
                  )
                )
              )
            ),
            
            br(),
            
            # Clean narrative section – no scroll, no blue background blocks
            HTML("
    <h3 style='color:#004A98;font-weight:bold; margin-top:5px; margin-bottom:10px;'>
      About the CHP Dashboard
    </h3>

    <p style='font-size:14px; line-height:1.6; color:#111827; text-align:justify;'>
      The Community Health Program (CHP) Dashboard is developed by HeDPAC to serve as an integrated decision-support platform 
      that strengthens analytics and generates actionable insights for policy, investment, and accountability in community health 
      programs across Africa and the Caribbean. It brings together diverse data sources to provide a comprehensive picture of how 
      community health programs are structured, resourced, and performing, and how they contribute to the achievement of 
      Universal Health Coverage (UHC).
    </p>

    <p style='font-size:14px; line-height:1.6; color:#111827; text-align:justify;'>
      The dashboard tracks the maturity and performance of national community health programs, highlights their contribution to 
      essential health service coverage, and supports evidence-informed decision-making. It was developed by consolidating and 
      harmonizing key information from established global and national sources, including the WHO Global Health Observatory, 
      the Global Community Health Dashboard, the ProCHW Policy Dashboard, the Demographic and Health Surveys (DHS), 
      World Bank Open Data, and country-level Community Health Program Maturity Assessments.
    </p>

    <!-- COUNTRY-LEVEL HIGHLIGHT (subtle gold accent) -->
    <div style='border-left:4px solid #C2A759;
                padding-left:12px;
                margin:20px 0;'>
      <p style='font-size:14px; line-height:1.6; color:#004A98; text-align:justify; margin:0;'>
        <span style='font-weight:700;'>At the country level,</span>
        the dashboard presents core demographic and health indicators alongside key features of the community health program, 
        including the type and number of Community Health Workers (CHWs), the existence and updated status of national policies, 
        pre-service training arrangements, service packages delivered by CHWs, and remuneration mechanisms. This allows users to 
        understand how CHW programs are organized and supported within each health system.
      </p>
    </div>

    <p style='font-size:14px; line-height:1.6; color:#111827; text-align:justify;'>
      The dashboard also visualizes how CHWs contribute to coverage of selected essential health services, drawing on the latest 
      DHS data, National Malaria Indicator Surveys, and other relevant national data on health service coverage. These analyses 
      demonstrate the role of CHWs in expanding access to care, reducing inequities, and advancing progress toward UHC. While the 
      dashboard captures service utilization, it does not yet fully reflect CHWs' broader functions in health promotion, community 
      engagement, and referral to health facilities, which remain important areas for future enhancement.
    </p>

    <p style='font-size:14px; line-height:1.6; color:#111827; text-align:justify;'>
      In addition, the platform displays the maturity level of community health programs using the Community Health Program 
      Maturity Framework, with scores disaggregated by category and component. The CHP Maturity Framework is a tool developed 
      by HeDPAC to provide a standardized, evidence-based framework for countries, policymakers, and program managers to 
      evaluate, strengthen, and sustain CHPs. It combines a maturity matrix, scoring system, and composite index to generate 
      clear, actionable insights. This enables countries and partners to assess governance, financing, workforce, supply chain, 
      outcomes, and system integration in a structured, comparable way.
    </p>

    <!-- WHY THIS MATTERS (simple, typographic) -->
    <div style='border-top:1px solid #004A98;
                margin-top:22px;
                padding-top:12px;'>
      <p style='margin:0 0 4px 0; font-weight:bold; color:#004A98; font-size:14px;'>
        Why this matters
      </p>
      <p style='margin:0; font-size:14px; line-height:1.6; color:#111827; text-align:justify;'>
        Overall, the CHP Dashboard empowers policymakers, program managers, and partners to monitor progress, stimulate dialogue, 
        prioritize investments, benchmark across countries, and share best practices to strengthen community health systems and 
        accelerate progress toward UHC.
      </p>
    </div>
  ")
          ),  # end Overview
          
          # 2) COUNTRY CONTEXT & CHP KEY FEATURES
          tabPanel(
            title = "Country context and CHP key features",
            
            div(class = "hedpac-title-bar", h3(textOutput("ctx_title"))),
            
            HTML("
      <p style='font-size:14px;'>
        This section brings together core country context indicators (population, UHC index,
        maternal and under-five mortality) and key CHW program features, including policy status,
        pre-service training, numbers of CHWs, service delivery package, and types of financial
        remuneration. It is intended to support learning, dialogue, and investment decisions – not
        as a scorecard.
      </p>
    "),
            
            br(),
            
            fluidRow(
              infoBoxOutput("info_pop",  width = 3),
              infoBoxOutput("info_uhc",  width = 3),
              infoBoxOutput("info_mmr",  width = 3),
              infoBoxOutput("info_u5mr", width = 3)
            ),
            
            br(),
            
            fluidRow(
              box(
                width = 6, status = "primary", solidHeader = TRUE,
                title = "Number of CHWs currently deployed in the country",
                leafletOutput("chw_numbers_map", height = 360) %>% 
                  withSpinner(color = hedpac_gold)
              ),
              box(
                width = 6, status = "primary", solidHeader = TRUE,
                title = "CHWs who received pre-service training (%)",
                leafletOutput("chw_training_map", height = 360) %>% 
                  withSpinner(color = hedpac_gold)
              )
            ),
            
            br(),
            
            fluidRow(
              box(
                width = 6, status = "primary", solidHeader = TRUE,
                title = "CHW Policy Status",
                leafletOutput("chw_policy_map", height = 360) %>% 
                  withSpinner(color = hedpac_gold)
              ),
              box(
                width = 3, status = "primary", solidHeader = TRUE,
                title = "CHW Service Delivery Package",
                uiOutput("chw_services_card")
              ),
              box(
                width = 3, status = "primary", solidHeader = TRUE,
                title = "Types of Financial Remuneration for CHWs",
                uiOutput("chw_remuneration_card")
              )
            ),
            
            # FOOTNOTE
            div(
              style = "margin-top: 25px; padding-top: 10px;",
              tags$hr(
                style = sprintf(
                  'border-top: 2px solid %s; width: 100%%; margin-top: 5px; margin-bottom: 10px;',
                  hedpac_gold
                )
              ),
              div(
                style = "
          font-size: 11.5px;
          color: #555555;
          line-height: 1.45;
          text-align: left;
          padding: 8px 4px;
          max-width: 950px;
        ",
                HTML("
          <em>
            Note: UHC service coverage index, maternal mortality ratio, and under-five mortality rate—and CHW programme features such as policy status, training, workforce numbers, service package, and financial remuneration are provided for learning, planning, and policy dialogue purposes only. They are not intended to serve as formal performance ratings. Displayed data reflect information extracted from the respective data sources as of December 2025.
          </em>
        ")
              )
            )
          ),  # end key features tab
          
          # 3) CHW Contribution to Essential Services 
          tabPanel(
            title = "CHW Contribution to Essential Services ",
            
            div(class = "hedpac-title-bar", h3(textOutput("uhc_header_title"))),
            br(),
            
            HTML("
      <p style='font-size:14px;'>
        This section shows how CHWs contribute to coverage of selected essential health services using the latest DHS data. It highlights CHWs’ role in expanding access and promoting equity, contributing to UHC. However, the section primarily captures service use and does not fully capture CHWs’ broader roles in health promotion and facility referrals.
      </p>
    "),
            
            br(),
            
            uiOutput("uhc8_cards") %>% withSpinner(color = hedpac_gold),
            
            # FOOTNOTE
            div(
              style = "margin-top: 25px; padding-top: 10px;",
              tags$hr(
                style = sprintf(
                  'border-top: 2px solid %s; width: 100%%; margin-top: 5px; margin-bottom: 10px;',
                  hedpac_gold
                )
              ),
              div(
                style = "
          font-size: 11.5px;
          color: #555555;
          line-height: 1.45;
          text-align: left;
          padding: 8px 4px;
          max-width: 850px;
        ",
                HTML("
          <em>
            CHWs’ contribution to essential health services is estimated using DSH Data. Visualisations are provided for learning, policy dialogue, and programme design and should be interpreted alongside each country’s survey year and national context. ICF, 2012. The DHS Program STATcompiler. Funded by USAID. http://www.statcompiler.com Dec 04, 2025.
          </em>
        ")
              )
            )
          ), # end CHW Contribution tab
          
          # 4) CHP MATURITY – PLACEHOLDER ONLY (NO DATA LOADED YET)
          tabPanel(
            title = "CHP Maturity",
            
            div(
              class = "hedpac-title-bar",
              h3("Community Health Program (CHP) Maturity")
            ),
            br(),
            # 🔔 Coming soon notice
            div(
              style = "
                border-radius:8px;
                padding:10px 14px;
                background-color:#FDF8EE;
                border-left:4px solid #C2A759;
                display:flex;
                align-items:flex-start;
                gap:10px;
                margin-bottom:18px;
              ",
              tags$span(
                icon("hourglass-half"),
                style = "font-size:16px; color:#C2A759; margin-top:2px;"
              ),
              tags$div(
                tags$p(
                  style = "margin:0; font-size:14px; color:#92400E; font-weight:600;",
                  "Coming soon – maturity assessments under way"
                ),
                tags$p(
                  style = "margin:3px 0 0 0; font-size:13px; color:#4B5563;",
                  "This section will be activated once countries complete Community Health Program (CHP) maturity assessments and 
                   grant approval for visualising and sharing results."
                )
              )
            ),
            
            # Main explanatory text
            HTML("
      <p style='font-size:14px; line-height:1.6; text-align:justify;'>
        This section will display results from <strong>Community Health Program (CHP) Maturity Assessments</strong> conducted by countries in 
        collaboration with HeDPAC and partners. The maturity framework provides a structured way to assess governance, financing, workforce, 
        supplies, service delivery and outcomes for community health programs.
      </p>
      
      <p style='font-size:14px; line-height:1.6; text-align:justify;'>
        At this stage, the CHP Maturity section is <strong>not yet populated with country-level results</strong>. Visualisations and dashboards 
        will be activated once maturity assessments are completed, validated, and officially cleared for dissemination by the respective 
        Ministries of Health and national authorities.
      </p>
      
      <div style='border-left:4px solid #C2A759; padding-left:12px; margin:20px 0;'>
        <p style='font-size:14px; line-height:1.6; color:#004A98; margin:0; text-align:justify;'>
          <strong>Coming soon:</strong> Once data are available and approvals are granted, this section will show:
          <ul style='margin-top:6px;'>
            <li>Overall CHP maturity scores for each country</li>
            <li>Maturity levels by category (e.g. leadership & governance, financing, workforce, supplies, outcomes)</li>
            <li>Component-level strengths and gaps to guide policy dialogue and investment decisions</li>
          </ul>
        </p>
      </div>
      
      <p style='font-size:13px; line-height:1.6; color:#555555; text-align:justify;'>
        Until then, the CHP Maturity tab is provided as a placeholder to signal ongoing work and upcoming country-led assessments. 
        All future displays of maturity scores will be aligned with country preferences on data sharing and visibility.
      </p>
    ")
          )
        )  # end tabBox
      )   # end column
    )     # end fluidRow
  )     # end body = dashboardBody
)    # end ui <- dashboardPage(...)
# ------------------------------------------------------------------
# Helper: read an Excel sheet by name, case-insensitive, trim spaces
# ------------------------------------------------------------------
read_excel_ci <- function(path, sheet_hint) {
  sheets <- readxl::excel_sheets(path)
  
  # match ignoring case & surrounding spaces
  target <- sheets[
    tolower(trimws(sheets)) == tolower(trimws(sheet_hint))
  ]
  
  if (length(target) == 0) {
    stop(
      sprintf(
        "Sheet '%s' not found in %s. Available sheets: %s",
        sheet_hint, basename(path), paste(sheets, collapse = ", ")
      )
    )
  }
  
  readxl::read_excel(path, sheet = target[1])
}

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ---------------------------
  # ADD THIS BLOCK HERE
  # ---------------------------
  add_selected_country_label <- function(map, sf_data, selected_country) {
    
    sel_sf <- sf_data %>%
      dplyr::filter(name_long == selected_country, !sf::st_is_empty(geometry))
    
    if (nrow(sel_sf) == 0) return(map)
    
    ctr <- sf::st_coordinates(sf::st_centroid(sel_sf))
    
    map %>%
      addLabelOnlyMarkers(
        lng = ctr[,1],
        lat = ctr[,2],
        label = selected_country,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textsize = "11px",
          style = list(
            "font-weight" = "bold",
            "color"       = "#7c0a02",
            "text-shadow" = "1px 1px 2px #ffffff",
            "background"  = "transparent",
            "padding"     = "0px",
            "border"      = "none",
            "border-radius" = "0px"
          )
        )
      )
  }
  # ---------------------------
  # END OF ADDED FUNCTION
  # ---------------------------
  
  # your existing outputs start here...
  
  # --------------- Region & Country selection (sidebar) -----
  observe({
    df <- policy_status()
    req(nrow(df) > 0)
    req(input$region_select)
    
    region_countries <- df %>%
      dplyr::filter(Region == input$region_select) %>%
      dplyr::distinct(Country) %>%
      dplyr::arrange(Country) %>%
      dplyr::pull(Country)
    
    if (length(region_countries) == 0) {
      updateSelectInput(
        session, "country",
        choices  = character(0),
        selected = NULL
      )
      return(NULL)
    }
    
    # Default: Angola for Africa if available, otherwise first in list
    default_country <- if (input$region_select == "Africa" &&
                           "Angola" %in% region_countries) {
      "Angola"
    } else {
      region_countries[1]
    }
    
    updateSelectInput(
      session, "country",
      choices  = region_countries,
      selected = default_country
    )
  })
  
  selected_region <- reactive({
    req(input$region_select)
    input$region_select
  })
  selected_country <- reactive({
    req(input$country)
    input$country[1]
  })
  
  # ---------------- Titles dependent on selection -----------
  output$ctx_title <- renderText({
    paste0(selected_country(), " — Country Context & CHP Features")
  })
  # --------------- Country context row from WHO + Population ---------------
  country_context_selected <- reactive({
    afr_health_for_join %>%
      filter(Country == selected_country()) %>%
      slice(1)
  })
  
  # Helper for safe label
  fmt_or_na <- function(x, digits = 1) {
    if (length(x) == 0 || is.na(x[1])) return(NA_real_)
    round(x[1], digits)
  }
  
  # ---- Population infoBox ----
  output$info_pop <- renderInfoBox({
    row <- country_context_selected()
    if (nrow(row) == 0 || is.na(row$Population_M[1])) {
      infoBox("Population", "No data", icon = icon("users"), color = "blue", fill = TRUE)
    } else {
      lab <- paste0(fmt_or_na(row$Population_M, 1), " M (", row$PopulationYear[1], ")")
      infoBox("Population (millions)", lab, icon = icon("users"), color = "blue", fill = TRUE)
    }
  })
  
  # ---- UHC index infoBox ----
  output$info_uhc <- renderInfoBox({
    row <- country_context_selected()
    if (nrow(row) == 0 || is.na(row$UHC_Index[1])) {
      infoBox("UHC Index", "No data", icon = icon("heartbeat"), color = "green", fill = TRUE)
    } else {
      lab <- paste0(fmt_or_na(row$UHC_Index, 1), " (", row$UHCYear[1], ")")
      infoBox("UHC service coverage(0–100)", lab, icon = icon("umbrella")
, color = "green", fill = TRUE)
    }
  })
  
  # ---- Maternal mortality infoBox ----
  output$info_mmr <- renderInfoBox({
    row <- country_context_selected()
    if (nrow(row) == 0 || is.na(row$MMR[1])) {
      infoBox("MMR", "No data", icon = icon("female"), color = "orange", fill = TRUE)
    } else {
      lab <- paste0(fmt_or_na(row$MMR, 1), " per 100,000 (", row$MMRYear[1], ")")
      infoBox("Maternal mortality (MMR)", lab, icon = icon("female"), color = "orange", fill = TRUE)
    }
  })
  
  # ---- Under-5 mortality infoBox ----
  output$info_u5mr <- renderInfoBox({
    row <- country_context_selected()
    if (nrow(row) == 0 || is.na(row$U5MR[1])) {
      infoBox("U5MR", "No data", icon = icon("child"), color = "maroon", fill = TRUE)
    } else {
      lab <- paste0(fmt_or_na(row$U5MR, 1), " per 1,000 (", row$U5MRYear[1], ")")
      infoBox("Under-five mortality (U5MR)", lab, icon = icon("child"), color = "maroon", fill = TRUE)
    }
  })
  
  # --------------- CHW Policy Status Map (safe version) ---------------------------
  # --------------- CHW Policy Map ---------------------------
  # --------------- CHW Policy Map ---------------------------
  output$chw_policy_map <- renderLeaflet({
    tryCatch({
      pol <- policy_status() %>%
        dplyr::filter(Region == selected_region())
      
      # If no data for region, still show a basic map with a message
      if (nrow(pol) == 0) {
        center <- if (selected_region() == "Africa") {
          list(lng = 20,  lat = 0,  zoom = 2.5)
        } else {
          list(lng = -70, lat = 15, zoom = 3.8)
        }
        
        return(
          leaflet(options = leafletOptions(
            minZoom = center$zoom, maxZoom = center$zoom
          )) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = center$lng, lat = center$lat, zoom = center$zoom) %>%
            addControl("No policy data for selected region", position = "topright")
        )
      }
      
      sel <- selected_country()
      
      # Join with world_sf by country name (name_long) and compute border style
      map_df <- world_sf %>%
        dplyr::left_join(pol, by = c("name_long" = "Country")) %>%
        dplyr::mutate(
          border_col = dplyr::if_else(name_long == sel, "#7c0a02", "#555555"),
          border_w   = dplyr::if_else(name_long == sel, 2.5, 0.5)
        )
      
      # Build palette based on available policy status categories
      status_levels <- sort(unique(na.omit(map_df$Policy_status)))
      if (length(status_levels) == 0) status_levels <- "Unknown"
      
      pal <- colorFactor(
        palette  = colorRampPalette(c("#00264D", "#0055A4", "#66B2FF"))(length(status_levels)),
        domain   = status_levels,
        na.color = "#cccccc"
      )
      
      # Simple region-based zoom
      center <- if (selected_region() == "Africa") {
        list(lng = 20,  lat = 0,  zoom = 2.5)
      } else {
        list(lng = -70, lat = 15, zoom = 3.8)
      }
      
      # Base map: all polygons, NO labels, NO hover highlight
      map <- leaflet(
        map_df,
        options = leafletOptions(
          worldCopyJump = FALSE,
          minZoom = center$zoom,   # 🔒 lock zoom
          maxZoom = center$zoom
        )
      ) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = center$lng, lat = center$lat, zoom = center$zoom) %>%
        addPolygons(
          fillColor   = ~pal(Policy_status),
          fillOpacity = 0.85,
          color       = ~border_col,
          weight      = ~border_w
          # no label, no highlight here
        ) %>%
        addLegend(
          position = "bottomleft",
          pal      = pal,
          values   = map_df$Policy_status,
          title    = "Policy Status",
          labFormat = labelFormat(big.mark = ","),
          opacity = 0.9
        )
      
      # Overlay: selected country only, with tooltip
      selected_poly <- map_df %>%
        dplyr::filter(name_long == sel, !sf::st_is_empty(geometry))
      
      if (nrow(selected_poly) > 0) {
        map <- map %>%
          addPolygons(
            data       = selected_poly,
            fillColor   = ~pal(Policy_status),
            fillOpacity = 0.85,
            color       = ~border_col,
            weight      = ~border_w,
            label       = ~htmltools::HTML(paste0(
              "<b>", name_long, "</b><br>",
              "Policy status: ",
              ifelse(is.na(Policy_status), "Unknown", Policy_status), "<br>",
              "Policy year: ",
              ifelse(is.na(Policy_year), "N/A", Policy_year)
            )),
            labelOptions = labelOptions(
              direction = "auto",
              html      = TRUE,
              sticky    = FALSE
            )
          )
      }
      
      # ⭐ Add bold country name label
      map <- add_selected_country_label(map, map_df, selected_country())
      
      map
      
    }, error = function(e) {
      # Fallback: at least show a base map and the error message
      leaflet() %>%
        addTiles() %>%
        addControl(
          paste("Map error:", htmltools::htmlEscape(conditionMessage(e))),
          position = "topright"
        )
    })
  })
  
  
  # --------------- CHW Training Map (zoom + selected tooltip only) --------------------
  output$chw_training_map <- renderLeaflet({
    tryCatch({
      train <- chw_training() %>%
        dplyr::filter(Region == selected_region())
      
      # If no data, still show map + message
      if (nrow(train) == 0) {
        center <- if (selected_region() == "Africa") {
          list(lng = 20,  lat = 0,  zoom = 2.5)
        } else {
          list(lng = -70, lat = 15, zoom = 3.8)
        }
        
        return(
          leaflet(options = leafletOptions(
            minZoom = center$zoom, maxZoom = center$zoom
          )) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = center$lng, lat = center$lat, zoom = center$zoom) %>%
            addControl("No training data for selected region", position = "topright")
        )
      }
      
      sel <- selected_country()
      
      map_df <- world_sf %>%
        dplyr::left_join(train, by = c("name_long" = "Country")) %>%
        dplyr::mutate(
          border_col = dplyr::if_else(name_long == sel, "#7c0a02", "#555555"),
          border_w   = dplyr::if_else(name_long == sel, 2.5, 0.5)
        )
      
      vals <- map_df$Training_pct
      if (all(is.na(vals))) vals <- c(0, 100)
      
      # HeDPAC Gold: light at low %, dark at high %
      pal <- colorNumeric(
        palette  = colorRampPalette(c("#F3E7C2", "#D8C68A", "#C2A759"))(6),
        domain   = vals,
        na.color = "#f0f0f0"
      )
      
      center <- if (selected_region() == "Africa") {
        list(lng = 20,  lat = 0,  zoom = 2.5)
      } else {
        list(lng = -70, lat = 15, zoom = 3.8)
      }
      
      # Base map: all polygons without labels
      map <- leaflet(
        map_df,
        options = leafletOptions(
          worldCopyJump = FALSE,
          minZoom = center$zoom,
          maxZoom = center$zoom
        )
      ) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = center$lng, lat = center$lat, zoom = center$zoom) %>%
        addPolygons(
          fillColor   = ~pal(Training_pct),
          fillOpacity = 0.85,
          color       = ~border_col,
          weight      = ~border_w
        ) %>%
        addLegend(
          position = "bottomleft",
          pal      = pal,
          values   = map_df$Training_pct,
          title    = "Trained & Deployed",
          labFormat = labelFormat(big.mark = ","),
          opacity = 0.9
        )
      
      # Overlay: selected country only, with tooltip
      selected_poly <- map_df %>%
        dplyr::filter(name_long == sel, !sf::st_is_empty(geometry))
      
      if (nrow(selected_poly) > 0) {
        map <- map %>%
          addPolygons(
            data       = selected_poly,
            fillColor   = ~pal(Training_pct),
            fillOpacity = 0.85,
            color       = ~border_col,
            weight      = ~border_w,
            label       = ~htmltools::HTML(paste0(
              "<b>", name_long, "</b><br>",
              "CHWs with pre-service training and deployed: ",
              ifelse(is.na(Training_pct),
                     "No data",
                     paste0(round(Training_pct, 1), "%"))
            )),
            labelOptions = labelOptions(
              direction = "auto",
              html      = TRUE,
              sticky    = FALSE
            )
          )
      }
      
      # ⭐ Add bold country name label
      map <- add_selected_country_label(map, map_df, selected_country())
      
      map
      
    }, error = function(e) {
      leaflet() %>%
        addTiles() %>%
        addControl(
          paste("Map error:", htmltools::htmlEscape(conditionMessage(e))),
          position = "topright"
        )
    })
  })
  
  
  # --------------- Number of CHWs Map ---------------------------
  output$chw_numbers_map <- renderLeaflet({
    chws <- chw_numbers() %>% dplyr::filter(Region == selected_region())
    
    if (nrow(chws) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addControl("No CHW numbers data for selected region", position = "topright")
      )
    }
    
    sel <- selected_country()
    
    # Join with world_sf and set border styling
    map_df <- world_sf %>%
      dplyr::left_join(chws, by = c("name_long" = "Country")) %>%
      dplyr::mutate(
        border_col = dplyr::if_else(name_long == sel, "#7c0a02", "#555555"),
        border_w   = dplyr::if_else(name_long == sel, 3, 0.7)
      )
    
    # Color scale for total CHWs
    vals <- map_df$CHWs_total
    if (all(is.na(vals))) vals <- c(0, 1)
    
    pal <- colorNumeric(
      palette  = colorRampPalette(c("#deebf7", "#08306b"))(7),  # blue gradient
      domain   = vals,
      na.color = "#f0f0f0"
    )
    
    center <- if (selected_region() == "Africa") {
      list(lng = 20,  lat = 5,  zoom = 2.5)
    } else {
      list(lng = -70, lat = 15, zoom = 3.8)
    }
    
    map <- leaflet(
      map_df,
      options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = center$zoom,
        maxZoom = center$zoom
      )
    ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = center$lng, lat = center$lat, zoom = center$zoom) %>%
      addPolygons(
        fillColor   = ~pal(CHWs_total),
        fillOpacity = 0.85,
        color       = ~border_col,
        weight      = ~border_w
        # ❌ no label here -> no hover tooltips at all on this map
      ) %>%
      addLegend(
        position = "bottomleft",
        pal      = pal,
        values   = map_df$CHWs_total,
        title    = "Total CHWs",
        labFormat = labelFormat(big.mark = ","),
        opacity = 0.9
      )
    
    # 🔵 SINGLE LABEL ONLY FOR SELECTED COUNTRY (name + number of CHWs)
    selected_row <- map_df %>%
      dplyr::filter(name_long == sel,
                    !is.na(CHWs_total),
                    !sf::st_is_empty(geometry))
    
    if (nrow(selected_row) > 0) {
      ctr <- sf::st_coordinates(sf::st_centroid(selected_row))
      
      label_txt <- paste0(
        sel, " – ",
        format(selected_row$CHWs_total[1], big.mark = ","),
        " CHWs"
      )
      
      map <- map %>%
        addLabelOnlyMarkers(
          lng   = ctr[, 1],
          lat   = ctr[, 2],
          label = label_txt,
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "center",
            textsize  = "11px",
            style = list(
              "font-weight"   = "bold",
              "color"         = "#222222",
              "text-shadow"   = "1px 1px 2px #ffffff",
              "background"  = "transparent",
              "padding"     = "0px",
              "border"      = "none",
              "border-radius" = "0px"
            )
          )
        )
    }
    
    map
  })
  
    # --------------- Service delivery package CARD ---------------------------
  output$chw_services_card <- renderUI({
    df <- services_package() %>%
      dplyr::filter(
        Region  == selected_region(),
        Country == stringr::str_trim(selected_country())
      )
    
    if (nrow(df) == 0 || is.na(df$Service_package[1]) || df$Service_package[1] == "") {
      return(div("No service package data for selected country."))
    }
    
    items <- df$Service_package[1] %>%
      strsplit(",") %>% .[[1]] %>%
      trimws() %>% .[. != ""]
    
    div(
      style = "
      background-color:#ffffff;
      border-left:4px solid #C2A759;   /* HeDPAC Gold */
      border-right:1px solid #dde2e5;
      border-bottom:1px solid #dde2e5;
      border-top:1px solid #dde2e5;
      padding:10px 12px;
      border-radius:6px;
      max-height:300px;
      overflow-y:auto;
      font-size:13px;
      line-height:1.35;
    ",
      tags$ul(
        lapply(items, function(i) tags$li(i))
      )
    )
  })
        # --------------- Financial remuneration CARD ---------------------------
  output$chw_remuneration_card <- renderUI({
    df <- chw_remuneration() %>%
      dplyr::filter(
        Region  == selected_region(),
        Country == stringr::str_trim(selected_country())
      )
    
    if (nrow(df) == 0 || is.na(df$Remuneration_kind[1]) || df$Remuneration_kind[1] == "") {
      return(div("No remuneration data for selected country."))
    }
    
    items <- df$Remuneration_kind[1] %>%
      strsplit(",") %>% .[[1]] %>%
      trimws() %>% .[. != ""]
    
    div(
      style = "
      background-color:#ffffff;
      border-left:4px solid #004A98;  /* HeDPAC Blue */
      border-right:1px solid #dde2e5;
      border-bottom:1px solid #dde2e5;
      border-top:1px solid #dde2e5;
      padding:10px 12px;
      border-radius:6px;
      max-height:300px;
      overflow-y:auto;
      font-size:13px;
      line-height:1.35;
    ",
      tags$ul(
        lapply(items, function(i) tags$li(i))
      )
    )
  })
 
  # ============================================================
  # COUNTRY CONTEXT – CHW FILES (All CHP Data Final)
  # ============================================================
  
  # 1) Availability of updated CHW policy
  policy_status <- reactiveFileReader(
    intervalMillis = 10000,
    session  = session,
    filePath = file_policy_status,
    readFunc = function(path) {
      # if it has a single sheet, this just reads that; otherwise update "Policy" hint if needed
      df <- read_excel_ci(path, "policy")
      df <- trim_colnames(df)
      
      colnames(df)[1] <- "Country"
      
      nms <- names(df)
      status_col <- nms[grepl("status", nms, ignore.case = TRUE)][1]
      year_col   <- nms[grepl("year",   nms, ignore.case = TRUE)][1]
      
      df <- df %>%
        mutate(
          Country = as.character(Country),
          Region  = assign_region(Country)
        )
      
      if (!is.na(status_col)) df <- df %>% rename(Policy_status = !!sym(status_col))
      if (!is.na(year_col))   df <- df %>% rename(Policy_year   = !!sym(year_col))
      
      df
    }
  )
  
  # 2) CHWs who received pre-service training and are deployed (% of total)
  chw_training <- reactiveFileReader(
    intervalMillis = 10000,
    session  = session,
    filePath = file_chw_training,
    readFunc = function(path) {
      # will match "Training", "training", " Training ", etc.
      df <- read_excel_ci(path, "training")
      df <- trim_colnames(df)
      colnames(df)[1] <- "Country"
      
      # Adjust this selection if your exact header is different
      pct_col <- names(df)[grepl("pre-service", names(df), ignore.case = TRUE) |
                             grepl("pre service", names(df), ignore.case = TRUE) |
                             grepl("%", names(df), ignore.case = TRUE)][1]
      
      df %>%
        mutate(
          Country      = as.character(Country),
          Region       = assign_region(Country),
          Training_pct = if (!is.na(pct_col)) as.numeric(.data[[pct_col]]) else NA_real_
        )
    }
  )
  
  # 3) Number of CHWs currently deployed
  chw_numbers <- reactiveFileReader(
    intervalMillis = 10000,
    session  = session,
    filePath = file_chw_numbers,
    readFunc = function(path) {
      # will match "CHWs", "chws", etc.
      df <- read_excel_ci(path, "CHWs")
      df <- trim_colnames(df)
      colnames(df)[1] <- "Country"
      
      df %>%
        mutate(
          Country            = as.character(Country),
          Region             = assign_region(Country),
          CHWs_total         = as.numeric(CHWs_total),
          CHWs_number_female = as.numeric(CHWs_number_female),
          CHWs_number_male   = as.numeric(CHWs_number_male)
        )
    }
  )
  
  # 4) Services delivery package
  services_package <- reactiveFileReader(
    intervalMillis = 10000,
    session  = session,
    filePath = file_services,
    readFunc = function(path) {
      df <- read_excel_ci(path, "service")
      df <- trim_colnames(df)
      colnames(df)[1] <- "Country"
      
      # Explicitly match the Service_package column
      if (!"Service_package" %in% names(df)) {
        svc_col <- names(df)[grepl("service", names(df), ignore.case = TRUE) &
                               grepl("package", names(df), ignore.case = TRUE)][1]
        if (!is.na(svc_col)) {
          df <- df %>% rename(Service_package = !!sym(svc_col))
        }
      }
      
      df %>%
        mutate(
          Country         = stringr::str_trim(as.character(Country)),
          Region          = assign_region(Country),
          Service_package = as.character(Service_package)
        )
    }
  )
  
  # 5) Financial remuneration
  chw_remuneration <- reactiveFileReader(
    intervalMillis = 10000,
    session  = session,
    filePath = file_remuneration,
    readFunc = function(path) {
      df <- read_excel_ci(path, "remuneration")
      df <- trim_colnames(df)
      colnames(df)[1] <- "Country"
      
      # Explicitly match the Remuneration_kind column
      if (!"Remuneration_kind" %in% names(df)) {
        kind_col <- names(df)[grepl("remuneration", names(df), ignore.case = TRUE) |
                                grepl("kind", names(df), ignore.case = TRUE)][1]
        if (!is.na(kind_col)) {
          df <- df %>% rename(Remuneration_kind = !!sym(kind_col))
        }
      }
      
      df %>%
        mutate(
          Country          = stringr::str_trim(as.character(Country)),
          Region           = assign_region(Country),
          Remuneration_kind = as.character(Remuneration_kind)
        )
    }
  )
  
  # ============================================================
  # CHW Contribution to Essential Services – UPDATED WITH LABELS + TOOLTIPS
  # ============================================================
  # ============================================================
  # CHW Contribution to Essential Services  – SERVER PLOTS
  # ============================================================
  
  output$uhc_header_title <- renderText({
    paste0(
      selected_country(),
      " — CHWs’ contribution to selected essential health service coverage indicators"
    )
  })
  
  # ---------- UHC 8 indicator → column mapping (headline + providers) ----------
  indicator_mapping <- reactiveFileReader(
    intervalMillis = 10000,
    session        = session,
    filePath       = file_indicator_colmap,
    readFunc       = function(path) read_indicator_colmap(path, sheet = 1)
  )
    # Read all 8 indicator datasets
  uhc8_data <- reactive({
    lst <- lapply(names(uhc8_files), function(ind_name) {
      path  <- uhc8_files[[ind_name]]
      sheet <- uhc8_sheets[[ind_name]]
      tryCatch(read_uhc8_sheet(path, sheet), error = function(e) NULL)
    })
    names(lst) <- names(uhc8_files)
    lst
  })
  
  # UI: cards for 8 indicators (2 per row), with header + chart tooltips
  output$uhc8_cards <- renderUI({
    lst <- uhc8_data()
    if (length(lst) == 0) return(tags$p("No indicator files configured."))
    
    cards <- lapply(names(lst), function(ind_name) {
      donut_id <- paste0("uhc8_donut_", gsub("[^A-Za-z0-9]", "_", ind_name))
      bar_id   <- paste0("uhc8_bar_",   gsub("[^A-Za-z0-9]", "_", ind_name))
      
      column(
        width = 6,
        div(
          class = "indicator-card",
          
          div(
            class = "indicator-header",
            title = uhc8_definitions[[ind_name]] %||% "",
            uhc8_titles[[ind_name]] %||% ind_name
          ),
          
          fluidRow(
            column(
              width = 5,
              div(
                title = uhc8_donut_tooltips[[ind_name]] %||% "",
                plotOutput(donut_id, height = "190px")
              )
            ),
            column(
              width = 7,
              div(
                title = uhc8_bar_tooltips[[ind_name]] %||% "",
                plotOutput(bar_id, height = "190px")
              )
            )
          )
        )
      )
    })
    
    rows <- split(cards, ceiling(seq_along(cards) / 2))
    do.call(tagList, lapply(rows, function(x) fluidRow(x)))
  })
  
  # --------- DYNAMIC SERVER-SIDE PLOTS (DONUT + BAR) ----------
  observe({
    lst          <- uhc8_data()
    mapping      <- indicator_mapping()
    # ── DEBUG: print loaded keys once ────────────────────────────────
    if (!exists(".mapping_keys_printed", envir = .GlobalEnv)) {
      message("Mapping keys loaded: ", paste(names(mapping), collapse = ", "))
      assign(".mapping_keys_printed", TRUE, envir = .GlobalEnv)
    }
    sel_country  <- selected_country()
    sel_region   <- selected_region()
    
    req(lst, mapping, sel_country, sel_region)
    
    for (ind_name in names(lst)) {
      local({
        ind       <- ind_name
        donut_id  <- paste0("uhc8_donut_", gsub("[^A-Za-z0-9]", "_", ind))
        bar_id    <- paste0("uhc8_bar_",   gsub("[^A-Za-z0-9]", "_", ind))
        
        # Helper to show error message in plot area
        draw_error <- function(msg) {
          plot.new()
          text(0.5, 0.5, msg, adj = c(0.5, 0.5), col = "red", cex = 1)
        }
        
        # ──────────────────────────────────────────────────────────────
        # DONUT CHART
        # ──────────────────────────────────────────────────────────────
        output[[donut_id]] <- renderPlot({
          tryCatch({
            df <- lst[[ind]] %>%
              dplyr::filter(Region == sel_region, Country == sel_country)
            
            if (nrow(df) == 0) {
              draw_error("No data for selected country")
              return()
            }
            
            # ── TEMPORARY AUTO-DETECT COVERAGE (bypass mapping issues) ───────────────
            candidate_cols <- setdiff(names(df), c("Country", "Survey", "Region", "region"))
            candidate_cols <- candidate_cols[vapply(df[candidate_cols], function(x) {
              is.numeric(x) || any(!is.na(to_numeric_pct(x)))
            }, logical(1))]
            
            if (length(candidate_cols) == 0) {
              draw_error("No numeric coverage column found")
              return()
            }
            
            cov_col <- candidate_cols[1]
            val_raw <- df[[cov_col]][1]
            val     <- to_numeric_pct(val_raw)
            val     <- max(0, min(100, val %||% 0))
            
            label_txt <- if (is.na(val)) "NA" else if (val < 1) "<1%" else paste0(round(val, 1), "%")
            
            donut_df <- tibble::tibble(
              seg   = c("Coverage", "Remaining"),
              value = c(val, 100 - val)
            )
            
            ggplot2::ggplot(donut_df, ggplot2::aes(x = 2, y = value, fill = seg)) +
              ggplot2::geom_col(width = 0.9, color = "white") +
              ggplot2::coord_polar("y") +
              ggplot2::xlim(0.8, 2.5) +
              ggplot2::theme_void() +
              ggplot2::scale_fill_manual(values = c("Coverage" = "#8cccf0", "Remaining" = "#E0E0E0")) +
              ggplot2::guides(fill = "none") +
              ggplot2::annotate("text", x = 2, y = 0, label = label_txt, size = 8, fontface = "bold") +
              ggplot2::labs(title = uhc8_donut_labels[[ind]] %||% ind) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5)
              )
            
          }, error = function(e) {
            draw_error(paste("Donut error:", conditionMessage(e)))
          })
        })
      })  # closes local({
    }      # closes for (...)
  })        # closes observe({
  
  # ──────────────────────────────────────────────────────────────
        
        # BAR CHART – ROBUST VERSION (mapping + fallback)
        # ──────────────────────────────────────────────────────────────
  observe({
    lst     <- uhc8_data()
    mapping <- indicator_mapping()
    
    # ── DEBUG: print loaded keys once ────────────────────────────────
    if (!exists(".mapping_keys_printed", envir = .GlobalEnv)) {
      message("Mapping keys loaded: ", paste(names(mapping), collapse = ", "))
      assign(".mapping_keys_printed", TRUE, envir = .GlobalEnv)
    }
    
    sel_country <- selected_country()
    sel_region  <- selected_region()
    
    # we REQUIRE lst, country, region (but NOT mapping)
    req(lst, sel_country, sel_region)
    
    for (ind_name in names(lst)) {
      local({
        ind      <- ind_name
        donut_id <- paste0("uhc8_donut_", gsub("[^A-Za-z0-9]", "_", ind))
        bar_id   <- paste0("uhc8_bar_",   gsub("[^A-Za-z0-9]", "_", ind))
        
        # Helper to show error message in plot area
        draw_error <- function(msg) {
          plot.new()
          text(0.5, 0.5, msg, adj = c(0.5, 0.5), col = "red", cex = 1)
        }
        
        # ──────────────────────────────────────────────────────────────
        # DONUT CHART
        # ──────────────────────────────────────────────────────────────
        output[[donut_id]] <- renderPlot({
          tryCatch({
            df <- lst[[ind]] %>%
              dplyr::filter(Region == sel_region, Country == sel_country)
            
            if (nrow(df) == 0) {
              draw_error("No data for selected country")
              return()
            }
            
            # Auto-detect a coverage column (numeric / % column)
            candidate_cols <- setdiff(names(df), c("Country", "Survey", "Region", "region"))
            candidate_cols <- candidate_cols[vapply(df[candidate_cols], function(x) {
              is.numeric(x) || any(!is.na(to_numeric_pct(x)))
            }, logical(1))]
            
            if (length(candidate_cols) == 0) {
              draw_error("No numeric coverage column found")
              return()
            }
            
            cov_col <- candidate_cols[1]
            val_raw <- df[[cov_col]][1]
            val     <- to_numeric_pct(val_raw)
            val     <- max(0, min(100, val %||% 0))
            
            label_txt <- if (is.na(val)) "NA" else if (val < 1) "<1%" else paste0(round(val, 1), "%")
            
            donut_df <- tibble::tibble(
              seg   = c("Coverage", "Remaining"),
              value = c(val, 100 - val)
            )
            
            ggplot2::ggplot(donut_df, ggplot2::aes(x = 2, y = value, fill = seg)) +
              ggplot2::geom_col(width = 0.9, color = "white") +
              ggplot2::coord_polar("y") +
              ggplot2::xlim(0.8, 2.5) +
              ggplot2::theme_void() +
              ggplot2::scale_fill_manual(values = c("Coverage" = "#8cccf0", "Remaining" = "#E0E0E0")) +
              ggplot2::guides(fill = "none") +
              ggplot2::annotate("text", x = 2, y = 0, label = label_txt, size = 8, fontface = "bold") +
              ggplot2::labs(title = uhc8_donut_labels[[ind]] %||% ind) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5)
              )
            
          }, error = function(e) {
            draw_error(paste("Donut error:", conditionMessage(e)))
          })
        })
        
        # ──────────────────────────────────────────────────────────────
        # BAR CHART – robust (mapping + fallback)
        # ──────────────────────────────────────────────────────────────
        output[[bar_id]] <- renderPlot({
          tryCatch({
            df <- lst[[ind]] %>%
              dplyr::filter(Region == sel_region, Country == sel_country)
            
            if (nrow(df) == 0) {
              draw_error("No data for selected country")
              return()
            }
            
            # Try to use mapping
            map_entry <- NULL
            if (!is.null(mapping) && !is.null(mapping[[ind]])) {
              map_entry <- mapping[[ind]]
            }
            
            candidate_providers <- character(0)
            if (!is.null(map_entry) &&
                !is.null(map_entry$providers) &&
                length(map_entry$providers) > 1) {
              # Skip first provider: assume it's the coverage column
              candidate_providers <- map_entry$providers[-1]
            }
            
            # Restrict to columns that exist in the DHS sheet
            existing_cols <- candidate_providers[candidate_providers %in% names(df)]
            
            # Fallback: auto-detect provider columns if mapping fails
            if (length(existing_cols) == 0) {
              meta_cols    <- c("Country", "Survey", "Region", "region", "Year", "year")
              possible_cols <- setdiff(names(df), meta_cols)
              
              possible_cols <- possible_cols[vapply(df[possible_cols], function(col) {
                any(!is.na(to_numeric_pct(col)))
              }, logical(1))]
              
              existing_cols <- possible_cols
            }
            
            if (length(existing_cols) == 0) {
              draw_error("No data for selected country")
              return()
            }
            
            # Only keep columns with usable numeric values
            # Only keep columns with usable numeric values
            numeric_cols <- existing_cols[vapply(df[existing_cols], function(col) {
              any(!is.na(to_numeric_pct(col)))
            }, logical(1))]
            
            if (length(numeric_cols) == 0) {
              draw_error("No numeric/percentage columns among providers")
              return()
            }
            
            # 🔹 NEW: drop the coverage/headline column so it doesn't appear as a bar
            # Anything whose name matches the indicator key or friendly title is treated as coverage
            drop_candidates <- c(
              ind,                          # e.g. "Postnatal mothers"
              uhc8_titles[[ind]] %||% ""    # e.g. "Mother’s First PNC"
            )
            drop_candidates <- drop_candidates[drop_candidates != ""]
            
            # --- normalize both sides so ' and ’ etc. are treated the same ---
            norm_numeric <- normalize_name(numeric_cols)
            norm_drop    <- normalize_name(drop_candidates)
            
            # columns whose *normalized* names match coverage-like names
            drop_cols <- numeric_cols[norm_numeric %in% norm_drop]
            
            if (length(drop_cols) > 0) {
              numeric_cols <- setdiff(numeric_cols, drop_cols)
            }
            
            if (length(numeric_cols) == 0) {
              draw_error("No data for the selected country")
              return()
            }
            
            # ---- Build long df, with CHWs label tweak ----
            long <- df %>%
              tidyr::pivot_longer(
                cols      = dplyr::all_of(numeric_cols),
                names_to  = "Provider",
                values_to = "Value",
                values_drop_na = TRUE
              ) %>%
              dplyr::mutate(
                Value        = to_numeric_pct(Value),
                Provider_raw = stringr::str_wrap(Provider, width = 28),
                
                # detect CHW rows
                is_chw       = stringr::str_detect(
                  tolower(Provider_raw),
                  "chw|community health|community worker"
                ),
                
                # CHWs = HeDPAC blue, others = gold
                fill_col     = dplyr::if_else(is_chw, hedpac_blue, hedpac_gold),
                
                # 🔷 Y-axis label: HeDPAC-style blue CHW marker
                Provider_lab = dplyr::if_else(
                  is_chw,
                  "🔷 CHWs",       # blue CHW label
                  Provider_raw
                )
              ) %>%
              dplyr::filter(!is.na(Value)) %>%
              dplyr::arrange(dplyr::desc(Value))
            
            if (nrow(long) == 0) {
              draw_error("No valid numeric values after filtering")
              return()
            }
            
            upper_lim <- max(long$Value, na.rm = TRUE) * 1.25
            upper_lim <- max(upper_lim, 10)
            
            ggplot2::ggplot(
              long,
              ggplot2::aes(
                x    = Value,
                y    = reorder(Provider_lab, Value),
                fill = fill_col
              )
            ) +
              ggplot2::geom_col() +
              ggplot2::geom_text(
                ggplot2::aes(label = paste0(round(Value, 1), "%")),
                hjust    = -0.1,
                size     = 3.2,
                fontface = "bold"
              ) +
              ggplot2::scale_fill_identity() +  # keep exact CHW vs non-CHW colors
              ggplot2::labs(
                title = uhc8_bar_labels[[ind]] %||% "Provider / Source",
                x     = "%",
                y     = NULL
              ) +
              ggplot2::xlim(0, 100) +
              ggplot2::theme_minimal(base_size = 10) +
              ggplot2::theme(
                plot.title   = ggplot2::element_text(size = 11, face = "bold", hjust = 0),
                axis.text.y  = ggplot2::element_text(
                  size  = 9,
                  color = "#333333",
                  face  = "bold"
                ),
                panel.grid.major.y = ggplot2::element_blank(),
                panel.grid.minor   = ggplot2::element_blank()
              )
            
          }, error = function(e) {
            draw_error(paste("Bar error:", conditionMessage(e)))
          })
        })
        
        })  # local
    }      # for
  })        # observe
  
  # ==========================================================
  # CHP Maturity – DATA READERS & OUTPUTS
  # ==========================================================
  
  countries_score <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = file_countries_score,
    readFunc = function(path) {
      df <- readxl::read_excel(path, sheet = "Countries_Score")
      df <- trim_colnames(df)
      df %>%
        mutate(
          Country = stringr::str_trim(Country),
          Band    = standardize_band(Band)
        )
    }
  )
  
  category_level <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = file_category_level,
    readFunc = function(path) {
      df <- readxl::read_excel(path)
      df <- trim_colnames(df)
      colnames(df)[1] <- "Category"
      df %>%
        mutate(Category = stringr::str_trim(Category))
    }
  )
  
  category_components <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = file_category_components,
    readFunc = function(path) {
      df <- readxl::read_excel(path)
      df <- trim_colnames(df)
      names(df)[1:3] <- c("Category", "Component", "Tooltip")
      df %>%
        mutate(
          Category  = stringr::str_trim(Category),
          Component = as.character(Component)
        ) %>%
        filter(!is.na(Category), !is.na(Component)) %>%
        group_by(Category) %>%
        summarise(
          Tooltip = paste(paste0("\u2022 ", Component), collapse = "\n"),
          .groups = "drop"
        )
    }
  )
  
  component_score <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = file_component_score,
    readFunc = function(path) {
      df <- readxl::read_excel(path, sheet = "Componet_score")
      df <- trim_colnames(df)
      df %>%
        mutate(
          Country   = stringr::str_trim(Country),
          Category  = stringr::str_trim(Category),
          Component = stringr::str_trim(Component),
          Score     = as.numeric(Score)
        )
    }
  )
  
  maturity_bands_new <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = file_maturity_bands_new,
    readFunc = function(path) {
      df <- readxl::read_excel(path, sheet = "Maturity_Band")
      df <- trim_colnames(df)
      df %>%
        filter(!is.na(Score)) %>%
        mutate(
          `Maturity band` = standardize_band(`Maturity band`),
          `Band Color`    = stringr::str_trim(`Band Color`),
          Definition      = as.character(Definition)
        )
    }
  )
  # ---------- Maturity band legend (uses Maturity_Bands.xlsx) ----------
  output$mat_band_legend <- renderUI({
    bands <- maturity_bands_new()
    if (nrow(bands) == 0) {
      return(NULL)
    }
    
    # Keep the order from the sheet (0–20, 21–40, 41–60, 61–80, >80)
    # but if needed, we can sort by Score numeric
    bands <- bands %>%
      dplyr::mutate(
        `Maturity band` = standardize_band(`Maturity band`)
      )
    
    # Build a small, compact legend
    div(
      style = "font-size:11px; line-height:1.3; margin-top:4px;",
      tags$strong("Maturity band legend"),
      tags$br(), tags$br(),
      # one row per band
      lapply(seq_len(nrow(bands)), function(i) {
        b <- bands[i, ]
        band_name  <- as.character(b$`Maturity band`[1])
        band_score <- as.character(b$Score[1])
        band_col   <- as.character(b$`Band Color`[1])
        band_def   <- as.character(b$Definition[1])
        
        div(
          style = "display:flex; align-items:flex-start; margin-bottom:4px;",
          # colour box
          div(
            style = sprintf(
              "width:14px; height:14px; background-color:%s; border-radius:2px;
               margin-right:6px; margin-top:2px; border:1px solid #cccccc;",
              band_col
            )
          ),
          # text
          div(
            tags$span(
              style = "font-weight:bold;",
              sprintf("%s (%s)", band_name, band_score)
            ),
            tags$br(),
            tags$span(band_def)
          )
        )
      })
    )
  })
  # Populate maturity country dropdown
  observe({
    cs <- countries_score()
    req(nrow(cs) > 0)
    
    mat_countries <- cs %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    updateSelectInput(
      session,
      "mat_country",
      choices  = mat_countries,
      selected = mat_countries[1]
    )
  })
  
  selected_mat_country <- reactive({
    req(input$mat_country)
    input$mat_country
  })
  
  output$mat_header_title <- renderText({
    paste0(selected_mat_country(), " – CHP Maturity")
  })
  
  # ---------- 1) Category barplot (base R, no validate) ----------
  # ---------- 1) Category barplot with band colours ----------
  # ---------- 1) Category barplot with band colours ----------
  # ---------- 1) Category barplot with band colours (Leadership at top, Outcomes at bottom) ----------
  output$mat_category_plot <- renderPlot({
    tryCatch({
      cs    <- countries_score()
      bands <- maturity_bands_new()
      
      if (nrow(cs) == 0 || nrow(bands) == 0) {
        plot.new()
        text(0.5, 0.5, "No maturity data loaded.", cex = 1.1)
        return()
      }
      
      country_name <- selected_mat_country()
      row <- cs %>% dplyr::filter(Country == country_name)
      
      if (nrow(row) == 0) {
        plot.new()
        text(0.5, 0.5,
             paste("No maturity data for", country_name),
             cex = 1.1)
        return()
      }
      
      # Build long data frame: one row per category
      df <- tibble::tibble(
        Category = c(
          "Leadership and Governance",
          "Financing",
          "Workforce",
          "Supplies",
          "Outcomes"
        ),
        Score = c(
          as.numeric(row$`Leadership and Governance`),
          as.numeric(row$Financing),
          as.numeric(row$Workforce),
          as.numeric(row$Supplies),
          as.numeric(row$Outcomes)
        )
      ) %>%
        dplyr::filter(!is.na(Score)) %>%
        dplyr::mutate(
          # reverse levels so ggplot shows: Leadership (top) → Outcomes (bottom)
          Category = factor(Category, levels = rev(maturity_category_order))
        )
      
      if (nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5,
             paste("No category-level scores for", country_name),
             cex = 1.1)
        return()
      }
      
      # Attach band + band colour + definition using 0–100 thresholds
      df <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          Band     = classify_score_to_band(Score, bands),
          Band_std = standardize_band(Band)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          bands %>%
            dplyr::select(`Maturity band`, `Band Color`, Definition),
          by = c("Band_std" = "Maturity band")
        ) %>%
        dplyr::mutate(
          BandColor = ifelse(is.na(`Band Color`), "#cccccc", `Band Color`),
          Label     = paste0(round(Score, 1), "% – ", Band_std)
        )
      
      ggplot(df, aes(x = Score, y = Category, fill = BandColor)) +
        geom_col(width = 0.40) +
        geom_text(
          aes(label = Label),
          hjust = -0.05,
          size  = 3.4,       # bigger
          fontface = "bold"
        ) +
        scale_fill_identity() +
        xlim(0, 110) +
        labs(
          x = "Score (0–100)",
          y = NULL
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_text(
            size = 13,          # 🔥 bigger
            face = "bold",      # 🔥 bold
            color = "#222222"   # optional: darker
          ),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          plot.margin        = margin(t = 5, r = 20, b = 10, l = 10)
        )
      
    }, error = function(e) {
      plot.new()
      text(
        0.02, 0.95,
        paste("Maturity progression plot error:\n", conditionMessage(e)),
        adj = c(0, 1),
        col = "red",
        cex = 0.8
      )
    })
  }, height = 360, res = 96)
  
  # ---------- 2) Overall maturity score bar (base R, no validate) ----------
  # ---------- 2) Overall maturity score bar (ggplot, no margin issues) ----------
  # ---------- 2) Overall maturity score bar with band colour ----------
  # ---------- Overall maturity score gauge (plotly) ----------
  # ---------- Overall maturity score gauge (band-labelled "status" indicator) ----------
  output$mat_overall_gauge <- plotly::renderPlotly({
    cs    <- countries_score()
    bands <- maturity_bands_new()
    
    if (nrow(cs) == 0 || nrow(bands) == 0) {
      return(NULL)
    }
    
    row <- cs %>% dplyr::filter(Country == selected_mat_country())
    if (nrow(row) == 0) {
      return(NULL)
    }
    
    score <- suppressWarnings(as.numeric(row$Maturity_Score[1]))
    if (is.na(score)) {
      return(NULL)
    }
    
    band  <- standardize_band(row$Band[1])
    
    # --- Parse band table into numeric ranges 0–100 and midpoints for tick labels ---
    bands_parsed <- bands %>%
      dplyr::mutate(
        Score_chr = gsub(" ", "", as.character(Score)),
        min_score = dplyr::case_when(
          grepl("-", Score_chr) ~ suppressWarnings(as.numeric(sub("-.*", "", Score_chr))),
          grepl(">", Score_chr) ~ suppressWarnings(as.numeric(gsub(">", "", Score_chr))),
          TRUE                  ~ suppressWarnings(as.numeric(Score_chr))
        ),
        max_score = dplyr::case_when(
          grepl("-", Score_chr) ~ suppressWarnings(as.numeric(sub(".*-", "", Score_chr))),
          grepl(">", Score_chr) ~ 100,
          TRUE                  ~ suppressWarnings(as.numeric(Score_chr))
        )
      ) %>%
      dplyr::filter(!is.na(min_score), !is.na(max_score)) %>%
      dplyr::mutate(
        mid_score   = (min_score + max_score) / 2,
        Band_std    = standardize_band(`Maturity band`)
      )
    
    # Colour of the gauge bar (needle fill) = colour of current band
    band_row <- bands_parsed %>%
      dplyr::filter(Band_std == band) %>%
      dplyr::slice(1)
    
    band_color <- if (nrow(band_row) == 0 || is.na(band_row$`Band Color`[1])) {
      hedpac_blue
    } else {
      band_row$`Band Color`[1]
    }
    
    # Build gauge step segments from bands table (like the coloured slices in the template)
    steps <- lapply(seq_len(nrow(bands_parsed)), function(i) {
      list(
        range = c(bands_parsed$min_score[i], bands_parsed$max_score[i]),
        color = bands_parsed$`Band Color`[i]
      )
    })
    
    # Tick labels along the arc = band names ("Non-existent", "Emerging", ...)
    tickvals <- bands_parsed$mid_score
    ticktext <- bands_parsed$Band_std
    
    status_label <- paste0(band, " (", round(score, 1), "%)")
    
    plotly::plot_ly(
      type  = "indicator",
      mode  = "gauge+number",
      value = score,
      number = list(
        suffix = "%",
        font   = list(size = 26, family = "Segoe UI", color = "#222222")
      ),
      gauge = list(
        shape = "angular",  # semicircle style
        axis = list(
          range    = list(0, 100),
          tickmode = "array",
          tickvals = tickvals,
          ticktext = ticktext,
          tickfont = list(size = 11, family = "Segoe UI", color = "#333333")
        ),
        bar  = list(
          color     = band_color,
          thickness = 0.18     # thinner "needle" fill, more like a pointer
        ),
        steps = steps,
        bgcolor = "#F7F7F7"
      ),
      domain = list(x = c(0, 1), y = c(0, 1))
    ) %>%
      plotly::layout(
        margin = list(l = 20, r = 20, t = 30, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        title = list(
          text = "Maturity Status",
          x    = 0.5,
          y    = 0.98,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 15, family = "Segoe UI", color = hedpac_text)
        ),
        annotations = list(
          list(
            x = 0.5, y = -0.15, xref = "paper", yref = "paper",
            text = status_label,
            showarrow = FALSE,
            font = list(size = 12, family = "Segoe UI", color = hedpac_text)
          )
        )
      )
  })
  # ---------- 3) Component-level scores by category (faceted ggplot) ----------
  # ---------- 3) Component-level scores by category (0–5, band colours) ----------
  # ---------- 3) Component-level scores by category (0–5, category colours) ----------
  # ---------- 4) Component-level scores by category (0–4, compact, wrapped labels) ----------
  # ---------- 4) Component-level scores by category (0–4, compact, wrapped labels) ----------
  # ---------- Component-level scores by category (0–4, compact, wrapped labels) ----------
  # Small helper to wrap text safely (always converts to character)
  # Safe text wrapper for ggplot (avoids is.character errors)
  wrap_safe <- function(x, width = 30) {
    vapply(
      as.character(x),
      function(z) paste(strwrap(z, width = width), collapse = "\n"),
      FUN.VALUE = character(1)
    )
  }
  
  # ---------- Component-level scores by category (0–4, base R version) ----------
  output$mat_component_bars <- renderPlot({
    comp <- component_score()
    req(!is.null(comp), nrow(comp) > 0)
    
    country_name <- selected_mat_country()
    
    df <- comp %>%
      dplyr::filter(Country == country_name)
    
    validate(need(nrow(df) > 0, "No component scores for selected country"))
    
    df <- df %>%
      mutate(
        Score_raw = as.numeric(Score),
        # enforce category order: Leadership → … → Outcomes
        Category  = factor(Category, levels = maturity_category_order)
      ) %>%
      arrange(Category, Score_raw) %>%
      mutate(
        # wrapped component labels for y-axis
        Component_label = wrap_safe(Component, width = 35),
        Component_label = factor(Component_label, levels = unique(Component_label)),
        
        # wrapped category labels, with the SAME order as maturity_category_order
        Category_label_raw   = as.character(Category),
        Category_label_wrapped = wrap_safe(Category_label_raw, width = 15),
        Category_label = factor(
          Category_label_wrapped,
          levels = wrap_safe(maturity_category_order, width = 20)
        )
      )
    
    ggplot(df, aes(x = Score_raw, y = Component_label, fill = Category)) +
      geom_col(width = 0.45) +
      geom_text(
        aes(label = round(Score_raw, 1)),
        hjust = -0.15,
        size  = 2.8
      ) +
      facet_grid(
        Category_label ~ .,
        scales  = "free_y",
        space   = "free_y",
        switch  = "y"          # 👉 move category strips to the LEFT
      ) +
      scale_fill_manual(values = category_colors, drop = FALSE) +
      scale_x_continuous(
        limits = c(0, 4.2),
        breaks = 0:4,
        labels = 0:4
      ) +
      labs(
        x = "Component score (0–4)",
        y = NULL
      ) +
      theme_minimal(base_size = 9) +
      theme(
        legend.position       = "left",
        legend.title          = element_blank(),
        axis.text.y           = element_text(
          size  = 8,
          face  = "bold",
          color = "#222222"
        ),
        strip.placement       = "outside",
        strip.background      = element_rect(fill = "#f0f0f0", colour = NA),
        strip.text.y.left     = element_text(      # 👉 clearer category labels
          size  = 8,
          face  = "bold",
          color = "#222222"
        ),
        panel.grid.major.y    = element_blank(),
        panel.grid.minor      = element_blank(),
        plot.margin           = margin(t = 10, r = 20, b = 10, l = 90)  # extra left room
      )
  }, height = 580, res = 96)
  
  # ------- Component-level score legend (0–4, band labels only) -------
  output$mat_component_legend <- renderUI({
    HTML('
      <p style="font-size:13px; margin-bottom:4px;">
        <strong>Component scoring scale (0–4)</strong>
      </p>
      <ul style="font-size:12px; padding-left:18px; margin-top:2px; margin-bottom:6px;">
        <li><strong>0 – Not-started</strong></li>
        <li><strong>1 – Emerging</strong></li>
        <li><strong>2 – Developing</strong></li>
        <li><strong>3 – Established</strong></li>
        <li><strong>4 – Mature</strong></li>
      </ul>
      <p style="font-size:11px; color:#555555; margin-top:0;">
        <em>Draft text – you can edit these descriptions directly in the script.</em>
      </p>
    ')
  })
  observe({
    session$onFlushed(function() {
      session$sendCustomMessage("hide_loading", TRUE)
    }, once = TRUE)
  })
  }  # <-- closes server <- function(...)

# ============================================================
# Run App
# ============================================================
shinyApp(ui = ui, server = server)

