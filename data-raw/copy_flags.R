## Copy and rename flag PNGs to inst/flags/<ISO2>.png
## Flags source: datasets/flags/<Country_Name>.png
## Destination:  inst/flags/<iso2c>.png
##
## Run once from the package root:
##   source("data-raw/copy_flags.R")

src_dir <- "datasets/flags"
dst_dir <- "inst/flags"
dir.create(dst_dir, showWarnings = FALSE, recursive = TRUE)

# Manual mapping: iso2c code -> source filename (no extension)
# Keys are CDR$code values; values are the flag filenames in datasets/flags/
flag_map <- c(
  AR = "Argentina",
  AM = "Armenia",
  AU = "Australia",
  AT = "Austria",
  BD = "Bangladesh",
  BE = "Belgium",
  BO = "Bolivia",
  BW = "Botswana",
  BR = "Brazil",
  BG = "Bulgaria",
  CA = "Canada",
  CL = "Chile",
  CN = "China",
  CO = "Colombia",
  CI = "Ivory_Coast",
  HR = "Croatia",
  DK = "Denmark",
  DO = "Dominican_Republic",
  EG = "Egypt",
  SV = "El_Salvador",
  EE = "Estonia",
  FI = "Finland",
  FR = "France",
  DE = "Germany",
  GH = "Ghana",
  GR = "Greece",
  HU = "Hungary",
  IN = "India",
  ID = "Indonesia",
  IR = "Iran",
  IE = "Ireland",
  IL = "Israel",
  IT = "Italy",
  JM = "Jamaica",
  JP = "Japan",
  JO = "Jordan",
  KZ = "Kazakhstan",
  KE = "Kenya",
  KR = "South_Korea",
  KG = "Kyrgyzstan",
  LV = "Latvia",
  LB = "Lebanon",
  LT = "Lithuania",
  MC = "North_Macedonia",
  MW = "Malawi",
  MY = "Malaysia",
  MR = "Mauritius",
  MX = "Mexico",
  MN = "Mongolia",
  MA = "Morocco",
  `NA` = "Namibia",
  NL = "Netherlands",
  NG = "Nigeria",
  NO = "Norway",
  OM = "Oman",
  PA = "Panama",
  PE = "Peru",
  PH = "Philippines",
  PL = "Poland",
  PT = "Portugal",
  RO = "Romania",
  RU = "Russia",
  SA = "Saudi_Arabia",
  RS = "Serbia",
  SG = "Singapore",
  SK = "Slovakia",
  SI = "Slovenia",
  ZA = "South_Africa",
  ES = "Spain",
  SE = "Sweden",
  CH = "Switzerland",
  TH = "Thailand",
  TT = "Trinidad_and_Tobago",
  TR = "Turkey",
  UG = "Uganda",
  UA = "Ukraine",
  GB = "United_Kingdom",
  US = "United_States",
  VN = "Vietnam"
)

missing <- character(0)
copied  <- character(0)

for (code in names(flag_map)) {
  src <- file.path(src_dir, paste0(flag_map[[code]], ".png"))
  dst <- file.path(dst_dir, paste0(code, ".png"))
  if (!file.exists(src)) {
    missing <- c(missing, sprintf("%s -> %s (NOT FOUND)", code, src))
  } else {
    file.copy(src, dst, overwrite = TRUE)
    copied <- c(copied, code)
  }
}

cat(sprintf("Copied %d flags to %s/\n", length(copied), dst_dir))
if (length(missing)) {
  cat("Missing source files:\n")
  cat(paste(" ", missing, collapse = "\n"), "\n")
}
