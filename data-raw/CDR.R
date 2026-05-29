## Script to prepare the CDR dataset for the package.
## Run this script once to regenerate data/CDR.rda.
## Source: Garcia & Llaugel CDR research dataset.

CDR <- read.csv("datasets/CDR.csv", stringsAsFactors = FALSE, na.strings = "")

# Namibia's ISO2 code is "NA", which read.csv converts to R's NA.
CDR$code[CDR$Country == "Namibia" & is.na(CDR$code)] <- "NA"

# Fix wrong ISO2 codes in source CSV:
# North Macedonia is MK (not MC, which is Monaco)
CDR$code[CDR$Country == "Macedonia"] <- "MK"
# Mauritius is MU (not MR, which is Mauritania)
CDR$code[CDR$Country == "Mauritius"] <- "MU"

save(CDR, file = "data/CDR.rda", compress = "xz")
