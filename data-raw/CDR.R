## Script to prepare the CDR dataset for the package.
## Run this script once to regenerate data/CDR.rda.
## Source: Garcia & Llaugel CDR research dataset.

CDR <- read.csv("datasets/CDR.csv", stringsAsFactors = FALSE)

save(CDR, file = "data/CDR.rda", compress = "xz")
