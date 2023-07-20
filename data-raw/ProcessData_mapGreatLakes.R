# Prepare data for use in package
# example data for Great Lakes map
# Great Lakes 2010, PFAS
#
# Erik.Leppo@tetratech.com
# 2023-07-19
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Data----
## Data, Values
fn_data <- "GLHHFTS 2010 PFAS_20210426.xlsx"
sh_data <- "2010 GLHHFTS PFAS Results"
path_data <- file.path("data-raw", "data", fn_data)
df_data <- readxl::read_excel(path_data, sheet = sh_data)

## Data, SampInfo
fn_sampinfo <- fn_data
sh_sampinfo <- "2010 GLHHFTS Sample Info"
path_sampinfo <- file.path("data-raw", "data", fn_sampinfo)
df_sampinfo <- readxl::read_excel(path_sampinfo, sheet = sh_sampinfo)

# Munge ----

## SampInfo to unique
col2keep <- c("Site ID","EPA Sample ID", "Latitude", "Longitude")
df_sampinfo_loc <- unique(df_sampinfo[, col2keep])

## Merge
df_merge <- merge(df_data
                  , df_sampinfo_loc
                  , by.x = c("Site ID","EPA Sample ID")
                  , by.y = c("Site ID","EPA Sample ID")
                  , all.x = TRUE
                  , all.y = TRUE
)
stopifnot(nrow(df_data) == nrow(df_merge)) #QC

#df_map_pfos <- df_merge[df_merge[, "Analyte"] == "Perfluorooctanesulfonate (PFOS)", ]

# Save ----
data_gl2010_map <- df_merge
## as RDA for use in package
usethis::use_data(data_gl2010_map, overwrite = TRUE)


# Document data
## Add to R/data.R
promptData(data_gl2010_map)
