# Prepare data for use in package
# example data for Great Lakes Upset Plot
# Great Lakes 2010, PFAS/Hg/PCBs
#
# Erik.Leppo@tetratech.com
# 2023-07-20
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages
library(readxl) # read Excel
library(tidyr)  # pivot wider
library(dplyr)  # select

# Global Variables ----
namestudy <- "Great Lakes"
studyyear <- 2010
namedata <- paste0(namestudy, ", ", studyyear)

# Data----
fn_hg <- "GLHHFTS 2010 Mercury_20210426.xlsx"
sh_hg <- "GLHHFTS Mercury data"
path_hg <- file.path("data-raw", "data", fn_hg)

fn_pcb <- "GLHHFTS 2010 PCB_20210426.xlsx"
sh_pcb <- "2010 GLHHFTS PCB data"
path_pcb <- file.path("data-raw", "data", fn_pcb)

fn_pfas <- "GLHHFTS 2010 PFAS_20210426.xlsx"
sh_pfas <- "2010 GLHHFTS PFAS Results"
path_pfas <- file.path("data-raw", "data", fn_pfas)


df_hg <- read_excel(path_hg, sheet = sh_hg)
df_pcb <- read_excel(path_pcb, sheet = sh_pcb)
df_pfas <- read_excel(path_pfas, sheet = sh_pfas)

# Munge----
# Combine files
col_common <- c("EPA Region"
                , "State"
                , "Lake"
                , "Site ID"
                , "EPA Sample ID"
                , "Analyte"
                , "CAS Number"
                , "Amount")

# rename (keep same as 2015)
df_hg[, "Lake"] <- df_hg[, "Lake Name"]
df_pcb[, "Lake"] <- df_pcb[, "Lake Name"]
df_pfas[, "Lake"] <- df_pfas[, "Lake Name"]


# Screeing Values ----
fishconsumer <- "general"
df_hg[, "SV_Value"] <- 300
df_hg[, "SV_Exceed"] <- ifelse(df_hg[, "Amount"] >= df_hg[, "SV_Value"]
                               , TRUE
                               , FALSE)
df_pfas[, "SV_Value"] <- 0.52
df_pfas[, "SV_Exceed"] <- ifelse(df_pfas[, "Amount"] >= df_pfas[, "SV_Value"]
                                 , TRUE
                                 , FALSE)
df_pcb[, "SV_Value"] <- 12
df_pcb[, "SV_Exceed"] <- ifelse(df_pcb[, "Amount"] >= df_pcb[, "SV_Value"]
                                , TRUE
                                , FALSE)


col_SV <- c("SV_Value", "SV_Exceed")

df_data <- rbind(df_hg[, c(col_common, col_SV)]
                 , df_pfas[, c(col_common, col_SV)]
                 , df_pcb[, c(col_common, col_SV)])

# Reorganize
# "SV_Exceed"
#df_plot <- summarize(group_by(df_data, 'EPA Sample ID', ))
# SV_Eval; Exceeds SV to 1, Does not exceed to 0
df_wide <- pivot_wider(df_data[, c("EPA Sample ID", "Analyte", "SV_Exceed")]
                       , names_from = "Analyte"
                       , values_from = "SV_Exceed")


# Remove columns all NA
df_plot <- df_wide %>% select(where(~!all(is.na(.x))))

# logical to integer
cols_boo <- sapply(df_plot, is.logical)
df_plot[, cols_boo] <- lapply(df_plot[, cols_boo], as.integer)

# Convert to dataframe
df_plot <- data.frame(df_plot)

# Filter for only interested analytes
analytes_plot <- c("Mercury", "Perfluorooctanesulfonate..PFOS.", "Total.PCBs")
df_plot <- df_plot[, c("EPA.Sample.ID", analytes_plot)]


# Save ----
data_gl2010_upset <- df_plot
## as RDA for use in package
usethis::use_data(data_gl2010_upset, overwrite = TRUE)


# Document data
## Add to R/data.R
promptData(data_gl2010_upset)
