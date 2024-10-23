## ---------------------------
##
## Script name: Obtaining validated microRNA to gene targets
##
## Purpose of script: This script takes in miRNA to validated gene targets from the https://mirnet.ca/ site.
##
## Date Created: 2024-10-23
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
library(RSQLite)
library(DBI)
library(dplyr)

# The sqlite file can be downloaded from
# For miRNetR users, the path to sqlite database is updated to: https://www.xialab.ca/rest/sqlite/mir2gene.sqlite (10/08/2024) 

# Step 1: Download the SQLite file
url <- "https://www.xialab.ca/rest/sqlite/mir2gene.sqlite"
destfile <- tempfile(fileext = ".sqlite")  # Temporary file to store the downloaded sqlite file
download.file(url, destfile, mode = "wb")

# Step 2: Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), destfile)

# View the tables in the database
dbListTables(con)

# Step 3: Collect only the human microRNA-to-gene targets table
hsa <- tbl(con, "hsa") %>% collect()

# Step 4: Save the table as a csv file
readr::write_csv(hsa, "mir_targets.csv")

dbDisconnect(con)
