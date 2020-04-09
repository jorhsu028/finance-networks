

source("/Users/jordanhsu/Google Drive/Programs/R/R Functions.R")
library(tidyverse)
library(magrittr)
library(haven)
library(purrr)
library(beepr)
library(readxl)

library(RSQLite)


# #######################################
# # Function definitions
# ########################################
sq <- function(...) {dbSendQuery(...)}
cr <- function(...) {dbClearResult(...)}
gq <- function(...) {dbGetQuery(...)}
wt <- function(...) {dbWriteTable(...)}
dbl <- function(...) {dbListTables(...)}


db_location <- "/Volumes/My Passport/DIME_v3/dime_v3.sqlite3"

dime <- dbConnect(RSQLite::SQLite(), dbname = db_location)
wt(dime, "dime_donorDB", read_csv("/Volumes/My Passport/DIME_v3/dime_contributors_1979_2018.csv"))
fq
dbl(dime)

gq(dime, "select * from contribDB_2014 limit 5")


sq(dime, "alter table contribDB_2006_old rename to contribDB_2006")
sq(dime, "create table contribDB_2006 ")
dbl(dime)

ALTER TABLE team RENAME TO team_orig;
Step 2: Create the replacement table with the original name and corrected column name:

CREATE TABLE team(Name TEXT, Coach TEXT, Location TEXT);
Step 3: Copy the data from the original table to the new table:

INSERT INTO team(Name, Coach, Location) SELECT Name, Coach, City FROM team_orig;
Note: The above command should be all one line.
Step 4: Drop the original table:

DROP TABLE team_orig;
ALTER TABLE "table_name"
RENAME COLUMN "column 1" TO "column 2"

gq(dime, "alter contribDB_2008 rename cycle to blah")

check <- gq(dime, "select * from contribDB_2008") %>% as_tibble 

names(check)

DBI::dbWriteTable(dime, "contribDB_2006", "/Users/jordanhsu/Desktop/DIME_v3/contribDBs/contribDB_2006.csv.gz")

# for (t in seq(1980, 2018, 2)) {

# 	data.table::fread(paste0("/Users/jordanhsu/Desktop/DIME_v3/contribDBs/contribDB_", t, ".csv.gz")) %>%
# 	as_tibble %>%
# 	rename_at(.vars = vars(contains(".")), .funs = list(~str_replace_all(., "\\.", "_"))) %>%
# 	wt(dime, paste0("contribDB_", t), ., overwrite = TRUE)

# 	print(t)
# }

gq(dime, "select * from contribDB_1980 limit 5")
dbl(dime)




