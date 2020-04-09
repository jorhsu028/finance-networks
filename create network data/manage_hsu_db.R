

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


db_location <- "/Users/jordanhsu/Google Drive/Research/Data/dime_plus/hsu_networks.sqlite3"

hsu_db <- dbConnect(RSQLite::SQLite(), dbname = db_location)

dime_location <- "/Volumes/My Passport/dime/dime.sqlite3"

dime <- dbConnect(RSQLite::SQLite(), dbname = dime_location)

node_crosswalk <- gq(hsu_db, "select bonica_cid, node from node_crosswalk") %>% as_tibble
node_crosswalk2 <- gq(hsu_db, "select bonica_rid, node from node_crosswalk") %>% as_tibble


#############################################################################################
# remove dbs
############################################################################################
dbl(dime)


for (t in seq(2006, 2018, 2))	{
	sq(dime, paste0("drop table contribDB_", t, "_new"))
	print(t)
}

for (t in seq(1980, 2004, 2))	{
	gq(hsu_db, paste0("drop table contribDB_", t, "TRUE"))
}




#############################################################################################
# rename dbs
############################################################################################
gq(hsu_db, "alter table leiden_res.5DB rename to factionDB_leiden_respoint5")

sq(dime, "ALTER TABLE contribDB_2006_fin RENAME TO contribDB_2006")



#############################################################################################
# create new raw DIME db
############################################################################################

# setwd("/Volumes/My Passport/DIME-raw")

# rm(db)
# library(data.table)
# library(R.utils)

# # readLines(transact, n=1)
# db_colnames <- c("cycle","transaction.id","transaction.type","amount","date","bonica.cid","contributor.name","contributor.lname","contributor.fname","contributor.mname","contributor.suffix","contributor.title","contributor.ffname","contributor.type","contributor.gender","contributor.address","contributor.city","contributor.state","contributor.zipcode","contributor.occupation","contributor.employer","is.corp","recipient.name","bonica.rid","recipient.party","recipient.type","recipient.state","seat","election.type","latitude","longitude","gis.confidence","contributor.district.90s","contributor.district.00s","contributor.district.10s","censustract","efec.memo","efec.memo2","efec.transaction.id.orig","bk.ref.transaction.id","efec.org.orig","efec.comid.orig","efec.form.type","excluded.from.scaling","contributor.cfscore","candidate.cfscore")


# file <- 'contribDB_2012.csv.gz'
# nrows <- 5e5
# skip <- 9500000

# for (i in seq(1, 100, 1)) {
# 	skip <- skip + nrowsf
# 	chunk <- read.csv(file, nrows = nrows, skip = skip)
# 	names(chunk) <- db_colnames

# 	wt(dime, paste0("2012chunk", i), chunk, overwrite = TRUE)
# 	rm(chunk)
# 	print(skip)
# }

# # test1 <- gq(dime, "select * from chunk1") %>% as_tibble
# # test2 <- gq(dime, "select * from chunk2") %>% as_tibble


# for (t in seq(2004, 2018, 2)) {
# 	db <- fread(paste0("contribDB_", t, ".csv.gz"))
# 	wt(dime, paste0("contribDB_", t), db, overwrite = TRUE)
# 	rm(db)
# 	print(t)
# }
# read.csv.sql("contribDB_2012.csv.gz", sql=..., ...other args...)

# dbl(dime)
