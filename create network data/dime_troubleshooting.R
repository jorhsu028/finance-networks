

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

dime_location <- "/Volumes/My Passport/DIME_v3/dime_v3.sqlite3"

dime <- dbConnect(RSQLite::SQLite(), dbname = dime_location)

node_crosswalk <- 
	gq(hsu_db, "select bonica_cid, node_id from node_crosswalk") %>%
	as_tibble %>%
	filter(!is.na(bonica_cid)) 

node_crosswalk2 <- 
	gq(hsu_db, "select bonica_rid, node_id from node_crosswalk") %>% 
	as_tibble %>% 
	filter(str_detect(node_id, "_"))

test <- 
	gq(hsu_db, "select bonica_cid, bonica_rid, amount, contributor_name, recipient_name, efec_memo, efec_memo2, recipient_type, contributor_type, transaction_type from contribDB_2012") %>%
	as_tibble


gq(hsu_db, "select * from contribDB_2004 limit 5") %$% names(.)



test %>% 
	count(transaction_type) %>%
	as.data.frame %>%
	arrange(n)

test %>%
	filter(transaction_type == "18U") %>% print
	count(efec_memo) %>%
	arrange(desc(n))

	select(efec_memo, contributor_name, recipient_name) %>% print
	count(efec_memo) 




# 

# question about calculating
# 17', '17R', '17Y', '17Z',
	# these are input as positive amounts should be subtracted right..?




# 18G - Transfer in from affiliated committee
	# money from campaign account 

# question about adding
# 24F - Communication cost for candidate (only for Form 7 filer)
# 24C - Coordinated party expenditure
# 29 - Electioneering communication 


# add
# 24I - Earmarked contributor's check passed on by intermediary committee to intended recipient (intermediary out)
# 24T - Earmarked contribution passed to intended recipient from intermediary's treasury (treasury out)
# 24G - Transfer out to affiliated committee 
	# looks like leadership PACs
# 22Z - Contribution refund to candidate or committee
# 18K - Contribution received from registered filer
# 18J - Memo - Recipient committee's percentage of contribution from a registered committee given to joint fundraising committee
# 15T - Earmarked contribution from an individual, partnership or limited liability company received by intermediary committee and entered into intermediary's treasury (intermediary treasury in)
# 15J - Memo - Recipient committee's percentage of contribution from an individual, partnership or limited liability company given to joint fundraising committee
# 11 - Tribal contribution 


# check out the FEC rules about transaction types

	# fec annual guidance

	