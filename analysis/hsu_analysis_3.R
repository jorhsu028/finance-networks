  

source("/Users/jordanhsu/Google Drive/Programs/R/R Functions.R")
# library(foreign)
# library(rio)
library(reshape2)
library(stringr)
library(tidyverse)
library(magrittr)
# library(haven)
library(purrr)
library(ggthemes)
# library(ggmcmc)
library(beepr)
library(readxl)

library(DBI)
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


donor_recip_crosswalk <-
	gq(hsu_db,
		"select bonica_rid, bonica_cid
		from recipDB"
		) %>%
	as_tibble %>%
	distinct(bonica_rid, bonica_cid, .keep_all = TRUE) %>%
	filter(!is.na(bonica_cid)) %>%
	print

setwd("Google Drive/Research/Dissertation")

# dime.location <- "/Volumes/My Passport/dime/dime.sqlite3" 

# dime <- dbConnect(RSQLite::SQLite(), dbname = dime.location) 

#############################################################################################
# get actors by cycle
############################################################################################


# all_nodes <- tibble(node = NA, cycle = NA)

# for (t in seq(1980, 2014, 2))	{

# 	dyad <- 
# 		gq(hsu_db, 
# 			paste0("select * 
# 			from dyadDB_", t) 
# 		) %>% 
# 		as_tibble %>% 
# 		left_join(., donor_recip_crosswalk, by = "bonica_cid") %>% 
# 		transmute(
# 			from = 
# 				case_when(
# 					is.na(bonica_rid.y) ~ as.character(bonica_cid),
# 					!is.na(bonica_rid.y) ~ bonica_rid.y
# 				),
# 			to = bonica_rid.x,
# 			num_contribs,
# 			total_contribs,
# 			cycle,
# 			party
# 		) %>%
# 		filter(total_contribs >= 200) %>% 
# 		print

# 	from <-
# 		dyad %>% 
# 		distinct(from, .keep_all = TRUE) %>% 
# 		transmute(node = from) %>%
# 		mutate(type1 = 1)

# 	to <-
# 		dyad %>% 
# 		distinct(to, .keep_all = TRUE) %>% 
# 		transmute(node = to) %>%
# 		mutate(type2 = 1)

# 	nodes <-
# 		full_join(from, to, by = c("node")) %>%
# 		mutate(
# 			type = 
# 				case_when(
# 					type1 == 1 & is.na(type2) ~ 1,
# 					is.na(type1) & type2 == 1 ~	2,
# 					type1 == 1 & type2 == 1 ~ 3
# 				),
# 			cycle = t
# 		) %>%
# 		select(-type1:-type2) %>%
# 		print

# 	all_nodes <-
# 		bind_rows(all_nodes, nodes) %>% 
# 		filter(!is.na(node)) %>% 
# 		print

# }

# wt(hsu_db, "nodes_by_cycle", all_nodes, overwrite = TRUE)
# 1: donor
# 2: recipient
# 3: both

#############################################################################################
# calculate networks over time
############################################################################################

temp <-
	all_nodes %>%
	filter(cycle == 1980) %>%
	print

nodes <-
	all_nodes %>%
	distinct(node) %>%
	left_join(., transmute(filter(all_nodes, cycle == 1980), node, e_1980 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1982), node, e_1982 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1984), node, e_1984 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1986), node, e_1986 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1988), node, e_1988 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1990), node, e_1990 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1992), node, e_1992 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1994), node, e_1994 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1996), node, e_1996 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 1998), node, e_1998 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2000), node, e_2000 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2002), node, e_2002 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2004), node, e_2004 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2006), node, e_2006 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2008), node, e_2008 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2010), node, e_2010 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2012), node, e_2012 = type), by = "node") %>%
	left_join(., transmute(filter(all_nodes, cycle == 2014), node, e_2014 = type), by = "node") %>%
	print	

 




# calculate the number of cycles involved since t
	# strict definition: each cycle since
	# loose definition: end point


# which cycles was a person active in?
# whats the percent of donors in a given cycle that also were present in year t
	# how many years out from year t am I interested in 


# try some different types of community detection algorithms and see what they return 
	# what about successful rounds of community detection

houseDB <-
	gq(hsu_db,
		"select * 
		from recipDB
		where chamber = 'H'
		and winner = 'W'"
	) %>% 
	as_tibble %>% 
	left_join(., lep, by = c("lname", "district", "congress_num")) %>% 
	mutate(
		backbench = ifelse(is_leader == 0 & incum_chall == "I", 1, 0),
		party_position =
			case_when(
				is_leader == 1 ~ "Leadership",
				chair == 1 & is_leader == 0 ~ "Chair",
				is_leader == 0 & chair == 0 ~ "Backbench"
			)
	) %>% 
	print

