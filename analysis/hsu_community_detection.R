

source("/Users/jordanhsu/Google Drive/Programs/R/R Functions.R")
library(tidyverse)
library(magrittr)
library(haven)
library(purrr)
library(beepr)
library(readxl)
library(RColorBrewer)
library(RSQLite)


#######################################
# Function definitions
# #####################################
sq <- function(...) {dbSendQuery(...)}
cr <- function(...) {dbClearResult(...)}
gq <- function(...) {dbGetQuery(...)}
wt <- function(...) {dbWriteTable(...)}
dbl <- function(...) {dbListTables(...)}

db_location <- "/Users/jordanhsu/Google Drive/Research/Data/dime_plus/hsu_networks.sqlite3"

hsu_db <- dbConnect(RSQLite::SQLite(), dbname = db_location)

node_crosswalk <- gq(hsu_db, "select * from node_crosswalk") %>% as_tibble

#############################################################################################
# run louvain community detection
############################################################################################

factions <-
	tibble(
		node_id = NA,
		louvain = NA,
		cycle = NA
	)

for (t in seq(1980, 2018, 2)) {
	comp1 <-
		gq(hsu_db, 
			paste0(
				"select node_id, cycle from nodeDB 
				where component = 1
				and cycle = ", t
			)
		) 

	dyad <-
		gq(hsu_db,
			paste0(
				"select node1, node2, total_contribs
				from dir_dyadDB_", t,
				" where total_contribs >= 200"
			)
		) %>%
		as_tibble %>%
		filter(node1 %in% comp1$node_id)

	net <- igraph::graph_from_data_frame(dyad, directed = FALSE)
	louv_cluster <- igraph::cluster_louvain(net, weights = net$total_contribs)
	louv_membership <- igraph::membership(louv_cluster)

	faction_temp <-
		tibble(
			node_id = row.names(as.matrix(louv_membership)),
			louvain = as.matrix(louv_membership)[,1],
			cycle = t
		)

	factions <-
		bind_rows(factions, faction_temp) %>%
		filter(!is.na(node_id))

	rm(dyad)
	print(t)
}

wt(hsu_db, "node_resultsDB", factions, overwrite = TRUE)

#############################################################################################
# run leiden community detection
############################################################################################

# res_par <- 2

# factions <-
# 	tibble(
# 		node_id = NA,
# 		leiden = NA,
# 		cycle = NA
# 	) %>%
# 	rename(!!paste0("leiden_", res_par) := leiden)

# for (t in seq(1980, 1980, 2)) {
# 	comp1 <-
# 		gq(hsu_db, 
# 			paste0(
# 				"select node_id, cycle from nodeDB 
# 				where component = 1
# 				and cycle = ", t
# 			)
# 		) 

# 	dyad <-
# 		gq(hsu_db,
# 			paste0(
# 				"select node1, node2, total_contribs
# 				from udir_dyadDB_", t,
# 				" where total_contribs >= 200"
# 			)
# 		) %>%
# 		as_tibble %>%
# 		filter(node1 %in% comp1$node_id)

# 	net <- igraph::graph_from_data_frame(dyad, directed = FALSE)
# 	leiden_cluster <- leiden::leiden(net, weights = net$total_contribs, resolution_parameter = res_par)

# 	faction_temp <-
# 		tibble(
# 			node_id = names(igraph::V(net)),
# 			leiden = leiden_cluster,
# 			cycle = t
# 		) %>%
# 		rename(!!paste0("leiden_", res_par) := leiden)

# 	factions <-
# 		bind_rows(factions, faction_temp) %>%
# 		filter(!is.na(node_id))

# 	rm(dyad)
# 	print(t)
# }

# factions_temp <-
# 	gq(hsu_db, "select * from node_resultsDB") %>%
# 	as_tibble


# factions_new <-
# 	factions %>%
# 	select(-leiden_1) %>%
# 	left_join(., factions_temp, by = c("node", "cycle")) %>%
# 	print

# wt(hsu_db, "node_resultsDB", factions_new, overwrite = TRUE)

#############################################################################################
# create faction DBs and add with node info
############################################################################################

node_info <- gq(hsu_db, "select cycle, node_id, state, node_type, gender, rec_party, rec_gender, rec_igcat, rec_district, con_district_90s, con_district_00s, con_district_10s, con_employer, con_occupation, con_is_corp from nodeDB") %>% as_tibble

node_results <- 
	gq(hsu_db, "select * from node_resultsDB") %>% 
	as_tibble %>%
	left_join(., node_info, by = c("cycle", "node_id")) %>%
	print

#######################################
# input faction type
#######################################
faction_str <- "louvain"
faction <- quo(louvain)

#######################################
# run faction descriptives
#######################################
{
	# size of faction
	faction_size <- 
		node_results %>%
		count(cycle, !!faction) %>%
		rename(
			louvain_num = !!faction,
			size = n
		) %>%
		print

	# party alignment of faction members
	faction_opp <-
		node_results %>%
		filter(rec_party %in% c("R", "D")) %>%
		count(cycle, !!faction, rec_party) %>%
		ungroup %>% 
		spread(rec_party, n) %>% 
		transmute(
			cycle,
			louvain_num = !!faction,
			num_D = ifelse(is.na(D), 0, D),
			num_R = ifelse(is.na(R), 0, R),
			tot_can = num_D + num_R,
			prop_D = num_D / (num_D + num_R),
			one_party_cat = 
				case_when(
					prop_D >= .90 | prop_D <= .10 ~ "90+",
					(prop_D >= .80 & prop_D < .90) | (prop_D > .10 & prop_D <= .20) ~ "80-90",
					(prop_D >= .70 & prop_D < .80) | (prop_D > .20 & prop_D <= .30) ~ "70-80",
					(prop_D >= .60 & prop_D < .70) | (prop_D > .30 & prop_D <= .40) ~ "60-70",
					prop_D < .60 | prop_D > .40 ~ "<60"
				)
		) %>%
		print

	# same state
	same_st_factions <-
		node_results %>%
		filter(!is.na(state) & state != "Other") %>%
		count(cycle, !!faction, state) %>% 
		ungroup %>% 
		group_by(cycle, !!faction) %>% 
		mutate(
			num_state = n(),
			num_member_state = sum(n),
			prop_same_state = n / num_member_state
		) %>% 
		filter(prop_same_state == max(prop_same_state)) %>% 
		ungroup %>%
		transmute(
			cycle,
			louvain_num = !!faction,
			num_state,
			prop_same_state,
			same_state_cat = 
				case_when(
					prop_same_state >= .90 | prop_same_state <= .10 ~ "90+",
					(prop_same_state >= .80 & prop_same_state < .90) | (prop_same_state > .10 & prop_same_state <= .20) ~ "80-90",
					(prop_same_state >= .70 & prop_same_state < .80) | (prop_same_state > .20 & prop_same_state <= .30) ~ "70-80",
					(prop_same_state >= .60 & prop_same_state < .70) | (prop_same_state > .30 & prop_same_state <= .40) ~ "60-70",
					prop_same_state < .60 | prop_same_state > .40 ~ "<60"
				)
		) %>%
		print

	# gender
	gender_factions <-
		node_results %>%
		filter(gender %in% c("F", "M")) %>%
		count(cycle, !!faction, gender) %>%
		ungroup %>% 
		spread(gender, n) %>% 
		transmute(
			cycle,
			louvain_num = !!faction,
			num_women = F,
			num_men = M,
			prop_women = F / (F + M)
		) %>%
		print

	# corps/unions in faction 
	corp_factions <-
		node_results %>%
		filter(con_is_corp == 1) %>%
		count(cycle, !!faction, con_is_corp) %>%
		ungroup %>%
		spread(con_is_corp, n) %>% 
		transmute(
			cycle,
			louvain_num = !!faction,
			num_is_corp = `1`,
			num_is_corp = ifelse(is.na(num_is_corp), 0, num_is_corp)
		) %>%
		print

}

#######################################
# get faction level contribution amounts
#######################################

faction_contribs <-
	tibble(
		louvain_num = NA,
		total_contribs = NA,
		num_contribs = NA
	) 

for (t in seq(1980, 2018, 2)) {

	dyad_temp <-
		gq(hsu_db, 
			paste0("select * from udir_dyadDB_", t, " 
			where total_contribs >= 200")
		)

	node_results <-
		gq(hsu_db, 
			paste0("select node_id, cycle, louvain from node_resultsDB where cycle = ", t)
		) 

	comm_dyads <-
		left_join(dyad_temp, node_results, by = c("node1" = "node_id", "cycle")) %>%
		left_join(., node_results, by = c("node2" = "node_id", "cycle")) %>% 
		mutate(
			same_louv = ifelse(louvain.x == louvain.y, 1, 0)
		) %>%
		filter(same_louv == 1) %>%
		group_by(louvain.x) %>%
		summarise(
			num_contribs = sum(num_contribs),
			total_contribs = sum(total_contribs)
		) %>%
		transmute(
			cycle = t,
			louvain_num = louvain.x,
			num_contribs,
			total_contribs
		) 

	faction_contribs <-
		bind_rows(faction_contribs, comm_dyads) %>%
		filter(!is.na(cycle))

	print(t)
}

faction_new <-
	left_join(faction_size, faction_opp, by = c("cycle", "louvain_num")) %>%
	left_join(., same_st_factions, by = c("cycle", "louvain_num")) %>%
	left_join(., gender_factions, by = c("cycle", "louvain_num")) %>%
	left_join(., corp_factions, by = c("cycle", "louvain_num")) %>%
	left_join(., faction_contribs, by = c("cycle", "louvain_num")) %>%
	mutate(louv_id = paste0(cycle, "_", louvain_num)) %>%
	print

wt(hsu_db, paste0("factionDB_", faction_str), faction_new, overwrite = TRUE)

#############################################################################################
# calculate in/out community contributions
############################################################################################




faction_contribs <-
	tibble(
		louvain_num = NA,
		total_contribs = NA,
		num_contribs = NA
	) 

t <- 2006

# for (t in seq(1980, 2018, 2)) {

	dyad_temp <-
		gq(hsu_db, 
			paste0("select * from udir_dyadDB_", t, " 
			where total_contribs >= 200")
		)

	node_results <-
		gq(hsu_db, 
			paste0("select node_id, cycle, louvain from node_resultsDB where cycle = ", t)
		) 

	comm_dyads <-
		left_join(dyad_temp, node_results, by = c("node1" = "node_id", "cycle")) %>%
		left_join(., node_results, by = c("node2" = "node_id", "cycle")) %>% 
		as_tibble %>%
		mutate(
			same_louv = ifelse(louvain.x == louvain.y, 1, 0)
		) %>% 
		group_by(node1, same_louv) %>%
		summarise(
			num_dyads = sum(num_contribs),
			total_contribs = sum(total_contribs)
		) %>% arrange(node1) %>% print
		transmute(
			cycle = t,
			louvain_num = louvain.x,
			num_contribs,
			total_contribs
		) 

	faction_contribs <-
		bind_rows(faction_contribs, comm_dyads) %>%
		filter(!is.na(cycle))

	print(t)
# }