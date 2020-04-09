

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

# dime_location <- "/Volumes/My Passport/DIME_v3/dime_v3.sqlite3"

# dime <- dbConnect(RSQLite::SQLite(), dbname = dime_location)

node_crosswalk <- 
	gq(hsu_db, "select bonica_cid, node_id from node_crosswalk") %>%
	as_tibble %>%
	filter(!is.na(bonica_cid)) 

node_crosswalk2 <- 
	gq(hsu_db, "select bonica_rid, node_id from node_crosswalk") %>% 
	as_tibble %>% 
	filter(str_detect(node_id, "_"))

#############################################################################################
# move dime DBs to hsu_db
#############################################################################################

# dbl(dime)

# gq(dime, "select * from dime_recipients_all_1979_2018") %>%
# 	wt(hsu_db, "dime_candDB", .)

# gq(dime, "select * from contribDB_2018 limit 5")

# dbl(hsu_db)
# candDB <- gq(dime, "select * from candDB") %>% as_tibble
# wt(hsu_db, "dime_candDB", candDB)

#############################################################################################
# rename dime columns with periods
#############################################################################################

# sq(dime, 
# 	"CREATE TABLE `contribDB_2018_new` (
# 	  `cycle` INTEGER,
# 	  `transaction_id` TEXT,
# 	  `transaction_type` TEXT,
# 	  `amount` REAL,
# 	  `date` TEXT,
# 	  `bonica_cid` INTEGER,
# 	  `contributor_name` TEXT,
# 	  `contributor_lname` TEXT,
# 	  `contributor_fname` TEXT,
# 	  `contributor_mname` TEXT,
# 	  `contributor_suffix` TEXT,
# 	  `contributor_title` TEXT,
# 	  `contributor_ffname` TEXT,
# 	  `contributor_type` TEXT,
# 	  `contributor_gender` TEXT,
# 	  `contributor_address` TEXT,
# 	  `contributor_city` TEXT,
# 	  `contributor_state` TEXT,
# 	  `contributor_zipcode` INTEGER,
# 	  `contributor_occupation` TEXT,
# 	  `contributor_employer` TEXT,
# 	  `is_corp` TEXT,
# 	  `recipient_name` TEXT,
# 	  `bonica_rid` TEXT,
# 	  `recipient_party` TEXT,
# 	  `recipient_type` TEXT,
# 	  `recipient_state` TEXT,
# 	  `seat` TEXT,
# 	  `election_type` TEXT,
# 	  `latitude` REAL,
# 	  `longitude` REAL,
# 	  `gis_confidence` REAL,
# 	  `contributor_district_90s` TEXT,
# 	  `contributor_district_00s` TEXT,
# 	  `contributor_district_10s` TEXT,
# 	  `censustract` INTEGER,
# 	  `efec_memo` TEXT,
# 	  `efec_memo2` INTEGER,
# 	  `efec_transaction_id_orig` REAL,
# 	  `bk_ref_transaction_id` INTEGER,
# 	  `efec_org_orig` INTEGER,
# 	  `efec_comid_orig` INTEGER,
# 	  `efec_form_type` INTEGER,
# 	  `excluded_from_scaling` INTEGER,
# 	  `contributor_cfscore` REAL,
# 	  `candidate_cfscore` REAL
# 	)"
# )

# sq(dime, "
# 	CREATE TABLE contribDB_2018_fin AS 
# 	SELECT * FROM contribDB_2018_new 
# 	UNION ALL 
# 	SELECT * FROM contribDB_2018
# ")

# beeploud()

#############################################################################################
# create cycle contribution DBs
#############################################################################################

# for (t in seq(2006, 2018, 2)) {
# 	gq(dime,
# 			paste0(
# 				"select cycle, transaction_id, transaction_type, bonica_cid, bonica_rid, amount, contributor_name, contributor_lname, contributor_state, contributor_type, contributor_gender, contributor_occupation, contributor_employer, is_corp,  contributor_district_90s, contributor_district_00s, contributor_district_10s, contributor_cfscore, candidate_cfscore, recipient_name, recipient_state, recipient_party, recipient_type, seat, election_type
# 				from contribDB_", t, " 
# 				where seat in ('federal:house', 'federal:senate', 'federal:527', 'federal:committee')
# 				and transaction_type in ('11', '15', '15E', '15J', '15T', '15Z', '17', '17R', '17Y', '17Z', '18G', '18J', '18K', '19', '22Y', '22Z', '24C', '24E', '24F', '24G', '24H', '24I', '24K', '24T', '24Z', '29')
# 			")
# 	) %>%
# 	wt(hsu_db, paste0("contribDB_", t), ., overwrite = TRUE)

# 	print(t)
# }
# beeploud()

#############################################################################################
# create id crosswalk for recipients
#############################################################################################

# dime_candDB <- 
# 	gq(hsu_db, "select * from dime_candDB") %>%
# 	as_tibble %>%
# 	rename_all(tolower) %>%
# 	filter(
# 		seat %in% c("federal:527", "federal:committee", "federal:house", "federal:senate")
# 	) %>%
# 	transmute(
# 		cycle, fecyear, name, 
# 		node_id = 
# 			case_when(
# 				!is.na(bonica_cid) ~ paste0(bonica_rid, "_", bonica_cid),
# 				TRUE ~ bonica_rid
# 			),
# 		bonica_rid, bonica_cid, cand_id, fec_id, icpsr, icpsr2, 
# 		cpr_id = nid
# 	) %>%
# 	print

# wt(hsu_db, "node_crosswalk", dime_candDB, overwrite = TRUE)

#############################################################################################
# create cycle dyads with appropriate/relevant transactions
#############################################################################################

db1 <-
	gq(hsu_db,
		paste0(
			"select recipient_party, amount, bonica_cid, bonica_rid, seat
			from contribDB_",t , " where transaction_type in ('15', '15E', '15Z', '17R', '19', '22Y', '24C', '24E', '24F', '24H', '24K', '24Z')"
		)
	) %>% 
	as_tibble %>%
	group_by(bonica_cid, bonica_rid) %>%
	mutate(
		num_contribs = n(),
		total_contribs = as.double(sum(as.numeric(amount)))
	) %>%
	ungroup %>%
	left_join(., node_crosswalk, by = "bonica_cid") %>% print
	transmute(
		cycle = t,
		node1 =
			case_when(
				is.na(node_id) ~ as.character(bonica_cid),
				!is.na(node_id) ~ node_id
			),
		node2 = bonica_rid,
		num_contribs,
		total_contribs,
		seat,
		recipient_party
	) %>% 
	left_join(., node_crosswalk2, by = c("node2" = "bonica_rid")) %>%
	transmute(
		cycle = t,
		node1,
		node2 =
			case_when(
				is.na(node_id) ~ node2,
				!is.na(node_id) ~ node_id
			),
		seat,
		recipient_party,
		num_contribs,
		total_contribs
	) %>%
	distinct(node1, node2, .keep_all = TRUE) %>%
	print

# udirDB <-
# 	bind_rows(db1, db2) %>%
# 	group_by(node1, node2) %>% 
# 	mutate(
# 		num_contribs = n(),
# 		total_contribs = as.double(sum(as.numeric(total_contribs)))
# 	) %>% 
# 	ungroup %>%
# 	distinct(node1, node2, .keep_all = TRUE) %>%
# 	print


# udir <- gq(hsu_db, "select node1, node2 from udir_dyadDB_2004 where total_contribs >= 200")
# net <- igraph::graph_from_data_frame(select(udir, node1, node2), directed = FALSE)

# undirected_graph <- igraph::as.undirected(net,
#                                   mode = "collapse", edge.attr.comb = "sum")


check <- gq(hsu_db, "select * from contribDB_2006 where seat = 'federal:house'")

check %>% count(transaction_type)
check %>% as_tibble %>% select(contributor_name, recipient_name)




check <- gq(hsu_db, "select * from contribDB_2006 where transaction_type = '22Z'") %>% as_tibble





transaction_type in ('11', '15', '15E', '15J', '15T', '15Z', '17', '17R', '17Y', '17Z', '18G', '18J', '18K', '19', '22Y', '22Z', '24C', '24E', '24F', '24G', '24H', '24I', '24K', '24T', '24Z', '29')

#----------------------------------------
# directed
#----------------------------------------

for (t in seq(1980, 2018, 2)) {
	pos_db <-
		gq(hsu_db,
			paste0(
				"select transaction_id, recipient_party, transaction_type, amount, bonica_cid, bonica_rid, seat
				from contribDB_", t, " 
				where transaction_type in ('15', '15E', '15Z', '19', '22Y', '24C', '24E', '24F', '24H', '24K', '24Z')"
			)
		) %>%
		distinct(transaction_id, .keep_all = TRUE)
		as_tibble %>%
		) %>%
		group_by(bonica_cid, bonica_rid) %>%
		mutate(
			num_contribs = n(),
			total_contribs = as.double(sum(as.numeric(amount)))
		) %>%
		ungroup %>%
		left_join(., node_crosswalk, by = "bonica_cid") %>%
		transmute(
			cycle = t,
			node1 =
				case_when(
					is.na(node_id) ~ as.character(bonica_cid),
					!is.na(node_id) ~ node_id
				),
			node2 = bonica_rid,
			num_contribs,
			total_contribs,
			seat
		) %>%
		left_join(., node_crosswalk2, by = c("node2" = "bonica_rid")) %>%
		transmute(
			cycle = t,
			node1,
			node2 =
				case_when(
					is.na(node_id) ~ node2,
					!is.na(node_id) ~ node_id
				),
			num_contribs,
			total_contribs,
			seat
		) %>%
		distinct(node1, node2, .keep_all = TRUE)

 	wt(hsu_db, paste0("dir_dyadDB_", t), db, overwrite = TRUE)
	rm(db)
	print(t)
}

beeploud()

#############################################################################################
# create recipDB
#############################################################################################

# recipDB <-
# 	tibble(
# 		cycle = NA,
# 		node_id = NA,
# 		rec_name = NA,
# 		rec_state = NA,
# 		rec_party = NA,
# 		rec_chamber = NA,
# 		rec_type = NA,
# 		can_cfscore = NA
# 	)

# state.abb <- c(state.abb, "DC")
 

# #----------------------------------------
# # get relevant recipients from contriDB
# #----------------------------------------
# for (t in seq(1980, 2018, 2)) {

# 	recip_details <-
# 		gq(hsu_db,
# 		paste0("
# 			select
# 				cycle, bonica_rid, seat, recipient_name,
# 				recipient_party, recipient_state, recipient_type, candidate_cfscore
# 			from contribDB_", t, 
# 			" where amount >= 200")
# 		) %>%
# 		distinct(bonica_rid, .keep_all = TRUE) %>%
# 		# mutate(
# 		# 	bonica_rid =
# 		# 		case_when(
# 		# 			bonica_rid == "cand160511" ~ "cand55542",
# 		# 			bonica_rid == "cand40704" ~ "cand105312",
# 		# 			bonica_rid == "cand131160" ~ "cand103622",
# 		# 			bonica_rid == "cand104969" ~ "cand1185",
# 		# 			bonica_rid == "cand45174" ~ "cand105106",
# 		# 			bonica_rid == "cand45265" ~ "cand53006",
# 		# 			bonica_rid == "cand103442" ~ "cand41150",
# 		# 			bonica_rid == "cand100025" ~ "cand105238",
# 		# 			bonica_rid == "cand104150" ~ "cand48398",
# 		# 			bonica_rid == "cand54437" ~ "cand104582",
# 		# 			bonica_rid == "cand37239" ~ "cand42670",
# 		# 			bonica_rid == "cand55622" ~ "cand107201",
# 		# 			bonica_rid == "cand103458" ~ "cand35778",
# 		# 			bonica_rid == "cand148333" ~ "cand699",
# 		# 			bonica_rid == "cand131148" ~ "cand105451",
# 		# 			bonica_rid == "cand45263" ~ "cand104229",
# 		# 			bonica_rid == "cand134553" ~ "cand39167",
# 		# 			TRUE ~ bonica_rid
# 		# 		)
# 		# ) %>%
# 		left_join(., node_crosswalk2, by = "bonica_rid") %>% 
# 		transmute(
#   			cycle,
# 			node_id =
# 				case_when(
# 					is.na(node_id) ~ bonica_rid,
# 					!is.na(node_id) ~ node_id
# 				),
# 			rec_name = tolower(recipient_name),
# 			rec_party =
# 				case_when(
# 					recipient_party == 100 ~ "D",
# 					recipient_party == 200 ~ "R",
# 					recipient_party == 328 ~ "I"
# 				),
# 			rec_state = toupper(recipient_state),
# 			rec_state = 
# 				case_when(
# 					rec_state %in% state.abb ~ rec_state,
# 					!(rec_state %in% state.abb) ~ "Other"
# 				),
# 			rec_chamber = 
# 				case_when(
# 					seat == "federal:house" ~ "H",
# 					seat == "federal:senate" ~ "S"
# 				),
# 			rec_type = tolower(recipient_type),
# 			can_cfscore = as.numeric(candidate_cfscore)
# 		) %>%
# 		distinct(node_id, .keep_all = TRUE) %>%
# 		as_tibble

# 		recipDB <-
# 			bind_rows(recipDB, recip_details) %>%
# 			filter(!is.na(node_id))

# 		print(t)

# }

# #----------------------------------------
# # pull info from dime_candDB based on recip_details
# #----------------------------------------

# candDB <- 
# 	gq(hsu_db, "select * from dime_candDB") %>% 
# 	as_tibble %>%
# 	rename_all(tolower) %>%
# 	left_join(., node_crosswalk2, by = "bonica_rid") %>% 
# 	mutate(
# 		node_id =
# 			case_when(
# 				is.na(node_id) ~ bonica_rid,
# 				!is.na(node_id) ~ node_id
# 			)
# 	) %>%
# 	filter(
# 		seat %in% c("federal:527", "federal:committee", "federal:house", "federal:senate") &
# 		node_id %in% recipDB$node_id
# 	) %>%
# 	transmute(
# 		cycle,
# 		node_id,
# 		rec_district = gsub("-", "", .$district),
# 		rec_district = 
# 			case_when(
# 				str_detect(rec_district, "S") & !(state %in% c("KS", "MS", "SD", "SC")) ~ "Senate",
# 				str_detect(rec_district, "SCS") ~ "Senate",
# 				str_detect(rec_district, "SDS") ~ "Senate",
# 				str_detect(rec_district, "KSS") ~ "Senate",
# 				str_detect(rec_district, "MSS") ~ "Senate",
# 				TRUE ~ rec_district
# 			),
# 		rec_gender = ifelse(cand_gender %in% c("M", "F"), cand_gender, NA),
# 		rec_incum = ifelse(incum_chall == "", NA, incum_chall),
# 		rec_primary = ifelse(ran_primary == "", NA, ran_primary),
# 		rec_general = ifelse(ran_general == "", NA, ran_general),
# 		rec_num_cons = num_givers,
# 		rec_num_cons_tot = num_givers_total,
# 		rec_igcat = ifelse(igcat == "", NA, igcat),
# 		rec_commtype = ifelse(comtype == "", NA, comtype),
# 		rec_winner = winner,
# 		rec_district_presvs = district_pres_vs
# 	) %>%
# 	distinct(cycle, node_id, .keep_all = TRUE) %>%
# 	print

# new_recipDB <- 
# 	left_join(recipDB, candDB, by = c("cycle", "node_id")) %>%
# 	print

# wt(hsu_db, "recipDB", new_recipDB, overwrite = TRUE)


#############################################################################################
# create donorDB
############################################################################################

# donorDB <-
# 	tibble(
# 		cycle = NA,
# 		node_id = NA,
# 		con_name = NA,
# 		con_state = NA,
# 		con_type = NA,
# 		con_is_corp = NA,
# 		con_gender = NA,
# 		con_district_90s = NA,
# 		con_district_00s = NA,
# 		con_district_10s = NA,
# 		con_cfscore = NA
# 	)

# state.abb <- c(state.abb, "DC")

# for (t in seq(1980, 2018, 2)) {

# 	donor_details <-
# 		gq(hsu_db,
# 		paste0("
# 			select
# 				cycle, bonica_cid,
# 				contributor_name, contributor_state, contributor_type, is_corp, contributor_gender, contributor_employer, contributor_occupation, contributor_district_90s, contributor_district_00s, contributor_district_10s, contributor_cfscore
# 			from contribDB_", t)
# 		) %>%
# 		distinct(bonica_cid, .keep_all = TRUE) %>%
# 		left_join(., node_crosswalk, by = "bonica_cid") %>% 
# 		transmute(
# 			cycle,
# 			node_id = 
# 				case_when(
# 					is.na(node_id) ~ as.character(bonica_cid),
# 					!is.na(node_id) ~ node_id
# 				),
# 			con_name = contributor_name,
# 			con_state = toupper(contributor_state),
# 			con_state = 
# 				case_when(
# 					con_state %in% state.abb ~ con_state,
# 					!(con_state %in% state.abb) ~ "Other"
# 				),
# 			con_type = toupper(contributor_type),
# 			con_employer = contributor_employer,
# 			con_occupation = contributor_occupation,
# 			con_is_corp = ifelse(is_corp == "", 0, 1),
# 			con_gender = ifelse(contributor_gender %in% c("", "U"), NA, contributor_gender),
# 			con_district_90s = ifelse(contributor_district_90s == "", NA, contributor_district_90s),
# 			con_district_00s = ifelse(contributor_district_00s == "", NA, contributor_district_00s),
# 			con_district_10s = ifelse(contributor_district_10s == "", NA, contributor_district_10s),
# 			con_cfscore = as.numeric(contributor_cfscore)
# 		) %>%
# 		distinct(node_id, .keep_all = TRUE) %>%
# 		as_tibble

# 		donorDB <-
# 			bind_rows(donorDB, donor_details) %>%
# 			filter(!is.na(node_id))

# 		rm(donor_details)
# 		print(t)

# }

# wt(hsu_db, "donorDB", donorDB, overwrite = TRUE)


#############################################################################################
# add detailed node info into nodeDB
############################################################################################

#######################################
# is node donor or recipient?
#######################################

node_types <-
	tibble(
		cycle = NA,
		node_id = NA,
		is_rec = NA,
		is_con = NA,
		node_type = NA
	)

for (t in seq(1980, 2018, 2)) {
	dyad <- 
		gq(hsu_db, 
			paste0(
				"select node1, node2
				from dir_dyadDB_", t
			)
		) %>%
		as_tibble 

	cons <-
		dyad %>%
		transmute(
			node_id = node1,
			is_con = 1
		) %>%
		distinct(node_id, .keep_all = TRUE)

	recs <-
		dyad %>%
		transmute(
			node_id = node2,
			is_rec = 1
		) %>%
		distinct(node_id, .keep_all = TRUE)

	node_types_temp <-
		full_join(cons, recs, by = "node_id") %>%
		mutate(
			cycle = t,
			is_rec = ifelse(is.na(is_rec), 0, is_rec),
			is_con = ifelse(is.na(is_con), 0, is_con),
			node_type = 
				case_when(
					is_rec == 1 & is_con == 0 ~ "Rec",
					is_rec == 0 & is_con == 1 ~ "Con",
					is_rec == 1 & is_con == 1 ~ "Both"
				)
		)

	node_types <-
		bind_rows(node_types, node_types_temp) %>%
		filter(!is.na(node_id))

	print(t)

}

#######################################
# bind all node frames together
#######################################

donorDB <-
	gq(hsu_db, "select * from donorDB") %>%
	as_tibble

recipDB <-
	gq(hsu_db, "select * from recipDB") %>%
	as_tibble

nodeDB <-
	gq(hsu_db, "select * from nodeDB") %>%
	as_tibble %>%
	left_join(., donorDB, by = c("cycle", "node_id")) %>%
	left_join(., recipDB, by = c("cycle", "node_id")) %>%
	left_join(., node_types, by = c("cycle", "node_id")) %>%
	mutate(
		name = 
			case_when(
				is.na(con_name) & !is.na(rec_name) ~ rec_name,
				is.na(rec_name) & !is.na(con_name) ~ con_name,
				!is.na(rec_name) & !is.na(con_name) ~ con_name,
				is.na(rec_name) & is.na(con_name) ~ "NA"
			),
		state = 			
			case_when(
				is.na(con_state) & !is.na(rec_state) ~ rec_state,
				is.na(rec_state) & !is.na(con_state) ~ con_state,
				!is.na(rec_state) & !is.na(con_state) ~ con_state,
				is.na(rec_state) & is.na(con_state) ~ "NA"
			),
		gender =
			case_when(
				is.na(con_gender) & !is.na(rec_gender) ~ rec_gender,
				is.na(rec_gender) & !is.na(con_gender) ~ con_gender,
				!is.na(rec_gender) & !is.na(con_gender) ~ con_gender,
				is.na(rec_gender) & is.na(con_gender) ~ "NA"
			)
	) %>%
	select(cycle, node_id, component, name, node_type, name, state, gender, everything()) %>%
	print

wt(hsu_db, "nodeDB", nodeDB, overwrite = TRUE)
# rm(nodes)


#############################################################################################
# get network isolates
############################################################################################

components <-
	tibble(
		node_id = NA,
		component = NA,
		cycle = NA
	)

for (t in seq(1980, 2018, 2)) {

	dyad <-
		gq(hsu_db,
			paste0(
				"select node1, node2, total_contribs
				from dir_dyadDB_", t,
				" where total_contribs >= 200"
			)
		) %>%
		as_tibble

	net <- igraph::graph_from_data_frame(dyad, directed = FALSE)
	comp <- igraph::components(net)

	component_temp <-
		tibble(
			node_id = row.names(as.matrix(comp$membership)),
			component = as.matrix(comp$membership),
			cycle = t
		)

	components <-
		bind_rows(components, component_temp) %>%
		filter(!is.na(node_id))

	print(t)
}

wt(hsu_db, "results_nodeDB", components, overwrite = TRUE)

#############################################################################################
# who do people give to?
############################################################################################

recipDB <-
	gq(hsu_db, "select * from recipDB") %>%
	as_tibble


#######################################
# partisanship
#######################################

party_contribs <-
	tibble(
		cycle = NA,
		node_id = NA,
		dem_contribs = NA,
		rep_contribs = NA,
		other_contribs = NA,
		total_contribs = NA
	)

for (t in seq(1980, 2018, 2)) {
	
	dyads <-
		gq(hsu_db, 
			paste0("select * from dir_dyadDB_", t, " 
				where total_contribs >= 200")
		) %>%
		as_tibble

	dyad_info <-
		left_join(dyads, recipDB, by = c("cycle", "node2" = "node_id"))

	party_contribs_temp <- 
		dyad_info %>%
		mutate(
			rec_party = ifelse(rec_party %in% c("R", "D"), rec_party, "Other")
		) %>%
		group_by(node1, rec_party) %>%
		summarise(
			total_contribs = sum(total_contribs)
		) %>%
		ungroup %>%
		spread(rec_party, total_contribs) %>%
		replace(., is.na(.), 0) %>%
		transmute(
			cycle = t,
			node_id = node1,
			dem_contribs = D,
			rep_contribs = R,
			other_contribs = Other,
			total_contribs = dem_contribs + rep_contribs + other_contribs
		)

	party_contribs <-
		bind_rows(party_contribs, party_contribs_temp) %>%
		filter(!is.na(cycle))

	print(t)

}

results_nodeDB <-
	gq(hsu_db, "select * from results_nodeDB") %>%
	left_join(., party_contribs, by = c("cycle", "node_id"))

# wt(hsu_db, "results_nodeDB", results_nodeDB, overwrite = TRUE)
rm(party_contribs)
rm(results_nodeDB)

#######################################
# in/out of state
#######################################

state_contribs <-
	tibble(
		cycle = NA,
		node_id = NA,
		instate_contribs = NA,
		outstate_contribs = NA
	)

for (t in seq(1980, 2018, 2)) {
	dyads <-
		gq(hsu_db, 
			paste0("select * from dir_dyadDB_", t, " 
				where total_contribs >= 200")
		) %>%
		as_tibble

	donorDB <-
		gq(hsu_db, paste0("select cycle, node_id, con_state from donorDB where cycle = ", t)) %>%
		as_tibble

	dyad_info <-
		left_join(dyads, recipDB, by = c("cycle", "node2" = "node_id")) %>%
		left_join(., donorDB, by = c("cycle", "node1" = "node_id")) 

	state_contribs_temp <-
		dyad_info %>%
		mutate(
			same_state = 
				case_when(
					con_state == rec_state ~ "instate_contribs",
					TRUE ~ "outstate_contribs"
				)
		) %>%
		group_by(node1, same_state) %>%
		summarise(
			total_contribs = sum(total_contribs)
		) %>%
		ungroup %>%
		spread(same_state, total_contribs) %>%
		replace(., is.na(.), 0) %>%
		transmute(
			cycle = t,
			node_id = node1,
			instate_contribs,
			outstate_contribs
		)
	
	state_contribs <-
		bind_rows(state_contribs, state_contribs_temp) %>%
		filter(!is.na(cycle))

	rm(dyads)
	rm(dyad_info)
	rm(donorDB)
	print(t)
}

results_nodeDB <-
	gq(hsu_db, "select * from results_nodeDB") %>%
	left_join(., state_contribs, by = c("cycle", "node_id"))

# wt(hsu_db, "results_nodeDB", results_nodeDB, overwrite = TRUE)
rm(state_contribs)
rm(results_nodeDB)

#######################################
# number of recipients
#######################################

num_recipients <-
	tibble(
		cycle = NA,
		node_id = NA,
		dem_recipients = NA,
		rep_recipients = NA,
		other_recipients = NA,
		total_recipients = NA
	)

for (t in seq(1980, 2018, 2)) {
	dyads <-
		gq(hsu_db, 
			paste0("select * from dir_dyadDB_", t, " 
				where total_contribs >= 200")
		) %>%
		as_tibble

	dyad_info <-
		left_join(dyads, recipDB, by = c("cycle", "node2" = "node_id"))

	num_recipients_temp <- 
		dyad_info %>%
		mutate(
			rec_party = ifelse(rec_party %in% c("R", "D"), rec_party, "Other")
		) %>%
		group_by(node1, rec_party) %>%
		summarise(
			num_recipients = n()
		) %>%
		ungroup %>% 
		spread(rec_party, num_recipients) %>%
		replace(., is.na(.), 0) %>%
		transmute(
			cycle = t,
			node_id = node1,
			dem_recipients = D,
			rep_recipients = R,
			other_recipients = Other,
			total_recipients = dem_recipients + rep_recipients + other_recipients
		)

	num_recipients <-
		bind_rows(num_recipients, num_recipients_temp) %>%
		filter(!is.na(cycle))

	print(t)
}

results_nodeDB <-
	gq(hsu_db, "select * from results_nodeDB") %>%
	left_join(., num_recipients, by = c("cycle", "node_id"))

# wt(hsu_db, "results_nodeDB", results_nodeDB, overwrite = TRUE)

#############################################################################################
# attrition/retention of network
############################################################################################

#######################################
# which cycles are nodes active
#######################################

active_temp <-
	tibble(
		node_id = NA,
		cycle = NA
	)

for (t in seq(1980, 2018, 2)) {
	nodes <- 
		gq(hsu_db, 
			paste0(
				"select node_id
				from results_nodeDB 
				where cycle = ", t
			)
		) %>%
		as_tibble %>%
		mutate(
			cycle = paste0("active_", t),
			active = 1
		)

	active_temp <-
		bind_rows(active_temp, nodes) %>%
		filter(!is.na(node_id))

	rm(nodes)
	print(t)

}

node_crosswalk  %>%
	distinct(bonica_rid)

node_crosswalk  %>%
	filter(!is.na(bonica_cid)) %>%
	distinct(bonica_rid)


#######################################
# attrition and retention over time
#######################################

active_attr <-
	tibble(
		cycle = NA,
		node_id = NA,
		tplus1 = NA,
		tplus2 = NA,
		tplus3 = NA,
		tplus4 = NA,
		tplus5 = NA,
		tmin1 = NA,
		tmin2 = NA,
		tmin3 = NA,
		tmin4 = NA,
		tmin5 = NA
	)

for (t in seq(1980, 2018, 2)) {
	attr_temp <-
		active_temp %>%
		filter(cycle == paste0("active_", t)) %>%
		select(node_id) %>%
		mutate(
			cycle = t,
			tplus1 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t + 2))$node_id ~ 1,
					TRUE ~ 0
				),
			tplus2 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t + 4))$node_id ~ 1,
					TRUE ~ 0
				),
			tplus3 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t + 6))$node_id ~ 1,
					TRUE ~ 0
				),
			tplus4 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t + 8))$node_id ~ 1,
					TRUE ~ 0
				),
			tplus5 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t + 10))$node_id ~ 1,
					TRUE ~ 0
				),
			tmin1 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t - 2))$node_id ~ 1,
					TRUE ~ 0
				),
			tmin2 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t - 4))$node_id ~ 1,
					TRUE ~ 0
				),
			tmin3 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t - 6))$node_id ~ 1,
					TRUE ~ 0
				),
			tmin4 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t - 8))$node_id ~ 1,
					TRUE ~ 0
				),
			tmin5 = 
				case_when(
					node_id %in% filter(active_temp, cycle == paste0("active_", t - 10))$node_id ~ 1,
					TRUE ~ 0
				)
		) %>%
		print

	active_attr <-
		bind_rows(active_attr, attr_temp) %>%
		filter(!is.na(node_id))

	print(t)

}

wt(hsu_db, "results_network_attrition", active_attr, overwrite = TRUE)

#############################################################################################
# get partisanship of contributors based on rough labels
############################################################################################

con_party <-
	gq(hsu_db, "select cycle, node_id, name from nodeDB where node_type = 'Con'") %>%
	distinct(name, .keep_all = TRUE) %>%
	as_tibble

not_gop <-
	tibble(
		cycle = c(2012, 2012, 2016, 2014, 2012, 2016, 2012, 2014, 2014, 2014, 2014, 2014, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2016, 2016, 2016, 2012, 2018, 2016, 2016, 2012),
		node_id = c(100141929, 101811344, 101183669, 101811345, 101811343, 100108633, 101811627, 101797496, 102011287, 100673487, 108511833, 101797495, 100673491, 100222058, 100316629, 100258466, 100673490, 100031441, 100673488, 100673494, 102011284, 102011288, 105712768, 101182921, 100031640, 32979, 32979, 105712767, 101178787, 100049236)
	)

not_dem <- 
	tibble(
		cycle = c(2012, 2004, 2012, 2012, 2012, 2012, 2012, 2016, 2014, 2012, 2016, 2012, 2014, 2014, 2014, 2014, 2014, 2014, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2016, 2016, 2016, 2012, 2018, 2016, 2012, 2014),
		node_id = c(101966438, 100080129, 102169916, 100031999, 100080129, 100141929, 101811344, 101183669, 101811345, 101811343, 100108633, 101811627, 101797498, 101797496, 102011287, 100673487, 108511833, 101797495, 100673491, 100222058, 100316629, 101797497, 100258466, 100673490, 100031441, 100673488, 100673494, 102011288, 105712768, 101182921, 100031640, 32979, 32979, 105712767, 100049236, 100960903)
	)

rep_con <-
	con_party %>%
	filter(
		str_detect(name, "republican") &
		!((cycle %in% not_gop$cycle) & (node_id %in% not_gop$node_id))
	) %>%
	print

dem_con <-
	con_party %>%
	filter(
		str_detect(name, "democrat") &
		!((cycle %in%  not_dem$cycle) & (node_id %in%  not_dem$node_id))
	) %>%
	print

party_comms <-
	bind_rows(rep_con, dem_con) %>%
	wt(hsu_db, "party_comms", ., overwrite = TRUE)


#############################################################################################
# add party leader data
############################################################################################

#----------------------------------------
# party leaders to recipient attributes
#----------------------------------------

# leaders <-
# 	read_csv("/Users/jordanhsu/Google Drive/Research/Data/dime_plus/network_dime/node_data/party_leaders.csv") %>%
# 	mutate(
# 		rank_leader =
# 			case_when(
# 				position_leader == "Leader" ~ 1,
# 				position_leader == "Deputy Leader" ~ 2,
# 				position_leader == "Assistant Leader" ~ 3,
# 				position_leader == "Caucus Chair" ~ 4,
# 				position_leader == "Policy Chair" ~ 5,
# 				position_leader == "Campaign Chair" ~ 6,
# 				position_leader == "Caucus Vice-Chair" ~ 7
# 			),
# 		position_time = position_end - position_begin
# 	) %>%
# 	print

# get_ids <-
# 	leaders %>%
# 	transmute(
# 		name,
# 		check = paste0(lname, "_", party, "_", state),
# 	) %>%
# 	print

# get_ids2 <-
# 	gq(hsu_db, "select * from recipDB") %>%
# 	as_data_frame %>%
# 	transmute(
# 		bonica_rid,
# 		check = paste0(lname, "_", party, "_", state)
# 	) %>%
# 	distinct(check, .keep_all = TRUE) %>%
# 	print

# left_join(get_ids, get_ids2, by = "check") %>% write_csv("bleh.csv")

# get_ids3 <-
# 	gq(hsu_db, "select bonica_rid, lname, party, state from recipDB") %>%
# 	as_data_frame

# recips <-
# 	gq(hsu_db, "select * from recipDB") %>%
# 	as_data_frame

# leader_terms <-
# 	leaders %>%
# 	group_by(bonica_rid) %>%
# 	summarise(
# 		leader_begin = min(position_begin),
# 		leader_end = max(position_end),
# 		leader_time = leader_end - leader_begin,
# 		max_leader = max(rank_leader)
# 	) %>%
# 	print

# recips2 <-
# 	recips %>%
# 	mutate(
# 		is_leader = ifelse(bonica_rid %in% leaders$bonica_rid, 1, 0),
# 		current_leader =
# 			case_when(
# 				bonica_rid %in% leader_terms$bonica_rid &
# 					cycle %in% leader_terms$leader_begin:leader_terms$leader_end ~ 1,
# 				TRUE ~ 0
# 			),
# 		future_leader =
# 			case_when(
# 				bonica_rid %in% leader_terms$bonica_rid &
# 					cycle < leader_terms$leader_begin ~ 1,
# 					TRUE ~ 0
# 			),
# 		past_leader =
# 			case_when(
# 				bonica_rid %in% leader_terms$bonica_rid &
# 					cycle > leader_terms$leader_end ~ 1,
# 					TRUE ~ 0
# 			),
# 		highest_leader = ifelse(bonica_rid %in% leaders$bonica_rid, leader_terms$max_leader, 0),

# 	) %>%
# 	print

# wt(hsu_db, "recipDB", recips2, overwrite = TRUE)



#############################################################################################
# congress number
############################################################################################

# temp <-
# 	gq(hsu_db, "Select * from recipDB") %>%
# 	as_tibble %>%
# 	mutate(
# 		congress_num = (fecyear - 1786) / 2
# 	) %>%
# 	print

# wt(hsu_db, "recipDB", temp, overwrite = TRUE)

#############################################################################################
# formal party committees
############################################################################################

# party_comms <-
# 	read_excel("/Users/jordanhsu/Google Drive/Research/Data/dime_plus/network_dime/node_data/fec_committee_types.xlsx") %>%
# 	filter(
# 		comm_type %in% c("X", "Y", "Z") &
# 		comm_party %in% c("DEM", "REP")
# 	) %>%
# 	select(comm_name, comm_party, comm_state, connected_org, org_type) %>%
# 	distinct(comm_name, .keep_all = TRUE) %>%
# 	write_csv("/Users/jordanhsu/Google Drive/Research/Data/dime_plus/network_dime/node_data/fec_party_committees.csv") %>%
# 	print
