
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
library(ggmcmc)
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

node_crosswalk <- gq(hsu_db, "select * from node_crosswalk") %>% as_tibble

setwd("/Users/jordanhsu/Downloads/spsa_2020")


node_info <- gq(hsu_db, "select cycle, node_id, state, node_type, gender, rec_party, rec_gender, rec_igcat, rec_district, con_district_90s, con_district_00s, con_district_10s from nodeDB") %>% as_tibble

node_results <- 
	gq(hsu_db, "select * from node_resultsDB") %>% 
	as_tibble %>%
	left_join(., node_info, by = c("cycle", "node")) %>%
	print

louv <- gq(hsu_db, "select * from factionDB_louvain")

#############################################################################################
# num of communities
############################################################################################

louv %>%
	distinct(cycle, louvain_num) %>%
	count(cycle) %>%
	ungroup %>%
	ggplot(aes(x = cycle, y = n)) +
		geom_point() +
		geom_line() +
		labs(y = NULL, x = NULL, title = "Number of Communities") +
		theme_jh()

ggsave("figs/num_comms.pdf", height = 4)

dev.off()
#############################################################################################
# percent one-party
############################################################################################

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = size)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Number of Members", title = "Distribution of Community Size") +
		theme(axis.text.x = element_text(angle = 45))

ggsave("figs/louv_size.pdf", height = 4)

#############################################################################################
# distribution of states
############################################################################################

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = num_state)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Number of States", title = "Distribution of States in Communities") +
		theme(axis.text.x = element_text(angle = 45))

ggsave("figs/num_states.pdf", height = 4)

#############################################################################################
# size of communities
############################################################################################

louv %>%
	filter(tot_can > 1) %>%
	count(cycle, one_party_cat) %>% 
	ungroup %>%
	group_by(cycle) %>%
	mutate(
		total = sum(n),
		prop = n / total
	) %>%
	ggplot(., aes(x = cycle, y = prop, color = one_party_cat)) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, y = "Proportion of Communities", x = NULL, title = "Communities by Percent One-Party") +
		scale_color_brewer(palette = "Dark2")

ggsave("figs/per_1pp.pdf", height = 4)


#############################################################################################
# same_st
############################################################################################

''

#############################################################################################
# descriptive statistics
############################################################################################

#----------------------------------------
# number of dyads and nodes by cycle and broken out by party
#----------------------------------------

dyad_descriptives <-
	tibble(
		cycle = NA,
		total_nodes = NA,
		dem_nodes = NA,
		rep_nodes = NA,
		total_dyads = NA,
		dem_dyads = NA,
		rep_dyads = NA,
		total_contribs = NA,
		dem_contribs = NA,
		rep_contribs = NA
	)

for (t in seq(1980, 2018, 2)) {

	dyads <-
		gq(hsu_db, 
			paste0(
				"select * 
				from dyadDB_", t
			)
		) %>% 
		as_tibble %>% 
		left_join(., donor_recip_crosswalk, by = "bonica_cid") %>% 
		transmute(
			from = 
				case_when(
					is.na(bonica_rid.y) ~ as.character(bonica_cid),
					!is.na(bonica_rid.y) ~ bonica_rid.y
				),
			to = bonica_rid.x,
			num_contribs,
			total_contribs,
			cycle,
			party
		) %>% 
		filter(total_contribs >= 200)

	g <- tidygraph::as_tbl_graph(dyads)
	dem_g <- dyads %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
	rep_g <- dyads %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

# 	descriptives <-
# 		tibble(
# 			cycle = t,
# 			total_nodes = igraph::vcount(g),
# 			dem_nodes = igraph::vcount(dem_g),
# 			rep_nodes = igraph::vcount(rep_g),
# 			total_dyads = igraph::gsize(g),
# 			dem_dyads =  igraph::gsize(dem_g),
# 			rep_dyads =  igraph::gsize(rep_g),
# 			total_contribs = sum(dyads$total_contribs),
# 			dem_contribs = sum(filter(dyads, party == "D")$total_contribs),
# 			rep_contribs = sum(filter(dyads, party == "R")$total_contribs)
# 		) %>% 
# 		as_tibble 

# 	dyad_descriptives <-
# 		bind_rows(dyad_descriptives, descriptives) %>% 
# 		filter(!is.na(cycle)) %>% 
# 		print

# }

# wt(hsu_db, "results_graph_descriptives", dyad_descriptives, overwrite = TRUE)

# dyad_descriptives <-
# 	gq(hsu_db, "select * from results_graph_descriptives") %>% 
# 	as_tibble

# dyad_descriptives %>% 
# 	select(cycle:rep_nodes) %>% 
# 	gather("variable", "n", total_nodes:rep_nodes) %>% 
# 	mutate(
# 		n = n/1000,
# 		variable = 
# 			case_when(
# 				variable == "dem_nodes" ~ "Democrats",
# 				variable == "rep_nodes" ~ "Republicans",
# 				variable == "total_nodes" ~ "Overall"
# 			),
# 		variable = factor(as.factor(variable), levels = c("Overall", "Democrats", "Republicans"))
# 	) %>% 
# 	ggplot(aes(x = cycle, y = n, color = variable)) +
# 		geom_point() +
# 		geom_line() +
# 		labs(y = "Number of Actors in 1000s", x = NULL, color = NULL) +
# 		theme_jh() +
# 		geom_vline(xintercept = 1994, linetype = "dashed") +
# 		geom_vline(xintercept = 2002, linetype = "dashed") +
# 		geom_vline(xintercept = 2010, linetype = "dashed") +
# 		scale_color_manual(values=c("seagreen4", "dodgerblue2", "red2")) +
# 		theme(legend.position = "bottom")

# ggsave("figures/num_actors.pdf", height = 4)
# dev.off()

# dyad_descriptives %>% 
# 	select(cycle, total_dyads:rep_dyads) %>% 
# 	gather("variable", "n", total_dyads:rep_dyads) %>% 
# 	mutate(
# 		n = n/1000,
# 		variable = 
# 			case_when(
# 				variable == "dem_dyads" ~ "Democrats",
# 				variable == "rep_dyads" ~ "Republicans",
# 				variable == "total_dyads" ~ "Overall"
# 			),
# 		variable = factor(as.factor(variable), levels = c("Overall", "Democrats", "Republicans"))
# 	) %>% 
# 	ggplot(aes(x = cycle, y = n, color = variable)) +
# 		geom_line() +
# 		geom_point() +
# 		labs(y = "Number of Dyads in 1000s", x = NULL, color = NULL) +
# 		theme_jh() +
# 		geom_vline(xintercept = 1994, linetype = "dashed") +
# 		geom_vline(xintercept = 2002, linetype = "dashed") +
# 		geom_vline(xintercept = 2010, linetype = "dashed") +
# 		scale_color_manual(values=c("seagreen4", "dodgerblue2", "red2")) +
# 		theme(legend.position = "bottom")

# ggsave("num_dyads.pdf", height = 4)
# dev.off()

# dyad_descriptives %>% 
# 	select(cycle, total_contribs:rep_contribs) %>% 
# 	gather("variable", "n", total_contribs:rep_contribs) %>% 
# 	mutate(
# 		n = log(n),
# 		variable = 
# 			case_when(
# 				variable == "dem_contribs" ~ "Democrats",
# 				variable == "rep_contribs" ~ "Republicans",
# 				variable == "total_contribs" ~ "Overall"
# 			),
# 		variable = factor(as.factor(variable), levels = c("Overall", "Democrats", "Republicans"))
# 	) %>% 
# 	ggplot(aes(x = cycle, y = n, color = variable)) +
# 		geom_line() +
# 		geom_point() +
# 		labs(y = "Total Contributions, Logged", x = NULL, color = NULL) +
# 		theme_jh() +
# 		geom_vline(xintercept = 1994, linetype = "dashed") +
# 		geom_vline(xintercept = 2002, linetype = "dashed") +
# 		geom_vline(xintercept = 2010, linetype = "dashed") +
# 		scale_color_manual(values=c("seagreen4", "dodgerblue2", "red2")) +
# 		theme(legend.position = "bottom")

# ggsave("⁨figures/num_contribs.pdf", height = 4)
# dev.off()

#----------------------------------------
# leadership
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

# leaders %>% 	
# 	group_by(name) %>% 
# 	summarise(
# 		leader_begin = min(position_begin),
# 		leader_end = max(position_end),
# 		leader_time = leader_end - leader_begin
# 	) %$% 
# 	mean(leader_time)
# 	print

#############################################################################################
# unique nodes from each cycle
############################################################################################

# donor_recip_crosswalk <-
# 	gq(hsu_db,
# 		"select bonica_rid, bonica_cid
# 		from recipDB"
# 		) %>%
# 	as_tibble %>%
# 	distinct(bonica_rid, bonica_cid, .keep_all = TRUE) %>%
# 	filter(!is.na(bonica_cid)) %>%
# 	print

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
# 		transmute(node = from, party) 

# 	to <-
# 		dyad %>% 
# 		distinct(to, .keep_all = TRUE) %>% 
# 		transmute(node = to, party) 

# 	nodes <-
# 		bind_rows(from, to) %>% 
# 		mutate(cycle = t) %>% 
# 		distinct(node, .keep_all = TRUE) 


# 	all_nodes <-
# 		bind_rows(all_nodes, nodes) %>% 
# 		filter(!is.na(node)) %>% 
# 		print

# }

# wt(hsu_db, "nodes_by_cycle", all_nodes, overwrite = TRUE)

#############################################################################################
# calculate centrality (Test)
############################################################################################

# dyads2008 <-
# 	gq(hsu_db, 
# 		"select * 
# 		from dyadDB_2008"
# 	) %>% 
# 	as_tibble %>% 
# 	left_join(., donor_recip_crosswalk, by = "bonica_cid") %>% 
# 	transmute(
# 		from = 
# 			case_when(
# 				is.na(bonica_rid.y) ~ as.character(bonica_cid),
# 				!is.na(bonica_rid.y) ~ bonica_rid.y
# 			),
# 		to = bonica_rid.x,
# 		num_contribs,
# 		total_contribs,
# 		cycle,
# 		party
# 	) %>% 
# 	print	

# # library(tidygraph)
# # library(igraph)

# g08 <- tidygraph::as_tbl_graph(dyads08)
# dem_g2008 <- dyads2008 %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
# rep_g2008 <- dyads2008 %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

# nodes_with_centrality <-
# 	tibble(
# 		node = as_tibble(g08, what = "vertices")$name, 
# 		all_degree = igraph::degree(g08),
# 		all_betweenness = igraph::betweenness(g08),
# 		all_closeness = igraph::closeness(g08)
# 	) %>% 
# 	arrange(desc(all_betweenness)) %>% 
# 	print

# dem_nodes_with_centrality <-
# 	tibble(
# 		node = as_tibble(dem_g08, what = "vertices")$name, 
# 		dem_degree = igraph::degree(dem_g08),
# 		dem_betweenness = igraph::betweenness(dem_g08),
# 		dem_closeness = igraph::closeness(dem_g08)
# 	) %>% 
# 	arrange(desc(dem_betweenness)) %>% 
# 	print

# rep_nodes_with_centrality <-
# 	tibble(
# 		node = as_tibble(rep_g08, what = "vertices")$name, 
# 		rep_degree = igraph::degree(rep_g08),
# 		rep_betweenness = igraph::betweenness(rep_g08),
# 		rep_closeness = igraph::closeness(rep_g08)
# 	) %>% 
# 	arrange(desc(rep_betweenness)) %>% 
# 	print

# main_frame <-
# 	left_join(nodes_with_centrality, dem_nodes_with_centrality, by = "node") %>% 
# 	left_join(., rep_nodes_with_centrality, by = "node") %>% 
# 	print


#############################################################################################
# centrality by cycle
###########################################################################################

# centrality_frame <- 
# 	tibble(
# 		node = NA, 
# 		cycle = NA,
# 		party = NA,
# 		all_degree = NA,
# 		all_betweenness = NA,
# 		all_closeness = NA,
# 		dem_degree = NA,
# 		dem_betweenness = NA,
# 		dem_closeness = NA,
# 		rep_degree = NA,
# 		rep_betweenness = NA,
# 		rep_closeness = NA
# 	)

# donor_recip_crosswalk <-
# 	gq(hsu_db,
# 		"select bonica_rid, bonica_cid
# 		from recipDB"
# 		) %>%
# 	as_tibble %>%
# 	distinct(bonica_rid, bonica_cid, .keep_all = TRUE) %>%
# 	filter(!is.na(bonica_cid)) 

# for (t in seq(1980, 2014, 2))	{
	
# 	dyads <-
# 		gq(hsu_db, 
# 			paste0(
# 				"select * 
# 				from dyadDB_", t
# 			)
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
# 		filter(total_contribs >= 200)

# 	nodes <-
# 		gq(hsu_db,
# 			paste0(
# 				"select *
# 				from nodes_by_cycle
# 				where cycle = ", t
# 			)
# 		) %>% 
# 		as_tibble

# 	g <- tidygraph::as_tbl_graph(dyads)
# 	dem_g <- dyads %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
# 	rep_g <- dyads %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

# 	nodes_with_centrality <-
# 		nodes %>% 
# 		mutate(
# 			all_degree = igraph::degree(g),
# 			all_betweenness = igraph::betweenness(g),
# 			all_closeness = igraph::closeness(g)
# 		) 

# 	dem_nodes_with_centrality <-
# 		tibble(
# 			node = as_tibble(dem_g, what = "vertices")$name, 
# 			dem_degree = igraph::degree(dem_g),
# 			dem_betweenness = igraph::betweenness(dem_g),
# 			dem_closeness = igraph::closeness(dem_g)
# 		) 

# 	rep_nodes_with_centrality <-
# 		tibble(
# 			node = as_tibble(rep_g, what = "vertices")$name, 
# 			rep_degree = igraph::degree(rep_g),
# 			rep_betweenness = igraph::betweenness(rep_g),
# 			rep_closeness = igraph::closeness(rep_g)
# 		) 

# 	main_frame <-
# 		left_join(nodes_with_centrality, dem_nodes_with_centrality, by = "node") %>% 
# 		left_join(., rep_nodes_with_centrality, by = "node") %>% 
# 		mutate(cycle = t) 

# 	centrality_frame <-
# 		bind_rows(centrality_frame, main_frame) %>% 
# 		filter(!is.na(node)) %>% 
# 		print
# }

# wt(hsu_db, "results_centrality_frame", centrality_frame, overwrite = TRUE)


#############################################################################################
# summary centrality by leaders and non-leaders
############################################################################################

#----------------------------------------
# centrality scores 
#----------------------------------------

#----------------------------------------
# pelosi, boehner, mccarthy
#----------------------------------------

# leaders <-
# 	gq(hsu_db, 
# 		"select cycle, lname, bonica_rid, current_leader, is_leader, incum_chall, highest_leader
# 		from recipDB 
# 		where is_leader = 1"
# 	) %>% 
# 	as_tibble %>% 
# 	distinct(bonica_rid, .keep_all = TRUE) %>% 
# 	rename(node = bonica_rid) %>% 
# 	print

# leader_centrality <-
# 	gq(hsu_db, 
# 		paste0(
# 			"select * 
# 			from results_centrality_frame 
# 			")
# 	) %>% 
# 	as_tibble %>% 
# 	left_join(., leaders, by = "node") %>% 
# 	filter(is_leader == 1) %>% 
# 	print


# leader_centrality %>% 
# 	filter(node %in% c("cand760", "cand1225", "cand560", "cand1023", "cand545", "cand584", "cand968", "cand1552")) %>% 
# 	filter(party %in% c("D", "R")) %>% 
# 	transmute(
# 		lname = 
# 			case_when(
# 				node == "cand60" ~ "foley",
# 				node == "cand188" ~ "lott",
# 				TRUE ~ lname
# 			),
# 		cycle = cycle.x,
# 		party,
# 		degree = 
# 			case_when(
# 				party == "D" ~ dem_degree,
# 				party == "R" ~ rep_degree
# 			),
# 		betweenness = 
# 			case_when(
# 				party == "D" ~ dem_betweenness,
# 				party == "R" ~ rep_betweenness
# 			)
# 	) %>% 
# 	gather("variable", "n", degree:betweenness) %>% 
# 	ggplot(aes(x = cycle, y = n, color = lname)) +
# 		facet_grid(variable~party) +
# 		geom_point() +
# 		geom_line() +
# 		theme_jh() +
# 		labs(x = NULL, color = NULL, y = "Centrality") +
# 		theme(legend.position = "bottom")

# ggsave("today_leader_centrality.pdf")
# dev.off()

#----------------------------------------
# centrality scores of current leaders versus current backbenchers
#----------------------------------------

# incumbents <-
# 	gq(hsu_db, 
# 		"select cycle, bonica_rid, current_leader, is_leader, incum_chall
# 		from recipDB 
# 		where incum_chall = 'I'"
# 	) %>% 
# 	as_tibble %>% 
# 	distinct(bonica_rid, .keep_all = TRUE) %>% 
# 	rename(node = bonica_rid) %>% 
# 	mutate(
# 		backbench = ifelse(is_leader == 0 & incum_chall == "I", 1, 0),
# 		party_position =
# 			case_when(
# 				is_leader == 1 ~ "Leadership",
# 				backbench == 1 ~ "Backbench",
# 				is_leader == 0 & backbench == 0 ~ "Party Member"
# 			)
# 	) %>% 
# 	print

# centrality_over_time <-
# 	tibble(
# 		party = NA,
# 		mean_degree = NA,
# 		median_degree = NA,
# 		mean_betweenness = NA,
# 		median_betweenness = NA,
# 		mean_closeness = NA,
# 		median_closeness = NA
# 	)

# for (t in seq(1980, 2014, 2)) {

# 	centrality <-
# 		gq(hsu_db, 
# 			paste0(
# 				"select * 
# 				from results_centrality_frame 
# 				where cycle = ", t)
# 		) %>% 
# 		as_tibble 

# 	summary <-
# 		left_join(centrality, incumbents, by = c("node", "cycle")) %>% 
# 		filter(party != "I") %>% 
# 		transmute(
# 			node,
# 			cycle,
# 			party,
# 			party_position = ifelse(is.na(party_position), "Party Member", party_position),
# 			degree = 
# 				case_when(
# 					party == "D" ~ dem_degree,
# 					party == "R" ~ rep_degree
# 				),
# 			betweenness = 
# 				case_when(
# 					party == "D" ~ dem_betweenness,
# 					party == "R" ~ rep_betweenness
# 				),
# 			closeness = 
# 				case_when(
# 					party == "D" ~ dem_closeness,
# 					party == "R" ~ rep_closeness
# 				)
# 		) 

# 	summary2 <-
# 		summary %>% 
# 		group_by(party, party_position) %>% 
# 		summarise(
# 			mean_degree = mean(degree),
# 			median_degree = median(degree),
# 			mean_betweenness = mean(betweenness),
# 			median_betweenness = median(betweenness),
# 			mean_closeness = mean(closeness),
# 			median_closeness = median(closeness)
# 		) %>% 
# 		mutate(cycle = t)

# 	centrality_over_time <-
# 		bind_rows(centrality_over_time, summary2) %>% 
# 		filter(!is.na(party)) %>%
# 		print
# }

# centrality_over_time %>% 
# 	select(party:median_betweenness, cycle, party_position) %>%
# 	gather("variable", "n", mean_degree:median_degree) %>%
# 	mutate(
# 		variable = ifelse(variable == "mean_degree", "Mean", "Median"),
# 		party = ifelse(party == "D", "Democrat", "Republican")
# 	) %>% 
# 	ggplot(aes(x = cycle, y = n, color = party_position)) +
# 		facet_wrap(variable~party) + 
# 		geom_line() +
# 		geom_point() +
# 		geom_vline(xintercept = 1994, linetype = "dashed") +
# 		geom_vline(xintercept = 2002, linetype = "dashed") +
# 		geom_vline(xintercept = 2010, linetype = "dashed") +
# 		theme_jh() +
# 		labs(color = NULL, shape = NULL, x = NULL, y = "Mean Degree Centrality") +
# 		theme(legend.position = "bottom")

# ggsave("mean_degree_by_time.pdf", height = 5)


# centrality_over_time %>% 
# 	select(party, mean_betweenness, median_betweenness, cycle, party_position) %>%
# 	gather("variable", "n", mean_betweenness:median_betweenness) %>%
# 	mutate(
# 		variable = ifelse(variable == "mean_betweenness", "Mean", "Median"),
# 		party = ifelse(party == "D", "Democrat", "Republican")
# 	) %>% 
# 	ggplot(aes(x = cycle, y = n, color = party_position)) +
# 		facet_wrap(variable~party) + 
# 		geom_line() +
# 		geom_point() +
# 		geom_vline(xintercept = 1994, linetype = "dashed") +
# 		geom_vline(xintercept = 2002, linetype = "dashed") +
# 		geom_vline(xintercept = 2010, linetype = "dashed") +
# 		theme_jh() +
# 		labs(color = NULL, shape = NULL, x = NULL, y = "Mean Betweenness Centrality") +
# 		theme(legend.position = "bottom")

# ggsave("mean_between_by_time.pdf", height = 5)


#############################################################################################
# 100 most central actors
############################################################################################

top_dems <-
	gq(hsu_db, 
		paste0(
			"select * 
			from results_centrality_frame 
			where cycle = 2008") 
	) %>% 
	as_tibble %>% 
	top_n(100, dem_degree) %>% 
	select(node, cycle, party, dem_betweenness) %>% 
	arrange(desc(dem_betweenness)) %>% 
	print





names <-
	gq(hsu_db, 
		"select bonica_rid, lname, district, state, party bonica_rid, is_leader, incum_chall 
		from recipDB
		where cycle = 2008" 
	) %>% 
	as_tibble %>% 
	distinct(bonica_rid, .keep_all = TRUE) %>% 
	rename(node = bonica_rid) %>% 
	print

left_join(top_dems, names, by = "node") %>% as.data.frame

#############################################################################################
# calculate centrality (Test)
############################################################################################

dyads2008 <-
	gq(hsu_db, 
		"select * 
		from dyadDB_2008"
	) %>% 
	as_tibble %>% 
	left_join(., donor_recip_crosswalk, by = "bonica_cid") %>% 
	transmute(
		from = 
			case_when(
				is.na(bonica_rid.y) ~ as.character(bonica_cid),
				!is.na(bonica_rid.y) ~ bonica_rid.y
			),
		to = bonica_rid.x,
		num_contribs,
		total_contribs,
		cycle,
		party
	) %>% 
	filter(total_contribs >= 200) %>% 
	print	

# library(tidygraph)
# library(igraph)

g08 <- tidygraph::as_tbl_graph(dyads2008)
dem_g2008 <- dyads2008 %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
rep_g2008 <- dyads2008 %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

test <-
	tibble(
		node = as_tibble(dem_g2008, what = "vertices")$name,
		dem_betweenness = igraph::betweenness(dem_g2008),
		dem_betweenness2 = igraph::betweenness(dem_g2008, weights = dem_g2008$total_contribs)
 	) %>% 
 	arrange(desc(dem_betweenness)) %>% 
 	as.data.frame %>% 
 	head(40) %>% 
 	print

igraph::betweenness(dem_g2008, weights = weight)

nodes_with_centrality <-
	tibble(
		node = as_tibble(g08, what = "vertices")$name, 
		all_degree = igraph::degree(g08),
		all_betweenness = igraph::betweenness(g08),
		all_closeness = igraph::closeness(g08)
	) %>% 
	arrange(desc(all_betweenness)) %>% 
	print

dem_nodes_with_centrality <-
	tibble(
		node = as_tibble(dem_g08, what = "vertices")$name, 
		dem_degree = igraph::degree(dem_g08),
		dem_betweenness = igraph::betweenness(dem_g08),
		dem_closeness = igraph::closeness(dem_g08)
	) %>% 
	arrange(desc(dem_betweenness)) %>% 
	print

rep_nodes_with_centrality <-
	tibble(
		node = as_tibble(rep_g08, what = "vertices")$name, 
		rep_degree = igraph::degree(rep_g08),
		rep_betweenness = igraph::betweenness(rep_g08),
		rep_closeness = igraph::closeness(rep_g08)
	) %>% 
	arrange(desc(rep_betweenness)) %>% 
	print

main_frame <-
	left_join(nodes_with_centrality, dem_nodes_with_centrality, by = "node") %>% 
	left_join(., rep_nodes_with_centrality, by = "node") %>% 
	print
