  

source("/Users/jordanhsu/Google Drive/Programs/R/R Functions.R")
# library(foreign)
# library(rio)
# library(reshape2)
library(stringr)
library(tidyverse)
library(magrittr)
# library(haven)
library(purrr)
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

db_location <- "/Volumes/My Passport/dissertation data/hsu_networks.sqlite3" 

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


factions <- 
	gq(hsu_db, "select * from factionsDB") %>% 
	as_tibble


setwd("Google Drive/Research/Dissertation")



#############################################################################################
# size of networks
############################################################################################

dyad_descriptives <-
	tibble(
		cycle = NA,
		total_nodes = NA,
		total_dyads = NA
	)

for (t in seq(1980, 2014, 2)) {

	dyads <-
		gq(hsu_db, 
			paste0(
				"select * 
				from dyadDB_", t,
				" where seat = 'federal:house'
				and total_contribs >= 200"
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
			total_contribs,
			cycle,
			recipient_party
		) %>% 
		filter(total_contribs >= 200)

	g <- tidygraph::as_tbl_graph(dyads)

	descriptives <-
		tibble(
			cycle = t,
			total_nodes = igraph::vcount(g),
			total_dyads = igraph::gsize(g)
		) %>% 
		as_tibble 

	dyad_descriptives <-
		bind_rows(dyad_descriptives, descriptives) %>% 
		filter(!is.na(cycle)) %>% 
		print

}

dyad_descriptives %>% 
	gather("variable", "n", total_nodes:total_dyads) %>% 
	mutate(
		n = n/1000,
		variable = 
			case_when(
				variable == "total_dyads" ~ "Dyads",
				variable == "total_nodes" ~ "Nodes"
			)
	) %>% 
	ggplot(aes(x = cycle, y = n, shape = variable)) +
		geom_point() +
		geom_line() +
		labs(y = NULL, x = NULL, shape = NULL, title = "Number of Nodes and Contributor-Recipient Pairs (1000s)") +
		theme_jh() +
		scale_shape_manual(values=c(15,17)) 

ggsave("polnet/poster/figures/network_size.pdf", height = 5)
dev.off()

#############################################################################################
# number of communities 
############################################################################################
factions %>%
	distinct(cycle, louvain) %>% 
	count(cycle) %>%
	ungroup %>% 
	ggplot(aes(x = cycle, y = n)) + 	
		geom_point() +
		geom_line() +
		labs(y = NULL, x = NULL, title = "Number of Louvain-Detected Communities") +
		theme_jh() 

ggsave("polnet/poster/figures/num_louvain.pdf", height = 5)
dev.off()

factions %>%
	count(cycle, louvain) %>% 
	group_by(cycle) %>%
	mutate(n = n/1000) %>% 
	summarise(
		Max = max(n),
		Mean = mean(n)
	) %>% 
	gather("variable", "n",  Max:Mean) %>% 
	ggplot(aes(x = cycle, y = n, shape = variable)) +
		geom_line() +
		geom_point() +
		labs(y = NULL, x = NULL, title = "Max and Mean Size of Louvain-Detected Communities", shape = NULL) +
		theme_jh() +
		scale_shape_manual(values=c(15,17)) 
ggsave("polnet/poster/figures/louvain_stats.pdf", height = 5)
dev.off()

factions %>% count(cycle, louvain) %>% count(cycle, n) %>% filter(cycle == 2004)

# number of comms altogether
factions %>%
	distinct(cycle, louvain)

# number of comms with a candidate
factions %>%
	filter(!is.na(prop_D)) %>%
	distinct(cycle, louvain)

# number of comms by party
factions %>%
	filter(!is.na(prop_D)) %>%
	distinct(cycle, louvain, .keep_all = TRUE) %>% 
	mutate(
		one_party = case_when(
			prop_D >= 2/3 | prop_D <= 1/3 ~ "Partisan",
			TRUE ~ "Bipartisan"
		)
	) %>%
	count(cycle, one_party) %>%
	ggplot(aes(x = cycle, y = n, shape = one_party)) + 	
		geom_point() +
		geom_line() +
		labs(x = NULL, y = NULL, shape = NULL, title = "Number of Communities by Party Affiliation") +
		theme_jh() +
		scale_shape_manual(values=c(15,17)) 
ggsave("polnet/poster/figures/louvain_by_party.pdf", height = 5)
dev.off()


#############################################################################################
# communities with leaders
############################################################################################

lep <- 
	bind_rows(
		read_excel("/Users/jordanhsu/Google Drive/Research/Data/lep/LEP97_110.xlsx"), 
		read_excel("/Users/jordanhsu/Google Drive/Research/Data/lep/LEP111_113.xlsx")
	) %>% 
	filter(
		!(st_name %in% c("GU", "PR", "VI"))
	) %>% 
	separate(thomas_name, c("lname", "fname"), sep = ",") %>%
	mutate(
		cd = str_pad(cd, 2, pad = "0"),
		district = paste0(st_name, cd),
		lname = str_to_lower(lname),
		congress_num = congress,
		state = st_name
	) %>% 
	select(lname, district, congress_num, female, afam, latino, chair, subchr, state_leg, ss_bills:les) %>% 
	print

leadersDB <-
	gq(hsu_db,
		"select * 
		from recipDB
		where chamber = 'H'"
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
	rename(node = bonica_rid) %>%
	filter(!is.na(les)) %>%
	select(node, cycle, party_position, les, current_leader, is_leader) %>%
	print

faction_detail <- 
	factions %>%
	left_join(., leadersDB, by = c("node", "cycle")) %>%
	print

faction_party <-
	faction_detail %>%
	mutate(
		contains_leader =
			case_when(
				is_leader == 1 ~ 1,
				TRUE ~ 0
			)
	) %>%
	group_by(cycle, louvain) %>%
	summarise(
		contains_leader = max(contains_leader),
		num_leaders = sum(contains_leader),
		prop_D = mean(prop_D),
		les = mean(les, na.rm = TRUE),
		size = n()
	) %>%
	mutate(
		contains_leader = ifelse(contains_leader == 1, "Leadership", "Non-Leadership")
	)

faction_party %>%
	filter(!is.na(les)) %>%
	ggplot(aes(x = as.factor(contains_leader), y = les)) +
		geom_point() +
		theme_jh() +
		labs(x = "Size of Community", y = "Mean LES", title = "Mean Legislative Effectiveness Scores by Community Type")

faction_party %>%
	filter(!is.na(les)) %>%
	ggplot(aes(x = size/1000, y = les)) +
		geom_point(aes(shape = contains_leader), alpha = .55) +
		theme_jh() +
		labs(x = "Size of Community", y = "Mean LES", title = "Mean Legislative Effectiveness Scores by Community Type", shape = NULL)

ggsave("polnet/poster/figures/les_louv_size.pdf", height = 5)

dev.off()

faction_party %>%
	group_by(cycle, contains_leader) %>%
	summarise(
		mean = mean(size) / 1000
	) %>%
	ggplot(aes(x = cycle, y = mean, shape = contains_leader)) +
		geom_point() +
		geom_line() +
		theme_jh() +
		labs(shape = NULL, x = NULL, y = NULL, title = "Mean Size of Louvain Communities in 1000s")

ggsave("polnet/poster/figures/leader_louv_size.pdf", height = 5)
dev.off()

#############################################################################################
# caucus
############################################################################################

min_cauc <- 
	read_excel("/Users/jordanhsu/Google Drive/Research/Data/black-hispanic-caucus.xlsx") %>%
	rename(lname = lastName) %>%
	mutate(
		lname = tolower(lname),
		check = paste0(lname,"_",party,"_",state)
	) %>% 
	distinct(check, .keep_all = TRUE) %>%
	select(state, black_caucus, hisp_caucus, check)

get_ids <-
	gq(hsu_db, "select * from recipDB where winner = 'W'") %>% 
	as_data_frame %>% 
	transmute(
		bonica_rid,
		check = paste0(lname, "_", party, "_", state)
	) %>% 
	distinct(check, .keep_all = TRUE) %>% 
	print

get_ids2 <-
	left_join(min_cauc, get_ids, by = "check") %>%
	filter(!is.na(bonica_rid)) %>%
	rename(node = bonica_rid) %>%
	select(-state, -check)

caucus_faction <-
	left_join(factions, get_ids2, by = "node") %>%
	mutate(
		black_caucus = 
			case_when(
				black_caucus == 1 ~ 1,
				TRUE ~ 0
			),
		hisp_caucus = 
				case_when(
				hisp_caucus == 1 ~ 1,
				TRUE ~ 0
			)
	)


test <-
	caucus_faction %>% 
	mutate(
		mc_fac = 
			case_when(
				node %in% get_ids$bonica_rid ~ 1,
				TRUE ~ 0
			)
	) %>% 
	filter(mc_fac == 1) %>%
	group_by(cycle, louvain) %>%
	summarise(
		black = max(black_caucus),
		hisp = max(hisp_caucus),
		size = n()
	) %>% 
	mutate(
		minority = 
			case_when(
				black == 1 ~ "CBC",
				hisp == 1 ~ "CHC",
				black == 0 & hisp == 0 ~ "Other"
			)
	) %>%
	print

caucus_faction %>%
	group_by(cycle, louvain) %>%
	summarise(
		num_black = sum(black_caucus),
		num_hisp = sum(hisp_caucus)
	) %>%
	filter(
		num_black >= 1 | num_hisp >= 1
	) %>%
	mutate(
		status1 = 
			case_when(
				num_black >= 1 ~ "CBC",
				num_black < 1 ~ "Other"
			),
		status2 = 
			case_when(
				num_hisp >= 1 ~ "CHC",
				num_hisp < 1 ~ "Other"
			)
	) %>%

test %>%
	count(cycle, minority) %>%
	ggplot(aes(x = cycle, y = n, shape = minority)) +
		geom_point() +
		theme_jh() +
		labs(x = NULL, y = NULL, shape = NULL, title = "Number of Communities with CBC/CHC Members") 
ggsave("polnet/poster/figures/num_min_fact.pdf", height = 5)
dev.off()

test %>%
	filter(!is.na(minority)) %>%
	group_by(cycle, minority) %>% 
	summarise(size = mean(size)) %>%
	ggplot(aes(x = cycle, y = size, shape = minority)) +
		geom_point() +
		theme_jh() +
		labs(x = NULL, y = NULL, shape = NULL, title = "Mean Size of Louvain Communities of Minority Caucus Members") 

	group_by(cycle) %>%
	summarise(
		black = sum(black),
		hisp = sum(hisp),
		size = mean(size)
	) %>%
	print

ggsave("polnet/poster/figures/mc_min_cauc.pdf", height = 5)
dev.off()


#############################################################################################
# test bipartite
############################################################################################

house2008 <-
	houseDB %>% 
	filter(cycle == 2008) %>% 
	print

dyads <-
	gq(hsu_db, 
		paste0("select * 
		from dyadDB_2008")
	) %>% 
	as_tibble %>% 
	filter(total_contribs >= 200) %>% 
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
	filter(
		!(str_detect(from, "cand")) &
		to %in% house2008$bonica_rid
	) %>% 
	print

dem_g <- dyads %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
rep_g <- dyads %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

igraph::V(dem_g)$type <- igraph::bipartite_mapping(dem_g)$type
igraph::V(rep_g)$type <- igraph::bipartite_mapping(rep_g)$type

#----------------------------------------
# calculate MH matrix
#----------------------------------------
library(Matrix)
b_mat <- igraph::as_incidence_matrix(g, sparse = TRUE)
mh_mat <- t(b_mat) %*% b_mat
diag(mh_mat) <- 0

mhg <- 
	igraph::graph_from_adjacency_matrix(mh_mat, mode = "undirected", weighted = TRUE) %>% 
	tidygraph::as_tbl_graph() %>% 
	print

id_map <-
	data_frame(
		node = as_tibble(mhg),
		num = as.numeric(row.names(as_tibble(mhg)))
	) %>% 
	print

mhg_frame <-
	mhg %>% 
	tidygraph::activate(edges) %>% 
	as_tibble %>% 
	left_join(., id_map, by = c("from" = "num")) %>% 
	left_join(., id_map, by = c("to" = "num")) %>% 
	transmute(from = node.x$name, to = node.y$name, weight = weight) %>% 
	filter(weight > 0) %>% 
	print

#############################################################################################
# calculate bipartite projection graphs
###########################################################################################

#----------------------------------------
# unweighted house projection
#----------------------------------------


unweighted_house_projection <-
	tibble(
		cycle = NA,
		from = NA,
		to = NA,
		weight = NA,
		party = NA
	)

for (t in seq(1980, 2014, 2))	{

	house <-
		houseDB %>% 
		filter(cycle == t) 

	dyads <-
		gq(hsu_db, 
			paste0("select * 
			from dyadDB_", t)
		) %>% 
		as_tibble %>% 
		filter(total_contribs >= 200) %>% 
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
		filter(
			!(str_detect(from, "cand")) &
			to %in% house$bonica_rid
		) 

	dem_g <- dyads %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
	rep_g <- dyads %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

	igraph::V(dem_g)$type <- igraph::bipartite_mapping(dem_g)$type
	igraph::V(rep_g)$type <- igraph::bipartite_mapping(rep_g)$type

	dem_bmat <- igraph::as_incidence_matrix(dem_g, sparse = TRUE)
	rep_bmat <- igraph::as_incidence_matrix(rep_g, sparse = TRUE)
	
	dem_mhmat <- t(dem_bmat) %*% dem_bmat
	rep_mhmat <- t(rep_bmat) %*% rep_bmat

	diag(dem_mhmat) <- 0
	diag(rep_mhmat) <- 0

	dem_mhg <- 
		igraph::graph_from_adjacency_matrix(dem_mhmat, mode = "undirected", weighted = TRUE) %>% 
		tidygraph::as_tbl_graph() 

	rep_mhg <- 
		igraph::graph_from_adjacency_matrix(rep_mhmat, mode = "undirected", weighted = TRUE) %>% 
		tidygraph::as_tbl_graph() 

	dem_id_map <-
		data_frame(
			node = as_tibble(dem_mhg),
			num = as.numeric(row.names(as_tibble(dem_mhg)))
		) 

	rep_id_map <-
		data_frame(
			node = as_tibble(rep_mhg),
			num = as.numeric(row.names(as_tibble(rep_mhg)))
		) 

	dem_mhg_frame <-
		dem_mhg %>% 
		tidygraph::activate(edges) %>% 
		as_tibble %>% 
		left_join(., dem_id_map, by = c("from" = "num")) %>% 
		left_join(., dem_id_map, by = c("to" = "num")) %>% 
		transmute(
			cycle = t,
			from = node.x$name, 
			to = node.y$name, 
			weight = weight,
			party = "D"
		) %>% 
		filter(weight > 0) 

	rep_mhg_frame <-
		rep_mhg %>% 
		tidygraph::activate(edges) %>% 
		as_tibble %>% 
		left_join(., rep_id_map, by = c("from" = "num")) %>% 
		left_join(., rep_id_map, by = c("to" = "num")) %>% 
		transmute(
			cycle = t,
			from = node.x$name, 
			to = node.y$name, 
			weight = weight,
			party = "R"
		) %>% 
		filter(weight > 0) 

	mhg_frame <-
		bind_rows(dem_mhg_frame, rep_mhg_frame) %>% 
		print

	unweighted_house_projection <-
		bind_rows(unweighted_house_projection, mhg_frame) %>% 
		filter(!is.na(cycle)) 

}


# wt(hsu_db, "unweighted_house_projection", unweighted_house_projection, overwrite = TRUE)


#############################################################################################
# get descriptives on graphs
############################################################################################

#----------------------------------------
# unweighted
#----------------------------------------

shared_donors <-
	unweighted_house_projection %>% 
	group_by(cycle, party) %>% 
	summarise(
		mean = mean(weight),
		median = median(weight),
		first_quart = quantile(weight, 0.25),
		third_quart = quantile(weight, 0.75),
		max = max(weight),
		min = min(weight)
	) %>% 
	print

# ggplot(shared_donors, aes(x = as.factor(cycle), lower = first_quart, upper = third_quart, middle = median, ymin = min, ymax = max, color = party)) + 
# 	geom_boxplot(stat = "identity") +
# 	theme_jh() +
# 	labs(x = NULL, y = "Number of Shared Donors", color = NULL) +
# 	scale_color_manual(values=c("dodgerblue2", "red2"))

ggplot(unweighted_house_projection, aes(x = as.factor(cycle), y = weight, color = party)) + 
	geom_boxplot(outlier.shape = 1) +
	theme_jh() +
	labs(x = NULL, y = "Tie Weight", color = NULL) +
	scale_color_manual(values=c("dodgerblue2", "red2")) +
	theme(legend.position = "bottom", panel.border = element_blank())

# ggsave("apw_draft/figures/num_shared_donors.pdf", height = 5)
# dev.off()

#----------------------------------------
# calculate centrality
#----------------------------------------

centrality_frame <- 
	tibble(
		node = NA, 
		num = NA,
		cycle = NA,
		party = NA,
		degree = NA,
		between = NA,
		close = NA,
		w_degree = NA,
		w_between = NA,
		w_close = NA
	)

for (t in seq(1980, 2014, 2))	{

	dem_mhg <-
		gq(hsu_db, 
			paste0("select * 
			from unweighted_house_projection 
			where party = 'D' 
			and cycle = ", t)
		) %>% 
		tidygraph::as_tbl_graph() 

	rep_mhg <-
		gq(hsu_db, 
			paste0("select * 
			from unweighted_house_projection 
			where party = 'R' 
			and cycle = ", t)
		) %>% 
		tidygraph::as_tbl_graph() 

	dem_mhg_df <-
		dem_mhg %>% 
		tidygraph::activate(edges) %>% 
		as_tibble 

	rep_mhg_df <-
		rep_mhg %>% 
		tidygraph::activate(edges) %>% 
		as_tibble 

	dem_id_map <-
		tibble(
			node = as_tibble(dem_mhg, what = "vertices")$name, 
			num = as.numeric(row.names(as_tibble(dem_mhg))),
			party = "D",
			degree = igraph::degree(dem_mhg),
			between = igraph::betweenness(dem_mhg),
			close = igraph::closeness(dem_mhg)
		) 

	rep_id_map <-
		tibble(
			node = as_tibble(rep_mhg, what = "vertices")$name, 
			num = as.numeric(row.names(as_tibble(rep_mhg))),
			party = "R",
			degree = igraph::degree(rep_mhg),
			between = igraph::betweenness(rep_mhg),
			close = igraph::closeness(rep_mhg)
		)

 	dem_tnet <-
		rbind(
			cbind(
				i = dem_mhg_df$from,
				j = dem_mhg_df$to,
				w = dem_mhg_df$weight
			),
			cbind(
				i = dem_mhg_df$to,
				j = dem_mhg_df$from,
				w = dem_mhg_df$weight
			)
		)

 	rep_tnet <-
		rbind(
			cbind(
				i = rep_mhg_df$from,
				j = rep_mhg_df$to,
				w = rep_mhg_df$weight
			),
			cbind(
				i = rep_mhg_df$to,
				j = rep_mhg_df$from,
				w = rep_mhg_df$weight
			)
		)

	dem_tnet_stats <-
		tibble(
			num = as_tibble(tnet::degree_w(dem_tnet, measure=c("alpha"), alpha=0.5))$node,
			w_degree = as_tibble(tnet::degree_w(dem_tnet, measure=c("alpha"), alpha=0.5))$alpha,
			w_between = as_tibble(tnet::betweenness_w(dem_tnet, alpha=0.5))$betweenness,
			w_close = as_tibble(tnet::closeness_w(dem_tnet, alpha=0.5))$closeness
		) 

	rep_tnet_stats <-
		tibble(
			num = as_tibble(tnet::degree_w(rep_tnet, measure=c("alpha"), alpha=0.5))$node,
			w_degree = as_tibble(tnet::degree_w(rep_tnet, measure=c("alpha"), alpha=0.5))$alpha,
			w_between = as_tibble(tnet::betweenness_w(rep_tnet, alpha=0.5))$betweenness,
			w_close = as_tibble(tnet::closeness_w(rep_tnet, alpha=0.5))$closeness
		) 

	dem_cent <-
		left_join(dem_id_map, dem_tnet_stats, by = c("num"))

	rep_cent <-
		left_join(rep_id_map, rep_tnet_stats, by = c("num"))

	main_frame <-
		bind_rows(dem_cent, rep_cent) %>% 
		mutate(cycle = t) 

	centrality_frame <-
		bind_rows(centrality_frame, main_frame) %>% 
		filter(!is.na(node)) %>% 
		print

}

wt(hsu_db, "house_projection_cent", centrality_frame, overwrite = TRUE)

house_plot <-
	houseDB  %>% 
	select(bonica_rid, cycle, party_position) %>% 
	print

plot_cent <-
	centrality_frame %>% 
	select(node:party, degree, w_degree, between, w_between) %>% 
	gather("variable", "n", degree:w_between) %>% 
	mutate(
		weighted = 
			case_when(
				str_detect(variable, "w_") ~ "Weighted",
				!(str_detect(variable, "w_")) ~ "Unweighted",
			),
		variable2 =
			case_when(
				variable %in% c("degree", "w_degree") ~ "Degree",
				variable %in% c("between", "w_between") ~ "Betweenness",
			)
	) %>% 
	print

left_join(plot_cent, house_plot, by = c("node" = "bonica_rid", "cycle")) %>% 
	filter(variable2 == "Degree") %>% 
	mutate(
		party = ifelse(party == "D", "Democrat", "Republican")
	) %>% 
	ggplot(aes(x = cycle, y = n, color = party_position)) + 
		facet_grid(weighted ~ party, scales = "free_y") +
		stat_summary(fun.y = median, geom = "line") +
		stat_summary(fun.y = median, geom = "point") +
		theme_jh() +
		labs(x = NULL, y = "Degree Centrality", color = NULL) + 
		scale_color_manual(values=c("dodgerblue2", "red2", "seagreen4")) 

ggsave("apw_draft/figures/house_degree_cent.pdf", height = 5)		
dev.off()

left_join(plot_cent, house_plot, by = c("node" = "bonica_rid", "cycle")) %>% 
	filter(variable2 == "Betweenness") %>% 
	mutate(
		party = ifelse(party == "D", "Democrat", "Republican")
	) %>% 
	ggplot(aes(x = cycle, y = n, color = party_position)) + 
		facet_grid(weighted ~ party, scales = "free_y") +
		stat_summary(fun.y = median, geom = "line") +
		stat_summary(fun.y = median, geom = "point") +
		theme_jh() +
		labs(x = NULL, y = "Betweenness Centrality", color = NULL) + 
		scale_color_manual(values=c("dodgerblue2", "red2")) 

ggsave("apw_draft/figures/house_between_cent.pdf", height = 5)		
dev.off()

#############################################################################################
# effect on LE
############################################################################################

cent_lep <-
	left_join(centrality_frame, houseDB, by = c("node" = "bonica_rid", "cycle"))

#----------------------------------------
# legislative effectiveness
#----------------------------------------

cent_lep %>% 
	filter(!is.na(party_position)) %>% 
	mutate(
		party.x = ifelse(party.x == "D", "Democrat", "Republican")
	) %>% 
	ggplot(aes(x = w_degree, y = les, color = party_position)) +
		facet_grid(~party.x, scales = "free_x") +
		geom_point(shape = 1, alpha = .75) +
		theme_jh() +
		labs(x = "Weighted Degree Centrality", y = "Legislative Effectiveness Score", color = NULL) +
		scale_color_manual(values=c("seagreen4", "dodgerblue2", "red2")) 


ggsave("apw_draft/figures/les_by_wdeg.pdf", height = 5)
dev.off()

cent_lep %>% 
	filter(!is.na(party_position)) %>% 
	mutate(
		party.x = ifelse(party.x == "D", "Democrat", "Republican")
	) %>% 
	ggplot(aes(x = w_between, y = les, color = party_position)) +
		facet_grid(~party.x, scales = "free_x") +
		geom_point(shape = 1, alpha = .75) +
		theme_jh() +
		labs(x = "Weighted Betweenness Centrality", y = "Legislative Effectiveness Score", color = NULL) +
		scale_color_manual(values=c("seagreen4", "dodgerblue2", "red2")) 

ggsave("apw_draft/figures/les_by_wbetween.pdf", height = 5)
dev.off()


deg_mod <- lm(les ~ degree + dwnom1 + party_position + party.x, data = cent_lep)
wdeg_mod <- lm(les ~ w_degree + dwnom1 + party_position + party.x + majority, data = cent_lep)
summary(wdeg_mod)

summary(lm(les ~ w_between + dwnom1 + party_position + party.x, data = cent_lep))

xtable::xtable(wdeg_mod)
#----------------------------------------
# 
#----------------------------------------
list(Unweighted = deg_mod, Weighted = wdeg_mod)%>% 
xtable::xtable(, caption = "Unique Donor-Recipient Dyads by Cycle") %>% 
	print(include.rownames = FALSE, align = "c") 


#----------------------------------------
# predicted values
#----------------------------------------
# new_cent <-
# 	data_frame(
# 		party.x = c(rep("D", 15), rep("R", 15)),
# 		party_position = rep(c(rep("Backbench", 5), rep("Chair", 5), rep("Leadership", 5)), 2),
# 		w_degree = 	rep(c(min(cent_lep$w_degree), as.numeric(quantile(cent_lep$w_degree, .25)), median(cent_lep$w_degree), as.numeric(quantile(cent_lep$w_degree, .75)), max(cent_lep$w_degree)), 6),
# 		dwnom1 = median(cent_lep$dwnom1, na.rm = TRUE)
# 	) %>% 
# 	cbind(., predict(wdeg_mod, ., interval = "prediction")) %>% 
# 	print

# new_cent <-
# 	data_frame(
# 		party.x = c(rep("D", 5), rep("R", 5)),
# 		w_degree = 	rep(c(min(cent_lep$w_degree), as.numeric(quantile(cent_lep$w_degree, .25)), median(cent_lep$w_degree), as.numeric(quantile(cent_lep$w_degree, .75)), max(cent_lep$w_degree)), 2),
# 		dwnom1 = median(cent_lep$dwnom1, na.rm = TRUE)
# 	) %>% 
# 	cbind(., predict(wdeg_mod, ., interval = "prediction")) %>% 
# 	print

# new_center %>% 
# 	filter(party_position = "Backbench") 


# ggplot(new_cent, aes(x = w_degree, y = fit, ymax = upr, ymin = lwr, color = party.x)) +
# 	geom_line()

