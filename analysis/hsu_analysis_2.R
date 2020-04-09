  

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
# get house and LEP data
############################################################################################


lep1 <- 
	read_excel("/Users/jordanhsu/Google Drive/Research/Data/lep/LEP97_110.xlsx") %>% 
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

lep2 <- 
	read_excel("/Users/jordanhsu/Google Drive/Research/Data/lep/LEP111_113.xlsx")  %>% 
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
	select(lname, district, congress_num, female, chair, subchr, ss_bills:les) %>% 
	print

lep <-
	bind_rows(lep1, lep2)

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


#############################################################################################
# descriptives
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

for (t in seq(1980, 2014, 2)) {

	house <-
		houseDB %>% 
		filter(cycle == t)

	dyads <-
		gq(hsu_db, 
			paste0(
				"select * 
				from dyadDB_", t
			)
		) %>% 
		as_tibble %>% 
		filter(bonica_rid %in% house$bonica_rid) %>% 
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

	descriptives <-
		tibble(
			cycle = t,
			total_nodes = igraph::vcount(g),
			dem_nodes = igraph::vcount(dem_g),
			rep_nodes = igraph::vcount(rep_g),
			total_dyads = igraph::gsize(g),
			dem_dyads =  igraph::gsize(dem_g),
			rep_dyads =  igraph::gsize(rep_g),
			total_contribs = sum(dyads$total_contribs),
			dem_contribs = sum(filter(dyads, party == "D")$total_contribs),
			rep_contribs = sum(filter(dyads, party == "R")$total_contribs)
		) %>% 
		as_tibble 

	dyad_descriptives <-
		bind_rows(dyad_descriptives, descriptives) %>% 
		filter(!is.na(cycle)) %>% 
		print

}

dyad_descriptives %>% 
	select(cycle:rep_nodes) %>% 
	gather("variable", "n", total_nodes:rep_nodes) %>% 
	mutate(
		n = n/1000,
		variable = 
			case_when(
				variable == "dem_nodes" ~ "Democrats",
				variable == "rep_nodes" ~ "Republicans",
				variable == "total_nodes" ~ "Overall"
			),
		variable = factor(as.factor(variable), levels = c("Overall", "Democrats", "Republicans"))
	) %>% 
	filter(variable != "Overall") %>% 
	ggplot(aes(x = cycle, y = n, color = variable)) +
		geom_rect(aes(xmin = 2007, xmax = 2011), ymin = -Inf, ymax = Inf, fill = "dodgerblue2", alpha = .01, color = NA) +
		geom_rect(aes(xmin = 1980, xmax = 1995), ymin = -Inf, ymax = Inf, fill = "dodgerblue2", alpha = .01, color = NA) +
		geom_rect(aes(xmin = 1995, xmax = 2007), ymin = -Inf, ymax = Inf, fill = "red2", alpha = .01, color = NA) +
				geom_rect(aes(xmin = 2011, xmax = 2014), ymin = -Inf, ymax = Inf, fill = "red2", alpha = .01, color = NA) +
		geom_point() +
		geom_line() +
		labs(y = "Number of Actors in 1000s", x = NULL, color = NULL) +
		theme_jh() +
		scale_color_manual(values=c("dodgerblue2", "red2")) +
		theme(legend.position = "bottom", panel.border = element_blank(), axis.ticks = ggplot2::element_blank())

ggsave("apw_draft/figures/num_house_actors.pdf", height = 4)
dev.off()

dyad_descriptives %>% 
	select(cycle, total_dyads:rep_dyads) %>% 
	gather("variable", "n", total_dyads:rep_dyads) %>% 
	mutate(
		n = n/1000,
		variable = 
			case_when(
				variable == "dem_dyads" ~ "Democrats",
				variable == "rep_dyads" ~ "Republicans",
				variable == "total_dyads" ~ "Overall"
			),
		variable = factor(as.factor(variable), levels = c("Overall", "Democrats", "Republicans"))
	) %>% 
	filter(variable != "Overall") %>% 
	ggplot(aes(x = cycle, y = n, color = variable)) +
		geom_rect(aes(xmin = 2007, xmax = 2011), ymin = -Inf, ymax = Inf, fill = "dodgerblue2", alpha = .01, color = NA) +
		geom_rect(aes(xmin = 1980, xmax = 1995), ymin = -Inf, ymax = Inf, fill = "dodgerblue2", alpha = .01, color = NA) +
		geom_rect(aes(xmin = 1995, xmax = 2007), ymin = -Inf, ymax = Inf, fill = "red2", alpha = .01, color = NA) +
				geom_rect(aes(xmin = 2011, xmax = 2014), ymin = -Inf, ymax = Inf, fill = "red2", alpha = .01, color = NA) +
		geom_line() +
		geom_point() +
		labs(y = "Number of Dyads in 1000s", x = NULL, color = NULL) +
		theme_jh() +
		scale_color_manual(values=c("dodgerblue2", "red2")) +
		theme(legend.position = "bottom", panel.border = element_blank(), axis.ticks = ggplot2::element_blank())

ggsave("apw_draft/figures/num_house_dyads.pdf", height = 4)
dev.off()

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

# ggsave("apw_draft/figures/num_house_contribs.pdf", height = 4)
# dev.off()

#############################################################################################
# centrality by cycle
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

# centrality_frame <- 
# 	tibble(
# 		node = NA, 
# 		cycle = NA,
# 		party = NA,
# 		dem_degree = NA,
# 		dem_betweenness = NA,
# 		dem_closeness = NA,
# 		rep_degree = NA,
# 		rep_betweenness = NA,
# 		rep_closeness = NA
# 	)

# for (t in seq(1980, 2014, 2))	{

# 	dyad <-
# 		gq(hsu_db, 
# 			paste0("select * 
# 			from dyadDB_", t)
# 		) %>% 
# 		as_tibble %>% 
# 		filter(bonica_rid %in% houseDB$bonica_rid) %>% 
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

# 	g <- tidygraph::as_tbl_graph(dyads)
# 	dem_g <- dyads %>% filter(party == "D") %>% tidygraph::as_tbl_graph(.)
# 	rep_g <- dyads %>% filter(party == "R") %>% tidygraph::as_tbl_graph(.)

# 	nodes_with_centrality <-
# 		tibble(
# 			node = as_tibble(g, what = "vertices")$name,
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

# wt(hsu_db, "results_house_centrality", centrality_frame, overwrite = TRUE)


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

