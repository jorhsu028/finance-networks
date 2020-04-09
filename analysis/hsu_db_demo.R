

source("/Users/jordanhsu/Google Drive/Programs/R/R Functions.R")
library(tidyverse)
library(magrittr)
# library(haven)
# library(purrr)
library(beepr)
library(readxl)
# library(RColorBrewer)
library(RSQLite)


#######################################
# Function definitions
# #####################################
sq <- function(...) {dbSendQuery(...)}
cr <- function(...) {dbClearResult(...)}
gq <- function(...) {dbGetQuery(...)}
wt <- function(...) {dbWriteTable(...)}
dbl <- function(...) {dbListTables(...)}



db_location <- "Desktop/hsu_networks.sqlite3"
hsu_db <- dbConnect(RSQLite::SQLite(), dbname = db_location)
node_crosswalk <- gq(hsu_db, "select * from node_crosswalk") %>% as_tibble

node_info <- gq(hsu_db, "select cycle, node_id, state, rec_type, gender, rec_party, rec_gender, rec_district, con_is_corp, con_district_90s, con_district_00s, con_district_10s from nodeDB") %>% as_tibble

node_results <- 
	gq(hsu_db, "select * from node_resultsDB") %>% 
	as_tibble %>%
	left_join(., node_info, by = c("cycle", "node")) %>%
	print

louv <- gq(hsu_db, "select * from factionDB_louvain") %>% 
	as_tibble

#############################################################################################
# example of campaign finance data
############################################################################################

gq(hsu_db, "select transaction_type, amount, contributor_name, recipient_name, recipient_type, seat from contribDB_2018 limit 3") 


#############################################################################################
# size of networks
############################################################################################

dyad_descriptives <-
	tibble(
		cycle = NA,
		total_nodes = NA,
		total_dyads = NA
	)

for (t in seq(1980, 2018, 2)) {

	dyads <-
		gq(hsu_db, 
			paste0(
				"select * 
				from dir_dyadDB_", t,
				" where seat = 'federal:house'
				and total_contribs >= 200"
			)
		) %>% 
		as_tibble %>% 
		transmute(
			from = node1,
			to = node2,
			total_contribs,
			cycle
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

#############################################################################################
# community detection
############################################################################################

factions <-
	tibble(
		node_id = NA,
		louvain = NA,
		cycle = NA
	)

for (t in seq(1980, 1980, 2)) {
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

# size of factions
louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = size)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Number of Members", title = "Distribution of Faction Size") +
		theme(axis.text.x = element_text(angle = 45))


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
		geom_point() +
		theme_jh() +
		labs(color = NULL, y = "Proportion of Factions", x = NULL, title = "Factions by Percent One-Party") +
		scale_color_brewer(palette = "Dark2")


louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = num_contribs, size = size)) +
		theme_jh() +
		labs(x = NULL, y = "Community Contributions", size = "Size of Community") +
		geom_point(shape = 21, fill = "white", alpha = .5) +
		stat_summary(stat = mean, geom = "point", color = "orange") +
		theme(axis.text.x = element_text(angle = 45))
		

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = log(num_contribs), size = size)) +
		geom_point(shape = 21, fill = "white", alpha = .5) +
		theme_jh() +
		labs(x = NULL, y = "Logged Community Transactions", size = "Size of Community") +
		stat_summary(stat = mean, geom = "point", color = "orange") +
		theme(axis.text.x = element_text(angle = 45))

