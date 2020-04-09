

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


node_info <- gq(hsu_db, "select cycle, node, state, node_type, gender, rec_party, rec_gender, rec_igcat, rec_district, con_is_corp, con_district_90s, con_district_00s, con_district_10s from nodeDB") %>% as_tibble

# node_results <- 
# 	gq(hsu_db, "select * from node_resultsDB") %>% 
# 	as_tibble %>%
# 	left_join(., node_info, by = c("cycle", "node")) %>%
# 	print

#############################################################################################
# check partisanship of contributors based on rough labels
############################################################################################

# con_party <-
# 	gq(hsu_db, "select cycle, node, name from nodeDB where node_type = 'Con'") %>%
# 	distinct(name, .keep_all = TRUE) %>%
# 	as_tibble

# dem_con <-
# 	con_party %>%
# 	filter(str_detect(name, "democrat")) 

# rep_con <-
# 	con_party %>%
# 	filter(str_detect(name, "republican")) 

# write_csv(dem_con, "dem_comms.csv")


#############################################################################################
# consistency of nodes over time
############################################################################################

# active_temp <-
# 	tibble(
# 		node = NA,
# 		cycle = NA
# 	)

# for (t in seq(1980, 2014, 2)) {
# 	nodes <- 
# 		gq(hsu_db, 
# 			paste0(
# 				"select node
# 				from node_resultsDB 
# 				where cycle = ", t
# 			)
# 		) %>%
# 		as_tibble %>%
# 		mutate(
# 			cycle = paste0("active_", t),
# 			active = 1
# 		)

# 	active_temp <-
# 		bind_rows(active_temp, nodes) %>%
# 		filter(!is.na(node))

# 	rm(nodes)
# 	print(t)

# }

# active_attr <-
# 	tibble(
# 		t = NA,
# 		node = NA,
# 		t_1 = NA,
# 		t_2 = NA,
# 		t_3 = NA,
# 		t_4 = NA,
# 		t_5 = NA
# 	)

# for (t in seq(1980, 2014, 2)) {
# 	attr_temp <-
# 		active_temp %>%
# 		filter(cycle == paste0("active_", t)) %>%
# 		select(node) %>%
# 		mutate(
# 			t = t,
# 			t_1 = 
# 				case_when(
# 					node %in% filter(active_temp, cycle == paste0("active_", t + 2))$node ~ 1,
# 					TRUE ~ 0
# 				),
# 			t_2 = 
# 				case_when(
# 					node %in% filter(active_temp, cycle == paste0("active_", t + 4))$node ~ 1,
# 					TRUE ~ 0
# 				),
# 			t_3 = 
# 				case_when(
# 					node %in% filter(active_temp, cycle == paste0("active_", t + 6))$node ~ 1,
# 					TRUE ~ 0
# 				),
# 			t_4 = 
# 				case_when(
# 					node %in% filter(active_temp, cycle == paste0("active_", t + 8))$node ~ 1,
# 					TRUE ~ 0
# 				),
# 			t_5 = 
# 				case_when(
# 					node %in% filter(active_temp, cycle == paste0("active_", t + 10))$node ~ 1,
# 					TRUE ~ 0
# 				)
# 		) %>%
# 		print

# 	active_attr <-
# 		bind_rows(active_attr, attr_temp) %>%
# 		filter(!is.na(node))

# 	print(t)

# }

# wt(hsu_db, "results_network_attrition", active_attr, overwrite = TRUE)

active_attr <- gq(hsu_db, "select * from results_network_attrition") %>% as_tibble

active_attr %>% 
	group_by(t) %>%
	summarise(
		sum = n(),
		`1` = sum(t_1) / sum,
		`2` = sum(t_2) / sum,
		`3` = sum(t_3) / sum,
		`4` = sum(t_4) / sum,
		`5` = sum(t_5) / sum
	) %>% 
	ungroup %>% 
	gather("attr", "rate", `1`:`5`) %>% 
	mutate(rate = ifelse(rate == 0, NA, rate)) %>%
	filter(!is.na(rate)) %>%
	ggplot(aes(x = as.numeric(attr), y = rate, color = as.factor(t))) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, y = "Retention Rate", x = "Election Cycles out from Year t", title = "Attrition in the Party Network") +
		theme(legend.direction = "horizontal")


#############################################################################################
# analyze factions
############################################################################################

louv <- gq(hsu_db, "select  * from factionDB_louvain") %>% as_tibble

#######################################
# factions that only have 1 candidate?
#######################################
louv %>% filter(tot_can == 1) %>% count(cycle, tot_can) 

louv %$% sum(tot_can)

# size of overall faction
louv %>% filter(tot_can == 1) %$% summary(size)
louv %>% filter(tot_can > 1) %$% summary(size)

# number of states?
louv %>% filter(tot_can == 1) %$% summary(num_state)
louv %>% filter(tot_can > 1) %$% summary(num_state)

louv %>% filter(tot_can == 1) %>% count(num_R)

#######################################
# size of factions
#######################################
louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = size)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Number of Members", title = "Distribution of Faction Size") +
		theme(axis.text.x = element_text(angle = 45))

louv %>%
	filter(tot_can > 1) %>%
	group_by(cycle) %>%
	mutate(tot_network = sum(size)) %>%
	ungroup %>%
	mutate(prop_network = size / tot_network) %>%
	select(cycle, size, prop_network) %>% 
	ggplot(aes(x = as.factor(cycle), y = prop_network)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Proportion of the Total Network", title = "Distribution of Faction Size")

#######################################
# percent democratic
#######################################

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = prop_D)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Percent Democratic", title = "Distribution of Factions by Percent Democratic") +
		theme(axis.text.x = element_text(angle = 45))

# percent one party
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
		labs(color = NULL, y = "Proportion of Factions", x = NULL, title = "Factions by Percent One-Party") +
		scale_color_brewer(palette = "Dark2")

#######################################
# same state
#######################################

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = num_state)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Number of States", title = "Distribution of States in Factions") +
		theme(axis.text.x = element_text(angle = 45))

louv %>%
	filter(tot_can > 1) %>%
	count(cycle, same_state_cat) %>% 
	ungroup %>%
	group_by(cycle) %>%
	mutate(
		total = sum(n),
		prop = n / total
	) %>%
	ggplot(., aes(x = cycle, y = prop, color = same_state_cat)) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, x = NULL, y = "Proportion of Factions", title = "Factions by Percentage of Same-State Members") +
		scale_color_brewer(palette = "Dark2")

#######################################
# gender
#######################################

louv %>%
	ggplot(., aes(x = as.factor(cycle), y = prop_women)) +
		geom_boxplot() +
		theme_jh() +
		labs(x = NULL, y = "Percent Women", title = "Distribution of Gender in Factions") +
		theme(axis.text.x = element_text(angle = 45))

louv %>%
	filter(!is.na(num_women) & tot_can > 1) %>%
	group_by(cycle) %>%
	mutate(
		tot_corp = sum(num_women)
	) %>%
	ungroup %>%
	mutate(
		prop_corp = num_women / tot_corp
	) %>%
	select(cycle, size, num_women, prop_corp) %>%
	ggplot(aes(x = as.factor(cycle), y = prop_corp)) +
		geom_boxplot() +
		theme_jh()


#######################################
# PACs
#######################################

louv %>%
	filter(!is.na(num_is_corp) & tot_can > 1) %>%
	group_by(cycle) %>%
	mutate(
		tot_corp = sum(num_is_corp)
	) %>%
	ungroup %>%
	mutate(
		prop_corp = num_is_corp / tot_corp
	) %>%
	select(cycle, size, num_is_corp, prop_corp) %>%
	group_by(cycle) %>%
	filter(prop_corp == max(prop_corp)) %>%
	ggplot(aes(x = as.factor(cycle), y = prop_corp)) +
		geom_point() +
		theme_jh()

louv %>%
	filter(!is.na(num_is_corp) & tot_can > 1) %>%
	group_by(cycle) %>%
	mutate(
		tot_corp = sum(num_is_corp)
	) %>%
	ungroup %>%
	mutate(
		prop_corp = num_is_corp / tot_corp
	) %>%
	select(cycle, size, num_is_corp, prop_corp) %>%
	ggplot(aes(x = as.factor(cycle), y = prop_corp)) +
		geom_boxplot() +
		theme_jh()


