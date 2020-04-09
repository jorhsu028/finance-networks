

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


setwd(db)

db_location <- "Desktop/hsu_networks.sqlite3"
hsu_db <- dbConnect(RSQLite::SQLite(), dbname = db_location)
node_crosswalk <- gq(hsu_db, "select * from node_crosswalk") %>% as_tibble


# node_info <- gq(hsu_db, "select cycle, node_id, state, rec_type, gender, rec_party, rec_gender, rec_district, con_is_corp, con_district_90s, con_district_00s, con_district_10s from nodeDB") %>% as_tibble

# node_results <- 
# 	gq(hsu_db, "select * from node_resultsDB") %>% 
# 	as_tibble %>%
# 	left_join(., node_info, by = c("cycle", "node")) %>%
# 	print

# louv <- gq(hsu_db, "select * from factionDB_louvain") %>% 
	# as_tibble


#############################################################################################
# analysis of in community contribution amounts
############################################################################################

louv %>%
	filter(tot_can > 1) %>%
	ggplot(., aes(x = as.factor(cycle), y = log(num_contribs), size = size)) +
		theme_jh() +
		labs(x = NULL, y = "Logged Community Contributions", size = "Size of Community") +
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


#############################################################################################
# consistency of nodes over time
############################################################################################

net_attr <- 
	gq(hsu_db, "select * from results_network_attrition") %>% 
	left_join(., node_info, by = c("cycle", "node_id")) %>% 
	as_tibble

net_attr %>% 
	mutate(node_type = ifelse(node_type == "Both", "Rec", node_type)) %>%
	group_by(cycle, node_type) %>%
	summarise(
		sum = n(),
		`1` = sum(tplus1) / sum,
		`2` = sum(tplus2) / sum,
		`3` = sum(tplus3) / sum,
		`4` = sum(tplus4) / sum,
		`5` = sum(tplus5) / sum
	) %>% 
	ungroup %>% 
	gather("attr", "rate", `1`:`5`) %>% 
	mutate(
		rate = ifelse(rate == 0, NA, rate),
		pres_yr =
			case_when(
				cycle %in% seq(1980, 2016, 4) ~ "Pres",
				TRUE ~ "Midterm"
			)
	) %>% 
	filter(!is.na(rate) & !is.na(node_type)) %>% 
	ggplot(aes(x = as.numeric(attr), y = rate, color = as.factor(cycle))) +
		facet_grid(~node_type) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, y = "Retention Rate", x = "Election Cycles After Year t", title = "Attrition in the Party Network") +
		theme(legend.direction = "horizontal") 

net_attr %>% 
	mutate(node_type = ifelse(node_type == "Both", "Rec", node_type)) %>%
	group_by(cycle, node_type) %>%
	summarise(
		sum = n(),
		`1` = sum(tmin1) / sum,
		`2` = sum(tmin2) / sum,
		`3` = sum(tmin3) / sum,
		`4` = sum(tmin4) / sum,
		`5` = sum(tmin5) / sum
	) %>% 
	ungroup %>% 
	gather("attr", "rate", `1`:`5`) %>%  
	mutate(rate = ifelse(rate == 0, NA, rate)) %>% 
	filter(!is.na(rate) & !is.na(node_type)) %>% 
	ggplot(aes(x = as.numeric(attr), y = rate, color = as.factor(cycle))) +
		facet_grid(~node_type) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, y = "Retention Rate", x = "Election Cycles Before Year t", title = "Attrition in the Party Network") +
		theme(legend.direction = "horizontal")

net_attr %>% 
	filter(node_type == "Con") %>%
	group_by(cycle, con_is_corp) %>%
	summarise(
		sum = n(),
		`1` = sum(tplus1) / sum,
		`2` = sum(tplus2) / sum,
		`3` = sum(tplus3) / sum,
		`4` = sum(tplus4) / sum,
		`5` = sum(tplus5) / sum
	) %>% 
	ungroup %>% 
	gather("attr", "rate", `1`:`5`) %>% 
	mutate(
		rate = ifelse(rate == 0, NA, rate),
		con_is_corp = ifelse(con_is_corp == 1, "Corp/Trade Contributor", "Other Contributor") 
	) %>%
	filter(!is.na(rate) & !is.na(con_is_corp)) %>%
	ggplot(aes(x = as.numeric(attr), y = rate, color = as.factor(cycle))) +
		facet_grid(~con_is_corp) +
		geom_line() +
		theme_jh() +
		labs(color = NULL, y = "Retention Rate", x = "Election Cycles out from Year t", title = "Attrition in the Party Network") +
		theme(legend.direction = "horizontal")


#######################################
# look at attrition by fec.ids
#######################################

active_temp <-
	tibble(
		fec_id = NA,
		cycle = NA
	)

fec_cross <-
	node_crosswalk %>%
	select(cycle, node_id, fec_id)

for (t in seq(1980, 2018, 2)) {
	nodes <- 
		gq(hsu_db, 
			paste0(
				"select node_id
				from node_resultsDB 
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

active_attr <-
	tibble(
		cycle = NA,
		node_id = NA,
		tplus1 = NA,
		tplus2 = NA,
		tplus3 = NA,
		tplus4 = NA,
		tplus5 = NA,


#############################################################################################
# check partisanship of contributors based on rough labels
############################################################################################

con_party <-
	gq(hsu_db, "select cycle, node_id, name from nodeDB where node_type = 'Con'") %>%
	distinct(name, .keep_all = TRUE) %>%
	as_tibble

fec_id <- 
	node_crosswalk %>%
	select(cycle, node_id, fec_id)

dem_con <-
	con_party %>%
	left_join(., fec_id, by = c("cycle", "node_id")) %>%
	filter(str_detect(name, "democrat")) 

rep_con <-
	con_party %>%
	left_join(., fec_id, by = c("cycle", "node_id")) %>%
	filter(str_detect(name, "republican"))  

dem_con %>% filter(!is.na(fec_id))

write_csv(dem_con, "dem_comms.csv")
write_csv(rep_con, "rep_comms.csv")

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


