

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
	filter(!is.na(bonica_cid))

setwd("/Users/jordanhsu/Google Drive/Research/Dissertation")


#############################################################################################
# test out CHAMP
############################################################################################

test06 <-
		gq(hsu_db,
			paste0(
				"select bonica_rid, bonica_cid, total_contribs
				from dyadDB_2006
				where total_contribs >= 200"
			)
		) %>%
		as_tibble %>%
		filter(bonica_rid %in% comp1$node) %>%
		left_join(., donor_recip_crosswalk, by = "bonica_cid") %>%
		transmute(
			from =
				case_when(
					is.na(bonica_rid.y) ~ as.character(bonica_cid),
					!is.na(bonica_rid.y) ~ bonica_rid.y
				),
			to = bonica_rid.x,
			total_contribs
		)

g06 <- igraph::graph_from_data_frame(test06, directed = TRUE)
adj06 <- igraph::as_adj(g06, attr = "total_contribs")



library(reticulate)

pyr <- function(...) {py_run_string(...)}


pyr("import warnings")
pyr("warnings.filterwarnings('ignore')")
pyr("import os")
pyr("import numpy as np")
pyr("import champ")
pyr("import igraph as ig")
pyr("import pandas as pd")
pyr("import matplotlib.pyplot as plt")
pyr("import seaborn as sbn")
pyr("import sklearn.metrics as skm")
pyr("import matplotlib.image as mpimg")
pyr("import forceatlas2 as fa2")

madoff_file <- '/Users/jordanhsu/Google Drive/Research/Conferences/Polnet/PolNet2019/data/MADOFF2.csv'
pyr("football_file='/Users/jordanhsu/Google Drive/Research/Conferences/Polnet/PolNet2019/data/football.gml'")

pyr("madoff_adj=pd.read_csv(madoff_file, index_col = 0).fillna(0)")
pyr("madoff_node_name=madoff_adj.index")
pyr("madoff_node_name")
ls()
madoff_adj=pd.read_csv(madoff_file,index_col=0).fillna(0)
madoff_node_name=madoff_adj.index
madoff_edlist=list(zip(*np.nonzero(madoff_adj.values)))
mygraph=ig.Graph(len(madoff_node_name),madoff_edlist)
mygraph.vs['name']=madoff_node_name
