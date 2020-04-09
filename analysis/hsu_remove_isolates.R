
#############################################################################################
# check isolates
############################################################################################

recips <-
	gq(hsu_db, "select bonica_rid, cycle, recipient_name, state, recipient_party from recipDB", overwrite = TRUE) %>%
	as_tibble

components <-
	gq(hsu_db, "select * from nodeDB", overwrite = TRUE) %>%
	as_tibble %>%
	left_join(., recips, by = c("node" = "bonica_rid", "cycle")) %>%
	group_by(cycle, component) %>%
	mutate(component_size = n()) %>%
	ungroup() %>%
	mutate(
		is_cand = ifelse(str_detect(node, "cand"), 1, 0)
	)

components %>%
	filter(component != 1) %>%
	group_by(cycle, component) %>%
	summarise(
		n = n()
	) %>%
	ungroup %>%
	group_by(cycle) %>%
	summarise(
		max = max(n),
		mean = mean(n)
	) %>%
	print


# percentage of nodes contained by largest size
components %>%
	group_by(cycle, component) %>%
	summarise(
		n = n()
	) %>%
	ungroup() %>%
	group_by(cycle) %>%
	mutate(
		prop = n / sum(n)
	) %>%
	filter(component == 1) %>%
	print
