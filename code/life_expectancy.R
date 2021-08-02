# ==============================================================================
# Import
# ==============================================================================

library(tidyverse)
library(lubridate)
source('code/utils.R')
source('code/excess_deaths.R')

# Overall life tables: 
lt_2014 <- parse_life_table(read_csv("data/lt_2014.csv", skip=1))
lt_2015 <- parse_life_table(read_csv("data/lt_2015.csv", skip=1))
lt_2016 <- parse_life_table(read_csv("data/lt_2016.csv", skip=1))
lt_2017 <- parse_life_table(read_csv("data/lt_2017.csv", skip=1))
lt_2018 <- parse_life_table(read_csv("data/lt_2018.csv", skip=1))

# Life tables for Hispanic:
lt_2014_h <- parse_life_table(read_csv("data/lt_2014_hispanic.csv", skip=1))
lt_2015_h <- parse_life_table(read_csv("data/lt_2015_hispanic.csv", skip=1))
lt_2016_h <- parse_life_table(read_csv("data/lt_2016_hispanic.csv", skip=1))
lt_2017_h <- parse_life_table(read_csv("data/lt_2017_hispanic.csv", skip=1))
lt_2018_h <- parse_life_table(read_csv("data/lt_2018_hispanic.csv", skip=1))

# Life tables for non-Hispanic Black:
lt_2014_nhb <- parse_life_table(read_csv("data/lt_2014_nonhispanicblack.csv", skip=1))
lt_2015_nhb <- parse_life_table(read_csv("data/lt_2015_nonhispanicblack.csv", skip=1))
lt_2016_nhb <- parse_life_table(read_csv("data/lt_2016_nonhispanicblack.csv", skip=1))
lt_2017_nhb <- parse_life_table(read_csv("data/lt_2017_nonhispanicblack.csv", skip=1))
lt_2018_nhb <- parse_life_table(read_csv("data/lt_2018_nonhispanicblack.csv", skip=1))

# Life tables for non-Hispanic White:
lt_2014_nhw <- parse_life_table(read_csv("data/lt_2014_nonhispanicwhite.csv", skip=1))
lt_2015_nhw <- parse_life_table(read_csv("data/lt_2015_nonhispanicwhite.csv", skip=1))
lt_2016_nhw <- parse_life_table(read_csv("data/lt_2016_nonhispanicwhite.csv", skip=1))
lt_2017_nhw <- parse_life_table(read_csv("data/lt_2017_nonhispanicwhite.csv", skip=1))
lt_2018_nhw <- parse_life_table(read_csv("data/lt_2018_nonhispanicwhite.csv", skip=1))

# Calculate overall life expectancy, 2014-2018:
le_2014 <- get_le_from_pdeath(lt_2014$PDeath)
le_2015 <- get_le_from_pdeath(lt_2015$PDeath)
le_2016 <- get_le_from_pdeath(lt_2016$PDeath)
le_2017 <- get_le_from_pdeath(lt_2017$PDeath)
le_2018 <- get_le_from_pdeath(lt_2018$PDeath)

# Calculate Hispanic life expectancy, 2014-2018:
le_2014_h <- get_le_from_pdeath(lt_2014_h$PDeath)
le_2015_h <- get_le_from_pdeath(lt_2015_h$PDeath)
le_2016_h <- get_le_from_pdeath(lt_2016_h$PDeath)
le_2017_h <- get_le_from_pdeath(lt_2017_h$PDeath)
le_2018_h <- get_le_from_pdeath(lt_2018_h$PDeath)

# Calculate non-Hispanic Black life expectancy, 2014-2018:
le_2014_nhb <- get_le_from_pdeath(lt_2014_nhb$PDeath)
le_2015_nhb <- get_le_from_pdeath(lt_2015_nhb$PDeath)
le_2016_nhb <- get_le_from_pdeath(lt_2016_nhb$PDeath)
le_2017_nhb <- get_le_from_pdeath(lt_2017_nhb$PDeath)
le_2018_nhb <- get_le_from_pdeath(lt_2018_nhb$PDeath)

# Calculate non-Hispanic White life expectancy, 2014-2018:
le_2014_nhw <- get_le_from_pdeath(lt_2014_nhw$PDeath)
le_2015_nhw <- get_le_from_pdeath(lt_2015_nhw$PDeath)
le_2016_nhw <- get_le_from_pdeath(lt_2016_nhw$PDeath)
le_2017_nhw <- get_le_from_pdeath(lt_2017_nhw$PDeath)
le_2018_nhw <- get_le_from_pdeath(lt_2018_nhw$PDeath)


le_diff_df <- tibble(le_calc=c(
	le_2014,
	le_2015,
	le_2016,
	le_2017,
	le_2018),
	le_actual=c(
	lt_2014$LifeExpectancy[1],
	lt_2015$LifeExpectancy[1],
	lt_2016$LifeExpectancy[1],
	lt_2017$LifeExpectancy[1],
	lt_2018$LifeExpectancy[1])
	) %>% 
	mutate(diff=le_calc-le_actual)

le_diff_df_h <- tibble(le_calc=c(
	le_2014_h,
	le_2015_h,
	le_2016_h,
	le_2017_h,
	le_2018_h),
	le_actual=c(
	lt_2014_h$LifeExpectancy[1],
	lt_2015_h$LifeExpectancy[1],
	lt_2016_h$LifeExpectancy[1],
	lt_2017_h$LifeExpectancy[1],
	lt_2018_h$LifeExpectancy[1])
	) %>% 
	mutate(diff=le_calc-le_actual)

le_diff_df_nhb <- tibble(le_calc=c(
	le_2014_nhb,
	le_2015_nhb,
	le_2016_nhb,
	le_2017_nhb,
	le_2018_nhb),
	le_actual=c(
	lt_2014_nhb$LifeExpectancy[1],
	lt_2015_nhb$LifeExpectancy[1],
	lt_2016_nhb$LifeExpectancy[1],
	lt_2017_nhb$LifeExpectancy[1],
	lt_2018_nhb$LifeExpectancy[1])
	) %>% 
	mutate(diff=le_calc-le_actual)

le_diff_df_nhw <- tibble(le_calc=c(
	le_2014_nhw,
	le_2015_nhw,
	le_2016_nhw,
	le_2017_nhw,
	le_2018_nhw),
	le_actual=c(
	lt_2014_nhw$LifeExpectancy[1],
	lt_2015_nhw$LifeExpectancy[1],
	lt_2016_nhw$LifeExpectancy[1],
	lt_2017_nhw$LifeExpectancy[1],
	lt_2018_nhw$LifeExpectancy[1])
	) %>% 
	mutate(diff=le_calc-le_actual)

# ==============================================================================
# Simulate: All race/ethnicity groups
# ==============================================================================
xi <- excess_rate_df %>% 
	filter(RaceEthnicity=="All Race/Ethnicity Groups") %>% 
	arrange(age) %>% 
	pull(xi)

le_sim_1 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018$PDeath), xi, 1)
le_sim_2 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018$PDeath), xi, 2)
le_sim_10 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018$PDeath), xi, 10)

le_sim_all <- rename(le_sim_1,le_1=le) %>% 
	left_join(rename(le_sim_2,le_2=le), by="t") %>% 
	left_join(rename(le_sim_10,le_10=le), by="t") %>% 
	mutate(t=t+2020) %>% 
	bind_rows(tibble(
		t=2014:2018, 
		le_1=c(le_2014,le_2015,le_2016,le_2017,le_2018),
		le_2=c(le_2014,le_2015,le_2016,le_2017,le_2018),
		le_10=c(le_2014,le_2015,le_2016,le_2017,le_2018)
		)) %>% 
	arrange(t) %>% 
	pivot_longer(-t) %>% 
	rename(le=value) %>% 
	rename(fold_decay=name) %>% 
	mutate(fold_decay=substr(fold_decay,4,1000)) %>% 
	mutate(fold_decay=as.numeric(fold_decay)) %>% 
	mutate(is_sim=case_when(t<=2018~"0",TRUE~"1"))
	
fig_overall <- ggplot(le_sim_all, aes(x=t, y=le, col=factor(fold_decay))) +
	geom_line(stat="smooth", method="loess", span=0.2, alpha=0.6) + 
	geom_point(aes(shape=is_sim), alpha=0.6) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90)) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	labs(title="All race/ethnicity groups", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom")

le_sim_all_o <- le_sim_all %>% 
	mutate(RaceEthnicity="Overall")

# ==============================================================================
# Simulate: Hispanic
# ==============================================================================
xi <- excess_rate_df %>% 
	filter(RaceEthnicity=="Hispanic") %>% 
	arrange(age) %>% 
	pull(xi)

le_sim_1 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_h$PDeath), xi, 1)
le_sim_2 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_h$PDeath), xi, 2)
le_sim_10 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_h$PDeath), xi, 10)

le_sim_all <- rename(le_sim_1,le_1=le) %>% 
	left_join(rename(le_sim_2,le_2=le), by="t") %>% 
	left_join(rename(le_sim_10,le_10=le), by="t") %>% 
	mutate(t=t+2020) %>% 
	bind_rows(tibble(
		t=2014:2018, 
		le_1=c(le_2014_h,le_2015_h,le_2016_h,le_2017_h,le_2018_h),
		le_2=c(le_2014_h,le_2015_h,le_2016_h,le_2017_h,le_2018_h),
		le_10=c(le_2014_h,le_2015_h,le_2016_h,le_2017_h,le_2018_h)
		)) %>% 
	arrange(t) %>% 
	pivot_longer(-t) %>% 
	rename(le=value) %>% 
	rename(fold_decay=name) %>% 
	mutate(fold_decay=substr(fold_decay,4,1000)) %>% 
	mutate(fold_decay=as.numeric(fold_decay)) %>% 
	mutate(is_sim=case_when(t<=2018~"0",TRUE~"1"))
	
fig_hispanic <- ggplot(le_sim_all, aes(x=t, y=le, col=factor(fold_decay))) +
	geom_line(stat="smooth", method="loess", span=0.2, alpha=0.6) + 
	geom_point(aes(shape=is_sim), alpha=0.6) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90)) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	labs(title="Hispanic", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom")

le_sim_all_h <- le_sim_all %>% 
	mutate(RaceEthnicity="Hispanic")

# ==============================================================================
# Simulate: Non-Hispanic Black
# ==============================================================================
xi <- excess_rate_df %>% 
	filter(RaceEthnicity=="Non-Hispanic Black") %>% 
	arrange(age) %>% 
	pull(xi)

le_sim_1 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhb$PDeath), xi, 1)
le_sim_2 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhb$PDeath), xi, 2)
le_sim_10 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhb$PDeath), xi, 10)

le_sim_all <- rename(le_sim_1,le_1=le) %>% 
	left_join(rename(le_sim_2,le_2=le), by="t") %>% 
	left_join(rename(le_sim_10,le_10=le), by="t") %>% 
	mutate(t=t+2020) %>% 
	bind_rows(tibble(
		t=2014:2018, 
		le_1=c(le_2014_nhb,le_2015_nhb,le_2016_nhb,le_2017_nhb,le_2018_nhb),
		le_2=c(le_2014_nhb,le_2015_nhb,le_2016_nhb,le_2017_nhb,le_2018_nhb),
		le_10=c(le_2014_nhb,le_2015_nhb,le_2016_nhb,le_2017_nhb,le_2018_nhb)
		)) %>% 
	arrange(t) %>% 
	pivot_longer(-t) %>% 
	rename(le=value) %>% 
	rename(fold_decay=name) %>% 
	mutate(fold_decay=substr(fold_decay,4,1000)) %>% 
	mutate(fold_decay=as.numeric(fold_decay)) %>% 
	mutate(is_sim=case_when(t<=2018~"0",TRUE~"1"))
	
fig_nonhispanicblack <- ggplot(le_sim_all, aes(x=t, y=le, col=factor(fold_decay))) +
	geom_line(stat="smooth", method="loess", span=0.2, alpha=0.6) + 
	geom_point(aes(shape=is_sim), alpha=0.6) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90)) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	labs(title="Non-Hispanic Black", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom")

le_sim_all_nhb <- le_sim_all %>% 
	mutate(RaceEthnicity="Non-Hispanic Black")

# ==============================================================================
# Simulate: Non-Hispanic White
# ==============================================================================
xi <- excess_rate_df %>% 
	filter(RaceEthnicity=="Non-Hispanic White") %>% 
	arrange(age) %>% 
	pull(xi)

le_sim_1 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhw$PDeath), xi, 1)
le_sim_2 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhw$PDeath), xi, 2)
le_sim_10 <- sim_covid(-1:10, calc_pdeath_inst(lt_2018_nhw$PDeath), xi, 10)

le_sim_all <- rename(le_sim_1,le_1=le) %>% 
	left_join(rename(le_sim_2,le_2=le), by="t") %>% 
	left_join(rename(le_sim_10,le_10=le), by="t") %>% 
	mutate(t=t+2020) %>% 
	bind_rows(tibble(
		t=2014:2018, 
		le_1=c(le_2014_nhw,le_2015_nhw,le_2016_nhw,le_2017_nhw,le_2018_nhw),
		le_2=c(le_2014_nhw,le_2015_nhw,le_2016_nhw,le_2017_nhw,le_2018_nhw),
		le_10=c(le_2014_nhw,le_2015_nhw,le_2016_nhw,le_2017_nhw,le_2018_nhw)
		)) %>% 
	arrange(t) %>% 
	pivot_longer(-t) %>% 
	rename(le=value) %>% 
	rename(fold_decay=name) %>% 
	mutate(fold_decay=substr(fold_decay,4,1000)) %>% 
	mutate(fold_decay=as.numeric(fold_decay)) %>% 
	mutate(is_sim=case_when(t<=2018~"0",TRUE~"1"))
	
fig_nonhispanicwhite <- ggplot(le_sim_all, aes(x=t, y=le, col=factor(fold_decay))) +
	geom_line(stat="smooth", method="loess", span=0.2, alpha=0.6) + 
	geom_point(aes(shape=is_sim), alpha=0.6) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90)) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	labs(title="Non-Hispanic White", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom")

le_sim_all_nhw <- le_sim_all %>% 
	mutate(RaceEthnicity="Non-Hispanic White")

# ==============================================================================
# Plotting
# ==============================================================================

le_sim_all_re <- rbind(le_sim_all_o, le_sim_all_h, le_sim_all_nhb, le_sim_all_nhw)

highalpha <- 0.6
lowalpha <- 0.1

fig_overlay_overall <- ggplot(le_sim_all_re, aes(x=t, y=le, col=factor(fold_decay), alpha=RaceEthnicity)) +
	geom_line(stat="smooth", method="loess", span=0.2) + 
	geom_point(aes(shape=is_sim)) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90),minor_breaks=65:90) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	scale_alpha_manual(values=c("Overall"=highalpha, "Hispanic"=lowalpha, "Non-Hispanic Black"=lowalpha, "Non-Hispanic White"=lowalpha),guide="none") + 
	labs(title="Overall", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom", text=element_text(size=16))

fig_overlay_hispanic <- ggplot(le_sim_all_re, aes(x=t, y=le, col=factor(fold_decay), alpha=RaceEthnicity)) +
	geom_line(stat="smooth", method="loess", span=0.2) + 
	geom_point(aes(shape=is_sim)) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90),minor_breaks=65:90) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	scale_alpha_manual(values=c("Overall"=lowalpha, "Hispanic"=highalpha, "Non-Hispanic Black"=lowalpha, "Non-Hispanic White"=lowalpha),guide="none") + 
	labs(title="Hispanic", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom", text=element_text(size=16))

fig_overlay_nonhispanicblack <- ggplot(le_sim_all_re, aes(x=t, y=le, col=factor(fold_decay), alpha=RaceEthnicity)) +
	geom_line(stat="smooth", method="loess", span=0.2) + 
	geom_point(aes(shape=is_sim)) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90),minor_breaks=65:90) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	scale_alpha_manual(values=c("Overall"=lowalpha, "Hispanic"=lowalpha, "Non-Hispanic Black"=highalpha, "Non-Hispanic White"=lowalpha),guide="none") + 
	labs(title="Non-Hispanic Black", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom", text=element_text(size=16))

fig_overlay_nonhispanicwhite <- ggplot(le_sim_all_re, aes(x=t, y=le, col=factor(fold_decay), alpha=RaceEthnicity)) +
	geom_line(stat="smooth", method="loess", span=0.2) + 
	geom_point(aes(shape=is_sim)) + 
	theme_minimal() + 
	scale_x_continuous(minor_breaks=2014:2030) + 
	scale_y_continuous(limits=c(65,90),minor_breaks=65:90) + 
	scale_color_manual(values=c("1"="black","2"="blue","10"="red")) + 
	scale_shape_manual(values=c("0"=16,"1"=1),guide="none") + 
	scale_alpha_manual(values=c("Overall"=lowalpha, "Hispanic"=lowalpha, "Non-Hispanic Black"=lowalpha, "Non-Hispanic White"=highalpha),guide="none") + 
	labs(title="Non-Hispanic White", x="Year", y="Life expectancy (years)", col="Fold-decline in probability of death\nfor each future year of life") + 
	theme(legend.position="bottom", text=element_text(size=16))

ggsave(fig_overlay_overall, file="figures/overlay_overall.pdf", width=7, height=6)
ggsave(fig_overlay_hispanic, file="figures/overlay_hispanic.pdf", width=7, height=6)
ggsave(fig_overlay_nonhispanicblack, file="figures/overlay_nonhispanicblack.pdf", width=7, height=6)
ggsave(fig_overlay_nonhispanicwhite, file="figures/overlay_nonhispanicwhite.pdf", width=7, height=6)

