pdeath_baseline <- c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1)
pdeath_inst_baseline <- calc_pdeath_inst(pdeath_baseline) 
xi <- c(1, 1.05, 1.1, 1.2, 1.3, 1.4, 1.6)
trng <- -1:10
k <- 2


# ==============================================================================
# Use function to simulate:

le_sim_2 <- sim_covid(trng, pdeath_inst_baseline, xi, k)
fig_le_example <- le_sim_2 %>% 
	ggplot(aes(x=t, y=le)) + 
		geom_point() + 
		geom_line()

# ==============================================================================
# Step through function:

# Initialize an output vector for life expectancies: 
levec <- c()

# Create a life matrix using the baseline probabilities of death: 
m <- make_upper_tri(pdeath_inst_baseline, shift=FALSE)
m <- norm_rows(m)
m_init <- m

# Begin the simulation
for(t in trng){

	if(t==0){

		# --- Calculate excess probability of death from COVID:
		# Turn the baseline probability of death...
		p_death <- diag(m)
		# into a baseline rate of death...
		r_death <- -log(1-p_death)
		# then calculate the rate of death from COVID...
		r_death_covid <- xi*r_death
		# and convert back into a probability of death during COVID.
		p_death_covid <- 1-exp(-r_death_covid)
		# Calculate the excess probability of death from COVID
		excess_p_death_covid <- p_death_covid - p_death


		p_death_df <- tibble(
			age=0:(length(p_death)-1), 
			p_death=p_death, 
			p_death_covid=p_death_covid)
		
		fig_p_death_comp <- p_death_df %>% 
			pivot_longer(-age) %>% 
			ggplot(aes(x=age, y=value, col=name)) + 
				geom_point(alpha=0.8) + 
				scale_color_manual(labels=c(p_death="2018",p_death_covid="2020"), values=c("black","red")) + 
				geom_segment(data=p_death_df, aes(x=age, y=p_death, xend=age, yend=p_death_covid), col="gray", alpha=0.8) + 
				scale_x_continuous(breaks=0:(length(p_death)-1)) + 
				scale_y_continuous(limits=c(0,1)) + 
				theme_minimal() + 
				labs(x="Age", y="Probability of death") + 
				theme(text=element_text(size=16), legend.title=element_blank())


		# --- Distribute excess probability across future-years: 
		# Create a matrix of future-year proportions: 
		fymat <- m-diag(diag(m))
		# Create a matrix of death-probability dropoff: 
		kmat <- make_upper_tri(c(0,k^(-(0:(ncol(m)-2)))), shift=TRUE)
		# Calculate the normalizing factor for death dropoff: 
		x <- excess_p_death_covid/rowSums(kmat*fymat)
		x[is.na(x)] <- 0
		# Turn this normalizing factor into a matrix:
		xmat <- matrix(rep(x,nrow(m)),nrow=nrow(m))
		# Calculate the distribution of excess death across future-years:
		fy_death_dist_mat <- xmat*kmat*fymat
		# Reduce future-year proportions to account for excess death:
		m <- m - fy_death_dist_mat
		# Add that excess death to this year's deaths:
		diag(m) <- diag(m) + excess_p_death_covid
	}

	# Calculate life expectancy
	levec <- c(levec, get_le(m))

	# Increment the life matrix
	m <- increment_lifematrix(m,pdeath_inst_baseline)

}

out <- tibble(t=trng,le=levec)