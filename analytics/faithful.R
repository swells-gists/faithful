#
# faithful.R
#
# inputs: 
# ------------
# n_breaks       - numeric
# individual_obs - logical
# density        - logical
# bw_adjust      - numeric
#
# outputs:
# ------------
# unnamed plot  ---> this will map to the name `plot` in UI directive 
#                    <di-rscript outputs="plot" />
#
#
# di$code("faithful.R", n_breaks, individual_obs, density, bw_adjust)

# n_breaks<-35
# individual_obs<-FALSE
# density<-FALSE
# bw_adjust<-1

hist(faithful$eruptions,
     probability = TRUE,
     breaks = as.numeric(n_breaks),
     xlab = "Duration (minutes)",
     main = "Geyser eruption duration")

if (individual_obs) {
  rug(faithful$eruptions)
}

if (density) {
  dens <- density(faithful$eruptions,
                  adjust = bw_adjust)
  lines(dens, col = "blue")
}

