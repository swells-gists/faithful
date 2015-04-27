#
# myScript.R
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
# n_breaks<-10
# individual_obs<-TRUE
# density<-TRUE
# bw_adjust<-1
# di$code("myScript.R", n_breaks, individual_obs, density, bw_adjust) 

df<-mtcars

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