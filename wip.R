# ----------------------
# REPORT CONFIGURATION
# ----------------------
library(ggplot2)        # LOAD LIBRARY
set.seed(2016-05-06)    # SET REPRODUCIBLE SEED

# ----------------------
# ASSIGNMENT PARAMETERS:
# - WHAT DO WE KNOW?
# ----------------------

# 1. Exponential Distribution Formula;
#    - Returns a collection of size n, of random exponents based on rate lambda
getExpDist <- function(n, lambda) rexp(n,lambda)

# 2. Mean of Exponential Distribution
getExpDistMean <- function(lambda) 1/lambda

# 3. Standard Deviation of Exponential Distribution
getExpDistStdDev <- function(lambda) 1/lambda

# 4. Rate Parameter is Lambda
lambda <- 0.2

# 5. Number of Random Observations to obtaim, given this length
n <- 40

# 6. Number of Simulations to Run
nosim <- 1000

# 7. Calculated Exponential Distribution Mean
expDistMean <- getExpDistMean(lambda = lambda)

# 8. True Standard Deviation of Sample Population:
#    - NOTE: the standard deviation of sample is to be divided by the square root of your sample size.
expDistStdDev <- getExpDistStdDev(lambda = lambda) / sqrt(n)


# -------------------------------
# DEFINE HELPER FUNCTIONS
# ------------------------------
getStepScaleBreaks <- function(mean_mu, distance) {
  lowerBound <- mean_mu - distance
  upperBound <-mean_mu + distance
  output <- numeric(length = abs(upperBound) + abs(lowerBound))
  for(i in lowerBound:upperBound) 
    output[i] <- i
  output
}
# ----------------------
# SIMULATE THE DISTRIBUTION OF N EXPONENTIALS
# ----------------------

# Obtain baseline exponential distribution formula: 
singleSampleDist <- getExpDist(n, lambda)

singleSampleDistMean <- mean(singleSampleDist)

# Construct Simulation Functions:
getSimulationExpDistMatrixDataFrame <- function(n, lambda, nosim) {
  data.frame(
    x_axis = matrix(getExpDist(n=n * nosim,lambda=lambda), nosim))
}

getSimulationExpDistMeansDataFrame <- function(n, lambda, nosim) {
  output = NULL
  for (i in 1 : nosim) {
    output = c(output, mean(getExpDist(n=n,lambda=lambda)))
  }
  data.frame(x_axis = output)
}

simulationExpDistMeansDF <- getSimulationExpDistMeansDataFrame(n,lambda,nosim)

simulationExpDistMean <- mean(simulationExpDistMeansDF$x_axis)

head(simulationExpDistMeansDF)

scaleStepBreaks <- function(mean_mu,distance) ((mean_mu-distance):(mean_mu+distance))

ggplot(
  data = simulationExpDistMeansDF) +    # SET DATA STRUCTURE AS DATAFRAME OF MEANS
  aes(x = x_axis) +                     # SET X AXIS FROM DF FOR PLOTTING
  geom_histogram(                       # PLOT HISTOGRAM, NOTABLE SCALE IT BY LAMBDA, PREP FOR DENSITY
    binwidth=0.05, 
    aes(y=..density..), 
    alpha=lambda, 
    colour="black", 
    fill="light blue") +                
  labs(x="Means",                       # ADD LABELS
       y="Density (Scaled by Lambda") +
  scale_x_continuous(                   # SET X-AXIS SCALE TO SHOW CONTINUES NUMBERS
    breaks=scaleStepBreaks(expDistMean,4)) +
  geom_density(                         # OVERLAY THE DENSITY OF THE SAMPLE MEANS DENSITY CURVE (RED)
    colour="red", 
    size= 1) +
  stat_function(                        # OVERLAY THE DISTRIBUTED NORMAL DENSITY CURVE FOR COMPARISON (BLUE)
    fun = dnorm, 
    size = 1, 
    colour = "blue",
    args = list(mean = expDistMean, 
                sd = expDistStdDev)) +
  geom_vline(                           # OVERLAY LOCATION OF SAMPLE MEAN LINE (RED)
    xintercept = simulationExpDistMean, 
    size=1, 
    colour="red",
    show.legend = TRUE) + 
  geom_vline(
    xintercept = expDistMean,   # OVERLAY LOCATION OF THEORETICAL MEAN LINE (BLUE)
    size=1, 
    colour="blue",
    show.legend = TRUE) +
  scale_colour_manual("", 
    breaks = c("red", "blue"),
    values = c( "red"="red", "red"="blue")) 


