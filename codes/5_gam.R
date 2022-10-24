# Generalized Additive Model (GAM)

### Import the dataset
malay <- load("/Users/semih/malay.rdata")

### Transformations
malay$logRT <- log(malay$LDT_RT)
malay$scaledON <- as.numeric(scale(malay$Orthographic.N))

### Install the necessary package
install.packages("mgcv")

### Load the package
library(mgcv)


### Now fit the model
gam.freq <- gam(logRT ~ lg_freq_malaysia + s(lg_freq_malaysia), data = malay)
gam.check(gam.freq)
summary(gam.freq)

### Visualize

#### Install the necessary package for visualization
install.packages("itsadug")

#### Load the package
library(itsadug)

plot_smooth(gam.freq, view="lg_freq_malaysia",
            col="red", 
            main="Change in RT as a Function of Word Frequency", 
            xlab="Word Frequency (log)", ylab = "Reaction Time (log)", rm.ranef=T)


### Another model for the effect of Orthographic Neighborhood
gam.ortn <- gam(logRT ~ scaledON + s(scaledON), data = malay)
gam.check(gam.ortn)
summary(gam.ortn)

### Visualize
plot_smooth(gam.ortn, view="scaledON",
            col="blue", 
            main="Change in RT as a Function of Orthographic Neighborhood", 
            xlab="Orhographic Neighborhood (scaled)", ylab = "Reaction Time (log)", rm.ranef=T)

### Effects of word frequency and orthographic neighborhood and their interaction
gam.interaction <- gam(logRT ~ lg_freq_malaysia*scaledON + te(lg_freq_malaysia, scaledON), data = malay)
gam.check(gam.interaction)
summary(gam.interaction)

### Visualize
vis.gam(gam.interaction, view = c("lg_freq_malaysia", "scaledON"),
        xlab = "Word Frequency (log)", ylab = "Orthographic Neighborhood (scaled)",
        plot.type = "contour", main = "Interaction between WF and ON")


### Model Comparison
AIC(gam.freq, gam.ortn, gam.interaction)
BIC(gam.freq, gam.ortn, gam.interaction)









# Concluding remarks

N <- 1e5 # number of data points
U <- rnorm(N) # simulate confounds
B1 <- rbinom(N, size=1, prob=0.5) # First predictor
M <- rnorm(N, 2*B1 + 1*U) # Second predictor
B2 <- rbinom(N, size=1, prob=0.5) # Third predictor
D <- rnorm(N, 2*B2 + 0*M + 1*U) # Outcome

## Check the model summaries and then change the coefficient of U


### First check of the effect of B2 on D
summary(lm(D ~ B2))
cov(B2,D)/var(B2)

### Then check out the effect of M on D
summary(lm(D ~ M))

### Extend the models for further investigation
summary(lm(D ~ M + B1))
summary(lm(D ~ M + B1 + B2))

### Real effect of M on D
cov(B1, D)/cov(B1, M)

### Effect of B1 on M
summary(lm(M ~ B1))
cov(B1,M)/var(B1)

