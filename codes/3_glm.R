## Generalized Linear Model (GLM)

## Import the dataset
ucb <- read.csv("~/ucbadmit.txt", sep=";")

## View the dataset
View(ucb)

## Visualize

### Compute probability of being admitted
ucb$padmit <- ucb$admit/ucb$applications

### and 95% CI
alpha = 0.05
ci <- qt(p=alpha/2, df=nrow(ucb), lower.tail=F)*(sd(ucb$padmit)/sqrt(nrow(ucb)))
ucb$Low <- ucb$padmit - ci
ucb$High <- ucb$padmit + ci

### Dummy variable for department indeces.
ucb$index <- rep(1:6, each=2)
ucb$gen <- rep(1:2, times=6)

### Subset the data for plotting
male <- subset(ucb, applicant.gender == "male")
female <- subset(ucb, applicant.gender == "female")

plot(male$index-0.05, male$padmit,
     type = "b", pch = 15, lwd = 2, col= "blue", xaxt='n',
     xlab = "Department", ylab = "P(Being Admitted)",
     ylim=c(0.0,1.0), main = "Probability of Being Admitted")
arrows(male$index-0.05, male$Low, male$index-0.05, male$High, length=0.05, angle=90, code=3, col = "blue")
lines(female$index+0.05, female$padmit, type = "b", pch = 16, lwd = 2, col = "red")
arrows(female$index+0.05, female$Low, female$index+0.05, female$High, length=0.05, angle=90, code=3, col = "red")
axis(1, at=1:6, labels=c("A", "B", "C", "D", "E", "F"))
legend("topright", legend=c("Male", "Female"), col=c("blue", "red"), lty = 1, lwd = 2, pch = c(15,16))

# Fitting the model

### Let's see linear model just for curiosity
lm0 <- lm(padmit ~ applicant.gender*dept, data = ucb)
summary(lm0)

## Investigate the structure of the dataset
str(ucb)

## Convert the necessary variables into factor
ucb$dept <- as.factor(ucb$dept)
ucb$applicant.gender <- as.factor(ucb$applicant.gender)

glm1 <- glm(padmit ~ applicant.gender, data = ucb, weight = applications, family = binomial(logit))
summary(glm1)

glm2 <- glm(padmit ~ applicant.gender + dept, data = ucb, weight = applications, family = binomial(logit))
summary(glm2)

glm3 <- glm(padmit ~ applicant.gender*dept, data = ucb, weight = applications, family = binomial(logit))
summary(glm3)

### Model Comparison
AIC(glm1, glm2, glm3)
BIC(glm1, glm2, glm3)

### Now predict the logit values.
ucb$predict <- predict(glm3)

### Define the inverse logit function to convert our values back to normal
invlogit <- function(x){return(exp(x)/(1+exp(x)))}

### Visualize the predicted values
ci <- qt(p=alpha/2, df=nrow(ucb), lower.tail=F)*(sd(ucb$predict)/sqrt(nrow(ucb)))
ucb$Low <- invlogit(ucb$predict - ci)
ucb$High <- invlogit(ucb$predict + ci)

male <- subset(ucb, applicant.gender == "male")
female <- subset(ucb, applicant.gender == "female")

plot(male$index-0.05, invlogit(male$predict),
     type = "b", pch = 15, lwd = 2, col= "blue", xaxt='n',
     xlab = "Department", ylab = "P(Being Admitted)",
     ylim=c(0.0,1.0), main = "Probability of Being Admitted")
arrows(male$index-0.05, male$Low, male$index-0.05, male$High, length=0.05, angle=90, code=3, col = "blue")
lines(female$index+0.05, invlogit(female$predict), type = "b", pch = 16, lwd = 2, col = "red")
arrows(female$index+0.05, female$Low, female$index+0.05, female$High, length=0.05, angle=90, code=3, col = "red")
axis(1, at=1:6, labels=c("A", "B", "C", "D", "E", "F"))
legend("topright", legend=c("Male", "Female"), col=c("blue", "red"), lty = 1, lwd = 2, pch = c(15,16))


## Specifying weights

### Generate a random dataset
p <- sort(round(runif(100, min = 0, max = 1), 3))
g <- rep(c(1,2), each = 50)
w1 <- round(runif(100, min = 5, max = 150))
w2 <- rep(50, 100)
d <- data.frame(p = p, g = g, w1 = w1, w2 = w2)

### Fit our models
mod1 <- glm(p ~ g, weight = w1, data = d, family = binomial(logit))
mod2 <- glm(p ~ g, weight = w2, data = d, family = binomial(logit))
mod3 <- glm(p ~ g, data = d, family = binomial(logit))

### Check out the summaries of the models
summary(mod1)
summary(mod2)
summary(mod3)



