# Linear Mixed Effects Model (LMEM)

### Import the dataset
rc <- read.csv("~/rc.csv")

## First let's remember our good old friend: t-test :)

### Make the necessary adjustments to the data for t-test
rc$logRT <- log(rc$rawRT)
rc$condition <- as.factor(rc$condition)
bysubj <- aggregate(logRT ~ subject + condition, FUN = mean, data = rc)
bysubj$subject <- as.factor(bysubj$subject)

### Run the t-test
ttest <- t.test(logRT ~ condition, paired = TRUE, data = bysubj)

### Print the t-value
ttest$statistic

## Fitting LMEM

### Install the necessary package
install.packages("lme4")
install.packages("lmerTest")

### Load the package
library(lme4)
library(lmerTest)

### Fit the model

#### A little tweak for better understanding
bysubj$condition <- factor(bysubj$condition, levels=c("subjgap","objgap"))

### Run!
lmem <- lmer(logRT ~ condition + (1 | subject), data = bysubj)

### Print the t-value of the linear mixed model and calculate the p-value
t.value <- summary(lmem)$coefficients[2,4]

p.value <- 2*pt(-abs(t.value), df = nrow(bysubj)-1)

### Let's see the t-values of both models side by side
print(c(summary(lmem)$coefficients[2,4], ttest$statistic))


### Merits of LMEM
rc$subject <- as.factor(rc$subject)
rc$item <- as.factor(rc$item)


### Shrinkage

#### Fit a separate model for each subject
m.list <- lmList(logRT ~ condition | subject, data = rc)

##### Show the intercept of each subject
s.intercept.list <- summary(m.list)$coefficients[1:42]

#### Fit a single model
m.single <- lmer(logRT ~ condition + (1 | subject), data = rc)

##### Show the intercept of each subject
s.intercept.single <- rep(summary(m.single)$coefficients[1,1], 42) + ranef(m.single)$subject[,1]


### Now see 
plot(x = 1:42, y = s.intercept.single, main = "Intercept of Each Subject", xaxt = "n",
     xlab = "Subject", ylab = "Intercept", pch = 16, col = "blue", ylim = c(5, 6.5))
points(x = 1:42, y = s.intercept.list, pch = 1)
axis(1, at=1:42, labels=1:42)
abline(h = 5.851225, col = "red", lwd = 1)
text(x = 34, y = 5.75, substitute(paste(italic("Intercept = 5.851"))), col = "red")
legend("bottomleft", legend=c("Single Model", "List of Models"), col=c("blue", "black"), pch = c(16, 1))

## Mean distance of the single model estimates to the intercept
sis <- mean(abs(s.intercept.single - summary(m.single)$coefficients[1,1]))

## Mean distance of the list of models' estimates to the intercept
sil <- mean(abs(s.intercept.list - summary(m.single)$coefficients[1,1]))

## Print the values

dt <- data.frame(`LoM Intercept` = sil,
                 `Single Intercept` = sis,
                 Difference = sil - sis)

print(dt)

#### Varying intercept and varying slope

lmem.intercept <- lmer(logRT ~ condition + (1 | subject) + (1 | item), data = rc, REML = FALSE)
summary(lmem.intercept)

lmem.slope <- lmer(logRT ~ condition + (1 + condition | subject) + (1 | item), data = rc, REML = FALSE)
summary(lmem.slope)

### Compare the models
anova(lmem.intercept, lmem.slope)

### Hypothesis testing
lmem.null <- lmer(logRT ~ 1 + (1 + condition | subject) + (1 + condition | item), data = rc, REML = FALSE)
summary(lmem.null)

### Compare null model to the varying slope model to reject the null hypothesis
anova(lmem.null, lmem.slope)


## Generalized Linear Mixed Model (GLMM)

### Load the dataset

hindi <- read.csv("~/hindi.txt", sep="")

### Inspect the dataset
View(hindi)
str(hindi)

### Change the dataypes
hindi$subj <- as.factor(hindi$subj)
hindi$item <- as.factor(hindi$item)

### Adjust our variables
hindi$skip <- ifelse(hindi$TFT == 0, 1, 0)
hindi$word_complexity <- scale(hindi$word_complex, scale = FALSE)
hindi$storage_complexity <- scale(hindi$SC, scale = FALSE)

## Fitting the models

### Varying intercept model

glmm.intercept <- glmer(skip ~ word_complexity + storage_complexity + 
                          (1 | subj) +
                          (1 | item),
                        family = binomial(), data = hindi)

summary(glmm.intercept)

### Varying slope model

glmm.slope <- glmer(skip ~ word_complexity + storage_complexity + 
                      (1 + word_complexity | subj) +
                      (1 + word_complexity | item) + 
                      (1 + storage_complexity | subj) + 
                      (1 + storage_complexity | item),
                    family = binomial(), data = hindi)

summary(glmm.slope)

### Compare the models
anova(glmm.slope, glmm.intercept)


### Hypothesis testing

#### We have witnessed that converging such models takes a long time. We can learn
#### how much time it takes a model to converge via the system.time function

system.time(
  glmm.slope.null <- glmer(skip ~ 1 + 
                             (1 + word_complexity | subj) +
                             (1 + word_complexity | item) + 
                             (1 + storage_complexity | subj) + 
                             (1 + storage_complexity | item),
                           family = binomial(), data = hindi)
)

### Reject or accept the null hypothesis
anova(glmm.slope.null, glmm.slope)

### Now let's remove storage complexity from the model
system.time(
  glmm.slope.wc <- glmer(skip ~ word_complexity + 
                           (1 + word_complexity | subj) +
                           (1 + word_complexity | item) + 
                           (1 + storage_complexity | subj) + 
                           (1 + storage_complexity | item),
                         family = binomial(), data = hindi)
)

summary(glmm.slope.wc)

### See how the effect of storage complexity improves the model fit
anova(glmm.slope.null, glmm.slope.wc, glmm.slope)


### Similarly, we can remove the word complexity from the model
system.time(
  glmm.slope.sc <- glmer(skip ~ storage_complexity + 
                           (1 + word_complexity | subj) +
                           (1 + word_complexity | item) + 
                           (1 + storage_complexity | subj) + 
                           (1 + storage_complexity | item),
                         family = binomial(), data = hindi)
)

summary(glmm.slope.sc)

### See how the effect of word complexity improves the model fit
anova(glmm.slope.wc, glmm.slope.sc, glmm.slope)
