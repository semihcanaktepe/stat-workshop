# Testing your hypotheses

## Let's first generate some random samples
sample <- rnorm(20, mean = 1, sd = 1)

### We do NOT know what is the true mean and sd in our sample.

## So state your hypothesis: My sample mean deviates from 0

### Let's see mean of the sample
x_bar <- mean(sample)

print(x_bar)

### State your null hypothesis: In our case, as we determined that
### our sample mean deviates from 0, our null hypothesis is that
### the sample mean is 0.

mu <- 0

### Now we compute the standard error (SE)

SE <- sd(sample)/sqrt(length(sample))

### Compute t-value and interpret it
t.value <- (x_bar - mu)/SE

print(t.value)

### Now using t-value, compute p-value

p.value <- 2*pt(-abs(t.value), df = length(sample)-1)

print(p.value)


### 95% CI
lower <- x_bar - 2*SE
upper <- x_bar + 2*SE

print(c(lower, upper))








### Congrats! You ran a t-test without any bespoke function!
ttest <- t.test(sample)

### Let's see our values match
ttest$statistic
ttest$p.value




