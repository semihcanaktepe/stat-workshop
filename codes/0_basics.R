# Quick R Tutorial (LangCogLab)

## Creating variables in R

### You can assign values to the variables using "<-" operator.
subject_1 <- "Immanuel"
age <- 5
rt <- 252.3
adultornot <- TRUE

### You can pipe multiple variables with the same value as follows:
lang1 <- lang3 <- lang4 <- "Turkish"
lang2 <- lang4 <- lang6 <- "English"

### To print values of the variables
print(subject1) #or simply
subject_1

## Data types in R

### There are some data types in R. For example, "subject_" is a character because
### it contains a string. "age" is integer, "rt" is numeric, and "adultornot" is a boolean (logical).
### You can learn the data type by using class() function.

class(subject_1)
class(age)
class(rt)
class(adultornot)

### To work on your data, it must sometimes be in certain type, and the types
### of the data must be of the same class to make computation on them. Try:

age2 <- 4
new_total <- age + age2

### Yet,
age + rt

### So, we may need to make conversions to calculate the values.

age <- as.numeric(age)
age + rt

### Now convert "age" back to integer and convert "rt" into an integer.
### Make the same calculation.

age <- as.integer(age)
rt <- as.integer(rt)

age + rt # What happened?

## Operators in R

### Math (using R as your calculator)
5 + 4
26 - 99
77*3
12/4
2^8

### Comparison
age == age2
age != age2
age > age2
age < age2
age >= age2
age <= age2

### Logical
TRUE && FALSE
TRUE || FALSE
!TRUE

## Conditionals in R
if (condition) {do x1} else if (condition2) {do x2} else {do x3}

if (age > age2) {
  print("age is greater than age2.")
} else if (age == age2) {
  print("age is equal to age2.")
} else {
  print("age is less than age2.")
}

### For loops in R

for (val in vector){
  do x for every value in vector
}

v <- 1:20

for (i in v){
  print(i*4)
}

### But often instead of using for loops, using "apply" functions works easier and faster.
#### tapply, sapply, etc.

### Functions in R
f <- function(variable1, variable2, ... ){do x}

my_mean <- function(vector){
  return (sum(vector)/length(vector))
}

facto <- function(n){
  if (n==1){
    return (n)
  } else {
    return (n*facto(n-1))
  }
}

## Data Structures

## loading datasets, packages, installing packages, checking package version



### rbind, cbind, append, aggregate

### Distributions

### Sd, mean, SE, cov, var, cor

## creating dataframes

## setwd



# rep, seq, 1:10, c(), rbinom, rnorm, runif



















