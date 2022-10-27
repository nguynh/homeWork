library(microbenchmark)
source("llr_function.R")

n = 150
omega = 2
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

# Note that we use the unit of milliseconds to check speed of function llr
# May need to check the unit first
m <- unlist(summary(microbenchmark(llr(x,y,x,omega))))
time <- unname(m[4])
cat("It takes", time, "milliseconds for the function to run.")