#############################################################
# From Statistics to Data Mining
# Computer Lab Session n° 8:
# Linear Regression (2/2)
#############################################################



#############################################
# Gradient Descent and Closed-Form Solution #
#############################################
# Create some values
xs <- seq(0,4,len=20) 

# Define the function we want to optimize
f <-  function(x) 
  {
  1.2 * (x-2)^2 + 3.2
  }

# Plot the function 
plot(xs, f(xs), type="l",xlab="x",ylab=expression(1.2(x-2)^2 +3.2))

# calculate the gradient df/dx
grad <- function(x)
  {
  1.2*2*(x-2)
  }

lines (c (2,2), c (3,8), col="red",lty=2)
text (2.1,7, "Closedform solution",col="red",pos=4)


# Gradient descent implementation
# Initialize the first guess for x-value
x <- 0.1 

# Store x-values for graphing purposes (initial)
xtrace <- x 

# Store y-values (function evaluated at x) for graphing purposes (initial)
ftrace <- f(x) 
alpha <- 0.6   # learning rate 'alpha'
for (step in 1:100) 
  {
  x <- x - alpha*grad(x)   # Gradient descent update
  xtrace <- c(xtrace,x)    # Update for graph
  ftrace <- c(ftrace,f(x)) # Update for graph
  }

lines ( xtrace , ftrace , type="b",col="blue")
text (0.5,6, "Gradient Descent",col="blue",pos= 4)

# Print final value of x
print(x) 
xtrace
# Result: x converges to 2.0



####################
# Gradient Descent #
####################

# Load data and initialize values
data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")
data <- read.csv("~/R/data/housing.csv") 
pairs(data)
cor(data)
reghousing <- lm(price ~ area+bedrooms,  data = data)
reghousing <- lm(price ~ area,  data = data)
reghousing



###########################################
# Logistic Regression and Newton's Method #
###########################################


# Create some values for x
x <- seq(-5,5,length.out=200)

# My function is an expression
myFunction <- expression(5 * x^3 - 7 * x^2 - 40 * x + 100)

# First derivative
derivativeF1 <- D(myFunction, 'x') ; print(derivativeF1)

# Second derivative
derivativeF2 <- D(derivativeF1, 'x') ; print(derivativeF2)

# Plot of my function, the first and the second derivatives
y <- eval(myFunction)
yF1 <- eval(derivativeF1)
yF2 <- eval(derivativeF2)

win.graph(800, 600, 10)
plot(x, y, type="l")
segments(-5, 0, 5, 0, lty=3)
par(new=TRUE)
plot(x, y = yF1, type="l", col="red", ylim=range(y), ylab="")
par(new=TRUE)
plot(x, y = yF2, type="l", col="blue", ylim=range(y), ylab="")


install.packages("animation")
library(animation)
 
oopt = ani.options(interval = 1, nmax = ifelse(interactive(), 50, 2))
par(pch = 20)
 
## default example
xx = newton.method()
xx$root  # solution
 
## take a long long journey
xx = newton.method(function(x) 
  5 * x^3 - 7 * x^2 - 40 * x + 100, 7.15, 
  c(-6.2, 7.1))
xx$root
 
## another function
ani.options(interval = 0.5)
xx = newton.method(function(x) exp(-x) * x, rg = c(0, 10), init = 2)
xx$root

## does not converge!
xx = newton.method(function(x) atan(x), rg = c(-5, 5), init = 1.5)
xx$root  # Inf

ani.options(oopt)
 
 ## interaction: use your mouse to select the starting point
if (interactive()) {
   ani.options(interval = 0.5, nmax = 50)
   xx = newton.method(function(x) atan(x), rg = c(-2, 2), interact = TRUE)
}
 
## HTML animation pages
saveHTML({
   ani.options(nmax = ifelse(interactive(), 100, 2))
   par(mar = c(3, 3, 1, 1.5), mgp = c(1.5, 0.5, 0), pch = 19)
   newton.method(function(x) 5 * x^3 - 7 * x^2 - 40 * x + 100, 
                 7.15, c(-6.2, 7.1), main = "")
 }, img.name = "newton.method", htmlfile = "newton.method.html", ani.height = 500, 
ani.width = 600, title = "Demonstration of the Newton-Raphson Method", 
description = "Go along with the tangent lines and iterate.")
 
