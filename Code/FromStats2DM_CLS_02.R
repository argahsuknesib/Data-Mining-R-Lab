#############################################################
# From Statistics to Data Mining
# Computer Lab Session n� 2:
# Probability, Random Variables and Probability Distributions
#############################################################

# Example of functions:

f <- function() {
  x <- 1; 
  y <- 2; 
  x + y;
  }
f()


g <- function(x, y ) {
  z <- x + y^2; 
  return(z);
  }
g(2,3)
# 2 + 9 = 11


i <- 5
repeat {if (i > 25) 
  break else {
    print(i); 
    i <- i + 5;
  }
}


i <- 5
while (i <= 25) {print(i);
  i <- i + 5
}


for (i in seq(from=5,to=25,by=5)) print(i)

data("mtcars")
summary(mtcars)


plot(mtcars$wt, mtcars$mpg)

library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)

# Same graph
qplot(wt, mpg, data=mtcars)
 
data("pressure")
summary(pressure)

plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="red")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))

data("BOD")
summary(BOD)
barplot(BOD$demand, names.arg=BOD$Time)

table(mtcars$cyl)
barplot(table(mtcars$cyl))

table(BOD$Time)
table(BOD$demand)
# It's not working anymore with the new version of ggplot2 library
qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")

# It must be changed into:
qplot(BOD$Time, BOD$demand) + geom_bar(stat="identity")

hist(mtcars$mpg)

hist(mtcars$mpg, breaks=10)
qplot(mpg, data=mtcars, binwidth=4)

data("ToothGrowth")
summary(ToothGrowth)

plot(ToothGrowth$supp, ToothGrowth$len)

boxplot(len ~ supp, data = ToothGrowth)

boxplot(len ~ supp + dose, data = ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose),
      ToothGrowth$len, geom="boxplot")

curve(x^3 - 5*x, from=-4, to=4)

myfun <- function(xvar) { 1/(1 + exp(-xvar + 10)) }

curve(myfun(x),from=0,to=20)

# Add a line:
curve(1-myfun(x), add = TRUE, col = "red")

# It's not working anymore with the new version of ggplot2 library
qplot(c(0,20), fun=myfun, stat="function", geom="line")

# It must be changed into:
f <- ggplot(data.frame(x = c(0, 20)), aes(x))
myfun <- function(xvar) { 1/(1 + exp(-xvar + 10)) }
myfun2 <- function(xvar) { 1 - (1/(1 + exp(-xvar + 10))) }
f + stat_function(fun = myfun, colour = "red") + stat_function(fun = myfun2, colour = "blue")

data("iris")
summary(iris)

pairs(iris[1:4])

pairs(iris[1:4], pch = 21,
      bg = c("red", "green3","blue")[unclass(iris$Species)])

myfun <- function(xvar) { 1/(1 + exp(-xvar + 10)) }
curve(myfun(x),
      xlab = "activation",
      ylab = "threshold function",
      from=0, to=20)

