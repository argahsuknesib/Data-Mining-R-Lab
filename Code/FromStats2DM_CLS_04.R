#############################################################
# From Statistics to Data Mining
# Computer Lab Session n° 4:
# Linear Algebra (1/2)
#############################################################

# Solving 
#     (1) 3 x1 - 4 x2 = 6 
# and (2) x1 + 2 x2 = -3

# (1) <=> 4 x2 = 3 x1 - 6 <=> x2 = (3 x1 - 6) / 4
# (2) <=> 2 x2 = -x1 - 3 <=> x2 = (-x1 - 3) / 2

# Graphical solution

x1 <- seq(-5,5, length=200)
l1 <- (3*x1 - 6) / 4
l2 <- (-x1 - 3) / 2

windows(800, 600, 10)
plot(x1, l1, col="blue", type="l",lwd=1, ylim=range(c(l1,l2)))
par(new=TRUE)
plot(x1, l2, col="red", type="l",lwd=1, ylim=range(c(l1,l2)))


segments(-5,-1.5,0,-1.5, col="black",lty=3)
segments(0,-5,0,-1.5, col="black", lty=3)

points(x=0, y=-1.5, pch=19, col="green")


# Linear Algebra solution

A <- matrix(c(3, 1, -4, 2), ncol=2) ; print(A)
Ainv <- solve(A) ; print(Ainv)
b <- matrix(c(6, -3), ncol=1) ; print(b)
x <- Ainv %*% b ; print(x)

# Problem
A <- matrix(c(3, 1, 2, -3), ncol=2) ; print(A)
Ainv <- solve(A) ; print(Ainv)
b <- matrix (c(7 , -5) , ncol=1) ; print(b)
x <- Ainv %*% b ; print (x)

# Check
x1 <- 1
x2 <- 2
3*x1 + 2*x2
# Result : 7 (correct)
x1 - 3*x2
# Result : -5 (correct)

# 2. Vectors and Matrices in R

# 3 x 3 Hilbert Matrix
H3 <- matrix(c(1, 1/2, 1/3, 1/2, 1/3, 1/4, 1/3, 1/4, 1/5), nrow=3)
H3

1/cbind(seq(1, 3), seq(2, 4), seq(3, 5))

X <- matrix(seq(1, 12), nrow=3)
X

diag(5)


############
# Exercise #
############


matrix(rep(seq(0, 4), 5), nrow=5) 
matrix(rep(seq(1, 5), 5), byrow=TRUE, nrow=5)

A <- matrix(rep(seq(0, 4), 5), nrow=5) +
     matrix(rep(seq(1, 5), 5), byrow=TRUE, nrow=5)

A

X[3,2]

colnames(X) <- c("X1", "X2", "Y", "Z")
rownames(X) <- c("obs1", "obs2", "obs3")
X


X["obs1","Y"]
X[,"Z"]
X["obs2",]

############
# Exercise #
############

height <- c(172, 168, 167, 175, 180)
weight <- c(62, 64, 51, 71, 69)

info <- cbind(height, weight)
rownames(info) <- c("Neil","Cindy","Pardeep","Deepak","Hao")
info

info[3, 1] <- 162
info[5, 1] <- 181
info[5, 2] <- 68
info

dim(X)

det(H3)

diag(X)
diag(H3)

trace <- function(data) sum(diag(data))
trace(X)
trace(H3)

t(X)

lower.tri(H3)
H3

Y <- 2 * X
Y

Y + X

X * Y

t(Y) %*% X

crossprod(Y,X)

H3inv <- solve(H3)
H3inv

H3inv %*% H3

b <- c(1, 2, 3)
x <- solve(H3, b)
x


############
# Exercise #
############

# A quintic polynomial is a polynomial of degree 5.
# Examples: 
#      x^5 - x^3 + x, 
#      y^5 + y^4 + y^3 + y^2 + y + 1

# Setting the quintic function g(x) = 0 and assuming a <> 0
# produces a quintic equation of the form:
# a x^5 + b x^4 + c x^3 + d x^2 + e x + f = 0.

X1 <- c(10, 11, 12, 13, 14, 15)
X2 <- X1^2
X3 <- X1^3
X4 <- X1^4
X5 <- X1^5
X0 <- c(rep(1,6))
A <- cbind(X0, X1, X2, X3, X4, X5)
A
f <- matrix(c(25, 16, 26, 19, 21, 20), nrow=6)
a <- solve(A, f)
a
# f(x) = 253610 - 102551 x + 16500.92 x^2 
#      - 1320.667 x^3 + 52.58333 x^4 - 0.8333 x^5

A %*% a

