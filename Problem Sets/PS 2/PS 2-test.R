install.packages("PolynomF")
install.packages("cmna")
install.packages("matlib")
require(PolynomF)
library(cmna)


x = polynom()
# p = x^3 - 3*x^2 - 2*x + 7
dpdx = deriv(p, "x")

curve(p,-2,3, col = "red", lwd = 3)
curve(dpdx, -2, 3, lty=2, lwd=3, add = T)
abline(v=0,h=0)
abline(v=c(dpdx_zeros[1],dpdx_zeros[2]), col = "blue")
points(dpdx_zeros[1],p(dpdx_zeros[1]), pch = 19)

f = function(x) {x^6 - x - 1}
curve(f, from=0, to=2, xlab='x', ylab='f(x)', col='red')
abline(h=0)
bi = bisection(f, 1, 1.5, tol = 0.00005, m=14)

f = function(x) {x^4 - 3*x^2}
curve(f, from=-3, to=3, xlab='x', ylab='f(x)', col='red')
abline(h=0)

f = function(x) {x^3 - x - 3}
curve(f, from=0, to=3, xlab='x', ylab='f(x)', col='red')
abline(h=0)
bi = bisection(f, 1.5, 2.0, tol = 0.0001, m=13)

# From PS 2
euler = exp(1)
euler
p = -3*x^3 - 3*euler^(x^2/2) - 1
dpdx = deriv(p, "x")

# Example matrix
A=matrix(c(1,1,1,), nrow = 3, byrow = T)
# Print matrix
A

# Get lower triangular of matrix
A_lower <- A
A_lower[lower.tri(A)] <- 0
A_lower

# ---------------------------------------

A=matrix(c(10.2, 2.4, -4.5, -2.3, -7.7, 11.1, -5.5, -3.2, 0.9), nrow = 3, byrow = T)
# Print matrix
A

# Get lower triangular of matrix
A_lower <- A
A_lower[lower.tri(A)] <- 0
A_lower

# ---------------------------------------

library(matlib)
A=matrix(c(10.2,2.4,-4.5,-2.3,-7.7,11.1,-5.5,-3.2,0.9),nrow = 3, byrow=T)

# L=matrix(c(10.2,0,0,-2.3,-7.1588235294119,0,-5.5,-1.9058823529415,4.09854317877454),nrow = 3, byrow=T)
lower <- A
lower[lower.tri(A)] <- 0
lower

# U=matrix(c(1,0.235294117647,-0.44117647,0,1,-1.408785866904657,0,0,4.09854317877454),nrow = 3, byrow=T)
upper <- A
upper[upper.tri(A)] <- 0
upper

A==(lower%*%upper)

# ----------------------------------------

A=matrix(c(1,1,1,4,3,2,0,-2,1),nrow = 3, byrow=T)
L=matrix(c(1,0,0,4,-1,0,0,-2,5),nrow = 3, byrow=T)
U=matrix(c(1,1,1,0,1,2,0,0,1),nrow = 3, byrow=T)

A==(L%*%U)

# -----------------------------------------

A=matrix(c(10.2,2.4,-4.5,-2.3,-7.7,11.1,-5.5,-3.2,0.9),nrow = 3, byrow=T)
L=matrix(c(1,0,0,-2.3,1,0,-5.5,2.3,1),nrow = 3, byrow=T)
U=matrix(c(10.2,2.4,-4.5,0,-5.4,13.4,0,0,4.1),nrow = 3, byrow=T)

A==(L%*%U)

# -------------------------------------------
# This will be the LU matrices used moving forward.

A=matrix(c(10.2,2.4,-4.5,-2.3,-7.7,11.1,-5.5,-3.2,0.9),nrow = 3, byrow=T)
L=matrix(c(10.2,0,0,-2.3,-7.15882352941177,0,-5.5,-1.90588235294118,1),nrow = 3, byrow=T)
U=matrix(c(1,0.235294117647059,-0.441176470588235,0,1,-1.40879211175021,0,0,1),nrow = 3, byrow=T)

A==(L%*%U)

# Solving for y
b=matrix(c(14.067,-0.996,-12.645))
y=solve(L,b)
y

# Solving for x
x=solve(U,y)
x

# Solution for PS2 No. 4c

A=matrix(c(10.2,2.4,-4.5,-2.3,-7.7,11.1,-5.5,-3.2,0.9),nrow = 3, byrow=T)
b=matrix(c(14.067,-0.996,-12.645))
x_bar=matrix(c(1.4531001,-1.5891949,-0.2748947))

r=b-(A%*%x_bar)
r
