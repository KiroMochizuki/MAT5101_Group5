install.packages("PolynomF")
install.packages("cmna")
require(PolynomF)
library(cmna)


x = polynom()
p = x^3 - 3*x^2 - 2*x + 7
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
bi = bisection(f, 1.5, 2.0, tol = 0.0001, m=14)