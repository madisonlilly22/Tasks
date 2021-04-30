setwd('~/Desktop/Evolution/Tasks/Task_12')
x <- rnorm(100, mean=5, sd=2)
x
y <- (x*5)+ 2 + (rnorm(100, 0:0.1))
y
plot(x,y)
abline(lm(y~x), col='purple')
coef(lm(y~x))
#y intercept = 1.656855, x intercept= 5.047081

z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i]<- runif(1)
	y <- (x* z[i])+ 2 +(rnorm(100,0:0.1))
	l<- coef(lm(z[1:100]~y))
	}
pdf("2.pdf", height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()

pdf("3.pdf", height=4, width=4)
plot(c(z, -0.03))
dev.off()

install.packages('meme')
library('meme')
u <- system.file("angry8.jpg", package="meme")
my_meme <- meme(u, "code", "all the things!", color="gray", size="2")
	