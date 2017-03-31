library(ggplot2)
library(psych)
data(mtcars)
head(mtcars)

ggplot(mtcars, aes(x=mpg)) + geom_histogram()

x <- mtcars$wt

#x[33:34] <- c(100,0)

hist(x)

describe(x)

boots <- numeric(100000)
for(i in 1:length(boots)) {
	s <- sample(x, size=length(x), replace=TRUE)
	boots[i] <- median(s)
}

hist(boots)
sd(boots)
