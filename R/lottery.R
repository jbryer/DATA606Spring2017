library(DATA606)

tickets <- as.data.frame(rbind(
	c(    '$1',    1,     15),
	c(    '$2',    2,     11),
	c(    '$4',    4,     62),
	c(    '$5',    5,    100),
	c(   '$10',   10,    143),
	c(   '$20',   20,    250),
	c(   '$30',   30,    562),
	c(   '$50',   50,   3482),
	c(  '$100',  100,   6681),
	c(  '$500',  500,  49440),
	c('$1500',  1500, 375214),
	c('$2500',  2500, 618000)
), stringsAsFactors=FALSE)
names(tickets) <- c('Winnings', 'Value', 'Odds')
tickets$Value <- as.integer(tickets$Value)
tickets$Odds <- as.integer(tickets$Odds)

tickets$xPx <- tickets$Value * (1 / tickets$Odds)
sum(tickets$xPx)

sum(tickets$xPx) - 1

(sum(tickets$xPx) - 1) * 365

nGames <- 365
runs <- numeric(1000)
for(j in seq_along(runs)) {
	odds <- sample(max(tickets$Odds), nGames, replace=TRUE)
	vals <- rep(-1, length(odds))
	for(i in 1:nrow(tickets)) {
		vals[odds %% tickets[i,'Odds'] == 0] <- tickets[i,'Value'] - 1
	}
	runs[j] <- cumsum(vals)[nGames]
}

hist(runs)
mean(runs)

shiny_demo('lottery')
