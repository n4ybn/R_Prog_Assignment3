
trial <- function(n)
sum(runif(n) < 0.5)
replicate(100, trial(10))
plot(table(replicate(10000, trial(50))))

x <- 0
p <- 0.5

x <- x + if (runif(1) < p) -1 else 1
step <- function(x, p=0.5)
x + if (runif(1) < p) -1 else 1
x <- step(x)
x <- step(x)
for (i in 1:20)
x <- step(x)
nsteps <- 200
x <- numeric(nsteps + 1)
x[1] <- 0 # start at 0
for (i in seq_len(nsteps))
x[i+1] <- step(x[i])
plot(x, type="l")
random.walk <- function(nsteps, x0=0, p=0.5) {
x <- numeric(nsteps + 1)
x[1] <- x0
for (i in seq_len(nsteps))
x[i+1] <- step(x[i])
x
}
walks <- replicate(30, random.walk(100))
matplot(walks, type="l", lty=1, col=rainbow(nrow(walks)))
random.walk <- function(nsteps, x0=0, p=0.5)
cumsum(c(x0, ifelse(runif(nsteps) < p, -1, 1)))
walks <- replicate(30, random.walk(100))
matplot(walks, type="l", lty=1, col=rainbow(nrow(walks)))
