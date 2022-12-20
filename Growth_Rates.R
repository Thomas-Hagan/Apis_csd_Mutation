Growth_Rates <- function(Iterations) {
  Growth_Rate = c()
  for (i in 2:(Iterations-1)) {
    Growth_Rate[i-1] = Queens_every_Gen[i]/Queens_every_Gen[i-1]
  }
  print(mean(Growth_Rate))
  x = c(seq(1, length(Growth_Rate)))
  s = seq(length(x-1))
  plot(x, Growth_Rate)
  segments(x[s], Growth_Rate[s], x[s+1], Growth_Rate[s+1])
  abline(lm(Growth_Rate ~ x))
  x1 = c(seq(1, length(Queens_every_Gen)))
  plot(x1, Queens_every_Gen)
  s = seq(length(x1 -1))
  segments(x1[s], Queens_every_Gen[s], x1[s+1], Queens_every_Gen[s+1])
  abline(lm(Queens_every_Gen ~ x1))
}