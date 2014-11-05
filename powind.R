ShapleyShubik <- function (t)
{
  n <- length(t$Faction)
  perms <- permutations(n, n)
  maj <- 226
  outcome <- apply(perms, 1, function(x) {
    x[sum(cumsum(t$Deputies[x]) < maj) + 1] })
  sspi <- prop.table(table(outcome))
  names(sspi) <- t$Faction[sort(unique(outcome))]
  plot(sspi / (t$Deputies / sum(t$Deputies)),
       xlab="constituency", ylab="power index / votes")
}

Johnston <- function (t)
{
  n <- length(t$Faction)
  maj <- 226
  perms <- permutations(2,n,c(0,1),rep = TRUE)
  h <- nrow(perms)
  jpower <- numeric(n)
  for (i in 1:h) 
  {
    s <- sum(t$Deputies[as.logical(perms[i,])])
    if (s > maj)
    {
      ncrit <- 0
      critical_numbers <- numeric()
      for (j in 1:n)
      {
        if (perms[i,j] == 1)
        {
          perms[i,j] <- 0
          if (sum(t$Deputies[as.logical(perms[i,])]) <= maj) 
          {
            critical_numbers <- c(critical_numbers,j)
            ncrit <- ncrit + 1
          }
          perms[i,j] <- 1
        }
      }
      jpower[critical_numbers] <- jpower[critical_numbers] + 1/ncrit
    }
  }
  jindex <- numeric(n)
  for (i in 1:n) 
  {
    jindex[i] <- jpower[i]/sum(jpower)  
  }
  t$jindex <- jindex
  t
}

DeeganPackel <- function(t)
{
  findvar <- function (anc, l, pl) 
  {
    if (l == 0)
    {
      if (sum(t$Deputies[c(anc,pl)]) > maj)
      {
        dppower[c(anc,pl)] <<- dppower[c(anc,pl)] + 1 / minn
      } 
    } else
    {
      for (i in (pl+1):(n-l+1)) 
        findvar(c(anc,pl),l-1,i)
    }
  }
  n <- length(t$Faction)
  maj <- 226
  p <- t$Deputies
  s <- 0
  minn <- 0
  while (s <= maj)
  {
    s <- s + max(p)
    p[which.max(p)]<-0
    minn <- minn + 1
  }
  dppower <- numeric(n)
  findvar(numeric(0),minn,1)
  dpindex <- numeric(n)
  for (i in 1:n) 
  {
    dpindex[i] <- dppower[i]/sum(dppower)  
  }
  t$dpindex <- dpindex
  t
}
