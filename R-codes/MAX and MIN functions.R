MAX<-function(h_p,h_n,D)
{
  return(pmax(h_p(x),h_n(x)))
}

MIN<-function(h_p,h_n,D)
{
  return(pmin(h_p(x),h_n(x)))
}
