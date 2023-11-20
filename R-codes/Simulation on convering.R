M=6

#Original hinge
d_f=rnorm(M,0,1)

#Total number of samples
N_x=250

# generating data from (M+1) dimentional space [-1,1]^(M+1)
D=matrix(0,nrow=(M+1),ncol=N_x)
for(j in 1:N_x)
{
  Z=rexp(M+1,1)
  #Z=runif((M+1),0,1)
  X=rep(0,M+1)
  X[1]=1
  for(i in 1:(M))
  {
    X[i+1]=Z[i+1]-Z[i]
  }
  D[,j]=X
}

k=0
while(k<(0.15*N_x) || k>(0.85*N_x))
{
  s=rnorm(1,0,1)
  d_k=c(s,d_f)
  N_k=d_k %*%D
  k=N_x-length(N_k[N_k<0])
}
d_f=c(s,d_f)

# Creating an initial hinge
d=rnorm(M,0,1)
k=0
# finding d(0) such that #{x_n:d.x_n>=0}=[N_x/2]
while(k!=(N_x/2))
{
  s=rnorm(1,0,1)
  d_k=c(s,d)
  N_k=d_k %*%D
  k=N_x-length(N_k[N_k<0])
}
d=c(s,d)

count=0
while(abs(sum(d*d_f)/(sqrt(sum(d*d))*sqrt(sum(d_f*d_f))))<0.99)
{ # Hinge findind Algorithm
  S_p=D[,which(d %*% D >=0)]
  S_n=D[,which(d %*% D <0)]

  S_11=S_p[,which(d_f %*% S_p>=0)]
  S_10=S_p[,which(d_f %*% S_p<0)]
  S_01=S_n[,which(d_f %*% S_n>=0)]
  S_00=S_n[,which(d_f %*% S_n<0)]


L_p = (S_p) %*% t(S_p)
L_n = (S_n) %*% t(S_n)
       
       L_11 = (S_11) %*% t(S_11)
       L_10 = (S_10) %*% t(S_10)
       L_01 = (S_01) %*% t(S_01)
       L_00 = (S_00) %*% t(S_00)

A=diag(1,nrow=(M+1),ncol=(M+1))-solve(L_p) %*% L_10-solve(L_n) %*% L_01
d=A %*%d
d=array(d)
# d=d[-1]
# k=0
# # finding d(0) such that #{x_n:d.x_n>=0}=[N_x/2]
# while(k!=(N_x/2))
# {
#   s=rnorm(1,0,1)
#   d_k=c(s,d)
#   N_k=d_k %*%D
#   k=N_x-length(N_k[N_k<0])
# }
# d=c(s,d)

count=count+1
}









 

