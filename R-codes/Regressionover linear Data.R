N_x=1000
x1=runif(N_x,-1,1)
x0=rep(1,len=N_x)
e=rnorm(1000,0,0.1)
y=x1+1+e

regress=data.frame("x"=x1,"y"=y)
D=rbind(x0,x1)
p<-ggplot(NULL,aes(x,y))+
  geom_line(data=regress)+
  expand_limits(x=0,y=0)+
  xlim(min(x)-0.5,max(y)+0.5)+
  ylim(min(y)-0.5,max(y)+0.5)+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

d=c(1/2,3/2)

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

for(i in 1:100)
{S_p=D[,which(d %*% D >=0)]
S_n=D[,which(d %*% D <0)]

y_p=y[which(d%*%D>=0)]
y_n=y[which(d%*%D<0)]

L_p = (S_p) %*% t(S_p)
L_n = (S_n) %*% t(S_n)

b_p= solve(L_p) %*% S_p %*% y_p
b_n= solve(L_n) %*% S_n %*% y_n

y_hp=t(S_p) %*% b_p
y_hn=t(S_n) %*% b_n

data_p=data.frame("x"=S_p[2,],"y"=y_hp)
data_n=data.frame("x"=S_n[2,],"y"=y_hn)

p1=p+
  geom_line(data=data_p, col="blue")+
  geom_line(data=data_n, col="red")
p1

d=b_p-b_n
d=array(d)}
