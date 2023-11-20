x=runif(1000,min = 0,max=2*pi)
y=sin(x)

data=data.frame(x,y)
N_x=length(data$x)
x0=rep(1,len=length(x))
x1=data$x
D=rbind(x0,x1)

new_data=data
# Repeat the process until get a convergence. Need improvement in code.
t=HFA(new_data)
t$plot
t$b_p
t$b_n
t$type
h_p=t(D) %*% (t$b_p)
h_n=t(D) %*% (t$b_n)

h=pmin(h_p,h_n)

y1=data$y-h
new_data$y=y1

t=HFA(new_data)
t$plot
t$b_p
t$b_n
t$type
h_p=t(D) %*% (t$b_p)
h_n=t(D) %*% (t$b_n)

h=pmax(h_p,h_n)

y1=data$y-h
new_data$y=y1


# final Answer for sinusodial function without jittering
h1_p<-c(7.213718,-1.655076)%*%D
h1_n<-c( 0.5436121, -0.2099680)%*%D

h1=pmax(h1_p,h1_n)

h2_p<-c(-4.6667782, 0.8455701)%*%D
h2_n<-c(-7.100882,2.309253)%*%D

h2=pmin(h2_p,h2_n)

h=h2

plot(x1,y)
final=data.frame(x,"y"=h[1,])

p<-ggplot(NULL,aes(x,y))+
  geom_point(data=data)+
  geom_line(data=final)+
  expand_limits(x=0,y=0)+
  xlim((min(x1)-0.5),(max(x1)+0.5))+
  ylim((min(y)-0.5),(max(y)+0.5))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

