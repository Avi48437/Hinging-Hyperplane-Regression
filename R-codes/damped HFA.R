damped_HFA<-function(data,u)
{
  Fit<-c("Linear"="#0080FF","Hinge"="#460083")
  y=data$y
  x1=data$x
  N_x=length(x1)
  x0=rep(1,len=N_x)
  
  regress=data.frame("x"=x1,"y"=y)
  D=rbind(x0,x1)
  p<-ggplot(NULL,aes(x,y))+
    geom_point(data=regress,col="#FF6666",size=0.5)+
    geom_smooth(method='lm',aes(color="Linear"))+
    expand_limits(x=0,y=0)+
    geom_hline(aes(yintercept = 0))+
    geom_vline(aes(xintercept = 0))
  
  #finding d(0) such that #{x_n:d.x_n>=0}=[N_x/2]
  d=rnorm(1,0,1)
  k=0
  while(k<(0.15*N_x) || k>(0.85*N_x))
  {
    s=rnorm(1,0,1)
    d_k=c(s,d)
    N_k=d_k %*%D
    k=N_x-length(N_k[N_k<0])
  }
  d_p=c(s,d)

  d=rnorm(1,0,1)
  k=0
  while(k<(0.15*N_x) || k>(0.85*N_x))
  {
    s=rnorm(1,0,1)
    d_k=c(s,d)
    N_k=d_k %*%D
    k=N_x-length(N_k[N_k<0])
  }
  d_n=c(s,d)
  dd=t(t(c(d_p,d_n)))
  d=d_p-d_n
  RSS_arr=c(1,1)
  #while(length(RSS_arr)<4 || (RSS_arr[length(RSS_arr)-1]/RSS_arr[length(RSS_arr)])!=1)
  for(i in 1:(10*1/u))
  {
    S_p=0
    S_n=0
    if(ncol(D[,which(d %*% D >=0)])>0)
    {
      S_p=D[,which(d %*% D >=0)]
    }
    if(ncol(D[,which(d %*% D <0)])>0)
    {
      S_n=D[,which(d %*% D <0)]
    }
    
    if(all(S_p==0)==FALSE){
      y_p=y[which(d%*%D>=0)]
      L_p = (S_p) %*% t(S_p)
      b_p= ginv(L_p) %*% S_p %*% y_p
      y_hp=t(S_p) %*% b_p
      data_p=data.frame("x"=S_p[2,],"y"=y_hp)
    }else{
      b_p=c(0,0)
    }
    if(all(S_n==0)==FALSE){
      y_n=y[which(d%*%D<0)]
      L_n = (S_n) %*% t(S_n)
      b_n= ginv(L_n) %*% S_n %*% y_n
      y_hn=t(S_n) %*% b_n
      data_n=data.frame("x"=S_n[2,],"y"=y_hn)
    }else{
      b_n=c(0,0)
    }
    
    data_pn=rbind(data_p,data_n)
    p1=p+
      geom_line(data=data_pn,size=1,aes(color="Hinge"))+
      scale_color_manual("Fit",values = Fit)+
      theme(legend.title=element_text(size=20),
            legend.text=element_text(size=14))
    
    dd_BR=t(t(c(b_p,b_n)))
    dd=dd+u*(dd_BR-dd)
    
    d=dd[1:2,]-dd[3:4,]
    d=array(d)
    RSS_1=RSS(y_p,y_hp)+RSS(y_n,y_hn)
    RSS_arr=c(RSS_arr,RSS_1)
  }
  TRUE(all(S_p)=0)
  TRUE(all(S_n)=0)
  if(y_hp[1]==S_p[,1]%*%b_n){
    if(y_hp[2]>S_p[,2]%*%b_n){
      return(list("b_p"=b_p,"b_n"=b_n,"delta"=d,"type"="max","plot"=p1))
    }else{return(list("b_p"=b_p,"b_n"=b_n,"delta"=d,"type"="min","plot"=p1))}
  }else{
    if(y_hp[1]>S_p[,1]%*%b_n){
      return(list("b_p"=b_p,"b_n"=b_n,"delta"=d,"type"="max","plot"=p1))
    }else{return(list("b_p"=b_p,"b_n"=b_n,"delta"=d,"type"="min","plot"=p1))}
  }
}

for(i in 1:1000)
{
  t=damped_HFA(data,1/2)
}
