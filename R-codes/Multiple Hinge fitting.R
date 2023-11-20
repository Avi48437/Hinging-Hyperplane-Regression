HFAn<-function(data,k,mode=c("Normal","Damped","New"))
{
  Fit<-c("Linear"="#0080FF","Hinge"="#460083")
  #Number of hinges is k
  y=data$y
  x1=data$x
  N_x=length(x1)
  x0=rep(1,len=N_x)
  
  mode=match.arg(mode)
  regress=data.frame("x"=x1,"y"=y)
  D=rbind(x0,x1)
  p<-ggplot(NULL, aes(x,y))+
    geom_point(data=regress,col="#FF6666",size=0.5)+
    stat_smooth(data = regress,method="lm"
                ,aes(color="Linear"))+
    expand_limits(x=0,y=0)+
    geom_hline(aes(yintercept = 0))+
    geom_vline(aes(xintercept = 0))
  
 print(p)
 
  if(k==1)
  {
    if(mode=="Normal"){t=HFA(data)
    }else if(mode=="Damped"){fin=damped_HFA(res_data,1/2)
    }else if(mode=="New"){t=New_HFA(data,1/2)}
    
    H=list()
    h_p=t(D) %*% (t$b_p)
    h_n=t(D) %*% (t$b_n)
    if(t$type=="max")
    {h=pmax(h_p,h_n)
    }else if(t$type=="min"){
    h=pmin(h_p,h_n)}
    y1=data$y-h
    res_data=data
    res_data$y=y1
    
    #list of hinge creation
    H_1=list("b_p"=t$b_p,"b_n"=t$b_n,"type"=t$type)
    H[[1]]=H_1
    names(H)[1]="H1"
    
    #Hinge data creation
    data_h=data.frame("H1"=h)
    
    #plot
    res_data=data
    res_data$y=data$y-rowSums(data_h)
    final=data.frame("x"=data$x,"y"=rowSums(data_h))
    
    p<-ggplot(NULL,aes(x,y))+
      geom_point(data=regress,col="#FF6666",size=0.5)+
      geom_smooth(method='lm',aes(color="Linear"))+
      expand_limits(x=0,y=0)+
      geom_hline(aes(yintercept = 0))+
      geom_vline(aes(xintercept = 0))+
      geom_line(data=final,size=1,aes(color="Hinge"))+
      scale_color_manual("Fit",values = Fit)+
      theme(legend.title=element_text(size=20),
            legend.text=element_text(size=14))+
      theme(legend.position = "top")
    
    return(list("res_data"=res_data,"data_h"=data_h,"hinge_list"=H,"plot"=p))
  }else{
    t=HFAn(data,(k-1),mode=mode)
    res_data=t$res_data
    data_h=t$data_h
    H=list()
    for(i in 1:(k-1))
    {
      H[[i]]=t$hinge_list[[i]]
    }
    H_names=names(t$hinge_list)
    H_names=c(H_names,paste0("H",k))
      
    if(mode=="Normal"){fin=HFA(res_data)
    }else if(mode=="Damped"){fin=damped_HFA(res_data,1/2)
    }else if(mode=="New"){fin=New_HFA(res_data,1)}

    h_p=t(D) %*% (fin$b_p)
    h_n=t(D) %*% (fin$b_n)
    if(fin$type=="max")
    {h=pmax(h_p,h_n)
    }else{h=pmin(h_p,h_n)}
    
    data_h=cbind(data_h,h)
    colnames(data_h)[k]=paste0("H",k)
    H[[k]]=list("b_p"=fin$b_p,"b_n"=fin$b_n,"type"=fin$type)
    names(H)=H_names
    
    user_input <- "y"
    
    while(user_input == "y")
    {
      for(i in 1:k)
      {
        res_data=data
        res_data$y=data$y-rowSums(data_h)
        res_data$y=res_data$y+data_h[,i]
        temp=HFAn(res_data,1)
        H[[i]]=temp$hinge_list[[1]]
        data_h[,i]=temp$data_h[,1]
      }
      user_input <- readline("Do you want to continue (yes/no)? ")
      if (tolower(user_input) != "y" && tolower(user_input) != "n") {
        print("Please enter either 'yes' or 'no'")
        user_input <- "y"
      }
    }
   
    res_data=data
    res_data$y=data$y-rowSums(data_h)
    final=data.frame("x"=data$x,"y"=rowSums(data_h))
    
    p<-ggplot(NULL,aes(x,y))+
      geom_point(data=regress,col="#FF6666",size=0.5)+
      geom_smooth(method='lm',aes(color="Linear"))+
      expand_limits(x=0,y=0)+
      geom_hline(aes(yintercept = 0))+
      geom_vline(aes(xintercept = 0))+
      geom_line(data=final,size=1,aes(color="Hinge"))+
      scale_color_manual("Fit",values = Fit)+
      theme(legend.title=element_text(size=20),
            legend.text=element_text(size=14))+
      theme(legend.position = "top")
    
    
    return(list("res_data"=res_data,"data_h"=data_h,"hinge_list"=H,"plot"=p)) 
  }
}
