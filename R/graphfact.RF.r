###############################
###############################
##  function to plot diversity with one or two factors
##  Input: NA
##  Output: NA
###############################
###############################

"graphfact.RF" <-function ()
{
param<-diversity.RF
fact<-fact.RF

  tt <- tktoplevel()
  tkwm.title(tt, "Plot of diversity index")
  tkgrid(tklabel(tt, text = "                                                                                      "))
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2, text = "Which diversity index do you want to plot ?")
  valparam <- tkwidget(tt2, "ComboBox", editable = FALSE, values = c(colnames(param)),height=c(length(colnames(param))),width=20)
  tkpack(text2,valparam,side="left")
  tkgrid(tt2)
  
  tkgrid(tklabel(tt,text="  "))
   tt3<-tkframe(tt)
  text3<-tklabel(tt3, text = "What is your 1st categorical variable ?")
  valfact1 <- tkwidget(tt3, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
  tkpack(text3,valfact1,side="left")
  tkgrid(tt3)
  
  tkgrid(tklabel(tt,text="  "))
   tt4<-tkframe(tt)
  text4<-tklabel(tt4, text = "What is your 2nd categorical variable ?")
  valfact2 <- tkwidget(tt4, "ComboBox", editable = FALSE, values = c("none",names(fact)),height=c(length(names(fact))+1),width=20)
  tkpack(text4,valfact2,side="left")
  tkgrid(tt4) 
  
  tkgrid(tklabel(tt,text="  "))
   tt5<-tkframe(tt)
  text5<-tklabel(tt5, text = "What kind of plot do you want ?")
  plotype <- tkwidget(tt5, "ComboBox", editable = FALSE, values = c("Points","Points and lines","boxplot","histogram"),height=4,width=20)
  tkpack(text5,plotype,side="left")
  tkgrid(tt5)  
  
   mm <- function() 
  {
    while(dev.cur()!=1) dev.off()
    valparam <- unlist(as.numeric(tcl(valparam, "getvalue")) + 1)
    valfact1 <- unlist(as.numeric(tcl(valfact1, "getvalue")) + 1)
    valfact2 <- unlist(as.numeric(tcl(valfact2, "getvalue")) + 1)
    plotype <- unlist(as.numeric(tcl(plotype, "getvalue")) + 1)
pp<-param[,valparam]
names(pp)<-colnames(param)[valparam]
    pp<<-pp
            casun<-function(){
            #fig_par(typee=1)
             print(paste("statistical analysis for",names(pp),"parameter"))
             print(Anova(aov(pp~fact[,valfact1]),type=2))
             print(TukeyHSD(aov(pp~fact[,valfact1]),type=2))
           
            if(plotype==2) lineplot.CI(fact[,valfact1],pp,xlab=names(fact)[valfact1],ylab=names(pp))
            if(plotype==1) lineplot.CI(fact[,valfact1],pp,type="p",xlab=names(fact)[valfact1],ylab=names(pp))           
            if(plotype==3)  boxplot(pp~fact[,valfact1],xlab=names(fact)[valfact1],ylab=names(pp))   ####
            if(plotype==4) bargraph.CI(fact[,valfact1],pp,xlab=names(fact)[valfact1],ylab=names(pp))
            if(plotype==2){  ##aj des segments
              aa<-split(pp,fact[,valfact1])
              bb<-vector(length=length(aa))
              for (i in 1:length(aa)){bb[i]<-mean(aa[[i]])}
              cc<-1:length(bb)
              dd<-which(bb!="NA")
              cc<-cc[dd];bb<-bb[dd]
              for(i in 1:c(length(dd)-1)){
              segments(cc[i],bb[i],cc[c(i+1)],bb[c(i+1)]) }
              }
              ##aj des segments 
           }
   
           casdeux<-function(){
           
             
            if(plotype==4) bargraph.CI(fact[,valfact1],pp,group=fact[,c(valfact2-1)],xlab=names(fact)[valfact1],ylab=names(pp),legend=TRUE)
            
            casdeuxd<-function(){
            lev<-levels(fact[,c(valfact2-1)])
            mm<-vector(length=2);mm[1]<-min(na.omit(pp));mm[2]<-max(na.omit(pp))
            for (i in 1:length(lev)){
              nn<-which(fact[,c(valfact2-1)]==lev[i])
              ppp<-pp[nn]
              factt<-fact[nn,valfact1]
              #fig_par(typee=1)
              if(plotype==2) lineplot.CI(factt,ppp,pch=2,xlab=names(fact)[valfact1],ylab=names(pp),ylim=c(mm[1],mm[2]),col=i,err.width=0.03,main=names(pp)[1])
              if(plotype==1) lineplot.CI(factt,ppp,pch=2,type="p",xlab=names(fact)[valfact1],ylab=names(pp),ylim=c(mm[1],mm[2]),col=i,err.width=0.03,main=names(pp)[1])           
              if(plotype==3)  boxplot(ppp~factt,xlab=names(fact)[valfact1],ylab=names(pp),ylim=c(mm[1],mm[2]),col=i,border=i)
              
              legend("topright",lev,lty=rep(1,length(lev)),col=c(1:length(lev)))
              if(plotype==2) {
              ##aj des segments
              aa<-split(ppp,factt)
              bb<-vector(length=length(aa))
              for (j in 1:length(aa)){bb[j]<-mean(aa[[j]])}
              cc<-1:length(bb)
              dd<-which(bb!="NA")
              cc<-cc[dd];bb<-bb[dd]
              for(j in 1:c(length(dd)-1)){
              segments(cc[j],bb[j],cc[c(j+1)],bb[c(j+1)],col=i) }
              }
              ##aj des segments
              if(i<=c(length(lev)+1)) par(new=TRUE)}
             }
          if(plotype!=4)casdeuxd()          
                      }   
   if(valfact2==1) casun()         
   if(valfact2!=1) casdeux()          
   tkfocus(tt)
  }


 

  tkgrid(tklabel(tt, text = " "))
  ff1<-tkframe(tt)
  b1<- tkbutton(ff1, text = "Plot", command = mm)
 
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(ff1,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(ff1)
  tkgrid(tklabel(tt, text = ""))
  tkfocus(tt)

}





