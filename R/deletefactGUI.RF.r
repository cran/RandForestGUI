###############################
###############################
##  Select or deselection of  profiles using levels of factor
##  Input: NA
##  Output: NA
###############################
###############################
"selection.RF"<-function(){

"deletefactGUI.RF"<-function()
{ thhh<-tktoplevel()
  tkwm.title(thhh, "Selection of microbial profiles according to categorical variables")
  tkgrid(tklabel(thhh, text = "                                                                                                                                                          "))
 
  select.factor<-function()
  {tt <- tktoplevel()
    tkwm.title(tt, "Select microbial profiles according to categorical variables")
    tkgrid(tklabel(tt, text = "                                                                                                                                                          "))
    tt2<-tkframe(tt)
    text2<-tklabel(tt2, text = "Choose the categorical variables:")
    repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact.RF),height=length(names(fact.RF)),width=20)
    tkpack(text2,repee,side="left")
    tkgrid(tt2)
    select.level<-function ()
    {
      th <- tktoplevel()
      tkwm.title(th, "Select microbial profiles according to categorical variables")
      tkgrid(tklabel(th, text = "                                                                                                                                                          "))
      tkgrid(tklabel(th, text = "Which level of the categorical variable ?"))
      repe=levels(fact.RF[,ae])
      repee <- tkwidget(th, "ComboBox", editable = FALSE, values = repe)
      tkgrid(repee)
       tkgrid(tklabel(th, text = "                                                                                                                                                          "))
      tkgrid(tklabel(th, text = "Keep or delete ?"))
      repea=c("Keep","Delete")
      repeaa <- tkwidget(th, "ComboBox", editable = FALSE, values = repea)
      tkgrid(repeaa)
      window.factor <- function() 
      {
        repee <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
        typp <- unlist(as.numeric(tcl(repeaa, "getvalue")) + 1)
        
	relevels <- function(ff) {
		temp<-ff
		for (i in 1:length(ff)){temp[,i]<-factor(as.character(ff[,i])) }
		return(temp)
		} 
        delete.selected<-function()
		{
          sel=which(fact.RF[,ae]==levels(fact.RF[,ae])[repee])
          fact.RF<-fact.RF[-sel,] 
          fact.RF<-relevels(fact.RF)
          fact.RF<<-fact.RF 
          mat.analyse.RF<<-mat.analyse.RF[-sel,]
          zero<-which(apply(mat.analyse.RF,2,sum)==0)
        if(length(zero)!=0) mat.analyse.RF<<-mat.analyse.RF[,-zero]
          if(exists("param.RF")) param.RF<<-param.RF[-sel,]
	    }
        keep.selected<-function()
		{
          sel=which(fact.RF[,ae]==levels(fact.RF[,ae])[repee])
          fact.RF<-fact.RF[sel,] 
          fact.RF<-relevels(fact.RF)
          fact.RF<<-fact.RF 
          mat.analyse.RF<<-mat.analyse.RF[sel,] 
          zero<-which(apply(mat.analyse.RF,2,sum)==0)
          if(length(zero)!=0) mat.analyse.RF<<-mat.analyse.RF[,-zero]
          if(exists("param.RF")) param.RF<<-param.RF[sel,]
	    }
        if (typp==1) keep.selected()
        if (typp==2) delete.selected()
        tkdestroy(th)
        if (typp==2) tkmessageBox(message="Microbial profiles successfully deleted")
        if (typp==1) tkmessageBox(message="Microbial profiles successfully selected")
      tkdestroy(tt)
      rf()
      }
      ff<-tkframe(th)
      tkgrid(tklabel(th, text = ""))
      b1<- tkbutton(ff, text = "OK", command = window.factor)
      b2<-tkbutton(ff,text="Cancel",command=function() tkdestroy(th))
      tkpack(b1,b2,side="left")
      tkgrid(ff)
      tkgrid(tklabel(th, text = ""))
      tkfocus(th)
    }
    window.level <- function()
    {
      sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
      ae<<-sel
      select.level()
      tkdestroy(tt)
    }
    tkgrid(tklabel(tt, text = " "))
    ff1<-tkframe(tt)
    b1<- tkbutton(ff1, text = "OK", command = window.level)
	b2<-tkbutton(ff1,text="Cancel",command=function() tkdestroy(tt))
    tkpack(b1,b2,side="left")
    tkgrid(ff1)
    tkgrid(tklabel(tt, text = ""))
    tkfocus(tt)
  }
  thhh2<-tkframe(thhh)
  tkgrid(tkbutton(thhh2,text="Step 2 : Select levels of the categorical variable",command=select.factor))
  tkgrid(thhh2)
  tkgrid(tklabel(thhh, text = "  "))
  tkgrid(tkbutton(thhh,text="Cancel",command=function() tkdestroy(thhh)))
  tkgrid(tklabel(thhh,text=""))
}
"deselect.RF"<-function(){
 if(exists("mat.analyse.RF.init")) mat.analyse.RF<<-mat.analyse.RF.init
 if(exists("fact.RF.init"))   fact.RF<<-fact.RF.init
 if(exists("param.RF.init"))  param.RF<<-param.RF.init
 tkmessageBox(message="All microbial profiles have been selected")
 tkdestroy(tt)
 RandForestGUI()
}
choiceGUI("Selection of microbial profiles","What do you want to do ?","1:select microbial profiles,  2:select all microbial profiles")
if(nf==1)  deletefactGUI.RF()
if(nf==2) deselect.RF()

}

