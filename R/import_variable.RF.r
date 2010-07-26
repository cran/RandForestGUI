###############################
###############################
##  Procedure to import variables
##  Input: NA
##  Output: NA
###############################
###############################
"import_variable.RF"<-function()
{
	import_variable.RF_temp<-function(){
  fil=if (interactive()) file.choose()
  tt<-tktoplevel()
	tkwm.title(tt,"Import categorical variables (ASCII Files)")
	tkgrid(tklabel(tt,text="                                                                                                            "))
	t1<-tkframe(tt)
	text1<-tklabel(t1,text="What is the field separator ?")
	sep<-c(";  (semicolon)",",  (comma)",".  (dot)")
	sepe<-tkwidget(t1,"ComboBox",editable=FALSE,values=sep,height=3)
	tkpack(text1,sepe,side="left")
	tkgrid(t1)
	t2<-tkframe(tt)
	text2<-tklabel(t2,text="What is the decimal symbol ?")
	dec <- c(",  (comma)",".  (dot)")
	dece <- tkwidget(t2,"ComboBox",editable=FALSE,values=dec,height=2)
	tkpack(text2,dece,side="left")
	tkgrid(t2)
	t3<-tkframe(tt)
	text3<-tklabel(t3,text="Do you have header ?")
	he <- c("Yes","No")
	hea <- tkwidget(t3,"ComboBox",editable=FALSE,values=he,height=2)
	tkpack(text3,hea,side="left")
	tkgrid(t3)
	import.file<-function()
	{
		sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
		if(sep1==1) sep<-";"
		if(sep1==2) sep<-","
		if(sep1==3) sep<-"."
		dec1 <-unlist(as.numeric(tcl(dece,"getvalue"))+1 )
		if(dec1==1) dec<-","
		if(dec1==2) dec<-"."
		he1 <-unlist(as.numeric(tcl(hea,"getvalue"))+1 )
		if(he1==1) he<-TRUE
		if(he1==2) he<-FALSE
    fact<-read.table(fil,header=he,dec=dec,sep=sep)
		rownames(fact)<-rownames(mat.analyse.RF)
	  if(dim(fact)[1]!=dim(mat.analyse.RF)[1]) stop("Errors: Number of categorical variables differs of this of microbial profiles")
		for (i in 1:dim(fact)[2]) {fact[i]<-factor(fact[[i]])}
	fact.RF<<-fact
  fact.RF.init<<-fact
     ###identification des faux facteurs
aa<-vector(length=1:dim(fact.RF.init)[2])
for (i in 1:dim(fact.RF.init)[2]){aa[i]<-length(levels(fact.RF.init[,i]))}
aa=which(aa==dim(fact.RF.init)[1])
fact.RF.init=fact.RF.init[,-aa]
fact.RF=fact.RF[,-aa]
fact.RF<<-fact.RF
fact.RF.init<<-fact.RF.init
print("categorical variables have been successfully loaded!")
	tkdestroy(tt)
	tkdestroy(tt)
	rf()
	}
	tkgrid(tklabel(tt,text="  "))
	t5<-tkframe(tt)
	b1<-tkbutton(t5,text="Import categorical variables",command=import.file)
	b2<-tkbutton(t5,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t5)
	tkgrid(tklabel(tt,text="   "))
		try(edit(scan(file=fil,what="list")))
	tkfocus(tt)
}


import_variable.RF_Robject<-function(){
none<<-0 
aaa<<-objects(envir=.GlobalEnv)
tt <- tktoplevel()
tkwm.title(tt,"Transfert categorical variables to RandForestGUI program")
tkgrid(tklabel(tt,text="                                                                                           ")) 
t5<-tkframe(tt)  
text5<-tklabel(t5,text="Select your categorical variables ?")
sep<-aaa
sepe<-tkwidget(t5,"ComboBox",editable=FALSE,values=sep,height=length(sep))
tkpack(text5,sepe,side="left")
tkgrid(t5)
ess<-function(){
	   sep11<-unlist(as.numeric(tcl(sepe,"getvalue"))+1 );sep11<-aaa[sep11]
     if (sep11=="none") tkmessageBox(message="Error, you have to select categorical variables")
     if (sep11=="none") stop()
    fact.RF<<-as.data.frame(get(sep11))
    fact.RF.init<<-as.data.frame(get(sep11))
     ###identification des faux facteurs
aa<-vector(length=1:dim(fact.RF.init)[2])
for (i in 1:dim(fact.RF.init)[2]){aa[i]<-length(levels(fact.RF.init[,i]))}
aa=which(aa==dim(fact.RF.init)[1])
fact.RF.init=fact.RF.init[,-aa]
fact.RF=fact.RF[,-aa]
fact.RF<<-fact.RF
fact.RF.init<<-fact.RF.init
print("categorical variables have been successfully loaded!")
tkdestroy(tt)
rf()
tk2notetab.select(nb, "Management")
}
tkgrid(tklabel(tt,text="                                                                                           "))
tkgrid(tkbutton(tt,text="Import categorical variables",command=ess))
tkgrid(tklabel(tt,text="                                                                                           "))
}
choiceGUI("Import categorical variable","Where is your categorical variables","1=in an external file,  2=always loaded in R environment")
if(nf==1)  import_variable.RF_temp()
if(nf==2) import_variable.RF_Robject()
}