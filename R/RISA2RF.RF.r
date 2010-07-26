###############################
###############################
##  Import the microarray dataset
##  Input: NA
##  Output: NA
###############################
###############################
"RISA2RF.RF"<-function(){
"RISA2RF.RF_temp"<-function(){
none<<-0 
aaa<<-objects(envir=.GlobalEnv)
tt <- tktoplevel()
tkwm.title(tt,"Transfert microbial profiles to RandForestGUI program")
tkgrid(tklabel(tt,text="                                                                                           ")) 
t5<-tkframe(tt)  
text5<-tklabel(t5,text="Where is your microbial profiles table ?")
sep<-aaa
sepe<-tkwidget(t5,"ComboBox",editable=FALSE,values=sep,height=length(sep))
tkpack(text5,sepe,side="left")
tkgrid(t5)
ess<-function(){
	   sep11<-unlist(as.numeric(tcl(sepe,"getvalue"))+1 );sep11<-aaa[sep11]
     if (sep11=="none") tkmessageBox(message="Error, you have to select a microbial profiles table")
     if (sep11=="none") stop()
     mat.analyse.RF<-as.matrix(get(sep11))
     mat.analyse.RF<<- mat.analyse.RF
     mat.analyse.RF.init<<-mat.analyse.RF
     print("successfull")
     tkdestroy(tt)
     rf()
     tk2notetab.select(nb, "Management")
      }
tkgrid(tklabel(tt,text="                                                                                           "))
tkgrid(tkbutton(tt,text="Import microbial profiles",command=ess))
tkgrid(tklabel(tt,text="                                                                                           "))
}

importGUI3<-function()
{
  fil=if (interactive()) choose.files(filters = Filters["All",],caption="Select ASCII files containing your microbial profiles table")
  tt<-tktoplevel()
  tkwm.title(tt,"Import an microbial profiles table")
  tkgrid(tklabel(tt,text="                                                                                            "))
  t1<-tkframe(tt)
  text1<-tklabel(t1,text="Each microbial profile is in")
  fa<-c("row","column")
  trans<-tkwidget(t1,"ComboBox",editable=FALSE,values=fa,height=2)
  tkpack(text1,trans,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text=" "))
  t2<-tkframe(tt)
  text2<-tklabel(t2,text="Do you have column header ?")
  he <- c("yes","no")
  heac <- tkwidget(t2,"ComboBox",editable=FALSE,values=he,height=2)
  tkpack(text2,heac,side="left")
  tkgrid(t2)
  t3<-tkframe(tt)
  text3<-tklabel(t3,text="Do you have row header ?")
  he <- c("yes","no")
  hear <- tkwidget(t3,"ComboBox",editable=FALSE,values=he,height=2)
  tkpack(text3,hear,side="left")
  tkgrid(t3)
  t4<-tkframe(tt)
  text4<-tklabel(t4,text="Choose the field separator")
  sep<-c("; (semi colon)",",  (comma)",".  (dot)")
  sepe<-tkwidget(t4,"ComboBox",editable=FALSE,values=sep,height=3)
  tkpack(text4,sepe,side="left")
  tkgrid(t4)
  t5<-tkframe(tt)
  text5<-tklabel(t5,text="Choose the decimal symbol")
  dec <- c(", (comma)",". (dot)")
  dece <- tkwidget(t5,"ComboBox",editable=FALSE,values=dec,height=2)
  tkpack(text5,dece,side="left")
  tkgrid(t5)
  mm<-function()
  {
    m1<-0
    m<-0
    trans1<- unlist(as.numeric(tcl(trans,"getvalue"))+1 )
    heac1 <-unlist(as.numeric(tcl(heac,"getvalue"))+1 )
      if(heac1==1) heac<-TRUE
      if(heac1==2) heac<-FALSE
    hear1 <-unlist(as.numeric(tcl(hear,"getvalue"))+1 )
      if(hear1==1) hear<-TRUE
      if(hear1==2) hear<-FALSE  
    sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
      if(sep1==1) sep<-";"
      if(sep1==2) sep<-","
      if(sep1==3) sep<-"."
    dec1 <-unlist(as.numeric(tcl(dece,"getvalue"))+1 )
      if(dec1==1) dec<-","
      if(dec1==2) dec<-"."
    m<-read.table(file=fil,colClasses = "character",dec=dec,sep=sep,h=heac)
      if (hear==TRUE) hear1<-m[[1]]
      if (hear==TRUE) m<-m[,-1]
    m1<-matrix(nr=dim(m)[1],nc=dim(m)[2])
    colnames(m1)<-colnames(m)
    for (i in 1:dim(m)[2])
    { 
      m1[,i]<-as.numeric(unlist(m))[c(dim(m)[1]*i-c(dim(m)[1]-1)):c(dim(m)[1]*i)]
    }
    if(hear==TRUE) rownames(m1)<-hear1
    if (trans1==2) m1<-t(m1)
    mat.analyse.RF.init<<-m1
    mat.analyse.RF<<-m1
    print("Successfully loaded!")
    tkdestroy(tt)
  }
  tkgrid(tklabel(tt,text=" "))
  t6<-tkframe(tt)
  b1<-tkbutton(t6,text="Import a microbial profiles table",command=mm)
  close<-function()
  {
    tkdestroy(tt)
  }
  b2<-tkbutton(t6,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=" "))
    try(edit(scan(file=fil,what="list")))
  tkfocus(tt)
}
choiceGUI("Import microbial profiles","Where is your microbial profiles table ?","1:in an external file,  2:always loaded in R environment")
if(nf==1)  importGUI3()
if(nf==2) RISA2RF.RF_temp()
}



   
    
    