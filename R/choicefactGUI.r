###############################
###############################
##  Produce simple Tcl TK GUI
##  Input: text1=title of the tcltk windows, text2=Question for the user, text3=Possibility of answer for the user
##  Output: nf=a numeric value corresponding to the number enters by the user
###############################
###############################

"choicefactGUI" <- function(text1,text2,text3){
tf <- tktoplevel()
tkwm.title(tf,text1)
nfvar <- tclVar()
done <- tclVar(0)
"getnf" <- function(){
nf  <- tclvalue(nfvar)
nf<<-nf
tkdestroy(tf)}
frame1 <- tkframe(tf, relief="groove", borderwidth=2)
nf.entry <- tkentry(frame1, textvariable=nfvar)
tk2tip(nf.entry, text3)
submit.but <- tkbutton(frame1, text="Choose", default="active", command=function() getnf())
tkgrid(tklabel(frame1,text=text2), nf.entry)
tkgrid(tklabel(frame1,text=text3))
tkgrid(submit.but)
tkpack(frame1, fill = "x")
tkfocus(nf.entry)
tkwait.variable(nfvar)
tkbind(tf, "<Destroy>", function() tclvalue(done) <- 2)
tkbind(tf, "<KeyPress-Return>", function() getnf())
tkfocus(nf.entry)
tkwait.variable(done)
if(tclvalue(done) == "2") return(nf)
}

