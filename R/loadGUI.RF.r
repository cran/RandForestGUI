###############################
###############################
##  Load a RandForestGUI project
##  Input: NA
##  Output: NA
###############################
###############################
"loadGUI.RF"<-function(type)
{
dev_delete()
	if (type==1) fil=if (interactive()) file.choose()
	if (type==2) fil=filename.RF
	tt1 <- tktoplevel()
	tkwm.title(tt1,"Loading")
	tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
	tkfocus(tt1)
	tkconfigure(tt1)
	load(file=fil)
 	if (type==1) print("Your project has been successfully loaded")
	tkdestroy(tt1)
}
