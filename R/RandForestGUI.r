###############################
###############################
##  Main tcl tk interface to start the program
##  Input: NA
##  Output: NA
###############################
###############################
"RandForestGUI"<-function(){
tclRequire("BWidget") 
tt <- tktoplevel()
tkwm.title(tt,"Conditional inference random Forest analysis of microbial profile (taxonomic sequences, fingerprints and microarrays)")
###colors
if(!exists("colo_fig_par")) colo_fig_par<<-c("white","black")
##language
if(exists("curlang")) setLanguage(curlang)
##memory
if(exists("mem_lim")) memory.limit(size=mem_lim)
###onglet
nb <- tk2notebook(tt, tabs = c("Management","Unsupervised random forest","Supervised random forest","Supervised random forest with top sufficient taxonomic units","Supplementary analysis","Miscellaneous"))
tkpack(nb, fill = "both", expand = 1)
nb<<-nb
##partie 1
tb1 <- tk2notetab(nb,"Management")
tk2tip(tb1, "Tools to import and select microbial profiles \nand to save and load a RandForestGUI project")
tkgrid(tklabel(tb1,text="                                                               ")) 
a1<-tkbutton(tb1,text="Import microbial profile table",command=RISA2RF.RF)
tkgrid(a1)
tk2tip(a1, "Import microbial profiles table \npresent either in an external ASCII file (.txt, .csv etc) \nor in R environment as an object")
a2<-tkbutton(tb1,text="Import categorical variables",command=import_variable.RF)
tkgrid(a2)
tk2tip(a2, "Import categorical variables corresponding to the microbial profiles.\n These categorical variables can be present either in an external ASCII file (.txt, .csv etc)\n or in R environment as an object")
tkgrid(tklabel(tb1,text="                                                                                                                                                                                                                                                                                                                                                                              "))
saveGUII.RF<-function(){saveGUI.RF(typesave=1)}
a3<-tkbutton(tb1,text="Save",command=saveGUII.RF)
tkgrid(a3)
tk2tip(a3, "Save or save as a RandForestGUI project \nin a specific directory")
loadGUI.RFF<-function(){loadGUI.RF(type=1)}
a4<-tkbutton(tb1,text="Load",command=loadGUI.RFF)
tkgrid(a4)
tk2tip(a4, "Import a RandForestGUI project \npreviously saved")
tkgrid(tklabel(tb1,text="                                                               "))
a5<-tkbutton(tb1,text="Selection of microbial profiles",command=selection.RF)
tkgrid(a5)
tk2tip(a5, "Select specific microbial profiles \nto perform random forest")
tkgrid(tklabel(tb1,text="                                                               "))
tb1<<-tb1  
##partie 2
tb2 <- tk2notetab(nb,"Unsupervised random forest")
tkgrid(tklabel(tb2,text="                                                                                                                                                                                                                                                                                                                                                                              "))
a6<-tkbutton(tb2,text="Choose unsupervised mode (1st step)",command=unsup.mode.RF)
tkgrid(a6)
tk2tip(a6, "Choose between the Addcl1 sampling method \n(synthetic data are added by randomly sampling from the product of empirical marginal distributions of the variables) \nand Addcl2 sampling method (synthetic data are added by randomly sampling \nfrom the hyperrectangle that contains the observed data, that is, the variables of synthetic observations \nhave a uniform distribution with range determined by the minimum and maximum of the corresponding observed variable)")
a7<-tkbutton(tb2,text="Best mtry determination (2nd step)",command=URF_tune_mtry)
tkgrid(a7)
tk2tip(a7, "The accuracy of random forest depends of the mtry value. \nThis function determines the best mtry for the unsupervised random forest analysis on your data set")
a8<-tkbutton(tb2,text="Calculate/plot unsupervised random forest (3rd step)",command=several_URF)
tkgrid(a8)
tk2tip(a8, "Calculate and/or plot unsupervised random forest")
a9<-tkbutton(tb2,text="4th step: Microbial cluster determination",command=Davies_Bouldin_URF)
tkgrid(a9)
tk2tip(a9, "Determines microbial clusters with the k-means or PAM procedures and the best number of microbial clusters using Davies Bouldin index")
tkgrid(tklabel(tb2,text="                                                               "))
tb2<<-tb2
tk2tip(tb2, "Perform conditional inference unsupervised random forest \nand determine clusters with K-means and PAM \n(Partitioning Around Medoids)") 
##partie 3
tb3 <- tk2notetab(nb,"Supervised random forest")
tkgrid(tklabel(tb3,text="                                                                                                                                                                                                                                                                                                                                                                              "))
a10<-tkbutton(tb3,text="Best mtry determination (1st step)",command=SRF.tune.mtryGUI)
tkgrid(a10)
tk2tip(a10, "The accuracy of random forest depends of the mtry value. \nThis function determines the best mtry for the supervised random forest analysis on your data set")
a11<-tkbutton(tb3,text="Calculate/plot supervised random forest (2nd step)",command=supRFGUI.RF)
tkgrid(a11)
tk2tip(a11, "Calculate and/or plot supervised random forest")
a12<-tkbutton(tb3,text="Taxonomic units importance (3rd step)",command=MDA.RF)
tkgrid(a12)
tk2tip(a12, "Calculate the mean decrease accuracy of taxonomic units")
a131<-tkbutton(tb3,text="Significance of taxonomic units importance (Optional)",command=perm.MDA.init)
tkgrid(a131)
tk2tip(a131, "Calculates p values of taxonomic units mean decrease accuracy using Monte Carlo permutations. This procedure may be long.")

a13<-tkbutton(tb3,text="Backward taxonomic units selection (4th step)",command=new.SRF.OOB)
tkgrid(a13)
tk2tip(a13, "Performed a backward selection method by iteratively deleting the less important taxonomic units \naccording to their mean decrease in accuracy and at each step calculating a new random forest. \nThis calculation is needed to choice the top sufficient taxonomic units \nof the next menu 'Supervised random forest with taxonomic units selection'. \nThis calculation is very long!")
tkgrid(tklabel(tb3,text="                                                               "))
#tkgrid(tkbutton(tb3,text="Decision tree",command=SRF_init_rpart))
tb3<<-tb3 
##partie 4
tb4 <- tk2notetab(nb,"Supervised random forest with top sufficient taxonomic units")
tkgrid(tklabel(tb4,text="                                                                                                                                                                                                                                                                                                                                                                              "))
a14<-tkbutton(tb4,text="Top sufficient taxonomic units selection (1st step)",command=plot_adjust.numb.var)
tkgrid(a14)
tk2tip(a14, "Determines a small set of taxonomic units sufficient to discriminate microbial clusters. \nIt leads to identify taxonomic units specifics to each cluster. \nThe random forest model with the lowest number of taxonomic units and the lowest out of bag was retained. \nTo comfort the robustness of this approach, two subsequent analyses were performed. \nA Pearson correlation was calculated between the initial random forest distances (all taxonomic units) and the new random forest distances \nand a filtered method using with the Kruskal Wallis test performed for each selected taxonomic unit. \nThe choice of the number of taxonomic units is determined according to the lowest out of bag and the best correlation.")
a15<-tkbutton(tb4,text="Best mtry determination (2nd step)",command=SRF.fin.tune.mtryGUI )
tkgrid(a15)
tk2tip(a15, "The accuracy of random forest depends of the mtry value. \nThis function determines the best mtry for the supervised random forest analysis with the to sufficient taxonomic units")
a16<-tkbutton(tb4,text="Calculate/plot supervised random forest (3rd step)",command=supRFGUI.fitted.RF)
tkgrid(a16)
tk2tip(a16, "Calculate and/or plot supervised random forest")
a17<-tkbutton(tb4,text="Information about top sufficient taxonomic units",command=plot_univ)
tkgrid(a17)
tk2tip(a17, "Plots and values information of the top sufficient taxonomic units according to the microbial clusters \n(mean, standard deviation, dominance index, barplot and circle plot")
tkgrid(tklabel(tb4,text=" "))
tb4<<-tb4
##partie 5
tb5 <- tk2notetab(nb,"Supplementary analysis")
tb5<<-tb5
tkgrid(tklabel(tb5,text="                                                                                                                                                                                                                                                                                                                                                                              "))
a18<-tkbutton(tb5,text="Diversity",command=diversity.calc.RF)
tkgrid(a18)
tk2tip(a18, "Calculates several diversity index \n(Richness, Number of individual, Dominance, Shanon, \nSimpson, Eveness, Equitability and Berger Parker dominance)")
a19<-tkbutton(tb5,text="Decision tree",command=SRF_fitted_rpart)
tkgrid(a19)
tk2tip(a19, "Calculates a conditional inference decision based on the top sufficient taxonomic units \n(see ctree function from party package)")
tkgrid(tklabel(tb5,text=" "))
##partie 6
tb6 <- tk2notetab(nb,"Miscellaneous")
tkgrid(tklabel(tb6,text="                                                                                                                                                                                                                                                                                                                                                                              "))
ccc<-function(){fig_par(typee=2)}
a19<-tkbutton(tb6,text="Language",command=language.RF)
tkgrid(a19)
tk2tip(a19, "Changes language. For instance, only USA english, french, german and Italian can be selected")
a20<-tkbutton(tb6,text="Memory increase",command=memory.increase)
tkgrid(a20)
tk2tip(a20, "Increases the RAM memory allocated to R. If RandForestGUI frequently bugs, this function is most of time the solution!")
a21<-tkbutton(tb6,text="Background plot colors",command=ccc)
tkgrid(a21)
tk2tip(a21, "Changes the background color of RandForestGUI plots")
tkgrid(tklabel(tb6,text=" "))
tb6<<-tb6
##selection
if(exists("fact.RF"))  tk_select()
tt<<-tt
tkfocus(tt)
}
rf<-function(){RandForestGUI()}
RF<-function(){RandForestGUI()}
Rf<-function(){RandForestGUI()}
RandforestGUI<-function(){RandForestGUI()}
randforestGUI<-function(){RandForestGUI()}
randForestGUI<-function(){RandForestGUI()}
Randforestgui<-function(){RandForestGUI()}
randforestgui<-function(){RandForestGUI()}
randForestgui<-function(){RandForestGUI()}
RandforestGui<-function(){RandForestGUI()}
randforestGui<-function(){RandForestGUI()}
randForestGui<-function(){RandForestGUI()}