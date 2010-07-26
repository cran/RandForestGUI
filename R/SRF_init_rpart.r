###############################
###############################
##  Procedure to compute conditionnal inference decision tree with the selection of taxonomic units used in the initial random forest
##  Input: NA
##  Output: NA
###############################
###############################
"SRF_init_rpart"<-function(){
choiceGUI("Conditionnal inference decision tree","What is your categorical variable ?",paste(colnames(fact.RF),collapse=","))
fact.temp<-nf
choiceGUI("Conditionnal inference decision tree","What is your p value threshold ","0.2 is advised")
ctree_ini_pval<<-nf
dat<-data.frame(mat.analyse.RF,fact.RF[,fact.temp])
rp2 = ctree(formula(paste(names(dat)[length(dat)],"~.")), data=dat, controls = ctree_control(mincriterion=c(1-ctree_ini_pval),minsplit=0.0000000000000000000000000001,minbucket=0.00000000000000000000000000001))
fig_par(typee=1)
plot(rp2); 
print(rp2)
}
