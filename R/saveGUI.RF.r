###############################
###############################
##  Save and save as function
##  Input: NA
##  Output: NA
###############################
###############################
"saveGUI.RF"<-function(typesave){
dev_delete()
if(exists("filename.RF")){  ##si existe et path actuel=path du projet	
  if(paste(strsplit(filename.RF,"/")[[1]][c(1:c(length(strsplit(filename.RF,"/")[[1]])-1))],collapse="/")!=getwd()) {
  filename.RF<-paste(getwd(),strsplit(filename.RF,"/")[[1]][c(c(length(strsplit(filename.RF,"/")[[1]])))],sep="/")
  filename.RF<<-filename.RF}
  }
if(!exists("filename.RF")){   #si existe pas
  filename.RF<-tclvalue(tkgetSaveFile())
	filename.RF<-paste(filename.RF,".Rdata",sep="")
  if (filename.RF == "")
	return()
	filename.RF<<-filename.RF
	}
a<-vector(length=10000);a[]<-NA
if(exists("mat.analyse.RF")) a[1]="mat.analyse.RF"	
if(exists("fact.RF")) a[2]="fact.RF"	
if(exists("param.RF")) a[3]="param.RF"
if(exists("filename.RF")) a[4]="filename.RF"
if(exists("mat.analyse.RF.init")) a[5]="mat.analyse.RF.init"
if(exists("fact.RF.init"))   a[6]="fact.RF.init"
if(exists("param.RF.init"))  a[7]="param.RF.init"
if(exists("SRF_ini_mtry")) a[8]="SRF_ini_mtry"
if(exists("SRF_ini_dat")) a[10]="SRF_ini_dat"
if(exists("SRF_ini_OOB")) a[11]="SRF_ini_OOB"
if(exists("SRF_ini_myvarimp")) a[12]="SRF_ini_myvarimp"
if(exists("SRF_ini_myvarimp_selected")) a[13]="SRF_ini_myvarimp_selected"
if(exists("SRF_ini_tune_nb_var")) a[14]="SRF_ini_tune_nb_var"
if(exists("SRF_ini_mds")) a[15]="SRF_ini_mds"
if(exists("SRF_ini_mean_probe")) a[16]="SRF_ini_mean_probe"
if(exists("pval.init"))  a[161]="pval.init"
if(exists("SRF_init_mds.type"))   a[162]<-"SRF_init_mds.type"
if(exists("SRF_init_nbperm"))   a[163]<-"SRF_init_nbperm"
if(exists("SRF.init.prox.raw"))   a[164]<-"SRF.init.prox.raw"
if(exists("pval.threshold"))   a[165]<-"pval.threshold"
if(exists("fact.supRF.init"))   a[166]<-"fact.supRF.init"
if(exists("perm.nbtree.temp"))   a[167]<-"perm.nbtree.temp"
if(exists("perm.number"))   a[168]<-"perm.number"
if(exists("SRF.init.mtry"))   a[169]<-"SRF.init.mtry"
if(exists("SRF.init.nbtree"))   a[170]<-"SRF.init.nbtree"
if(exists("SRF_ini_tunemtry_nbtree"))   a[171]<-"SRF_ini_tunemtry_nbtree"
if(exists("decrease_nb_var_nbtree"))   a[172]<-"decrease_nb_var_nbtree"
if(exists("decrease_nb_var_mtryincr"))   a[173]<-"decrease_nb_var_mtryincr"
if(exists("SRF_ini_tunemtry_nbforest"))   a[174]<-"SRF_ini_tunemtry_nbforest"
if(exists("diversity.RF")) a[17]="diversity.RF"
if(exists("mat.analyse.RF.fitted")) a[18]="mat.analyse.RF.fitted"
if(exists("SRF_ini_dat_fitted"))   a[20]<-"SRF_ini_dat_fitted"
if(exists("SRF_ini_OOB_fitted"))   a[22]<-"SRF_ini_OOB_fitted" 
if(exists("SRF_fitted_mds"))  a[23]<-"SRF_fitted_mds"
if(exists("SRF_ini_myvarimp_fitted")) a[24]<-"SRF_ini_myvarimp_fitted"
if(exists("pval.fitted"))  a[241]<-"pval.fitted"
if(exists("SRF_fitted_mds.type"))   a[242]<-"SRF_fitted_mds.type"
if(exists("SRF_fitted_nbperm"))   a[243]<-"SRF_fitted_nbperm"
if(exists("SRF.fitted.prox.raw"))   a[244]<-"SRF.fitted.prox.raw"
if(exists("SRF_ini_tune_nb_var_selected"))   a[245]<-"SRF_ini_tune_nb_var_selected"
if(exists("SRF_fin_tunemtry_nbtree"))   a[246]<-"SRF_fin_tunemtry_nbtree"
if(exists("ctree_fitted_pval"))   a[247]<-"ctree_fitted_pval"
if(exists("ctree_ini_pval"))   a[248]<-"ctree_ini_pval"
if(exists("SRF_ini_nbforest_fitted"))   a[249]<-"SRF_ini_nbforest_fitted"
if(exists("SRF.fit.nbtree"))   a[251]<-"SRF.fit.nbtree"
if(exists("SRF.fit.mtry"))   a[252]<-"SRF.fit.mtry"
if(exists("SRF_ini_nbforest"))   a[253]<-"SRF_ini_nbforest"
if(exists("SRF_ini_prediction"))   a[254]<-"SRF_ini_prediction"
if(exists("SRF_fit_prediction"))   a[255]<-"SRF_fit_prediction"
if(exists("SRF_ini_tunemtry_restree"))   a[257]<-"SRF_ini_tunemtry_restree"
if(exists("SRF_fit_tunemtry_restree"))   a[258]<-"SRF_fit_tunemtry_restree"
if(exists("SRF_fit_tunemtry_nbforest"))   a[259]<-"SRF_fit_tunemtry_nbforest"
if(exists("SRF_ini_mtry_fitted"))   a[260]<-"SRF_ini_mtry_fitted"
if(exists("SRF_ini_tune_nb_var_aff"))   a[261]<-"SRF_ini_tune_nb_var_aff"
if(exists("decrease_nb_var_nbfor_aff"))   a[262]<-"decrease_nb_var_nbfor_aff"
if(exists("decrease_nb_var_nbtree_aff"))   a[263]<-"decrease_nb_var_nbtree_aff"
if(exists("SRF_ini_tune_nb_var_aff_maxmin"))   a[264]<-"SRF_ini_tune_nb_var_aff_maxmin"
if(exists("tree_algo_fitted"))   a[265]<-"tree_algo_fitted"
if(exists("tree_algo"))   a[266]<-"tree_algo"
if(exists("SRF_ini_tune_nb_var_aff_cor"))   a[267]<-"SRF_ini_tune_nb_var_aff_cor"
if(exists("SRF_ini_tunemtry_inc"))   a[268]<-"SRF_ini_tunemtry_inc"
if(exists("SRF_fin_tunemtry_inc"))   a[269]<-"SRF_fin_tunemtry_inc"
if(exists("fact.supRF.fitted"))   a[270]<-"fact.supRF.fitted"
if(exists("correlation"))   a[271]<-"correlation"
if(exists("MDA_conditional"))   a[272]<-"MDA_conditional"
if(exists("colo_fig_par"))   a[273]<-"colo_fig_par"
if(exists("URF_meth")) a[25]<-"URF_meth"
if(exists("URF_mtry")) a[26]<-"URF_mtry"
if(exists("URF_nbforest")) a[27]<-"URF_nbforest"
if(exists("URF_nbtree"))  a[28]<-"URF_nbtree"
if(exists("URF_mtry"))  a[29]<-"URF_mtry"
if(exists("URF_mtryy"))  a[2900]<-"URF_mtryy"
if(exists("URF_mean"))    a[31]<-"URF_mean"
if(exists("URF_median"))  a[32]<-"URF_median"
if(exists("URF_sd"))  a[33]<-"URF_sd"
if(exists("DB_index"))  a[34]<-"DB_index"
if(exists("cluster_algo"))  a[35]<-"cluster_algo" 
if(exists("nb_clust_URF"))   a[36]<-"nb_clust_URF"    
if(exists("URF_mds"))   a[37]<-"URF_mds"
if(exists("URF_mds.type"))   a[38]<-"URF_mds.type"
if(exists("URF_mds_nbperm"))   a[39]<-"URF_mds_nbperm"
if(exists("URF_tune_mtry_incr"))   a[41]<-"URF_tune_mtry_incr"
if(exists("URF_tune_mtry_forest"))   a[42]<-"URF_tune_mtry_forest"  
if(exists("URF_tune_mtry_nbtree"))   a[43]<-"URF_tune_mtry_nbtree"
if(exists("prop_echan_sond"))   a[44]<-"prop_echan_sond"
if(exists("prop_echan_sond_nom"))   a[45]<-"prop_echan_sond_nom"
if(exists("prop_echan_sond_temp3"))   a[46]<-"prop_echan_sond_temp3"
if(exists("prop_echan_sond_nf"))   a[47]<-"prop_echan_sond_nf"
if(exists("curlang"))   a[48]<-"curlang"
if(exists("mem_lim"))   a[49]<-"mem_lim"

a<-na.omit(a)
save(list=a,file=filename.RF)
save(list=a,file=paste(strsplit(filename.RF,".Rdata")[[1]],".cop",sep=""))
if(typesave==2){
save(list=a,file=paste(strsplit(filename.RF,".Rdata")[[1]],".sav",sep=""))
}
print("Successfully saved!")
}
