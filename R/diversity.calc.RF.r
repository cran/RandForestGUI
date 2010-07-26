###############################
###############################
##  calculates several diversity index (Richness, Number of individual, Dominance, Shanon, Simpson, Eveness, Equitability and berger parker dominance) 
##  Input: NA
##  Output: NA
###############################
###############################
"diversity.calc.RF"<-function(){
dev_delete()
diversity.calc<-function(){
##creation list sans NA
temp<-mat.analyse.RF
temp[temp==0]<-NA
aa<- vector("list", dim(temp)[1])
names(aa)<-rownames(temp)
for (i in 1: dim(temp)[1]){
aa[[i]]<-na.omit(temp[i,] )
}
aa<<-aa
##richesse calcule nb sonde
nombre_sonde<-vector(length=length(aa))
for (i in 1:length(aa)){
nombre_sonde[i]<-length(aa[[i]])
}
##sum = total nb of ind
ind<-vector(length=length(aa))
for (i in 1:length(aa)){
ind[i]<-sum(aa[[i]])
}

##calcul of pi
pii<-aa
for (i in 1:length(aa)){
pii[[i]]<-aa[[i]]/ind[[i]]
}
pii<<-pii
##dominance
dom<-vector(length=length(aa))
for (i in 1:length(aa)){
dom[i]<-sum((pii[[i]])*(pii[[i]]))
}
##calcul shanon
shanon.RF<-vector(length=length(aa))
for (i in 1:length(aa)){
shanon.RF[i]<--sum(pii[[i]]*log(pii[[i]]))
 }

##calcul simpson
simpson.RF<-1-dom

##eveness
eveness<-simpson.RF
eveness<-exp(shanon.RF)/nombre_sonde

##equitability
equitability<-simpson.RF
equitability<-shanon.RF/log(nombre_sonde)

##berger parker dominance
maxx<-simpson.RF
for (i in 1:length(aa)){
maxx[i]<-max(aa[[i]])
}
berger_parker_dominance<-simpson.RF
berger_parker_dominance<-maxx/ind

diversity.RF<-data.frame(nombre_sonde,ind,dom,shanon.RF,simpson.RF,eveness,equitability,berger_parker_dominance)
rownames(diversity.RF)<-rownames(mat.analyse.RF)
diversity.RF<<-diversity.RF
print(diversity.RF)}
choiceGUI("Diversity","What do you want to do ?","1:print diversity results, 2:plot diversity, 3:cancel")
if(nf==3){ stop()}
if(nf==1){diversity.calc()}
if(nf==2){diversity.calc();graphfact.RF()}

}