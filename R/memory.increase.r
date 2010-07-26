###############################
###############################
##  Increase the RAM memory allocated to R
##  Input: NA
##  Output: NA
###############################
###############################
"memory.increase"<-function(){
choiceGUI("RAM memory increase","Do you want to increase your RAM memory ?","1:yes, if R bugs frequently, 2:no, 3:cancel")
if(nf!=1){stop()}
if(nf==1){
ini<-memory.limit()
a<-vector(length=10000-memory.limit())
for (i in 1:length(a)){a[i]<-try(memory.limit(size=c(i+memory.limit())),silent=TRUE)}
fin<-memory.limit()
print(paste("Your RAM attribute for R was initially of",ini, "Mb, and has been now extend to",fin,"Mb")  )}
mem_lim<<-fin
}