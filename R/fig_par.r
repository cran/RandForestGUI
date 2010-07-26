###############################
###############################
##  Choose the background colors of plots
##  Input: NA
##  Output: NA
###############################
###############################
"fig_par"<-function(typee){
palette("default")
if (typee==2){
choiceGUI("Colors","Colors of the background","1:white,2:black, 3:red, 4:green, 5:blue, 6:cyan, 7:magenta, 8:yellow, 9:gray, 10:cancel")
a<-palette("default")
if (nf==10){stop()}
if (nf==1) {colo_fig_par<<-c(NA,"black")  }
if (nf==2){ colo_fig_par<<-c("black","white"); b<-which(a=="black");c<-a[-b];palette(c)}
if (nf==3){ colo_fig_par<<-c("red","white"); b<-which(a=="red");c<-a[-b];palette(c)}
if (nf==4){ colo_fig_par<<-c("green3","white"); b<-which(a=="green3");c<-a[-b];palette(c)}
if (nf==5){ colo_fig_par<<-c("blue","white"); b<-which(a=="blue");c<-a[-b];palette(c)}
if (nf==6){ colo_fig_par<<-c("cyan","white"); b<-which(a=="cyan");c<-a[-b];palette(c)}
if (nf==7){ colo_fig_par<<-c("magenta","white"); b<-which(a=="magenta");c<-a[-b];palette(c)}
if (nf==8){ colo_fig_par<<-c("yellow","white"); b<-which(a=="yellow");c<-a[-b];palette(c)}
if (nf==9){ colo_fig_par<<-c("gray","white"); b<-which(a=="gray");c<-a[-b];palette(c)}
}
if (typee==1){
par(bg=colo_fig_par[1],col.axis=colo_fig_par[2],col.lab=colo_fig_par[2],col.main=colo_fig_par[2],col.sub=colo_fig_par[2],col=colo_fig_par[2])
}
}                   