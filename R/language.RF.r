###############################
###############################
##  Procedure to change the language
##  Input: NA
##  Output: NA
###############################
###############################
"language.RF"<-function(){
choiceGUI("Change RandForestGUI language","Select your language","1:USA english, 2:French (Francais), 3:German (Deutsch), 4:Italian (Italiana), 5:cancel")
if(nf==1) {curlang<-"en_US";setLanguage("en_US")}
if(nf==2) {curlang<-"fr_FR";setLanguage("fr_FR")}
if(nf==3) {curlang<-"de_DE";setLanguage("de_DE")}
if(nf==4) {curlang<-"it_IT";setLanguage("it_IT")}
if(nf==5) {stop()}
curlang <<- curlang
}
