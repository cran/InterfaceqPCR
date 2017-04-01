
#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                             |
#      | Interface qPCR  package                     |
#      | Analyse des donnees PMA qPCR                |
#      | chargement fichier gamme etalon             |
#      | chargement fichier echantillons             |
#      | Decembre 2016                               |
#      |______________________Oooo.__________________|
#                   .oooO     (    )
#                   (    )     )  /
#                    \  (      (_/
#                     \_)
#
# Author: Olivier Le Goff
###############################################################################
###############################################################################






Interface_PMAqPCR<-function(){

  choix_langue<- texte1<-texte2<-texte3<-texte4<-texte5<-texte6<-texte7<-texte8<-texte9<-texte10<-texte11<-texte12<-texte13<-
    texte14<-texte15<-texte16<-texte17<-texte18<-texte19<-texte20<-texte21<-texte22<-texte23<-texte24<-texte25<-
    texte26<-texte27<-texte28<-texte29<-texte30<-texte31<-texte32<-texte33<-texte33bis<-texte34<-texte35<-texte36<-
    texte37<-texte38<-texte39<-texte40<-texte41<-texte42<-texte43<-texte44<-texte45<-texte46<-texte47<-texte48<-
    texte48bis<-texte49<-texte50<-texte51<-texte52<-texte52bis<-texte53<-texte54<-texte55<-texte56<-texte57<-
    texte58<-texte59<-texte60<-texte61<-texte62<-Echantillons_bruts<-choixconcentration<-Condition<-regressionnontraite<-
    dataimportees_echantillons<-data_moy_ADN_traite<-Efficacitetraite<-regressiontraite<-choixconcentration<-
    Ct<-dataimportees_gamme_etalon<-couleur1_ech<-couleur1<-Nbechantillonstraites2<-NULL




 # require(tcltk)
  tclRequire("BWidget")
  rm(list=ls())
  if(exists("dataimportees_gamme_etalon")==TRUE) {rm(dataimportees_gamme_etalon, pos=1)}
  if(exists("dataimportees_echantillons")==TRUE) {rm(dataimportees_echantillons, pos=1)}

  couleur1 <<- "red"
  couleur1_ech <<- "red"
  Nbechantillonstraites2 <<- 0

# Du00e9but interface graphique
Interface_PMA_qPCR <- tktoplevel()
#tkwm.title(Interface_PMA_qPCR,"Interface qPCR u00e9chantilons traitu00e9s au PMA")
tkwm.title(Interface_PMA_qPCR,texte9)
tkconfigure(Interface_PMA_qPCR,bg="wheat1",cursor="hand2")


# Taille
tkwm.geometry(Interface_PMA_qPCR, "700x700")   # Largueur x hauteur
tkwm.resizable(Interface_PMA_qPCR,F,F)


# Police
fontHeading <- tkfont.create(family="times",size=20,weight="bold",slant="italic")
font2 <- tkfont.create(family="times",size=14,weight="bold",slant="italic")
font3 <- tkfont.create(family="times",size=12,weight="bold")





## Import xlsx file
importfilexlsx_gamme_etalon <- function(){
  #require(xlsx)
  PMAgamme <- matrix(c(".xlsx",".xlsx"),1, 2, byrow = TRUE)


  filPMAgamme <- if(interactive()) tk_choose.files(filters = PMAgamme)


  #fil=if (interactive()) file.choose()

  # Detectionchargementxlsx <- loadedNamespaces()
  # if(any(Detectionchargementxlsx =="xlsx") == TRUE){dataimportees_gamme_etalon=read.xlsx(filPMAgamme[1],header=T, 1)
  # } else {requireNamespace(readxl)
  #   dataimportees_gamme_etalon=read_excel(filPMAgamme[1])
  # }

  dataimportees_gamme_etalon=read.xlsx(filPMAgamme[1],header=T, 1)

  dataimportees_gamme_etalon <<- dataimportees_gamme_etalon

  dataimportees_gamme_etalon <<- dataimportees_gamme_etalon
  donnees1_gamme_etalon <- dataimportees_gamme_etalon
  donnees1_gamme_etalon <<- donnees1_gamme_etalon
}







## Import xlsx file
importfilexlsx_echantillons <- function(){
  #require(xlsx)

  PMAech <- matrix(c(".xlsx",".xlsx"),1, 2, byrow = TRUE)


  filPMAech <- if(interactive()) tk_choose.files(filters = PMAech)


  # Detectionchargementxlsx <- loadedNamespaces()
  # if(any(Detectionchargementxlsx =="xlsx") == TRUE){dataimportees_echantillons=read.xlsx(filPMAech[1],header=T, 1)
  # } else {requireNamespace(readxl)
  #   dataimportees_echantillons <- read_excel(filPMAech[1])
  # }

  dataimportees_echantillons=read.xlsx(filPMAech[1],header=T, 1)
  dataimportees_echantillons <<- dataimportees_echantillons

  dataimportees_echantillons <<- dataimportees_echantillons
  donnees1_echantillons <- dataimportees_echantillons
  donnees1_echantillons <<- donnees1_echantillons
}






###########################################################################################
## Importer le fichier des donnu00e9es brutes

#etape1=tklabel(Interface_PMA_qPCR, text="1 Importer les fichiers",font=fontHeading,foreground="chocolate4",bg="wheat1")
etape1 <- tklabel(Interface_PMA_qPCR, text=texte18,font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(etape1, row=1,  columnspan=4, sticky="w")


## Fichier contenant la gamme u00e9talon
#etape11=tklabel(Interface_PMA_qPCR, text="Fichier Gamme u00e9talon",font=font2,foreground="chocolate4",bg="wheat1")
etape11 <- tklabel(Interface_PMA_qPCR, text=texte19,font=font2,foreground="chocolate4",bg="wheat1")
tkgrid(etape11, row=2,  columnspan=4)





#import xlsx file
IMPORTxlsx.but <- tkbutton(Interface_PMA_qPCR,command=importfilexlsx_gamme_etalon)
tkconfigure(IMPORTxlsx.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"xlsx.gif",
                                                                       fsep=.Platform$file.sep)))

tkgrid(IMPORTxlsx.but,row=3,column=4)



# Vu00e9rification de l'importation du fichier de donnu00e9es brutes


Verif_import23 <- function(){
  #if(exists("dataimportees_gamme_etalon")==TRUE) {"Oui"} else {"Non"}
  if(exists("dataimportees_gamme_etalon")==TRUE) {texte15}else {texte16}
}
Verif_couleur=function()
{
  couleur1 <- if(exists("dataimportees_gamme_etalon")==TRUE) {"green"}else {"red"}
  couleur1 <<- couleur1
}

Verif_couleur()
Verifimport2=Verif_import23()




#ImportationFichiertest<-tklabel(Interface_PMA_qPCR, text=" Fichier cru00e9u00e9 :" ,bg="wheat1")
ImportationFichiertest <- tklabel(Interface_PMA_qPCR, text=texte23 ,bg="wheat1")
tkgrid(ImportationFichiertest, row=3, column=5, sticky="e")

ImportationFichiercase <- tkentry(Interface_PMA_qPCR,width=10, foreground=couleur1, bg="black", textvariable=tclVar(Verifimport2))
tkgrid(ImportationFichiercase,row=3,column=6)

# Actualiser la vu00e9rif du fichier importe
Actualisationimport <- function(){

  Verif_import23<-function(){
    #if(exists("dataimportees_gamme_etalon")==TRUE) {"Oui"} else {"Non"}
    if(exists("dataimportees_gamme_etalon")==TRUE) {texte15}else {texte16}
  }
  Verif_couleur=function()
  {
    couleur1 <- if(exists("dataimportees_gamme_etalon")==TRUE) {"green"}else {"red"}
    couleur1 <<- couleur1
  }

  Verif_couleur()
  Verifimport22 <- Verif_import23()
  #ImportationFichiertest<-tklabel(Interface_PMA_qPCR, text=" Fichier cru00e9u00e9 :" ,bg="wheat1")
  ImportationFichiertest <- tklabel(Interface_PMA_qPCR, text=texte23,bg="wheat1")
  tkgrid(ImportationFichiertest, row=3, column=5, sticky="e")

  ImportationFichiercase <- tkentry(Interface_PMA_qPCR,width=10,bg="black",foreground=couleur1, textvariable=tclVar(Verifimport22))
  tkgrid(ImportationFichiercase,row=3,column=6)

}




#Actualiser.but<-tkbutton(Interface_PMA_qPCR,text="Actualiser", command=Actualisationimport)
Actualiser.but <- tkbutton(Interface_PMA_qPCR,text=texte24, command=Actualisationimport)
tkgrid(Actualiser.but,row=3,column=7)




## Fichier contenant les echantillons
vide1 <- tklabel(Interface_PMA_qPCR, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
tkgrid(vide1, row=4,  columnspan=4)


#etape1111=tklabel(Interface_PMA_qPCR, text="Fichier Echantillons",font=font2,foreground="chocolate4",bg="wheat1")
etape1111 <- tklabel(Interface_PMA_qPCR, text=texte25,font=font2,foreground="chocolate4",bg="wheat1")
tkgrid(etape1111, row=5,  columnspan=4)




#import xlsx file echantillons
IMPORTxlsx_echantillons.but <- tkbutton(Interface_PMA_qPCR,command=importfilexlsx_echantillons)
tkconfigure(IMPORTxlsx_echantillons.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"xlsx.gif",
                                                                       fsep=.Platform$file.sep)))

tkgrid(IMPORTxlsx_echantillons.but,row=6,column=4)




# Vu00e9rification de l'importation du fichier de donnu00e9es brutes

Verif_import24 <- function(){
  #if(exists("dataimportees_gamme_etalon")==TRUE) {"Oui"} else {"Non"}
  if(exists("dataimportees_echantillons")==TRUE) {texte15}else {texte16}
}
Verif_couleur_ech=function()
{
  couleur1_ech <- if(exists("dataimportees_echantillons")==TRUE) {"green"}else {"red"}
  couleur1_ech <<- couleur1_ech
}
Verif_couleur_ech()

Verifimport_ech2 <- Verif_import24()


#ImportationFichierechantillons<-tklabel(Interface_PMA_qPCR, text=" Fichier cru00e9u00e9 :" ,bg="wheat1")
ImportationFichierechantillons <- tklabel(Interface_PMA_qPCR, text=texte23 ,bg="wheat1")
tkgrid(ImportationFichierechantillons, row=6, column=5, sticky="e")

ImportationFichier_ech_case <- tkentry(Interface_PMA_qPCR,width=10,foreground=couleur1_ech,bg="black", textvariable=tclVar(Verifimport_ech2))
tkgrid(ImportationFichier_ech_case,row=6,column=6)

# Actualiser la vu00e9rif du fichier importe
Actualisationimport_ech <- function(){

  #if(exists("dataimportees_echantillons")==TRUE) {"Oui"} else {"Non"}
  if(exists("dataimportees_echantillons")==TRUE) {texte15}else {texte16}

  Verif_import24<-function(){
    #if(exists("dataimportees_gamme_etalon")==TRUE) {"Oui"} else {"Non"}
    if(exists("dataimportees_echantillons")==TRUE) {texte15}else {texte16}
  }
  Verif_couleur_ech <- function()
  {
    couleur1_ech <- if(exists("dataimportees_echantillons")==TRUE) {"green"}else {"red"}
    couleur1_ech<<-couleur1_ech
  }
  Verif_couleur_ech()

  Verifimport_ech22 <- Verif_import24()
  #ImportationFichierechantillons<-tklabel(Interface_PMA_qPCR, text=" Fichier cru00e9u00e9 :" ,bg="wheat1")
  ImportationFichierechantillons <- tklabel(Interface_PMA_qPCR, text=texte23 ,bg="wheat1")
  tkgrid(ImportationFichierechantillons, row=6, column=5, sticky="e")

  ImportationFichier_ech_case <- tkentry(Interface_PMA_qPCR,width=10, foreground=couleur1_ech,bg="black", textvariable=tclVar(Verifimport_ech22))
  tkgrid(ImportationFichier_ech_case,row=6,column=6)



Calcul_Nb_echantillonstraites2 = function(){

      if(exists("dataimportees_echantillons")==FALSE) {" "}else{
      Nbechantillonstraites <- subset(dataimportees_echantillons, Condition %in% c("PMA"))
      Nbechantillonstraites2 <- length(Nbechantillonstraites$Nom_Echantillon)
      Nbechantillonstraites2 <<- Nbechantillonstraites2
    }

 }
    Calcul_Nb_echantillonstraites = function(){

    if(exists("dataimportees_echantillons")==FALSE) {" "} else {Calcul_Nb_echantillonstraites2()}
  }

  ImportationNbFichier_ech_case <- tkentry(Interface_PMA_qPCR,width=10,foreground=couleur1_ech,font=font3,
                                           bg="black", textvariable=tclVar(Calcul_Nb_echantillonstraites()))
  tkgrid(ImportationNbFichier_ech_case,row=14,column=3)


  } # fin fonction Actualisation

#Actualiserech.but<-tkbutton(Interface_PMA_qPCR,text="Actualiser", command=Actualisationimport_ech)
Actualiserech.but <- tkbutton(Interface_PMA_qPCR,text=texte24, command=Actualisationimport_ech)
tkgrid(Actualiserech.but,row=6,column=7)


##################################################################################################
## Importer le fichier des donnu00e9es brutes
vide2 <- tklabel(Interface_PMA_qPCR, text="",font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(vide2, row=7,  columnspan=4)

#etape2=tklabel(Interface_PMA_qPCR, text="2 Droite de ru00e9gression",font=fontHeading,foreground="chocolate4",bg="wheat1")
etape2 <- tklabel(Interface_PMA_qPCR, text=texte27,font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(etape2, row=8,  columnspan=4, sticky ="w")


## Ajout du choix de la concentration

#etape21=tklabel(Interface_PMA_qPCR, text="Concentration en :",font=font2,foreground="chocolate4",bg="wheat1")
etape21 <- tklabel(Interface_PMA_qPCR, text=texte51,font=font2,foreground="chocolate4",bg="wheat1")
tkgrid(etape21, row=9,  column=2)

# radio boutons

rb1_UFC <- tkradiobutton(Interface_PMA_qPCR, font=font3,foreground="black",bg="wheat1")
rb2_copies <- tkradiobutton(Interface_PMA_qPCR, font=font3,foreground="black",bg="wheat1")
# Choix par du00e9faut
rbValue <- tclVar(texte52)

# config des boutons radio. Une seule variable tcl pour 2 boutons
#tkconfigure(rb2_copies,variable=rbValue,value=" UFC/mL", text=" UFC/mL")
tkconfigure(rb1_UFC,variable=rbValue,value=texte52, text=texte52)
#tkconfigure(rb2_copies,variable=rbValue,value="Nb copies/mL", text="Nb copies/mL")
tkconfigure(rb2_copies,variable=rbValue,value=texte53, text=texte53)

tkgrid(rb1_UFC,row=9,  column=3)
tkgrid(rb2_copies,row=9,column=5)

#etape212=tklabel(Interface_PMA_qPCR, text="ou",font=font3,foreground="chocolate4",bg="wheat1")
etape212=tklabel(Interface_PMA_qPCR, text=texte22,font=font3,foreground="chocolate4",bg="wheat1")
tkgrid(etape212,row=9,column=4)

OnOK <- function() {

  #message<-paste("Votre choix est :", tclvalue(rbValue))
  message<-paste(texte54, tclvalue(rbValue))
  tkmessageBox(title="Validation", message=message, icon="info", type="ok")
  choixconcentration <<-tclvalue(rbValue)
}

OK.but <- tkbutton(Interface_PMA_qPCR, text="OK", command=OnOK)
tkgrid(OK.but,row=9,column=6)




#  Calcul et tracage de droite de regression issue des standards

Calcul_regression <- function(){

#library(reshape2)
#library(plyr)


  fusion_data <- dataimportees_gamme_etalon
  head(fusion_data)


# Calculer moyenne et ecart type par Standard

data_moy_sd <- ddply(fusion_data,c("Numero","Standard","log10_Sdt","Condition"),summarise,
                   Moy=mean(Ct,na.rm=TRUE),
                   sd=sd(Ct,na.rm=TRUE))
data_moy_sd


data_moy_sd <<- data_moy_sd[complete.cases(data_moy_sd), ]


# equation de la droite de regression ADN  traite issue de data_moy_sd

data_moy_ADN_traite <- subset(data_moy_sd, Condition=="PMA")
data_moy_ADN_traite <<- data_moy_ADN_traite

regressiontraite <- lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite)

regressiontraite <<- regressiontraite

pentetraite <- format(regressiontraite$coefficients[2], digits = 3)
interceptiontraite <- format(regressiontraite$coefficients[1], digits = 4)
r2traite <- format(summary(regressiontraite)$r.squared, digits = 3)
r2traite <<- r2traite
# #  recup ecart sur l'ordonee
# ecart_ordonneetraite<-summary.lm(regressiontraite)$coefficients[1,2]
# # LOD  et LOQ
# LODtraite <- 10^(3*ecart_ordonneetraite/abs(pentetraite))
# LODtraite <<- LODtraite
# LOQtraite <- 10^(10*ecart_ordonneetraite/abs(pentetraite))
# LOQtraite <<- LOQtraite

Efficacitetraite <- format((-1+(10^(-1/summary(regressiontraite)$coefficients[2])))*100, digits = 3)

if(Efficacitetraite>100){Efficacitetraite=100}else{Efficacitetraite}

Efficacitetraite <<- Efficacitetraite

equation_droitetraite <- paste("y =",interceptiontraite , pentetraite ,"x" )
R2equation_droitetraite <- paste("R2 =", r2traite)
Efficaciteequation_droitetraite <- paste("E =", Efficacitetraite,"%")


equation_droitetraite <<- equation_droitetraite
R2equation_droitetraite <<- R2equation_droitetraite
Efficaciteequation_droitetraite <<- Efficaciteequation_droitetraite

}


graphique_regression <- function(){
  #library(tkrplot)

fenetre_fraphique <- tktoplevel()
#tkwm.title(fenetre_fraphique,"Droite de regression issue des Standards qPCR")
tkwm.title(fenetre_fraphique, texte28)
tkconfigure(fenetre_fraphique,bg="wheat1",cursor="hand2")


# Taille
tkwm.geometry(fenetre_fraphique, "625x700")   # Largueur x hauteur
tkwm.resizable(fenetre_fraphique,F,F)


  image1plot <- function(){
    #
    #
        ###########
    # Condition si concentration en UFC/mL

    if(choixconcentration == texte52) {

    XX <- log10(data_moy_ADN_traite$Standard)

    YY <- data_moy_ADN_traite$Moy
    REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
    REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
    r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
   Efficacitetrace <- Efficacitetraite
   LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
   LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))

par(lab=c(10,10,10),oma=c(5,2,0,0))
    plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
         pch=16,col="red",
         main="",
         xlab=" ",
         ylab=expression(paste(C[t], "value")))


    abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
    legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                   "\nE=",Efficacitetrace, " %",
                                   "\nLOD=",LODtracetraite, texte52,
                                   "\nLOQ=",LOQtracetraite, texte52),bty="n",cex=0.75)

    arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
           XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
           length = 0.05, # width of the arrowhead
           angle = 90,
           code = 3)



      axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits = 3),
           line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)

      # text(-1,-10,"UFC/mL", cex= 0.8,col="blue",xpd=TRUE)
      text(-1,-10, texte52 , cex= 0.8,col="blue",xpd=TRUE)
      #text(-1,-6,expression(paste(Log[10],"(UFC/mL)")), cex= 0.7,col="black",xpd=TRUE)
      text(-1,-6, texte52bis , cex= 0.7,col="black",xpd=TRUE)

      #
      #
      #  si condition concentration est Nb copies/mL

    }else{
      ###########
      XX <- log10(data_moy_ADN_traite$Standard)

      YY <- data_moy_ADN_traite$Moy
      REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
      REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
      r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)

      Efficacitetrace <- Efficacitetraite
      LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
      LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))

      par(lab=c(10,10,10),oma=c(5,2,0,0))
      plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
           pch=16,col="red",
           main="",
           xlab=" ",
           ylab=expression(paste(C[t], "value")))


      abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
      legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                     "\nE=",Efficacitetrace, " %",
                                     "\nLOD=",LODtracetraite, "copies/mL",
                                     "\nLOQ=",LOQtracetraite, "copies/mL"),bty="n",cex=0.75)

      arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
             XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
             length = 0.05, # width of the arrowhead
             angle = 90,
             code = 3)



      axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits = 3),
           line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)

      text(-1,-10,"copies/mL", cex= 0.8,col="blue",xpd=TRUE)
      text(-1,-6,expression(paste(Log[10], " (copies/mL)")), cex= 0.7,col="black",xpd=TRUE)

    }
  }# fin fonction image1


image1 <- tkrplot(fenetre_fraphique,fun=image1plot,hscale=1.9, vscale=1.9)
tkgrid(image1,columnspan=5,rowspan=4)


exportertiffplot <- function() {

  # Exportationfichiertiff1=tkmessageBox(title="Enregistrer en .tiff", message="Voulez-vous enregistrer en .tiff ?",
  #                                     icon="info",type="yesno")

  Exportationfichiertiff1=tkmessageBox(title=texte55,  message=texte56 ,
                                       icon="info",type="yesno")


  Exportationfichiertiff11<-tclvalue(Exportationfichiertiff1)
  if(Exportationfichiertiff11=="yes" & choixconcentration ==texte52) {


    XX <- log10(data_moy_ADN_traite$Standard)
    #XX<-data_moy_ADN_traite$Standard
    YY <- data_moy_ADN_traite$Moy
    REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
    REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
    r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
    Efficacitetrace <- Efficacitetraite
    LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
    LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))


    FigureExportee_Tiff <- tclvalue(tkgetSaveFile())
    figureExportee_Tiff <- paste(FigureExportee_Tiff,".tiff",sep="")

    Standard_curves_UFC_mL <- function() {
      par(lab=c(10,10,10),oma=c(5,2,0,0))
      plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
           pch=16,col="red",
           main="",
           xlab=" ",
           ylab=expression(paste(C[t], "value")))
      #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
      arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
             XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
             length = 0.05, # width of the arrowhead
             angle = 90,
             code = 3)

      abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
      legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                     "\nE=",Efficacitetrace, " %",
                                     "\nLOD=",LODtracetraite, texte52,
                                     "\nLOQ=",LOQtracetraite, texte52),bty="n",cex=0.75)


            axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits = 3),
           line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
      # text(-1,-10,"UFC/mL", cex= 0.8,col="blue",xpd=TRUE)
      text(-1,-10, texte52, cex= 0.8,col="blue",xpd=TRUE)
      #text(-1,-6,expression(paste(Log[10],"(UFC/mL)")), cex= 0.7,col="black",xpd=TRUE)
      text(-1,-6, texte52bis , cex= 0.7,col="black",xpd=TRUE)

    }

    tiff(filename=figureExportee_Tiff, compression = "lzw",
         height=18, width=18, units="cm", res=400)

    Standard_curves_UFC_mL()

    dev.off()

  }else{
    if(Exportationfichiertiff11=="yes" & choixconcentration == texte53){

      XX <- log10(data_moy_ADN_traite$Standard)
      #XX<-data_moy_ADN_traite$Standard
      YY <- data_moy_ADN_traite$Moy
      REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
      REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
      r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
      Efficacitetrace <- Efficacitetraite

      LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
      LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))


      FigureExportee_Tiff <- tclvalue(tkgetSaveFile())
      figureExportee_Tiff <- paste(FigureExportee_Tiff,".tiff",sep="")

      Standard_curves_copies_mL <- function() {
        par(lab=c(10,10,10),oma=c(5,2,0,0))
        plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
             pch=16,col="red",
             main="",
             xlab="",
             ylab=expression(paste(C[t], "value")))
        #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
        arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
               XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
               length = 0.05, # width of the arrowhead
               angle = 90,
               code = 3)

        abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
        legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                       "\nE=",Efficacitetrace, " %",
                                       "\nLOD=",LODtracetraite, "copies/mL",
                                       "\nLOQ=",LOQtracetraite, "copies/mL"),bty="n",cex=0.75)



        axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits=3),
             line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
        text(-1,-10,"copies/mL", cex= 0.8,col="blue",xpd=TRUE)
        text(-1,-6,expression(paste(Log[10], " (copies/mL)")), cex= 0.7,col="black",xpd=TRUE)
      }

      tiff(filename=figureExportee_Tiff, compression = "lzw",
           height=18, width=18, units="cm", res=400)

      Standard_curves_copies_mL()

      dev.off()
    }else{}
  }

}

Export_Tiff.but <- tkbutton(fenetre_fraphique, command=exportertiffplot)
tkconfigure(Export_Tiff.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"tiff.gif",
                                                                        fsep=.Platform$file.sep)))

tkgrid(Export_Tiff.but, row=4, column=0)



exporterjpegplot <- function() {

  Exportationfichierjpeg1=tkmessageBox(title= texte57, message= texte58,
                                       icon="info",type="yesno")

  Exportationfichierjpeg11<-tclvalue(Exportationfichierjpeg1)
  if(Exportationfichierjpeg11=="yes" & choixconcentration == texte52) {

    XX <- log10(data_moy_ADN_traite$Standard)
    #XX<-data_moy_ADN_traite$Standard
    YY <- data_moy_ADN_traite$Moy
    REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
    REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
    r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
    Efficacitetrace <- Efficacitetraite
    LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
    LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))


    FigureExportee_Jpeg <- tclvalue(tkgetSaveFile())
    figureExportee_Jpeg <- paste(FigureExportee_Jpeg,".jpeg",sep="")

    Standard_curves_UFC_mL <- function() {
      par(lab=c(10,10,10),oma=c(5,2,0,0))
      plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
           pch=16,col="red",
           main="",
           xlab=" ",
           ylab=expression(paste(C[t], "value")))
      #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
      arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
             XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
             length = 0.05, # width of the arrowhead
             angle = 90,
             code = 3)

      abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
      legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                     "\nE=",Efficacitetrace, " %",
                                     "\nLOD=",LODtracetraite, texte52,
                                     "\nLOQ=",LOQtracetraite, texte52),bty="n",cex=0.75)


      axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits = 3),
           line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
      # text(-1,-10,"UFC/mL", cex= 0.8,col="blue",xpd=TRUE)
      text(-1,-10, texte52, cex= 0.8,col="blue",xpd=TRUE)
      #text(-1,-6,expression(paste(Log[10],"(UFC/mL)")), cex= 0.7,col="black",xpd=TRUE)
      text(-1,-6, texte52bis , cex= 0.7,col="black",xpd=TRUE)

    }

    jpeg(filename=figureExportee_Jpeg, height=18, width=18, units="cm", res=400)

    Standard_curves_UFC_mL()

    dev.off()

  }else{
    if(Exportationfichierjpeg11=="yes" & choixconcentration == texte53){

      XX <- log10(data_moy_ADN_traite$Standard)
      #XX<-data_moy_ADN_traite$Standard
      YY <- data_moy_ADN_traite$Moy
      REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
      REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
      r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
      Efficacitetrace <- Efficacitetraite
      LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
      LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))



      FigureExportee_Jpeg <- tclvalue(tkgetSaveFile())
      figureExportee_Jpeg <- paste(FigureExportee_Jpeg,".jpeg",sep="")

      Standard_curves_copies_mL <- function() {
        par(lab=c(10,10,10),oma=c(5,2,0,0))
        plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
             pch=16,col="red",
             main="",
             xlab="",
             ylab=expression(paste(C[t], "value")))
        #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
        arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
               XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
               length = 0.05, # width of the arrowhead
               angle = 90,
               code = 3)

        abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")

        legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                       "\nE=",Efficacitetrace, " %",
                                       "\nLOD=",LODtracetraite, "copies/mL",
                                       "\nLOQ=",LOQtracetraite, "copies/mL"),bty="n",cex=0.75)


        axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits=3),
             line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
        text(-1,-10,"copies/mL", cex= 0.8,col="blue",xpd=TRUE)
        text(-1,-6,expression(paste(Log[10], " (copies/mL)")), cex= 0.7,col="black",xpd=TRUE)
      }

      jpeg(filename=figureExportee_Jpeg, height=18, width=18, units="cm", res=400)

      Standard_curves_copies_mL()


      dev.off()
    }else{}
  }

}

Export_jpeg.but <- tkbutton(fenetre_fraphique, command=exporterjpegplot)
tkconfigure(Export_jpeg.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"jpg.gif",
                                                                        fsep=.Platform$file.sep)))

tkgrid(Export_jpeg.but, row=4, column=1)



exporterpdfplot <- function() {

  Exportationfichierpdf1=tkmessageBox(title= texte59, message= texte60,
                                      icon="info",type="yesno")


  Exportationfichierpdf11<-tclvalue(Exportationfichierpdf1)
  if(Exportationfichierpdf11=="yes" & choixconcentration == texte52) {


    XX <- log10(data_moy_ADN_traite$Standard)
    #XX<-data_moy_ADN_traite$Standard
    YY <- data_moy_ADN_traite$Moy
    REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
    REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
    r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
    Efficacitetrace <- Efficacitetraite
    LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
    LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))


    FigureExportee_Pdf <- tclvalue(tkgetSaveFile())
    figureExportee_Pdf <- paste(FigureExportee_Pdf,".pdf",sep="")

    Standard_curves_UFC_mL <- function() {
      par(lab=c(10,10,10),oma=c(5,2,0,0))
      plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
           pch=16,col="red",
           main="",
           xlab=" ",
           ylab=expression(paste(C[t], "value")))
      #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
      arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
             XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
             length = 0.05, # width of the arrowhead
             angle = 90,
             code = 3)

      abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
      legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                     "\nE=",Efficacitetrace, " %",
                                     "\nLOD=",LODtracetraite, texte52,
                                     "\nLOQ=",LOQtracetraite, texte52),bty="n",cex=0.75)


      axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits = 3),
           line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
      # text(-1,-10,"UFC/mL", cex= 0.8,col="blue",xpd=TRUE)
      text(-1,-10, texte52 , cex= 0.8,col="blue",xpd=TRUE)
      #text(-1,-6,expression(paste(Log[10],"(UFC/mL)")), cex= 0.7,col="black",xpd=TRUE)
      text(-1,-6, texte52bis , cex= 0.7,col="black",xpd=TRUE)

    }

    pdf(file=figureExportee_Pdf, paper="a4r", height=9, width=9)

    Standard_curves_UFC_mL()

    dev.off()

  }else{
    if(Exportationfichierpdf11=="yes" & choixconcentration ==texte53){

      XX <- log10(data_moy_ADN_traite$Standard)
      #XX<-data_moy_ADN_traite$Standard
      YY <- data_moy_ADN_traite$Moy
      REGRESSION_PENTE <- format(regressiontraite$coefficients[2], digits = 3)
      REGRESSION_INTERCEPTION <- format(regressiontraite$coefficients[1], digits = 3)
      r2STD <- format(summary(regressiontraite)$r.squared, digits = 3)
      Efficacitetrace <- Efficacitetraite
      LODtracetraite <- ceiling(10^(3*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))
      LOQtracetraite  <- ceiling(10^(10*(summary.lm(regressiontraite)$coefficients[1,2])/abs(regressiontraite$coefficients[2])))

      FigureExportee_Pdf <- tclvalue(tkgetSaveFile())
      figureExportee_Pdf <- paste(FigureExportee_Pdf,".pdf",sep="")

      Standard_curves_copies_mL <- function() {
        par(lab=c(10,10,10),oma=c(5,2,0,0))
        plot(XX,YY,cex.axis=0.7,las=1,xlim=c(0,10),ylim=c(0,50),
             pch=16,col="red",
             main="",
             xlab="",
             ylab=expression(paste(C[t], "value")))
        #abline(lm(data_moy_ADN_traite$Moy~data_moy_ADN_traite$log10_Sdt,data_moy_ADN_traite))
        arrows(XX, data_moy_ADN_traite$Moy + data_moy_ADN_traite$sd,
               XX, data_moy_ADN_traite$Moy - data_moy_ADN_traite$sd,
               length = 0.05, # width of the arrowhead
               angle = 90,
               code = 3)

        abline(b = REGRESSION_PENTE, a = REGRESSION_INTERCEPTION,col="black")
        legend("topright",legend=paste("y  =", REGRESSION_PENTE ,"x  + ",REGRESSION_INTERCEPTION, "\nR2 =", r2STD,
                                       "\nE=",Efficacitetrace, " %",
                                       "\nLOD=",LODtracetraite, "copies/mL",
                                       "\nLOQ=",LOQtracetraite, "copies/mL"),bty="n",cex=0.75)


        axis(1,at=c(0,sort(XX)),labels=format(c(0,sort(data_moy_ADN_traite$Standard)),scientific=TRUE,digits=3),
             line=3,col="blue",col.ticks="blue",col.axis="blue",cex.axis=0.75, las=2)
        text(-1,-10,"copies/mL", cex= 0.8,col="blue",xpd=TRUE)
        text(-1,-6, texte52bis , cex= 0.7,col="black",xpd=TRUE)
      }

      pdf(file=figureExportee_Pdf, paper="a4r", height=9, width=8.5)

      Standard_curves_copies_mL()

      dev.off()
    }else{}
  }

}


Export_pdf.but <- tkbutton(fenetre_fraphique, command=exporterpdfplot)
tkconfigure(Export_pdf.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"pdf.gif",
                                                                       fsep=.Platform$file.sep)))

tkgrid(Export_pdf.but, row=4, column=2)


} # fin fonction graphique_regression


videregression <- tklabel(Interface_PMA_qPCR, text=" ",font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(videregression, row=10,  columnspan=4)

Calcul_regression.but <- tkbutton(Interface_PMA_qPCR,command=Calcul_regression)
tkconfigure(Calcul_regression.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"calcul.gif",
                                                                              fsep=.Platform$file.sep)))

tkgrid(Calcul_regression.but,row=11,column=2)


graphique_regression.but <- tkbutton(Interface_PMA_qPCR,command=graphique_regression)
tkconfigure(graphique_regression.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"Plot.gif",
                                                                                 fsep=.Platform$file.sep)))

tkgrid(graphique_regression.but,row=11,column=3)




##############################################################################################
###############

vide3 <- tklabel(Interface_PMA_qPCR, text="",font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(vide3, row=12,  columnspan=4)

#etape3=tklabel(Interface_PMA_qPCR, text="3 Concentration dans les u00e9chantillons",font=fontHeading,foreground="chocolate4",bg="wheat1")
etape3 <- tklabel(Interface_PMA_qPCR, text=texte29,font=fontHeading,foreground="chocolate4",bg="wheat1")
tkgrid(etape3, row=13,  columnspan=6, sticky="w")


#etape31=tklabel(Interface_PMA_qPCR, text="Nb u00e9chantillons",font=font2,foreground="chocolate4",bg="wheat1")
etape31 <- tklabel(Interface_PMA_qPCR, text=texte61, font=font2,foreground="chocolate4",bg="wheat1")
tkgrid(etape31, row=14,  column=2)



Calcul_Nb_echantillonstraites = function(){
  if(exists("dataimportees_echantillons")==FALSE) {" "} else {
    Calcul_Nb_echantillonstraites2 = function(){
      Nbechantillonstraites <- subset(dataimportees_echantillons, Condition %in% c("PMA"))
      Nbechantillonstraites2 <- length(Nbechantillonstraites$Nom_Echantillon)
      Nbechantillonstraites2 <<- as.character(Nbechantillonstraites2)
    }
  }
}

ImportationNbFichier_ech_case <- tkentry(Interface_PMA_qPCR,width=10,foreground=couleur1_ech,
                                         bg="black", textvariable=tclVar(Calcul_Nb_echantillonstraites()))
tkgrid(ImportationNbFichier_ech_case,row=14,column=3)



# Ru00e9cupu00e9ration de l'u00e9quation pour calculer les nombres de copies des u00e9chantillons

## ATTENTION DONNEES EN LOG 10 PENSEZ A CONVERTIR
## La formule qui suit corrige le log10

calcul_echantillons <- function(){
regequation <- data.frame(summary(regressiontraite)$coef[,1])
#row.names(regequation)=c("interception","pente")
row.names(regequation) <- c(texte30,texte31)
#colnames(regequation)=c("Valeurs")
colnames(regequation) <- c(texte32)

regequation <<- regequation



# Ru00e9cupu00e9ration des Echantillons et Ct
# Subset garde uniquement les donnu00e9es non traite au PMA
dataimportees_echantillons_traite <- subset(dataimportees_echantillons, Condition %in% c("PMA"))

Echantillons_bruts <- dataimportees_echantillons_traite
# Selection analyse UFC/mL ou nb copies/mL
# Condition permettant de vu00e9rifier si la colonne Concentration UFC/mL existe
# Comme elle n'existe pas, elle se cru00e9u00e9e
# Condition si anglais renomme les header colonnes

if(choixconcentration == texte52 ) {

if(exists("dataimportees_echantillons_traite$Concentration_UFC_mL")==FALSE) {dataimportees_echantillons_traite$Concentration_UFC_mL=0} else {dataimportees_echantillons_traite$Concentration_UFC_mL}

Echantillons_bruts <<- Echantillons_bruts

Echantillons_bruts$Concentration_UFC_mL <- 10^((Echantillons_bruts$Ct-regequation[1,])/regequation[2,])
Echantillons_bruts <<- Echantillons_bruts


}else{

  if(exists("dataimportees_echantillons_traite$Concentration_copies_mL")==FALSE) {dataimportees_echantillons_traite$Concentration_copies_mL=0} else {dataimportees_echantillons_traite$Concentration_copies_mL}

  Echantillons_bruts <<- Echantillons_bruts

  Echantillons_bruts$Concentration_copies_mL <- 10^((Echantillons_bruts$Ct-regequation[1,])/regequation[2,])
  Echantillons_bruts <<- Echantillons_bruts

}


# langue selectionnee : anglais

#if(choix_langue2 == "ANGLAIS" & choixconcentration == texte52) {
  if(choix_langue == "ANGLAIS" & choixconcentration == texte52) {
  names(Echantillons_bruts)[1] <- "Name_Sample"
  names(Echantillons_bruts)[4] <- "Concentration_CFU_mL"
  Echantillons_bruts <<- Echantillons_bruts

}else{}

#if(choix_langue2 == "ANGLAIS" & choixconcentration == texte53) {
  if(choix_langue == "ANGLAIS" & choixconcentration == texte53) {
  names(Echantillons_bruts)[1] <- "Name_Sample"
  Echantillons_bruts <<- Echantillons_bruts

}else{}



} # fin fonction calcul_echantillons




Tableau_Echantillons_concentration <- function(){
  ## covert a data frame into a character based on
  .toCharacter <- function(x,width,...) UseMethod(".toCharacter")
  .toCharacter.default <- function(x,width,...) as.character(x)
  .toCharacter.integer <- function(x,width,...) {
    if(missing(width)) width <- max(nchar(as.character(x))) + 2
    format(x, justify = "right", width = width)
  }
  .toCharacter.numeric <- function(x,width,...) {
    if(missing(width)) width <- max(nchar(as.character(x))) + 2
    format(x,trim = FALSE, width = width, justify = "right")
  }
  .toCharacter.factor <- function(x,width,...) {
    if(missing(width)) width <- max(nchar(as.character(x))) + 2
    .toCharacter(as.character(x),width,...)
  }
  .toCharacter.logical <- function(x,width,...) {
    if(missing(width)) width <- 7
    format(as.character(x), justify = "centre", width = width)
  }
  .toCharacter.data.frame <- function(x,width =  10, ...) {
    nms <- dimnames(x)
    DF <- as.data.frame(lapply(x,function(i) .toCharacter(i, width = width)),
                        stringsAsFactors = FALSE)
    dimnames(DF) <- nms
    return(DF)
  }

  addScrollbars <- function(parent, widget) {
    xscr <- ttkscrollbar(parent, orient = "horizontal",
                         command = function(...) tkxview(widget, ...))
    yscr <- ttkscrollbar(parent, orient = "vertical",
                         command = function(...) tkyview(widget, ...))

    tkconfigure(widget,
                xscrollcommand = function(...) tkset(xscr,...),
                yscrollcommand = function(...) tkset(yscr,...))

    tkgrid(widget, row = 0, column = 0, sticky = "news")
    tkgrid(yscr,row = 0,column = 1, sticky = "ns")
    tkgrid(xscr, row = 1, column = 0, sticky = "ew")
    tkgrid.columnconfigure(parent, 0, weight = 1)
    tkgrid.rowconfigure(parent, 0, weight = 1)
  }



  affichage_concentration_Echantillons <<- Echantillons_bruts
  affichage_concentration_Echantillons[,1] <- as.character(Echantillons_bruts[,1])
  affichage_concentration_Echantillons[,2] <- as.character(Echantillons_bruts[,2])
  affichage_concentration_Echantillons[,3] <- as.numeric(Echantillons_bruts[,3])
  affichage_concentration_Echantillons[,4] <- as.numeric(Echantillons_bruts[,4])


  DF <- affichage_concentration_Echantillons


  ###################################################
  window_affichage_concentration_Echantillons <- tktoplevel()
  #tkwm.title(window_affichage_concentration_Echantillons, "Ru00e9sultats des qPCR")
  tkwm.title(window_affichage_concentration_Echantillons, texte33)
  tkwm.geometry(window_affichage_concentration_Echantillons, "600x450")
  frame <- ttkframe(window_affichage_concentration_Echantillons, padding = c(3,3,3,12))
  tkpack(frame, expand = TRUE, fill = "both")


  ###################################################
  frame_0 <- ttkframe(frame); tkpack(frame_0, fill = "x")
  #label <- ttklabel(frame_0, text = "filter:")
  label <- ttklabel(frame_0, text = texte34)
  tkpack(label, side = "left")
  filter_var <- tclVar("")
  filter_entry <- ttkentry(frame_0, textvariable = filter_var)
  tkpack(filter_entry, side = "left")
  ###################################################
  frame_1 <- ttkframe(frame)
  tkpack(frame_1, expand = TRUE, fill = "both")
  treeview <- ttktreeview(frame_1, columns = 1:ncol(DF),
                          #displaycolumns = 1:ncol(DF),
                          show = "headings",     # not "tree"
                          selectmode = "browse") # single selection
  addScrollbars(frame_1, treeview)


  frame_2 <- ttkframe(frame); tkpack(frame_2, fill = "both")


  Savefunctioncsv <- function(){
    #### Select field to import ASCII profiles ####
    #fil=if (interactive()) file.choose()
    tt2 <- tktoplevel()
    tkwm.geometry(tt2, "400x200")
    #tkwm.title(tt2,"Exporter fichier.csv")
    tkwm.title(tt2, texte35)
    tkgrid(tklabel(tt2,text=""))
    t1 <- tkframe(tt2)
    #text1<-tklabel(t1,text="Quel est le su00e9parateur ?")
    text1 <- tklabel(t1,text=texte36)
    #sep1<-c("tab (tabulation)",", (virgule)","; (point virgule)",". (point)"," (espace)")
    sep1 <- c(texte37,texte13,texte38,texte12,texte39)
    sep2 <- tkwidget(t1,"ComboBox",editable=FALSE,values=sep1,height=3)
    tkpack(text1,sep2,side="left")
    tkgrid(t1)
    t2 <- tkframe(tt2)
    #text2<-tklabel(t2,text="Quel est le symbole du00e9cimal ?")
    text2 <- tklabel(t2,text=texte11)
    #dec1<-c(". (point)",", (virgule)")
    dec1 <- c(texte12,texte13)
    dec2 <- tkwidget(t2,"ComboBox",editable=FALSE,values=dec1,height=2)
    tkpack(text2,dec2,side="left")
    tkgrid(t2)
    #t3<-tkframe(tt2)
    #text3<-tklabel(t3,text="Do you have header")
    #hea1 <- c("yes","no")
    #hea2 <- tkwidget(t3,"ComboBox",editable=FALSE,values=hea1,height=2)
    #tkpack(text3,hea2,side="left")
    #tkgrid(t3)
    tkgrid(tklabel(tt2,text=""))
    #### Preview channels ####
    view.channels<-function()
    {
      sep3 <-unlist(as.numeric(tcl(sep2,"getvalue"))+1 )
      if(sep3==1) sep4 <- "\t"
      if(sep3==2) sep4 <- ","
      if(sep3==3) sep4 <- ";"
      if(sep3==4) sep4 <- "."
      if(sep3==5) sep4 <- " "
      dec3 <- unlist(as.numeric(tcl(dec2,"getvalue"))+1 )
      if(dec3==1) dec4 <- "."
      if(dec3==2) dec4 <- ","
      #hea3 <-unlist(as.numeric(tcl(hea2,"getvalue"))+1 )
      #if(hea3==1) hea4<-TRUE
      #if(hea3==2) hea4<-FALSE

      dataExportees <- tclvalue(tkgetSaveFile())
      dataexportees <- paste(dataExportees,".csv",sep="")
      write.table(affichage_concentration_Echantillons,file = dataexportees,col.names=T,row.names=F,dec=dec4,sep=sep4)
      tkdestroy(tt2)
    }
    #tkgrid(tkbutton(tt2,text="Exporter fichier",command=view.channels))
    tkgrid(tkbutton(tt2,text=texte40,command=view.channels))
    tkgrid(tklabel(tt2,text=""))

  }
  #Savecsv.but<-tkbutton(frame_2,text=" Exporter\ncsv ",command=Savefunctioncsv)
  Savecsv.but <- tkbutton(frame_2,text=texte41,command=Savefunctioncsv)
  tkgrid(Savecsv.but,row=1,column=2)



  Savefunctionxlsx <- function(){
    #Exportationfichierxlsx=tkmessageBox(title="Exporter fichier en xlsx", message=" Voulez-vous exporter votre fichier en xlsx ?",
    #                                    icon="info",type="yesno")
    Exportationfichierxlsx <- tkmessageBox(title=texte42, message=texte43,
                                        icon="info",type="yesno")
    Exportationfichierxlsx1 <- tclvalue(Exportationfichierxlsx)
    if(Exportationfichierxlsx1=="yes") {

      dataExportees <- tclvalue(tkgetSaveFile())
      dataexportees <- paste(dataExportees,".xlsx",sep="")
      write.xlsx(affichage_concentration_Echantillons,file = dataexportees,col.names=T,row.names=F,sheetName="1")


    }else{tkdestroy(Exportationfichierxlsx)}
  }
  #Savexlsx.but<-tkbutton(frame_2,text=" Exporter\nxlsx ",command=Savefunctionxlsx)
  Savexlsx.but <- tkbutton(frame_2,text= texte44,command=Savefunctionxlsx)
  tkgrid(Savexlsx.but,row=1,column=4)

  ###################################################
  widths <- c(rep(100,dim(DF)[2])) #c(100,200) #rep(100,dim(DF)[2]))           # hard coded
  nms <- names(DF)
  for(i in 1:dim(DF)[2]) {
    tcl(treeview, "heading", i, text = nms[i])
    tcl(treeview, "column", i, width = widths[i],
        stretch = TRUE, anchor = "w")
  }
  ###################################################
  fillTable <- function(treeview, DF) {
    children <- as.character(tcl(treeview, "children", ""))
    for(i in children)
      tcl(treeview, "delete", i)
    #shade <- c("none", "gray")
    for(i in seq_len(nrow(DF)))
      tcl(treeview, "insert", "", "end",
          text = "",
          values = unlist(DF[i,]))
    tktag.configure(treeview, "gray", background = "red")
  }
  ###################################################
  fillTable(treeview, DF)
  ## permet utiliser la commade filtre
  cur_ind <- 1:nrow(DF)
  tkbind(filter_entry, "<KeyRelease>", function(W, K) {
    val <- tclvalue(tkget(W))
    poss_vals <- apply(DF, 1, function(...)
      paste(..., collapse = " "))
    ind<- grep(val, poss_vals)
    if(length(ind) == 0) ind <- 1:nrow(DF)
    fillTable(treeview, DF[ind,])
  })

} # fin de la function table

#}

Calulechantillons_et_tableau_resultats <- function(){

  calcul_echantillons()
  Tableau_Echantillons_concentration()
}


Calcul_echantillons.but <- tkbutton(Interface_PMA_qPCR,command=Calulechantillons_et_tableau_resultats)
tkconfigure(Calcul_echantillons.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"Tubes.gif",
                                                                              fsep=.Platform$file.sep)))

tkgrid(Calcul_echantillons.but,row=15,column=3, columnspan=3)

##########################################################################################################
######
apropos <- function(){
  aidefenetre<-tktoplevel()

  #tkwm.title(aidefenetre," A propos de InterfaceqPCR")
  tkwm.title(aidefenetre,texte6)
  tkconfigure(aidefenetre,bg="gray80",cursor="hand2") #  wheat1  hand2


  # Taille
  tkwm.geometry(aidefenetre, "500x650")   # Largueur x hauteur
  tkwm.resizable(aidefenetre,F,F)
  # Police
  font2 <- tkfont.create(family="times",size=14,weight="bold",slant="italic")
  font3 <- tkfont.create(family="times",size=12,weight="bold")

  path.logobatman <- file.path(path.package("InterfaceqPCR"),"a_laise_BZH2.gif", fsep=.Platform$file.sep)
  mon.imagebatman <- tkimage.create("photo", file=path.logobatman )

  montre.batman <- tklabel(aidefenetre, image=mon.imagebatman, background="white")
  tkgrid(montre.batman,row=1, column=3,columnspan=2)

  videaide <- tklabel(aidefenetre,text=" ",font=font2,bg="gray80")
  tkgrid(videaide,row=2)

  #     message<-tklabel(aidefenetre,text="InterfaceqPCR\nTraitement des donnu00e9es issues de la qPCR Version 1.0
  #                      \nOlivier Le Goff
  #                      \nLaboratoire EA CIDAM Clermont-Ferrand\nUniversitu00e9 d'Auvergne\n2017",bg="gray80",font=font3)
  #
  message <- tklabel(aidefenetre,text=texte7,bg="gray80",font=font3)

  tkgrid(message,row=3, column=3,columnspan=2)

  path.logoimmunoaide <- file.path(path.package("InterfaceqPCR"),"logoEAUDA.gif", fsep=.Platform$file.sep)
  mon.imageimmunoaide <- tkimage.create("photo", file=path.logoimmunoaide)

  montre.imageimmunoaide <- tklabel(aidefenetre, image=mon.imageimmunoaide, background="white")
  tkgrid(montre.imageimmunoaide,row=4, column=3)

  fermeraide <- function(){
    tkdestroy(aidefenetre)
  }
  #Ok.butaide<-tkbutton(aidefenetre,text="Quitter",command=fermeraide,font=font2,foreground="Blue")
  Ok.butaide <- tkbutton(aidefenetre,text=texte8, command=fermeraide,font=font2,foreground="Blue")

  tkgrid(Ok.butaide,row=4, column=4)


}

fermeture_Interface_PMAqPCR <- function(){
  tkdestroy(Interface_PMA_qPCR)
  Interface_qPCR_choix()
}

Fermeture_PMAqPCR.but <- tkbutton(Interface_PMA_qPCR,command=fermeture_Interface_PMAqPCR)
tkconfigure(Fermeture_PMAqPCR.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"start.gif",
                                                                 fsep=.Platform$file.sep)))

tkgrid(Fermeture_PMAqPCR.but,row=16,column=2)



Aide.but <- tkbutton(Interface_PMA_qPCR,command=apropos)
tkconfigure(Aide.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"apropos.gif",
                                                                                fsep=.Platform$file.sep)))

tkgrid(Aide.but,row=16,column=7)


} # fin de la fonction Interface_PMAqPCR()

