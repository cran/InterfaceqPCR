
#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                             |
#      | Interface qPCR  package                     |
#      | Analyse des donnees qPCR                    |
#      | Langue                                      |
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



LANGUE <- function(choix_langue=NULL){


  texte1<-texte2<-texte3<-texte4<-texte5<-texte6<-texte7<-texte8<-texte9<-texte10<-texte11<-texte12<-texte13<-
    texte14<-texte15<-texte16<-texte17<-texte18<-texte19<-texte20<-texte21<-texte22<-texte23<-texte24<-texte25<-
    texte26<-texte27<-texte28<-texte29<-texte30<-texte31<-texte32<-texte33<-texte33bis<-texte34<-texte35<-texte36<-
    texte37<-texte38<-texte39<-texte40<-texte41<-texte42<-texte43<-texte44<-texte45<-texte46<-texte47<-texte48<-
    texte48bis<-texte49<-texte50<-texte51<-texte52<-texte52bis<-texte53<-texte54<-texte55<-texte56<-texte57<-
    texte58<-texte59<-texte60<-texte61<-texte62<-NULL

  choix_langue2<-choix_langue2

  choix_langue <- choix_langue2


##### texte issu de Start_select_qPCR_PMA_qPCR
  texte1 <<- if(choix_langue=="FRANCAIS") {" Bienvenue s\u00e9lection de l'analyse "} else {" Welcome Select the method "}
  texte2 <<- if(choix_langue=="FRANCAIS") {" Choisir le type d'analyse "} else {" Select the method "}
  texte3 <<- if(choix_langue=="FRANCAIS") {" pour les \u00e9chantillons quantifi\u00e9s "} else {" to analyse the samples "}
  texte4 <<- if(choix_langue=="FRANCAIS") {" Analyse \u00e9chantillons trait\u00e9s au PMA "} else {" Samples treated with PMA "}
  texte5 <<- if(choix_langue=="FRANCAIS") {" Analyse \u00e9chantillons non trait\u00e9s au PMA "} else {" Samples not treated with PMA "}

  #### texte de l'a propos

  texte6 <<- if(choix_langue=="FRANCAIS") {" A propos de InterfaceqPCR "} else {" About of InterfaceqPCR"}
  texte7 <<- if(choix_langue=="FRANCAIS") {" InterfaceqPCR\nTraitement des donn\u00e9es issues de la qPCR \nVersion 1.0
    \nOlivier Le Goff
    \nLaboratoire MEDIS (Microbiologie Environnement DIgestif Sant\u00e9)
    \nUniversit\u00e9 Clermont Auvergne
    \nClermont-Ferrand
    \n2017 "}
  else {"InterfaceqPCR\nTreatment of datas results from qPCR\nVersion 1.0
    \nOlivier Le Goff
    \nLaboratoire MEDIS (Microbiologie Environnement DIgestif Sant\u00e9)
    \nUniversit\u00e9 Clermont Auvergne
    \nClermont-Ferrand
    \n2017 "}


  texte8 <<- if(choix_langue=="FRANCAIS") {" Quitter "} else {" Exit "}

##### fin du texte issu de Start_select_qPCR_PMA_qPCR

##### texte issu de Interface_PMAqPCR
  texte9 <<- if(choix_langue=="FRANCAIS") {" Interface qPCR \u00e9chantillons trait\u00e9s au PMA "} else {" Samples treated with PMA"}
  texte10 <<- if(choix_langue=="FRANCAIS") {" Importer fichier.csv "} else {" Import file.csv "}
  texte11 <<- if(choix_langue=="FRANCAIS") {" Quel est le symbole d\u00e9cimal ? "} else {" Decimal symbol "}
  texte12 <<- if(choix_langue=="FRANCAIS") {" . (point) "} else {" . (dot)  "}
  texte13 <<- if(choix_langue=="FRANCAIS") {" , (virgule) "} else {" , (comma) "}
  texte14 <<- if(choix_langue=="FRANCAIS") {" Pr\u00e9sence d'un ent\u00eate ? "} else {" Header ? "}
  texte15 <<- if(choix_langue=="FRANCAIS") {" Oui "} else {" Yes "}
  texte16 <<- if(choix_langue=="FRANCAIS") {" Non "} else {" No "}
  texte17 <<- if(choix_langue=="FRANCAIS") {" Importer fichier"} else {" Import file "}
  texte18 <<- if(choix_langue=="FRANCAIS") {" 1 Importer les fichiers " } else {" 1 Import files  "}
  texte19 <<- if(choix_langue=="FRANCAIS") {" Fichier Gamme \u00e9talon "} else {" Standard file "}
  texte20 <<- if(choix_langue=="FRANCAIS") {" Format fichier de la gamme \u00e9talon "} else {" Format of Standard file "}
  texte21 <<- if(choix_langue=="FRANCAIS") {" Trame fichier "} else {" Model File "}
  texte22 <<- if(choix_langue=="FRANCAIS") {" ou "} else {" or "}
  texte23 <<- if(choix_langue=="FRANCAIS") {" Fichier cr\u00e9\u00e9  :  "} else {" File created:  "}
  texte24 <<- if(choix_langue=="FRANCAIS") {" Actualiser "} else {" Refresh "}
  texte25 <<- if(choix_langue=="FRANCAIS") {" Fichier Echantillons "} else {" Samples File "}
  texte26 <<- if(choix_langue=="FRANCAIS") {" Format fichier des \u00e9chantillons "} else {" Format of samples file  "}

  texte27 <<- if(choix_langue=="FRANCAIS") {" 2 Droite de r\u00e9gression "} else {" 2 Regression curve "}
  texte28 <<- if(choix_langue=="FRANCAIS") {" Droite de r\u00e9gression issue des Standards qPCR "} else {" Regression curve resulting from qPCR standards "}

  texte29 <<- if(choix_langue=="FRANCAIS") {" 3 Concentration dans les \u00e9chantillons  "} else {" 3 Concentration in samples "}
  texte30 <<- if(choix_langue=="FRANCAIS") {" Interception  "} else {" Intercept "}
  texte31 <<- if(choix_langue=="FRANCAIS") {" Pente  "} else {" Slope  "}
  texte32 <<- if(choix_langue=="FRANCAIS") {" Valeurs "} else {" Values "}
  texte33 <<- if(choix_langue=="FRANCAIS") {" R\u00e9sultats des PMA qPCR "} else {" Results of PMA qPCR  "}
  texte33bis <<- if(choix_langue=="FRANCAIS") {" R\u00e9sultats des qPCR "} else {" Results of qPCR  "}
  texte34 <<- if(choix_langue=="FRANCAIS") {" Filtre : "} else {" Filter: "}
  texte35 <<- if(choix_langue=="FRANCAIS") {" Exporter fichier.csv "} else {" Export file.csv "}
  texte36 <<- if(choix_langue=="FRANCAIS") {" Quel est le s\u00e9parateur ? "} else {" Separator ? "}
  texte37 <<- if(choix_langue=="FRANCAIS") {" tab (tabulation) "} else {" tab (tabulation) "}
  texte38 <<- if(choix_langue=="FRANCAIS") {" ; (point virgule) "} else {" ; (semicolon) "}
  texte39 <<- if(choix_langue=="FRANCAIS") {"  (espace) "} else {"  (space) "}
  texte40 <<- if(choix_langue=="FRANCAIS") {" Exporter fichier "} else {" Export file "}
  texte41 <<- if(choix_langue=="FRANCAIS") {" Exporter\ncsv "} else {" Export\ncsv "}
  texte42 <<- if(choix_langue=="FRANCAIS") {" Exporter fichier en xlsx "} else {" Export file in xlsx "}
  texte43 <<- if(choix_langue=="FRANCAIS") {" Voulez-vous exporter votre fichier en xlsx ? "} else {" Save file in xlsx ? "}
  texte44 <<- if(choix_langue=="FRANCAIS") {" Exporter\nxlsx "} else {" Export\nxlsx "}

##### fin du texte issu de Interface_PMAqPCR

  ##### texte issu de Interface_qPCR

  texte45 <<- if(choix_langue=="FRANCAIS") {" Interface qPCR \u00e9chantillons non trait\u00e9s au PMA "} else {"Samples not treated with PMA "}


  ####texte issu de interface chargement Fichiers
  texte46 <<- if(choix_langue=="FRANCAIS") {" Saisir les donn\u00e9es"} else {" Import Data "}
  texte47 <<- if(choix_langue=="FRANCAIS") {" Saisir les donn\u00e9es "} else {" Complete Data files "}
  texte48 <<- if(choix_langue=="FRANCAIS") {" Renommer les fichiers "} else {" Rename files "}
  texte48bis <<- if(choix_langue=="FRANCAIS") {" Enregistrer dans un autre r\u00e9pertoire"} else {" Save in other folder "}
  texte49 <<- if(choix_langue=="FRANCAIS") {" Fichiers Standards "} else {" Standard File "}
  texte50 <<- if(choix_langue=="FRANCAIS") {" Fichiers Echantillons "} else {" Sample File "}

  texte51 <<- if(choix_langue=="FRANCAIS") {" Concentration en : "} else {" Concentration in: "}
  texte52 <<- if(choix_langue=="FRANCAIS") {" UFC/mL "} else {" CFU/mL "}
  texte52bis <<- if(choix_langue=="FRANCAIS") {expression(paste(Log[10], " (UFC/mL)" ))} else {expression(paste(Log[10], " (CFU/mL)" ))}
  texte53 <<- if(choix_langue=="FRANCAIS") {" Nb copies/mL "} else {" Nb copies/mL "}
  texte54 <<- if(choix_langue=="FRANCAIS") {" Votre choix est : " } else {" Your choice is: "}
  texte55 <<- if(choix_langue=="FRANCAIS") {" Enregistrer en .tiff "} else {" Save in .tiff "}
  texte56 <<- if(choix_langue=="FRANCAIS") {" Voulez-vous enregistrer le graphique en .tiff ? "} else {" Save the graph in tiff ? "}
  texte57 <<- if(choix_langue=="FRANCAIS") {" Enregistrer en .jpeg "} else {" Save in .jpeg "}
  texte58 <<- if(choix_langue=="FRANCAIS") {" Voulez-vous enregistrer le graphique en .jpeg ? "} else {" Save the graph in jpeg ? "}
  texte59 <<- if(choix_langue=="FRANCAIS") {" Enregistrer en .pdf "} else {" Save in .pdf "}
  texte60 <<- if(choix_langue=="FRANCAIS") {" Voulez-vous enregistrer le graphique en .pdf ? "} else {" Save the graph in pdf ? "}
  texte61 <<- if(choix_langue=="FRANCAIS") {" Nb \u00e9chantillons "} else {" Nb of samples "}
  texte62 <<- if(choix_langue=="FRANCAIS") {" Pour citer ce package taper citation('InterfaceqPCR')"} else {" For citation this package please see citation('InterfaceqPCR')"}
}
