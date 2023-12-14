
#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                             |
#      | Interface qPCR  package                     |
#      | Analyse des donnees qPCR                    |
#      | Création fichiers Standard et Echantillons  |
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


Interface_Chargement_fichiers <- function(){


  texte1<-texte2<-texte3<-texte4<-texte5<-texte6<-texte7<-texte8<-texte9<-texte10<-texte11<-texte12<-texte13<-
    texte14<-texte15<-texte16<-texte17<-texte18<-texte19<-texte20<-texte21<-texte22<-texte23<-texte24<-texte25<-
    texte26<-texte27<-texte28<-texte29<-texte30<-texte31<-texte32<-texte33<-texte33bis<-texte34<-texte35<-texte36<-
    texte37<-texte38<-texte39<-texte40<-texte41<-texte42<-texte43<-texte44<-texte45<-texte46<-texte47<-texte48<-
    texte48bis<-texte49<-texte50<-texte51<-texte52<-texte52bis<-texte53<-texte54<-texte55<-texte56<-texte57<-
    texte58<-texte59<-texte60<-texte61<-texte62<-Echantillons_bruts<-choixconcentration<-Condition<-regressionnontraite<-
    dataimportees_echantillons<-data_moy_ADN_traite<-Efficacitetraite<-regressiontraite<-choixconcentration<-
    Ct<-dataimportees_gamme_etalon<-couleur1_ech<-couleur1<-Nbechantillons2 <-NULL

#   listedespackages <- c("xlsx", "readxl", "XLConnect","reshape2",
#                         "gtools", "tcltk", "tkrplot", "gplots")
#   packagesistalles <- listedespackages [!(listedespackages  %in% installed.packages()[,"Package"])]
#   if(length(packagesistalles )) install.packages(packagesistalles )
#
  rm(list=ls())
   LANGUE()
#   require(tcltk)
  tclRequire("BWidget")



  # Début interface graphique
  Interface_Chargement_fichiers <- tktoplevel()
  #tkwm.title(Interface_Chargement_fichiers,"Données")
  tkwm.title(Interface_Chargement_fichiers,texte46)
  tkconfigure(Interface_Chargement_fichiers,bg="wheat1",cursor="hand2")


  # Taille
  tkwm.geometry(Interface_Chargement_fichiers, "580x680")   # Largueur x hauteur
  tkwm.resizable(Interface_Chargement_fichiers,F,F)


  # Police
  fontHeading  <-  tkfont.create(family="times",size=20,weight="bold",slant="italic")
  font2  <-  tkfont.create(family="times",size=14,weight="bold",slant="italic")
  font3  <-  tkfont.create(family="times",size=12,weight="bold")


  #etape1=tklabel(Interface_Chargement_fichiers, text="Saisir les données ",font=fontHeading,foreground="chocolate4",bg="wheat1")
  etape1 <- tklabel(Interface_Chargement_fichiers, text=texte47,font=fontHeading,foreground="chocolate4",bg="wheat1")

  tkgrid(etape1, row=1,  columnspan=4)#, sticky="w")

  #etape11=tklabel(Interface_Chargement_fichiers, text="Enregistrer les données sous un autre nom",font=fontHeading,foreground="chocolate4",bg="wheat1")
  etape11 <- tklabel(Interface_Chargement_fichiers, text=texte48,font=fontHeading,foreground="chocolate4",bg="wheat1")

  tkgrid(etape11, row=2,  columnspan=4)#, sticky="w")


  #etape11=tklabel(Interface_Chargement_fichiers, text="Enregistrer les données sous un autre nom",font=fontHeading,foreground="chocolate4",bg="wheat1")
  etape2 <- tklabel(Interface_Chargement_fichiers, text=texte48bis,font=fontHeading,foreground="chocolate4",bg="wheat1")
  tkgrid(etape2, row=3,  columnspan=4)#, sticky="w")

  etape21 <- tklabel(Interface_Chargement_fichiers, text=" ",font=fontHeading,foreground="chocolate4",bg="wheat1")
  tkgrid(etape21, row=4,  columnspan=2)
  # Image Ouverture Fichiers Standards
  #############################

  fermeture_etape0 <- function(){
    tkdestroy(Interface_Chargement_fichiers)
    Interface_PMAqPCR()
  }

  TOTO1 <- function(){
    browseURL(file.path(path.package("InterfaceqPCR"),"doc","Fichier_Standards.xlsx",fsep=.Platform$file.sep))
  }

  #cliquable
  PMAqPCR.but <- tkbutton(Interface_Chargement_fichiers,command=TOTO1)

  tkconfigure(PMAqPCR.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"xlsx_Fichiers_Standards.gif",
                                                                       fsep=.Platform$file.sep)))
  tkgrid(PMAqPCR.but,row=5,column=2)


  #etape29=tklabel(Interface_Chargement_fichiers, text=" Fichiers Standards ",font=font2,foreground="chocolate4",bg="wheat1")
  etape29 <- tklabel(Interface_Chargement_fichiers, text=texte49,font=font2,foreground="chocolate4",bg="wheat1")

  tkgrid(etape29, row=5, column=3,  columnspan=3)




  Format_data_gammeetalon <- function(){
    fenetreaffichage_format_donnes_gamme_etalon <- tktoplevel()
    #tkwm.title(fenetreaffichage_format_donnes_gamme_etalon,"Format fichier de la gamme etalon")
    tkwm.title(fenetreaffichage_format_donnes_gamme_etalon, texte20)

    fermeturefenetregammeetalon <- function(){

      tkdestroy(fenetreaffichage_format_donnes_gamme_etalon)

    }

    Formatgammeetalon.but <- tkbutton(fenetreaffichage_format_donnes_gamme_etalon,command=fermeturefenetregammeetalon)
    tkconfigure(Formatgammeetalon.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"Format_gamme_etalon.gif",
                                                                                  fsep=.Platform$file.sep)))

    tkgrid(Formatgammeetalon.but,row=3,column=2)

  }


  #affichage_foramt_gamme_etalon<-tkbutton(Interface_Chargement_fichiers,text = "Trame fichier",command=Format_data_gammeetalon)
  affichage_foramt_gamme_etalon <- tkbutton(Interface_Chargement_fichiers,text = texte21,command=Format_data_gammeetalon)
  tkgrid(affichage_foramt_gamme_etalon,row=5,column=6)







  etape3 <- tklabel(Interface_Chargement_fichiers, text="   ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape3, row=6,  columnspan=2)

  etape31 <- tklabel(Interface_Chargement_fichiers, text="   ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape31, row=7,  columnspan=2)


  # Image Ouverture Fichiers Echantillons
  #############################

  fermeture_etape0qPCR <- function(){
    tkdestroy(Interface_Chargement_fichiers)
    Interface_qPCR()
  }

  TOTO <- function(){
    browseURL(file.path(path.package("InterfaceqPCR"),"doc","Fichier_Echantillons.xlsx",fsep=.Platform$file.sep))
  }

  #cliquable
  Fichiers_Echantillons_excel.but <- tkbutton(Interface_Chargement_fichiers,command=TOTO)

  tkconfigure(Fichiers_Echantillons_excel.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"xlsx_Fichiers_Echantillons.gif",
                                                                   fsep=.Platform$file.sep)))
  tkgrid(Fichiers_Echantillons_excel.but,row=10,column=2)

  #etape40=tklabel(Interface_Chargement_fichiers, text=" Fichiers echantillons ",font=font2,foreground="chocolate4",bg="wheat1")
  etape40 <- tklabel(Interface_Chargement_fichiers, text=texte50 ,font=font2,foreground="chocolate4",bg="wheat1")

  tkgrid(etape40, row=10, column=3,  columnspan=3)


  Format_data_Echantillons <- function(){

    fenetreaffichage_format_donnes_echantillons <- tktoplevel()
    tkwm.geometry(fenetreaffichage_format_donnes_echantillons, "290x250")
    #tkwm.title(fenetreaffichage_format_donnes_echantillons,"Format fichier des échantillons")
    tkwm.title(fenetreaffichage_format_donnes_echantillons, texte26)

    fermeturefenetreechantillons <- function(){

      tkdestroy(fenetreaffichage_format_donnes_echantillons)

    }

    Formaechantillons.but <- tkbutton(fenetreaffichage_format_donnes_echantillons,command=fermeturefenetreechantillons)
    tkconfigure(Formaechantillons.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"Format_echantillons.gif",
                                                                                  fsep=.Platform$file.sep)))

    tkgrid(Formaechantillons.but,row=3,column=2)

  }



  #affichage_format_echantillons<-tkbutton(Interface_Chargement_fichiers,text = "Trame fichier",command=Format_data_Echantillons)
  affichage_format_echantillons <- tkbutton(Interface_Chargement_fichiers,text = texte21,command=Format_data_Echantillons)
  tkgrid(affichage_format_echantillons,row=10,column=6)





  #########################################

  fermeture_etape_suivant <- function(){
    tkdestroy(Interface_Chargement_fichiers)
    Interface_qPCR_choix()
  }

  #cliquable
  Suivant.but <- tkbutton(Interface_Chargement_fichiers,command=fermeture_etape_suivant)

  tkconfigure(Suivant.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"Next.gif",
                                                                                          fsep=.Platform$file.sep)))
  tkgrid(Suivant.but,row=13,column=3)







  ##################################################################################################
  ##########################################################################################################
  ######
  apropos <- function(){
    aidefenetre <- tktoplevel()

    #tkwm.title(aidefenetre," A propos de InterfaceqPCR")
    tkwm.title(aidefenetre,texte6)
    tkconfigure(aidefenetre,bg="gray80",cursor="hand2") #  wheat1  hand2


    # Taille
    tkwm.geometry(aidefenetre, "500x650")   # Largueur x hauteur
    tkwm.resizable(aidefenetre,F,F)
    # Police
    font2  <-  tkfont.create(family="times",size=14,weight="bold",slant="italic")
    font3  <-  tkfont.create(family="times",size=12,weight="bold")

    path.logobatman <- file.path(path.package("InterfaceqPCR"),"a_laise_BZH2.gif", fsep=.Platform$file.sep)
    mon.imagebatman <- tkimage.create("photo", file=path.logobatman )

    montre.batman <- tklabel(aidefenetre, image=mon.imagebatman, background="white")
    tkgrid(montre.batman,row=1, column=3,columnspan=2)

    videaide<-tklabel(aidefenetre,text=" ",font=font2,bg="gray80")
    tkgrid(videaide,row=2)

    #     message<-tklabel(aidefenetre,text="InterfaceqPCR\nTraitement des données issues de la qPCR Version 1.0
    #                      \nOlivier Le Goff
    #                      \nLaboratoire EA CIDAM Clermont-Ferrand\nUniversité d'Auvergne\n2016",bg="gray80",font=font3)
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

  Aide.but <- tkbutton(Interface_Chargement_fichiers,command=apropos)
  tkconfigure(Aide.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"apropos.gif",
                                                                   fsep=.Platform$file.sep)))

  tkgrid(Aide.but,row=13,column=6)



}   # Fin du srcipt Interface_Chargement_fichiers
