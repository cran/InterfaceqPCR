
#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                             |
#      | Interface qPCR  package                     |
#      | Analyse des donnees qPCR                    |
#      | choix analyse fichiers:  PMA-qPCR ou qPCR   |
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




Interface_qPCR_choix <- function(){

#   listedespackages <- c("xlsx", "readxl", "XLConnect","reshape2",
#                         "gtools", "tcltk", "tkrplot", "gplots")
#   packagesistalles <- listedespackages [!(listedespackages  %in% installed.packages()[,"Package"])]
#   if(length(packagesistalles )) install.packages(packagesistalles )
  texte1<-texte2<-texte3<-texte4<-texte5<-texte6<-texte7<-texte8<-NULL

  LANGUE()
  #require(tcltk)
  tclRequire("BWidget")
  rm(list=ls())


  # Début interface graphique
  Interface_qPCR_choix <- tktoplevel()
  #tkwm.title(Interface_qPCR_choix,"Bienvenue  sélection de l'analyse")
  tkwm.title(Interface_qPCR_choix,texte1)
  tkconfigure(Interface_qPCR_choix,bg="wheat1",cursor="hand2")


  # Taille
  tkwm.geometry(Interface_qPCR_choix, "700x600")   # Largueur x hauteur
  tkwm.resizable(Interface_qPCR_choix,F,F)


  # Police
  fontHeading <- tkfont.create(family="times",size=20,weight="bold",slant="italic")
  font2 <- tkfont.create(family="times",size=14,weight="bold",slant="italic")
  font3 <- tkfont.create(family="times",size=12,weight="bold")


  #etape1=tklabel(Interface_qPCR_choix, text="Choisir le type d'analyse ",font=fontHeading,foreground="chocolate4",bg="wheat1")
  etape1 <- tklabel(Interface_qPCR_choix, text=texte2,font=fontHeading,foreground="chocolate4",bg="wheat1")

    tkgrid(etape1, row=1,  columnspan=6,sticky="e")

  #etape11=tklabel(Interface_qPCR_choix, text="pour les échantillons quantifiés ",font=fontHeading,foreground="chocolate4",bg="wheat1")
  etape11 <- tklabel(Interface_qPCR_choix, text=texte3,font=fontHeading,foreground="chocolate4",bg="wheat1")

  tkgrid(etape11, row=2,  columnspan=6,sticky="e")



  etape2 <- tklabel(Interface_qPCR_choix, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape2, row=3,  columnspan=2)


  # Image Selection PMA qPCR
  #############################

  fermeture_etape0 <- function(){
    tkdestroy(Interface_qPCR_choix)
    Interface_PMAqPCR()
  }

  #cliquable
  PMAqPCR.but <- tkbutton(Interface_qPCR_choix,command=fermeture_etape0)

  tkconfigure(PMAqPCR.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"PMAqPCR.gif",
                                                                    fsep=.Platform$file.sep)))
  tkgrid(PMAqPCR.but,row=5,column=2)


  #etape29=tklabel(Interface_qPCR_choix, text=" Analyse échantillons traités au PMA ",font=font2,foreground="chocolate4",bg="wheat1")
  etape29 <- tklabel(Interface_qPCR_choix, text=texte4,font=font2,foreground="chocolate4",bg="wheat1")

  tkgrid(etape29, row=5, column=3,  columnspan=3)


  etape3 <- tklabel(Interface_qPCR_choix, text="   ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape3, row=6,  columnspan=2)

  etape31 <- tklabel(Interface_qPCR_choix, text="   ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape31, row=7,  columnspan=2)


  # Image Selection qPCR
  #############################

  fermeture_etape0qPCR <- function(){
    tkdestroy(Interface_qPCR_choix)
    Interface_qPCR()
  }



  #cliquable
  qPCR.but <- tkbutton(Interface_qPCR_choix,command=fermeture_etape0qPCR)

  tkconfigure(qPCR.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"qPCR.gif",
                                                                  fsep=.Platform$file.sep)))
  tkgrid(qPCR.but,row=10,column=2)

  #etape40=tklabel(Interface_qPCR_choix, text=" Analyse échantillons non traités au PMA ",font=font2,foreground="chocolate4",bg="wheat1")
  etape40 <- tklabel(Interface_qPCR_choix, text=texte5 ,font=font2,foreground="chocolate4",bg="wheat1")

  tkgrid(etape40, row=10, column=3,  columnspan=3)





##################################################################################################
  ##########################################################################################################
  ######
  apropos<-function(){
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

    videaide <- tklabel(aidefenetre,text=" ",font=font2,bg="gray80")
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

    fermeraide=function(){
      tkdestroy(aidefenetre)
    }
    #Ok.butaide<-tkbutton(aidefenetre,text="Quitter",command=fermeraide,font=font2,foreground="Blue")
    Ok.butaide <- tkbutton(aidefenetre,text=texte8, command=fermeraide,font=font2,foreground="Blue")

    tkgrid(Ok.butaide,row=4, column=4)


  }

  Aide.but <- tkbutton(Interface_qPCR_choix,command=apropos)
  tkconfigure(Aide.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"apropos.gif",
                                                                   fsep=.Platform$file.sep)))

  tkgrid(Aide.but,row=13,column=8)

  }   # Fin du script Interface_qPCR_choix
