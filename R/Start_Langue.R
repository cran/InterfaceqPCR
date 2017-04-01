
#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                             |
#      | Interface qPCR  package                     |
#      | Analyse des donnees qPCR                    |
#      | choix de la langue                          |
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



Interface_qPCR_choix_Langue<-function(){

#   listedespackages <- c("xlsx", "readxl", "XLConnect","reshape2",
#                         "gtools", "tcltk", "tkrplot", "gplots")
#   packagesistalles <- listedespackages [!(listedespackages  %in% installed.packages()[,"Package"])]
#   if(length(packagesistalles )) install.packages(packagesistalles )
#
#
#   require(tcltk)
  tclRequire("BWidget")
  rm(list=ls())

  # choix_langue2 <- NULL
  # choix_langue2 <<- "FRANCAIS"
  # LANGUE()

  # Début interface graphique
  Interface_qPCR_choix_Langue_FRA_ENGL <- tktoplevel()
  #tkwm.title(Interface_qPCR_choix_Langue_FRA_ENGL,"Bienvenue  sélection de l'analyse")
  tkwm.title(Interface_qPCR_choix_Langue_FRA_ENGL,"Bienvenue s\u00e9lection de la langue / Welcome select your language")
  tkconfigure(Interface_qPCR_choix_Langue_FRA_ENGL,bg="wheat1",cursor="hand2")
  #tkconfigure(Interface_qPCR_choix_Langue_FRA_ENGL,bg="hotpink1",cursor="hand2") # pour celine !!

  # Taille
  tkwm.geometry(Interface_qPCR_choix_Langue_FRA_ENGL, "600x450")   # Largueur x hauteur
  tkwm.resizable(Interface_qPCR_choix_Langue_FRA_ENGL,F,F)


  # Police
  fontHeading <- tkfont.create(family="times",size=20,weight="bold",slant="italic")
  font2 <- tkfont.create(family="times",size=14,weight="bold",slant="italic")
  font3 <- tkfont.create(family="times",size=12,weight="bold")



  etape1 <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text="S\u00e9lectionner votre langue \nSelect your language ",font=fontHeading,foreground="chocolate4",bg="wheat1")
  tkgrid(etape1, row=1, column=1, columnspan=4)


  etape11 <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape11, row=2,  columnspan=4)


  etape2 <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etape2, row=3,  columnspan=4)

  # Image drapeau Francais
  #############################

  fermeture_etape0_francais <- function(){
    tkdestroy(Interface_qPCR_choix_Langue_FRA_ENGL)
    choix_langue2 <- "FRANCAIS"
    choix_langue2 <<- choix_langue2
    LANGUE()
    #Interface_qPCR_choix()
    Interface_Chargement_fichiers()
  }

  #cliquable
  etapevide <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(etapevide, row=5,  column=1)

  Francais.but <- tkbutton(Interface_qPCR_choix_Langue_FRA_ENGL,command=fermeture_etape0_francais)

  tkconfigure(Francais.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"francais_flag.gif",
                                                                        fsep=.Platform$file.sep)))
  tkgrid(Francais.but,row=5,column=1)




  # Image drapeau Anglais
  # #############################

  fermeture_etape0English <- function(){
    tkdestroy(Interface_qPCR_choix_Langue_FRA_ENGL)
    choix_langue2 <- "ANGLAIS"
    choix_langue2 <<- choix_langue2
    LANGUE()
    #Interface_qPCR_choix()
    Interface_Chargement_fichiers()
  }


  Anglais.but <- tkbutton(Interface_qPCR_choix_Langue_FRA_ENGL,command=fermeture_etape0English)
  tkconfigure(Anglais.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"GB_flag3.gif",
                                                                       fsep=.Platform$file.sep)))
  tkgrid(Anglais.but,row=5,column=3)


  citationvide<- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text="",font=fontHeading,foreground="chocolate4",bg="wheat1")
  tkgrid(citationvide,row=6,column=1)

  citationtexteFR <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" Pour citer ce package taper : citation('InterfaceqPCR') ",font=font2,foreground="chocolate4",bg="wheat1")
 tkgrid(citationtexteFR,row=7,columnspan=4,column=1)

  citationtexteANG <- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" To cite this package please see: citation('InterfaceqPCR') ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(citationtexteANG,row=8,columnspan=4,column=1)


  videligne<- tklabel(Interface_qPCR_choix_Langue_FRA_ENGL, text=" ",font=font2,foreground="chocolate4",bg="wheat1")
  tkgrid(videligne,row=9,columnspan=4,column=1)

  Helpfile <- function(){
    browseURL(file.path(path.package("InterfaceqPCR"),"doc","Help_InterfaceqPCR.pdf",fsep=.Platform$file.sep))
  }

  HELP.but <- tkbutton(Interface_qPCR_choix_Langue_FRA_ENGL,command=Helpfile)
  tkconfigure(HELP.but,image=tkimage.create("photo",file=file.path(path.package("InterfaceqPCR"),"help_icon.gif",
                                                                      fsep=.Platform$file.sep)))
  tkgrid(HELP.but,row=10,column=3)



} # fin du script
# Interface_qPCR_choix_Langue()
