#                        \\\||||||///
#                         \\ ~  ~ //
#                         (  @ @ )
#       ________________oOOo-(_)-oOOo________________
#      |                                            |
#      | Interface qPCR  package                    |
#      | Analyse des donnees qPCR                   |
#      | Création fichiers Standard et Echantillons |
#      | Decembne 2016                              |
#      |______________________Oooo._________________|
#                   .oooO     (    )
#                   (    )     )  /
#                    \  (      (_/
#                     \_)
#
# Author: Olivier Le Goff
###############################################################################
###############################################################################




.onAttach <- function(lib,pkg) {
  packageStartupMessage("Pour lancer l'interface faire Start_qPCR() \nFor Launch the interface write Start_qPCR()
                        \nFor citation this package please see citation('InterfaceqPCR')")
}



#' GUI to Analyse qPCR Results after PMA Treatment or not'


#' @details
#' \describe{
#' \item{Package:}{Interface_qPCR}
#' \item{Version:}{1.0}
#' \item{Date:}{2017-03-31}
#' }


#' @author Olivier LE GOFF
#' Maintainer Olivier Le Goff  <olivierlegoff1@gmail.com>


#' @description
#' A multi-platform user interface to determine the concentration in CFU/mL or in number of copies/mL
#' obtained by qPCR after with or without PMA treatment.
#'
#'
#'For loaded, you can write at the R prompt the following line command to run InterfaceqPCR: Start_qPCR()
#'
#' This interface is composed of 4 steps
#' \describe{
#' \item{Step 1:}{Select your language French or English, select the flag corresponding}
#' \item{Step 2:}{Complete files.
#' click on the Standards icon to open on xlsx file and write the datas, to help click on model file
#' click on the Samples icon to open on xlsx file and write the datas, to help click on model file
#' The two files must be save in other folder}
#' \item{Step 3:}{Select the analysis method
#' - Click on the first icon for the samples treated with PMA
#' - Click on the second icon for the samples no treated with PMA}
#' \item{Step 4:}{Analyse samples
#'   \describe{
#'    \item{1}{Import the files:
#'    clik on xlsx icon to open the File Browser and select the standard file then click on refresh
#'    clik on xlsx icon to open the File Browser and select the sample file then click on refresh
#'    The files are loaded if two 'yes' are present.}
#'     \item{2}{Regression curve:
#'     In the first time select your concentration CFU/ml or copies/mL.
#'     Click on calculate button to determine the standard curve then click on the graphic icon,
#'     a new window appears showing the regression curve.
#'     At the top right several parameters: the regression equation,
#'     the coefficient of determination (R2)² the qPCR efficiency (E) and limits of detection and quantitation were indicated.
#'     You can output the graphic into the directory of your choice in vector or bitmap formats,
#'     for that click on the tiff button or/and the jpeg button or/and the pdf button.}
#'      \item{3}{Concentration in samples:
#'      Click on the tubes icon and a new window appears representing a table.
#'      This table contains the datas coming from sample file added of a new column
#'      mentionning the concentration in CFU per mL or number of copies per mL.
#'      The case filter is upper the table and filters the table in according to a word or a number.
#'      You can save the table into the directory of your choice in Excel (xlsx) or/and text (csv) formats.
#'      For that, click on the csv button or/and the xlsx button.
#'      If you have finished, you close the interface else click on the Start icon to return to the third step: select the analysis method.
#'      }
#'   }
#'  }
#' }
#'
#' @examples
#' Start_qPCR()

#' @export

#' @import reshape2 tcltk tkrplot xlsx plyr graphics grDevices stats utils


Start_qPCR <- function(){
  #listedespackages <- c("xlsx", "readxl", "XLConnect","reshape2",
  #                      "gtools", "tcltk", "tkrplot", "gplots")
  #packagesistalles <- listedespackages [!(listedespackages  %in% installed.packages()[,"Package"])]
  #if(length(packagesistalles )) install.packages(packagesistalles )

  #require(tcltk)
  #tclRequire("BWidget")
  #rm(list=ls())
  choix_langue2<-NULL
  #choix_langue2 <<- "FRANCAIS"

  Interface_qPCR_choix_Langue()

  #choix_langue2 <<- "FRANCAIS"
  #LANGUE()
}
# Fin de la fonction Start_qPCR()


