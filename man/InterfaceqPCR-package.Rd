\name{InterfaceqPCR-package}
\alias{InterfaceqPCR-package}
\alias{InterfaceqPCR}
\docType{package}
\title{GUI to Analyse qPCR Results after PMA Treatment or not}

\description{
A multi-platform user interface to determine the concentration in CFU/mL or in number of copies/mL obtained by qPCR after with or without PMA treatment.
For loaded, you can write at the R prompt the following line command to run InterfaceqPCR: Start_qPCR()

This interface is composed of 4 steps:

\describe{
\item{Step 1:}{Select your language French or English, select the flag corresponding}
\item{Step 2:}{Complete files.
 - Click on the Standards icon to open on xlsx file and write the datas, to help click on model file
 - Click on the Samples icon to open on xlsx file and write the datas, to help click on model file
 The two files must be save in other folder}
\item{Step 3:}{Select the analysis method
 - Click on the first icon for the samples treated with PMA
 - Click on the second icon for the samples no treated with PMA}
\item{Step 4:}{Analyse samples:
   \describe{
    \item{1}{Import the files:
    - Click on xlsx icon to open the File Browser and select the standard file then click on refresh
    - Click on xlsx icon to open the File Browser and select the sample file then click on refresh
    The files are loaded if two 'yes' are present.}
     \item{2}{Regression curve:
     In the first time select your concentration CFU/ml or copies/mL.
     Click on calculate button to determine the standard curve then click on the graphic icon,
     a new window appears showing the regression curve.
     At the top right several parameters: the regression equation,
     the coefficient of determination (R²), the qPCR efficiency (E) and limits of detection and
     quantitation were indicated.
     You can output the graphic into the directory of your choice in vector or bitmap formats,
     for that click on the tiff button or/and the jpeg button or/and the pdf button.}
      \item{3}{Concentration in samples:
      Click on the tubes icon and a new window appears representing a table.
      This table contains the datas coming from sample file added of a new column
      mentionning the concentration in CFU per mL or number of copies per mL.
      The case filter is upper the table and filters the table in according to a word or a number.
      You can save the table into the directory of your choice in Excel (xlsx) or/and text (csv) formats.
      For that, click on the csv button or/and the xlsx button.
      If you have finished, you close the interface else click on the Start icon to return to the
      third step: select the analysis method
      }
   }
  }
 }
}

\examples{

Start_qPCR()
}

\details{
\tabular{ll}{
Package: \tab InterfaceqPCR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-03-31\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}



\author{
Olivier Le Goff
<olivierlegoff1@gmail.com>
}
