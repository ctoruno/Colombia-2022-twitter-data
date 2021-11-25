## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data tracking
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     November 24th, 2021
##
## This version:      November 24th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##                1.  
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/MA in Development Economics/Thesis/Data")
rm(list=ls())

# Required packages
lapply(list("rtweet"), 
       library, character.only = T)

# Default option for kable
options(knitr.table.format = "latex")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Defining extraction algorithm                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

parties <- paste("@MovimientoMAIS OR @PartidoVerdeCoL OR @CeDemocratico OR @NvLiberalismo",
                 "@PartidoLiberal OR @ColombiaHumana_ OR @UP_Colombia OR @LaFuerzaDLaPaz",
                 "@SoyRenaciente OR @PactoCol OR @PartidoMIRA OR @PoloDemocratico",
                 "@compromisociu OR @PAC_Colombia OR @partidodelaucol OR @PCambioRadical",
                 sep = " OR ")




elections <- search_tweets("(nicaragua OR mora OR chamorro OR maradiaga OR arturo OR george OR
                            ortega OR medardo OR comandante) AND (elecciones OR candidatos OR 
                            fsln OR cxl OR plc OR yatama OR alianza OR unab OR noviembre)", 
                           n = 12000, include_rts = F, lang = "es")


