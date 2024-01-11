library(shiny); library(ncdf4);      library(stringr); library(tidyverse)
library(plyr);  library(data.table); library(viridis)

bm.path   <- "../../vadt/ForVADT/biomass2020.csv"
naa.path  <- "../../vadt/ForVADT/naa_out.csv"
ssb.path  <- "../../vadt/ForVADT/ssb_atlantis.csv"
har.path  <- "../../vadt/ForVADT/harvest_for_atlantisD.csv"
ntot.path <- "../../vadt/ForVADT/ntot.csv"
caa.path  <- "../../vadt/ForVADT/caa.csv"

setwd('../../vadt')
source('vadt_fishing_disc_jk_V2.3.r')
source('create_vadt_fishing_disc_V2.2.r')
setwd("../out/Atlantis6610/")
biol <- 'B391ES3JK6.1'

outdir <- 'M6610_5met1214_01'
assign(paste0(outdir),
       create_vadt(outdir = paste0(outdir, '/'),
                   funfile =
                       '../../input/Atlantis6610/GroupsIceland.csv',
                   ncout = 'Out',
                   startyear = 1948,
                   endyear = 2020,
                   toutinc = 365,
                   biolprm = paste0('../../input/Atlantis6610/', biol, '.prm'),
                   fishing = TRUE,
                   fishfile =
                       '../../input/Atlantis6610/FisheriesIceland3.csv',
                   toutfinc     = 365,
                   bmsummaries  = bm.path,
                   naasummaries = naa.path,
                   harsummaries = har.path,
                   ssbmri       = ssb.path,
                   ntot         = ntot.path,
                   caa          = caa.path))


vadt(M6610_5met1109_03, 'M6610_5met1109_03', anim = NULL)










