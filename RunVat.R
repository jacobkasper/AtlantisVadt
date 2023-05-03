##devtools::install_github(repo="erlasturl/vat", ref = "erlasturl_fish")
## you will need a bunch of packages to run this,
## the github thingy will show you what packages are needed.

defaultwd <- getwd()
library(shiny); library(ncdf4);      library(stringr); library(tidyverse)
library(plyr);  library(data.table); library(viridis)

Opath    <- 'path to observation directory'
bm.path  <- paste0(Opath, "/bmsummary.csv")
naa.path <- paste0(Opath, "/naa_out.csv")
har.path <- paste0(Opath, "/harvest_for_atlantis.csv")
ssb.path <- paste0(Opath, "/ssb_atlantis.csv")

setwd(defaultwd)
source('vadt_fishing_disc_jk_V2_1.R')
source('create_vadt_fishing_disc_V2_1_jk.R')
setwd('path to Atlantis directory')

biol <- "B428" ##name of biological prm
fprm <- "F230" ##name of fishing prm
outdir <- paste0("M90", biol, fprm, "/") ## name of your Atlantis output directory
biolprm <- paste0(biol, ".prm")
assign(paste0(biol, fprm),
       create_vadt(outdir = outdir,
                   funfile = "GroupsIceland.csv", ##functional group file
                   ncout = "Out",
                   startyear = 1948,
                   endyear = 2012,
                   toutinc = 365,
                   biolprm = biolprm,
                   fishing = TRUE,
                   fishfile = 'FisheriesIceland.csv', ##fishing prm file
                   toutfinc = 365,
                   bmsummaries = bm.path,
                   naasummaries = naa.path,
                   harsummaries = har.path,
                   ssbmri = ssb.path))

vadt(B436F230, "B436F230", anim = NULL)
