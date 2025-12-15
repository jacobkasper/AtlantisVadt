library(shiny); library(tidyverse); library(viridis)



bm.path   <- "../../AtlantisVadt/ForVADT/biomass2020.csv"
naa.path  <- "../../AtlantisVadt/ForVADT/naa_out.csv"
ssb.path  <- "../../AtlantisVadt/ForVADT/ssb_atlantis.csv"
har.path  <- "../../AtlantisVadt/ForVADT/harvest_for_atlantisD.csv"
ntot.path <- "../../AtlantisVadt/ForVADT/ntot.csv"
caa.path  <- "../../AtlantisVadt/ForVADT/caa.csv"


setwd('Atlantis/AtlantisVadt')


##setwd('../../AtlantisVadt')
source('vadt.R')
source('create_vadt.R')
setwd("../output/Atlantis_2025_07/")
# Your sequence
x <- 01
# Add leading zeros
x_padded <- sprintf("%02d", x)
for(nr in x_padded){
  outdir <- paste0('M2025_07_2025_07_15_', nr)
  assign(paste0(outdir),
         create_vadt(outdir = paste0(outdir, '/'),
                     funfile = 'GroupsIceland.csv',
                     fishfile = 'FisheriesIceland.csv',
                     ncout = 'Out',
                     startyear = 1948,
                     endyear =  2021,
                     toutinc = 365,
                     toutfinc = 365,
                     fishing = TRUE,
                     bmsummaries  = bm.path,
                     naasummaries = naa.path,
                     harsummaries = har.path,
                     ssbmri       = ssb.path,
                     ntot         = ntot.path,
                     caa          = caa.path))
}

shiny::runApp(vadt(M2025_07_2025_07_15_01, 'M2025_07_2025_07_15_01', anim = NULL),
              port = httpuv::randomPort(min = 3840, max = 3840),
              host = "127.0.0.1")

localhost:3840

