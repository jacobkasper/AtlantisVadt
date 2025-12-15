library(shiny); library(ncdf4);      library(stringr); library(tidyverse)
library(plyr);  
library(data.table); 
library(viridis)
library(purrr)

library(dplyr)
library(reshape)


bm.path   <- "../../AtlantisVadt/ForVADT/biomass2020.csv"
naa.path  <- "../../AtlantisVadt/ForVADT/naa_out.csv"
ssb.path  <- "../../AtlantisVadt/ForVADT/ssb_atlantis.csv"
har.path  <- "../../AtlantisVadt/ForVADT/harvest_for_atlantisD.csv"
ntot.path <- "../../AtlantisVadt/ForVADT/ntot.csv"
caa.path  <- "../../AtlantisVadt/ForVADT/caa.csv"

dir.inputs <- "/home/sjor/jacob/Atlantis/input/atlantis6702"

setwd('../../../AtlantisVadt')
source('vadt_fishing_disc_jk_V2.3.r')
source('create_vadt_fishing_disc_V2.4.r')
setwd("../output/Atlantis6702/M6702_2025_02_14_05")
dir()
biol <- 'B391ES3JK6.6702_2025_02_12_06'
har <- 'F155ES18JK11.35.6702'
outbgm <- 'atlantis_L93.bgm'


files <- tibble(filename = dir()) %>%
  filter(
    str_detect(filename, "^(Bio|Har|Run).*\\.prm$") |   # .prm files
      str_detect(filename, "^(Groups|migration).*\\.csv$") |  # .csv files
      str_detect(filename, "\\.bgm$")  # .bgm file
  ) %>%
  pull(filename)

# Assign files to variables based on their prefixes
bio <- files[str_detect(files, "^Bio")][1]
har <- files[str_detect(files, "^Har")]
run <- files[str_detect(files, "^Run")]
groups <- files[str_detect(files, "^Groups")]
migration <- files[str_detect(files, "^migration")]
bgm <- files[str_detect(files, "\\.bgm$")]


assign(paste0("test"),
       create_vadt(outdir = '../M6702_2025_02_14_05',
                   funfile = paste0(groups),                     
                   ncout = 'Out',
                   startyear = 1948,
                   endyear =  2020,
                   toutinc = 365,
                   funfileH = 'GroupsIceland6702.csv',
                   biolprm = paste0('../../input/atlantis6702/', biol, '.prm'),
                   biolprmH = paste0(biol, '.prm'),
                   harprmH = paste0(har, '.prm'),
                   outbgmH = outbgm,
                   fishing = TRUE,
                   fishfile =
                     '../../input/atlantis6702/FisheriesIceland3.csv',
                   toutfinc = 365,
                   bmsummaries  = bm.path,
                   naasummaries = naa.path,
                   harsummaries = har.path,
                   ssbmri       = ssb.path,
                   ntot         = ntot.path,
                   caa          = caa.path, 
                   dir.inputs = dir.inputs))



getwd()
ls()
setwd('../../AtlantisVadt')
source('vadt_fishing_disc_jk_V2.3.r')
source('create_vadt_fishing_disc_V2.4.r')
setwd("../output/Atlantis6702/")
setwd("../output/AtlantisTrunk/")


biol <- 'Bio391ES3JK6.6702_2025_02_25_01'
har <- 'Har155ES18JK11.35.6702_2025_02_14_01'
outbgm <- 'atlantis_L93.bgm'
outdir <- 'M6702_2025_02_27_01'


getwd()
assign(paste0(outdir),
       create_vadt(outdir = paste0(outdir, '/'),
                   funfile =
                     '../../input/atlantis6702/GroupsIceland6702.csv',
                   ncout = 'Out',
                   startyear = 1948,
                   endyear =  2020,
                   toutinc = 365,
                   funfileH = 'GroupsIceland6702.csv',
                   biolprm = paste0('../../input/atlantis6702/', biol, '.prm'),
                   biolprmH = paste0(biol, '.prm'),
                   harprmH = paste0(har, '.prm'),
                   outbgmH = outbgm,
                   fishing = TRUE,
                   fishfile =
                     '../../input/atlantis6702/FisheriesIceland3.csv',
                   toutfinc = 365,
                   bmsummaries  = bm.path,
                   naasummaries = naa.path,
                   harsummaries = har.path,
                   ssbmri       = ssb.path,
                   ntot         = ntot.path,
                   caa          = caa.path, 
                   dir.inputs = dir.inputs))


vadt(M6702_2025_02_27_01, 'M6702_2025_02_27_01', anim = NULL)


tibble(M67XX_2025_02_25_01$totalnums) %>%
  mutate(.id = gsub("[0-9]+_Nums", "", .id)) %>%
  rename_with(~ "abundance", .cols = any_of("V1")) %>%
  select(-c(X1, Ageclass)) %>% 
  filter(.id == "Whale_Baleen", Age==1) %>% 
  group_by(Time) %>% 
  arrange(Time, Age) %>% 
  print(n=Inf)

tibble(M6702_2025_02_12_07$totalnums) %>%
  mutate(.id = gsub("[0-9]+_Nums", "", .id)) %>%
  rename_with(~ "abundance", .cols = any_of("V1")) %>%
  select(-c(X1, Ageclass)) %>% 
  filter(.id == "Whale_Baleen", Age==1) %>% 
  group_by(Time) %>% 
  arrange(Time, Age) %>% 
  print(n=Inf)

tibble(M6702_2025_02_12_03$totalnums) %>%
  mutate(.id = gsub("[0-9]+_Nums", "", .id)) %>%
  rename_with(~ "abundance", .cols = any_of("V1")) %>%
  select(-c(X1, Ageclass)) %>% 
  filter(.id == "Minke_Whale") %>% 
  group_by(Time) %>% 
  arrange(Time, Age) %>% 
  print(n=Inf)

tibble(M6702_2025_02_12_07$totalnums) %>%
  mutate(.id = gsub("[0-9]+_Nums", "", .id)) %>%
  rename_with(~ "abundance", .cols = any_of("V1")) %>%
  select(-c(X1, Ageclass)) %>% 
  filter(.id == "Minke_Whale", Age==1) %>% 
  group_by(Time) %>% 
  arrange(Time, Age) %>% 
  print(n=Inf)

arrange(Age)

tibble(M6702_2025_02_11_03$totalnums) %>% 
  filter(.id == 'Whale_Baleen1_Nums')


vadt(M6702_2025_02_12_07, 'M6702_2025_02_12_07', anim = NULL)
