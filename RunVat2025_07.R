library(shiny); library(tidyverse); library(viridis)



bm.path   <- "../../AtlantisVadt/ForVADT/biomass2020.csv"
naa.path  <- "../../AtlantisVadt/ForVADT/naa_out.csv"
ssb.path  <- "../../AtlantisVadt/ForVADT/ssb_atlantis.csv"
har.path  <- "../../AtlantisVadt/ForVADT/harvest_for_atlantisD.csv"
ntot.path <- "../../AtlantisVadt/ForVADT/ntot.csv"
caa.path  <- "../../AtlantisVadt/ForVADT/caa.csv"

setwd('Atlantis/AtlantisVadt')


setwd('../../AtlantisVadt')
source('vadt.R')
source('create_vadt.R')
setwd("../output/Atlantis_2025_07/")
# Your sequence
x <- 04
# Add leading zeros
x_padded <- sprintf("%02d", x)
for(nr in x_padded){
  outdir <- paste0('M2025_07_2025_12_08_', nr)
  assign(paste0(outdir),
         create_vadt(outdir = paste0(outdir, '/'),
                     funfile = 'GroupsIceland.csv',
                     fishfile = 'FisheriesIceland.csv',
                     ncout = 'Out',
                     startyear = 1948,
                     endyear =  1949,
                     toutinc = 1,
                     toutfinc = 1,
                     fishing = TRUE,
                     bmsummaries  = bm.path,
                     naasummaries = naa.path,
                     harsummaries = har.path,
                     ssbmri       = ssb.path,
                     ntot         = ntot.path,
                     caa          = caa.path))
}


##M2025_07_2025_07_15_
ddepon  <- M2025_07_2025_12_08_03
ddepoff <- M2025_07_2025_12_08_04



left_join(ddepon$erla_plot$"Cod Adult" |>
          rename(Nddepon = number),
          ddepoff$erla_plot$"Cod Adult" |>
          rename(Nddepoff = number)) |>
    filter(Nddepon  != Nddepoff)|> filter(Box %in% c("Box20", "Box31"))

left_join(ddepon$erla_plot$"Cod Adult" |> group_by(Time) |> summarize(totalon = sum(number)),
          ddepoff$erla_plot$"Cod Adult" |> group_by(Time) |> summarize(totaloff = sum(number))) |>
    pivot_longer(!Time, names_to = "model", values_to = "number") |>
    ggplot()+
    geom_line(aes(x = Time, y = number, col = model))

left_join(ddepon$erla_plot$"Cod Adult" |> group_by(Time, Layer) |> summarize(totalon = sum(number)),
          ddepoff$erla_plot$"Cod Adult" |> group_by(Time, Layer) |> summarize(totaloff = sum(number))) |>
    pivot_longer(!c(Time, Layer), names_to = "model", values_to = "number") |>
    ggplot()+
    geom_line(aes(x = Time, y = number, col = Layer, linetype = Layer)) +
    facet_wrap(~model)

left_join(ddepon$erla_plot$"Cod Adult" |> group_by(Time, Box) |> summarize(totalon = sum(number)),
          ddepoff$erla_plot$"Cod Adult" |> group_by(Time, Box) |> summarize(totaloff = sum(number))) |>
    pivot_longer(!c(Time, Box), names_to = "model", values_to = "number") |>
    ggplot()+
    geom_line(aes(x = Time, y = number, col = Box, linetype = Box)) +
    facet_wrap(~model)


?pivot_longer
ddepo$erla_plot$"Cod Adult" |> filter(Time == 1) |> group_by(Box) |> summarize(n = n()) |>
  print(n=Inf)


str(names(ddepon$erla_biomass))
(ddepon$fun_group$Name)

shiny::runApp(vadt(M2025_07_2025_12_08_01, 'M2025_07_2025_12_08_01', anim = NULL),
              port = httpuv::randomPort(min = 3840, max = 3840),
              host = "127.0.0.1")

shiny::runApp(vadt(M2025_07_2025_12_08_02, 'M2025_07_2025_12_08_02', anim = NULL),
              port = httpuv::randomPort(min = 3840, max = 3840),
              host = "127.0.0.1")

localhost:3840

