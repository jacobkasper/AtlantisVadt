create_vadt <- function(outdir, funfile, biolprm, ncout, startyear, endyear, 
                        toutinc, fishing = FALSE, fishfile = NULL, toutfinc, 
                        bmsummaries, naasummaries, harsummaries, ssbmri, ntot,
                        caa){
  # contants
  nsecs <- 86400
  ndays <- 365
  g_per_ton <- 1e6
  xCN <- 5.7
  species <- c("BIRD", "FISH", "MAMMAL", "SHARK")
  tons <- function(mgN) {
    return(mgN * 5.7 * 20 / 1e9)
  }

  # Functions to aggregate arrays
  mean_agg <- function(x) {
    adply(x, 3, mean, na.rm = T)
  }

  sum_agg <- function(x) {
    adply(x, 3, sum)
  }

  sum_agg_2 <- function(x) {
    adply(x, 2, sum)
  }

  cat("### ------------ Reading in data                                         ------------ ###\n")
  nc_out <- nc_open(paste(outdir, ncout, ".nc", sep = ""))
  prod_out <- nc_open(paste(outdir, ncout, "PROD.nc", sep = ""))
  bio_agg <- read.table(paste(outdir, ncout, "BoxBiomass.txt", sep = ""), header = T)
  ssb <- read.table(paste(outdir, ncout, "SSB.txt", sep = ""), header = TRUE)
  yoy <- read.table(paste(outdir, ncout, "YOY.txt", sep = ""), header = TRUE)
  bgm <- readLines(paste(outdir, grep(".bgm", dir(outdir), value = T), sep = ""))
  if (length(bgm) == 0) {
    bgm <- readLines(paste(grep(".bgm", dir(), value = T), sep = ""))
  }
  biomass   <- read.table(paste(outdir, ncout, "BiomIndx.txt", sep = ""), header = T)
  diet      <- read.table(paste(outdir, ncout, "DietCheck.txt", sep = ""), header = TRUE, stringsAsFactors = TRUE)
  biolprm   <- readLines(biolprm)
  fun_group <- read.csv(funfile, header = T, stringsAsFactors = FALSE)
  fun_group_off <- subset(fun_group, fun_group$IsTurnedOn == 0)
  fun_group <- subset(fun_group, fun_group$IsTurnedOn == 1)

  if (sum(names(fun_group) == "InvertType") > 0) {
    names(fun_group)[names(fun_group) == "InvertType"] <- "GroupType"
  }
    bms <- read.csv(bmsummaries) %>%
        dplyr::filter(Time <= endyear + 1)
    naa <- read.csv(naasummaries)
    naasp <- unique(naa$species)
    har <- read.csv(harsummaries) %>%
        dplyr::filter(Time <= endyear + 1)
    ssbmri <- read.csv(ssbmri)
    ntot <- read.csv(ntot)
    caa <- read_csv(caa)
  rs_names <- str_trim(fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Name"]) # trim white space
  rs_codes <- str_trim(fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Code"])
  invert_names <- fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")), ]

  # Get some variables
  bioagg_names <- colnames(bio_agg)[c(-1, -2)]
  max_tracer <- nc_out$nvars
  max_layers <- length(nc_out$dim$z$vals)
  max_time <- length(nc_out$dim$t$vals)
  var_names <- names(nc_out$var)


  # ------------------------------------- #
  # -     Length Weight Parameters      - #
  # ------------------------------------- #

  # Extract a and b parameters from biology parameter
  biol_a <- grep("li_a", biolprm, value = TRUE)
  biol_b <- grep("li_b", biolprm, value = TRUE)
  a_split <- unlist(str_split_fixed(biol_a, pattern = " ", n = 2))
  a_split <- apply(a_split, 2, str_trim, side = "both")
  a_group <- a_split[, 1]
  a_param <- str_split_fixed(a_split[, 2], pattern = " ", n = 2)[, 1]

  # trim trailing white space
  b_split <- unlist(str_split_fixed(biol_b, pattern = " ", n = 2))
  b_split <- apply(b_split, 2, str_trim, side = "both")
  b_group <- b_split[, 1]
  b_param <- str_split_fixed(b_split[, 2], pattern = " ", n = 2)[, 1]

  ab_params <- data.frame(
    a_name = a_group, a = as.numeric(as.character(a_param)),
    b_name = b_group, b = as.numeric(as.character(b_param))
  )



  # ------------------------------------- #
  # -           spatial Plots           - #
  # ------------------------------------- #

  cat("### ------------ Setting up spatial plots                                ------------ ###\n")

  # Find number of boxes
  numboxes <- length(grep("# Box number", bgm))

  # Find area of boxes
  areaboxes <- grep("area", bgm, value = TRUE)
  areaboxes <- strsplit(areaboxes, "area\t")
  boxarea <- as.numeric(sapply(areaboxes, `[`, 2)) / 1e6 # to km2
  boxnr <- sapply(areaboxes, `[`, 1)
  boxnr <- strsplit(boxnr, "box")
  boxnr <- sapply(boxnr, `[`, 2)
  boxnr <- as.numeric(gsub(".$", "", boxnr))
  areaboxes <- data.frame("box" = boxnr, "area" = boxarea)
  areaboxes <- areaboxes[order(areaboxes$box), ]

  # Extract the box vertices
  vertices <- data.frame()
  for (i in 1:numboxes) {
    vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), bgm)
    vertices <- rbind(vertices, cbind(i - 1, bgm[vert_tmp]))
  }

  # Extract latitude and longitude
  coords_tmp <- strsplit(as.character(vertices$V2), " ")
  x <- sapply(coords_tmp, `[`, 2)
  y <- sapply(coords_tmp, `[`, 3)

  # Create the map for ggplot2
  map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
  map_base$x <- as.double(as.character(map_base$x))
  map_base$y <- as.double(as.character(map_base$y))

  # Check for islands

  islands <- grep("botz\t0", bgm, value = T)
  if (length(islands > 0)) {
    islands <- strsplit(islands, "[.]")
    islands <- sapply(islands, `[`, 1)
    islands <- strsplit(islands, "box")
    islands <- sapply(islands, `[`, 2)
  }

  # ------------------------------------- #
  # -          Whitin Box Plots         - #
  # ------------------------------------- #

  # Calculate by biomass by box - this is for the within-box plot
  biomass_by_box <- bio_agg %>%
    gather("Code", "value", -c(Time, Box))
  biomass_by_box <- merge(biomass_by_box, fun_group[, c(1, 2, 4)])
  biomass_by_box <- arrange(biomass_by_box, Index)
  biomass_by_box$Name <- factor(biomass_by_box$Name, levels = unique(biomass_by_box$Name))
  biomass_by_box$Time <- startyear + biomass_by_box$Time / 365

  # ------------------------------------- #
  # -     Interactive spatial Plots     - #
  # ------------------------------------- #

  cat("### ------------ This part takes a while                                 ------------ ###\n")
  cat("### ------------ Go grab a kleina                                        ------------ ###\n")

  nums <- grep("Nums", var_names, value = TRUE)
  N <- grep("_N", var_names, value = TRUE)
  N <- N[-grep("_Nums", N, value = FALSE)]
  tot_num <- c(nums)
  vert_group <- str_split_fixed(nums, "_Nums", n = 2)[, 1]
  str_N <- grep("StructN", var_names, value = TRUE)
  res_N <- grep("ResN", var_names, value = TRUE)


  # extract tracers from the ncdf4 object (vertebrates)
  vars <- list()
  dens <- list()
  sn_list <- list()
  rn_list <- list()
  for (i in 1:length(nums)) {
    tempNums <- ncvar_get(nc = nc_out, varid = paste0(vert_group[i], "_Nums"))
    Temp_dens <- apply(tempNums, c(2, 3), sum) / areaboxes$area
    Temp_dens[1, ] <- 0 # set as 0 in box 0
    dens[[i]] <- Temp_dens
    vars[[i]] <- tempNums
    tempRN <- ncvar_get(nc = nc_out, varid = paste0(vert_group[i], "_ResN"))
    tempRN[tempNums <= 1e-16] <- NA
    tempSN <- ncvar_get(nc = nc_out, varid = paste0(vert_group[i], "_StructN"))
    tempSN[tempNums <= 1e-16] <- NA
    sn_list[[i]] <- tempSN
    rn_list[[i]] <- tempRN
  }
  names(vars) <- paste0(vert_group, "_Nums")
  names(dens) <- paste0(vert_group, "_Nums")
  names(sn_list) <- paste0(vert_group, "_StructN")
  names(rn_list) <- paste0(vert_group, "_ResN")



  # Density N mg per m3 and invertabrates per m2
  dens_km2 <- list()
  dens_km3 <- list()
  j <- 1
  volume <- ncvar_get(nc = nc_out, varid = "volume")
  vol <- apply(volume, c(2, 3), sum)[, 1]
  for (i in 1:length(N)) {
    tempN <- ncvar_get(nc = nc_out, varid = N[i])
    if (length(dim(tempN)) == 3) {
      totBioBox <- tempN * volume
      Temp_dens_km3 <- apply(totBioBox, c(2, 3), sum) / (vol + 1e-6) # mg N km3
      Temp_dens_km3 <- tons(Temp_dens_km3) # convert to tons
      Temp_dens_km3[1, ] <- 0 # set as 0 in box 0
      Temp_dens_km3 <- Temp_dens_km3[, 2:ncol(Temp_dens_km3)] # Low values for time 0
      dens_km3[[i]] <- Temp_dens_km3
      Temp_dens_km2 <- apply(totBioBox, c(2, 3), sum) / areaboxes$area # mg N km2
      Temp_dens_km2 <- tons(Temp_dens_km2) # convert to tons
      Temp_dens_km2[1, ] <- 0 # set as 0 in box 0
      Temp_dens_km2 <- Temp_dens_km2[, 2:ncol(Temp_dens_km2)] # Low values for time 0
      dens_km2[[i]] <- Temp_dens_km2
    } else {
      totBioBox <- tempN * areaboxes$area
      dens_km3[[i]] <- tons(totBioBox / (vol + 1e-6))
      dens_km2[[i]] <- tons(tempN)
      j <- j + 1
    }
  }
  names(dens_km3) <- N
  names(dens_km2) <- N

  # ------------------------------------- #
  # -   Distribution by boxes Plots     - #
  # ------------------------------------- #

  # Create Erla's plots
  nominal_dz <- ncvar_get(nc = nc_out, varid = "nominal_dz")
  depth_layers <- nominal_dz[, which.max(colSums(nominal_dz))]
  depth_layers <- depth_layers[-c(1, length(depth_layers))]
  depth_layers <- cumsum(rev(depth_layers))

  depth_labels <- rep(NA, (length(depth_layers) + 1))
  for (i in 1:(length(depth_layers) + 1)) {
    if (i == 1) {
      depth_labels[i] <- paste("0 - ", depth_layers[i], sep = "")
    } else if (i == (length(depth_layers) + 1)) {
      depth_labels[i] <- paste(depth_layers[i - 1], " + ", sep = "")
    } else {
      depth_labels[i] <- paste(depth_layers[i - 1], " - ", depth_layers[i], sep = "")
    }
  }
  depth_labels <- c(depth_labels, "Sediment")

  vert_names <- fun_group[fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD"), "Code"]
  mat_age <- grep("_age_mat", biolprm, value = T)
  species_ids <- str_split_fixed(mat_age, "_age_mat", n = 2)[, 1]
  # juvenile_age <- as.numeric(gsub("[^\\d]+", "", mat_age, perl=TRUE))[which(species_ids %in% vert_names)]
  get_first_number <- function(x) {
    yy <- gsub("([^\\d])", "#", x, perl = TRUE)
    yyy <- unlist(str_split(yy, "#"))
    xPos <- grep("[^\\d]", yyy)[1]
    thisNum <- as.numeric(yyy[xPos])
  }
  temp <- lapply(mat_age, FUN = get_first_number)
  juvenile_age <- as.numeric(unlist(temp))
  species_ids <- species_ids[which(species_ids %in% vert_names)]

  erla_plots <- list()
  for (i in 1:length(species_ids)) {
    spp <- fun_group[fun_group$Code == species_ids[i], c("Name", "NumCohorts")]
    spp <- str_trim(spp)
    if (juvenile_age[i] > 0) {
      juv <- paste(spp[[1]], 1:(juvenile_age[i]), "_Nums", sep = "") ### ERROR
      ad <- paste(spp[[1]], (juvenile_age[i] + 1):spp[[2]], "_Nums", sep = "") ### ERROR
      # Create the juveniles data
      juv_tmp <- NULL
      for (j in juv) {
        x <- adply(vars[[j]], c(1, 3))
        juv_tmp <- rbind(juv_tmp, x)
      }
      juv_tmp <- juv_tmp %>%
        group_by(X1, X2) %>%
        summarize_all(list(sum))
      colnames(juv_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(juv_tmp) - 3), sep = " "))
      juv_tmp$Layer <- factor(juv_tmp$Layer, levels(juv_tmp$Layer)[c(((length(unique(juv_tmp$Layer))) - 1):1, length(unique(juv_tmp$Layer)))])
      levels(juv_tmp$Layer) <- depth_labels
      juv_tmp <- as.data.frame(juv_tmp)
      juv_tmp <- gather(juv_tmp, Box, number, 3:ncol(juv_tmp))

      erla_plots[[paste(spp[[1]], "Juvenile")]] <- juv_tmp

      # Create the adults data
      ad_tmp <- NULL
      for (j in ad) {
        x <- adply(vars[[j]], c(1, 3))
        ad_tmp <- rbind(ad_tmp, x)
      }

      ad_tmp <- ad_tmp %>%
        group_by(X1, X2) %>%
        summarize_all(list(sum))
      colnames(ad_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(ad_tmp) - 3), sep = " "))
      ad_tmp$Layer <- factor(ad_tmp$Layer, levels(ad_tmp$Layer)[c(((length(unique(ad_tmp$Layer))) - 1):1, length(unique(ad_tmp$Layer)))])
      levels(ad_tmp$Layer) <- depth_labels
      ad_tmp <- as.data.frame(ad_tmp)
      ad_tmp <- gather(ad_tmp, Box, number, 3:ncol(ad_tmp))

      erla_plots[[paste(spp[[1]], "Adult")]] <- ad_tmp
    } else {
      ad <- paste(spp[[1]], juvenile_age[i]:spp[[2]], "_Nums", sep = "")
      ad_tmp <- NULL
      for (j in ad) {
        x <- adply(vars[[j]], c(1, 3))
        ad_tmp <- rbind(ad_tmp, x)
      }
      ad_tmp <- ad_tmp %>%
        group_by(X1, X2) %>%
        summarize_all(list(sum))
      colnames(ad_tmp) <- c("Layer", "Time", paste("Box", 0:(ncol(ad_tmp) - 3), sep = " "))
      ad_tmp$Layer <- factor(ad_tmp$Layer, levels(ad_tmp$Layer)[c(((length(unique(ad_tmp$Layer))) - 1):1, length(unique(ad_tmp$Layer)))])
      levels(ad_tmp$Layer) <- depth_labels
      ad_tmp <- as.data.frame(ad_tmp)
      ad_tmp <- gather(ad_tmp, Box, number, 3:ncol(ad_tmp))
      erla_plots[[paste(spp[[1]], "Adult")]] <- ad_tmp
    }
  }


  # extract physical tracers from the ncdf4 object
  phy_names <- names(nc_out$var)[!(names(nc_out$var) %in% tot_num)]
  phy_names <- phy_names[-grep("_ResN", phy_names)]
  phy_names <- phy_names[-grep("_StructN", phy_names)]
  vert_N <- paste(str_trim(rs_names), "_N", sep = "")
  phy_names <- phy_names[!(phy_names %in% vert_N)]
  phy_names <- phy_names[-which(phy_names == "nominal_dz")]
  invert_nums <- grep("_N", phy_names, value = F)
  invert_mnames <- phy_names[invert_nums]
  trace_names <- phy_names[-(invert_nums)]

  invert_vars <- list()
  for (i in 1:length(invert_mnames)) {
    tmp <- ncvar_get(nc = nc_out, varid = invert_mnames[i])

    if (length(dim(tmp)) == 2) {
      if (dim(tmp)[1] == numboxes) {
        tmp_invert <- tmp
        tmp_invert <- as.data.frame(tmp_invert)
        tmp_invert$Box <- paste("Box", 0:(numboxes - 1))
        tmp_invert <- gather(tmp_invert, Time, value = number, 1:(ncol(tmp_invert) - 1))
        tmp_invert$Time <- substring(tmp_invert$Time, 2)
        tmp_invert$Time <- as.numeric(as.character(tmp_invert$Time))
        erla_plots[[invert_mnames[i]]] <- tmp_invert
        invert_vars[[i]] <- tmp
      }
    } else {
      tmp_invert <- adply(tmp, c(1, 3))
      colnames(tmp_invert) <- c("Layer", "Time", paste("Box", 0:(ncol(tmp_invert) - 3), sep = " "))
      tmp_invert$Layer <- factor(tmp_invert$Layer, levels(tmp_invert$Layer)[c(((length(unique(tmp_invert$Layer))) - 1):1, length(unique(tmp_invert$Layer)))])
      levels(tmp_invert$Layer) <- depth_labels
      tmp_invert <- as.data.frame(tmp_invert)
      tmp_invert <- gather(tmp_invert, Box, number, 3:ncol(tmp_invert))
      invert_vars[[i]] <- tmp
      erla_plots[[invert_mnames[i]]] <- tmp_invert
    }
  }
  names(invert_vars) <- invert_mnames

  trace_vars <- list()
  for (i in 1:length(trace_names)) {
    tmp <- ncvar_get(nc = nc_out, varid = trace_names[i])
    if (length(dim(tmp)) == 2) {
      if (dim(tmp)[1] == numboxes) {
        tmp_trace <- tmp
        tmp_trace <- as.data.frame(tmp_trace)
        tmp_trace$Box <- paste("Box", 0:(numboxes - 1))
        tmp_trace <- gather(tmp_trace, Time, value = number, 1:(ncol(tmp_trace) - 1))
        tmp_invert$Time <- substring(tmp_invert$Time, 2)
        tmp_invert$Time <- as.numeric(as.character(tmp_invert$Time))
        erla_plots[[trace_names[i]]] <- tmp_trace
        trace_vars[[i]] <- tmp
      }
    } else {
      tmp_array <- adply(tmp, c(1, 3))
      tmp_trace <- tmp_array %>%
        group_by(X1, X2) %>%
        summarize_all(list(sum))
      colnames(tmp_trace) <- c("Layer", "Time", paste("Box", 0:(ncol(tmp_trace) - 3), sep = " "))
      tmp_trace$Layer <- factor(tmp_trace$Layer, levels(tmp_trace$Layer)[c(((length(unique(tmp_trace$Layer))) - 1):1, length(unique(tmp_trace$Layer)))])
      levels(tmp_trace$Layer) <- depth_labels
      tmp_trace <- as.data.frame(tmp_trace)
      tmp_trace <- gather(tmp_trace, Box, number, 3:ncol(tmp_trace))
      trace_vars[[i]] <- tmp
      erla_plots[[trace_names[i]]] <- tmp_trace
    }
  }
  names(trace_vars) <- trace_names

  # --- End Erla Plots -- #





  # ------------------------------------- #
  # - Reserve/Structural Nitrogen Plots - #
  # ------------------------------------- #

  cat("### ------------ Setting up aggregated diagnostic plots                  ------------ ###\n")
  # tmp <- within(tmp, Box <- factor(Box, levels = paste("Box", 0:(nrbox-1))))

  # Aggregate arrays

  structN <- ldply(sn_list, mean_agg)
  reserveN <- ldply(rn_list, mean_agg)
  totalnums <- ldply(vars, sum_agg)

  structN$.id <- factor(structN$.id, levels = unique(structN$.id))
  structN$Time <- as.numeric(as.character(structN$X1)) * toutinc / 365 + startyear - 1
  structN$Age <- as.numeric(gsub("[^0-9]", "", structN$.id, ""))
  structN$Ageclass <- paste("Ageclass", structN$Age)
  structN <- within(structN, Ageclass <- factor(Ageclass, levels = paste("Ageclass", 1:max(Age))))

  reserveN$.id <- factor(reserveN$.id, levels = unique(reserveN$.id))
  reserveN$Time <- as.numeric(as.character(reserveN$X1)) * toutinc / 365 + startyear - 1
  reserveN$Age <- as.numeric(gsub("[^0-9]", "", reserveN$.id, ""))
  reserveN$Ageclass <- paste("Ageclass", reserveN$Age)
  reserveN <- within(reserveN, Ageclass <- factor(Ageclass, levels = paste("Ageclass", 1:max(Age))))

  totalnums$.id <- factor(totalnums$.id, levels = unique(totalnums$.id))
  totalnums$Time <- as.numeric(as.character(totalnums$X1)) * toutinc / 365 + startyear - 1
  totalnums$Age <- as.numeric(gsub("[^0-9]", "", totalnums$.id, ""))
  totalnums$Ageclass <- paste("Ageclass", totalnums$Age)
  totalnums <- within(totalnums, Ageclass <- factor(Ageclass, levels = paste("Ageclass", 1:max(Age))))

  # Extract preference of building reserve weight
  biol_pR <- grep("pR_", biolprm, value = TRUE)
  pR_split <- unlist(str_split_fixed(biol_pR, pattern = " ", n = 2))
  pR_split <- apply(pR_split, 2, str_trim, side = "both")
  pR_group <- pR_split[, 1]
  pR_group <- str_split_fixed(pR_group, pattern = "pR_", n = 2)[, 2]
  pR_group <- fun_group$Name[match(pR_group, fun_group$Code)]
  pR_param <- str_split_fixed(pR_split[, 2], pattern = " ", n = 2)[, 1]
  pR_parms <- data.frame("Group" = pR_group, "pR" = pR_param)

  # ------------------------------------- #
  # -            Diet Plots             - #
  # ------------------------------------- #


  cat("### ------------ Setting up diet matrix plot                             ------------ ###\n")
  if (any(names(diet) == "Updated")) {
    colnames(diet) <- c("Time", "Code", "Cohort", "Stock", "Updated", fun_group[fun_group$IsTurnedOn == 1, 4])
  } else if (any(names(diet) == "Habitat")) {
    colnames(diet) <- c("Time", "Code", "Habitat", fun_group[fun_group$IsTurnedOn == 1, 4])
  } else {
    colnames(diet) <- c("Time", "Code", "Cohort", "Stock", fun_group[fun_group$IsTurnedOn == 1, 4])
  }

  diet <- merge(diet, fun_group[fun_group$IsTurnedOn == 1, c(1, 2, 4)])
  diet <- arrange(diet, Index)
  diet$Code <- diet$Name
  diet$Name <- NULL
  diet$Index <- NULL

  if (any(names(diet) == "Habitat")) {
    diet_l <- diet %>%
      gather("Prey", "eaten", 4:ncol(diet))
    colnames(diet_l) <- c("Predator", "Time", "Habitat", "Prey", "eaten")
  } else if (any(names(diet) == "Updated")) {
    diet_l <- diet %>%
      gather("Prey", "eaten", 6:ncol(diet))
    colnames(diet_l) <- c("Predator", "Time", "Cohort", "Stock", "Updated", "Prey", "eaten")
  } else {
    diet_l <- diet %>%
      gather("Prey", "eaten", 5:ncol(diet))
    colnames(diet_l) <- c("Predator", "Time", "Cohort", "Stock", "Prey", "eaten")
  }
  diet_l <- diet_l[diet_l$Time > 0, ] # delete lines at time 0 because 0 diet
  diet_l$Time <- startyear + diet_l$Time / 365
  tot_pred <- diet_l %>%
    group_by(Predator, Prey) %>%
    summarize(Eaten = mean(eaten))
  diet_l <- data.table(diet_l)
  tot_pred <- as.data.frame(tot_pred)





  cat("### ------------ Setting up summary plot                                 ------------ ###\n")


  # ------------------------------------- #
  # -          Summary Plots            - #
  # ------------------------------------- #

  ## Biomass Plots
  rel_bio <- biomass[, c(1, grep("Rel", colnames(biomass)))]
  tot_bio <- biomass[, c(1:(grep("Rel", colnames(biomass))[1] - 1))]
  # Biomass Long
  tot_bio_l <- tot_bio %>%
    gather("Code", "value", -Time)
  # Subset vertebrates
  tot_bio_v <- dplyr::filter(tot_bio_l, Code %in% rs_codes)
  # Subset invertebrates
  tot_bio_i <- dplyr::filter(tot_bio_l, !(Code %in% rs_codes))
  tot_bio_v <- merge(tot_bio_v, fun_group[c(1, 2, 4)], by = "Code")
  tot_bio_v <- arrange(tot_bio_v, Index)
  tot_bio_v$Name <- factor(tot_bio_v$Name, levels = unique(tot_bio_v$Name))
  tot_bio_v$Time <- startyear + tot_bio_v$Time / 365
  tot_bio_i <- merge(tot_bio_i, fun_group[c(1, 2, 4)], by = "Code")
  tot_bio_i <- arrange(tot_bio_i, Index)
  tot_bio_i$Name <- factor(tot_bio_i$Name, levels = unique(tot_bio_i$Name))
  tot_bio_i$Time <- startyear + tot_bio_i$Time / 365

  colnames(rel_bio) <- c("Time", fun_group$Name, "DIN")
  colnames(tot_bio) <- c("Time", fun_group$Name, "DIN")
  rel_bio$Time <- startyear + rel_bio$Time / 365
  tot_bio$Time <- startyear + tot_bio$Time / 365

  ## YOY Plots
  # Get recruitment SN and RN weight
  kwrr <- grep("KWRR", biolprm, value = T)
  if (length(grep("##", kwrr, value = FALSE)) > 0) {
    kwrr <- kwrr[-grep("##", kwrr, value = FALSE)]
  }
  kwrr_split <- unlist(str_split_fixed(kwrr, pattern = " ", n = 2))
  kwrr_split <- apply(kwrr_split, 2, str_trim, side = "both")
  kwrr_group <- str_split_fixed(kwrr_split[, 1], pattern = "_", n = 2)[, 2]
  kwrr_param <- str_split_fixed(kwrr_split[, 2], pattern = " ", n = 2)[, 1]

  kwsr <- grep("KWSR", biolprm, value = T)
  if (length(grep("##", kwsr, value = FALSE)) > 0) {
    kwsr <- kwsr[-grep("##", kwsr, value = FALSE)]
  }
  kwsr_split <- unlist(str_split_fixed(kwsr, pattern = " ", n = 2))
  kwsr_split <- apply(kwsr_split, 2, str_trim, side = "both")
  kwsr_group <- str_split_fixed(kwsr_split[, 1], pattern = "_", n = 2)[, 2]
  kwsr_param <- str_split_fixed(kwsr_split[, 2], pattern = " ", n = 2)[, 1]

  kwrr <- data.frame(group = kwrr_group, "kwrr" = as.numeric(as.character(kwrr_param)))
  kwsr <- data.frame(group = kwsr_group, "kwsr" = as.numeric(as.character(kwsr_param)))
  kwrr_kwsr_params <- merge(kwrr, kwsr)
  kwrr_kwsr_params$weight <- kwrr_kwsr_params$kwrr + kwrr_kwsr_params$kwsr
  kwrr_kwsr_params$weightTons <- kwrr_kwsr_params$weight * 20 * xCN / 1e9

  yoy_nums <- matrix(rep(NA, dim(yoy)[1] * dim(yoy)[2]), ncol = dim(yoy)[2])
  for (i in 2:ncol(yoy)) {
    findColumn <- paste(kwrr_kwsr_params$group, ".0", sep = "") == names(yoy)[i]
    yoy_nums[, i] <- yoy[, i] / kwrr_kwsr_params$weightTons[findColumn]
  }
  yoy_nums[, 1] <- yoy$Time
  yoy_nums <- as.data.frame(yoy_nums)

  # Rename columns
  colnames(yoy) <- c("Time", rs_names)
  colnames(yoy_nums) <- c("Time", rs_names)

  yoy$Time <- startyear + yoy$Time / 365
  yoy_nums$Time <- startyear + yoy_nums$Time / 365

  ## SSB PLOTS
  colnames(ssb) <- c("Time", rs_names)
  ssb$Time <- startyear + ssb$Time / 365

  # yoy[ssb$Time< 356 & ssb[1:ncol(ssb)] == 0] <- NA # Set rec as NA before SSB is calculated
  # ssb[ssb$Time< 356 & ssb[1:ncol(ssb)] == 0] <- NA # Set SSB as NA before SSB is calculated

  # Labels
  ssb_names <- colnames(ssb)[-1]
  yoy_names <- colnames(yoy)[-1]



  # Subset invertebrates
  invert_names <- fun_group[!(fun_group$GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")), ]

  # Create the production output
  prod_names <- names(prod_out$var)
  t <- prod_out$dim$t$vals
  time <- t / 60 / 60 / 24 / 365
  time <- startyear + time
  b <- prod_out$dim$b$vals
  z <- prod_out$dim$z$vals

  # Seperate the vertebrate and invertebrate groups
  vert_group <- unique(grep(paste(rs_names, collapse = "|"), prod_names, value = TRUE))
  invert_group <- unique(grep(paste(invert_names$Name, collapse = "|"), prod_names, value = TRUE))

  # Read in the data and put it into long format
  invert_all <- list()
  for (i in invert_group) {
    invert_all[[i]] <- ncvar_get(prod_out, i)
  }

  invert_all <- ldply(invert_all, colSums)
  colnames(invert_all) <- c("id", time)
  invert_l <- invert_all %>%
    gather("variable", "value", -id)

  vert_all <- list()
  for (i in vert_group) {
    vert_all[[i]] <- ncvar_get(prod_out, i)
  }
  vert_all <- ldply(vert_all, colSums)
  colnames(vert_all) <- c("id", time)
  vert_l <- vert_all %>%
    gather("variable", "value", -id)


  # ------------------------------------- #
  # -        Fisheries Plots            - #
  # ------------------------------------- #


  if (fishing) {
    cat("### ------------ Setting up fisheries plots                              ------------ ###\n")
    # Read in Data
    fish_groups <- read.csv(fishfile, header = T, stringsAsFactors = FALSE)
    fish_total <- read.table(paste(outdir, ncout, "Catch.txt", sep = ""), header = T)
    fish_fishery <- read.table(paste(outdir, ncout, "CatchPerFishery.txt", sep = ""), header = T)
    discard_total <- read.table(paste(outdir, ncout, "Discard.txt", sep = ""), header = T)
    discard_fishery <- read.table(paste(outdir, ncout, "DiscardPerFishery.txt", sep = ""), header = T)
    effort <- read.table(paste(outdir, ncout, "Effort.txt", sep = ""), header = T)
    fish_out <- nc_open(paste(outdir, ncout, "CATCH.nc", sep = ""))
    fishtot_out <- nc_open(paste(outdir, ncout, "TOTCATCH.nc", sep = ""))

    # Get variables
    var_names_fished <- names(fish_out$var)
    # remove groups that are tuned off
    GroupsOff <- fun_group_off$Name
    for (gOff in GroupsOff) {
      remove_group <- grep(gOff, var_names_fished)
      if (length(remove_group) == 0) {

      } else {
        var_names_fished <- var_names_fished[-remove_group]
      }
    }



    catch_names <- grep("Catch", var_names_fished, value = TRUE)
    catch_names <- catch_names[-grep("_FC", catch_names)]
    dis_names <- grep("Discard", var_names_fished, value = TRUE)
    dis_names <- dis_names[-grep("_FC", dis_names)]
    names_age <- str_split_fixed(dis_names, "_Discards", n = 2)[, 1]
    fishing_names <- gsub("[[:digit:]]+", "", names_age)

    # Set up fisheries files
    ts_act <- grep("TsAct", colnames(fish_total))
    fish_biomass_year <- fish_total[, 1:(ts_act[1] - 1)]
    fish_tsact_year <- fish_total[, c(1, ts_act)]
    fish_biomass_year$Time <- startyear + fish_biomass_year$Time / 365
    fish_tsact_year$Time <- startyear + fish_tsact_year$Time / 365
    fishedFish <- colnames(fish_biomass_year)[-1]
    fishedFish <- fun_group[match(fishedFish, fun_group$Code), "Name"]
    colnames(fish_biomass_year) <- c("Time", fishedFish)
    colnames(fish_tsact_year) <- c("Time", fishedFish)
    colnames(fish_fishery) <- c("Time", "Fishery", fishedFish)
    fish_fishery_l <- gather(fish_fishery, "Group", "biomass", 3:ncol(fish_fishery))
    fish_fishery_l$Time <- round(startyear - 1 + fish_fishery_l$Time / 365)
    fish_biomass_year_l <- gather(fish_biomass_year, "Group", "Biomass", 2:ncol(fish_biomass_year))
    fish_fishery_l$Fishery <- fish_groups$Name[match(fish_fishery_l$Fishery, fish_groups$Code)]

    fished_names <- names(fishtot_out$var)
    fished_catch <- grep("_Catch", fished_names, value = TRUE)
    fished_catch <- fished_catch[-grep("Avg_Catch_Sze", fished_catch, value = FALSE)]
    fished_group <- str_split_fixed(fished_catch, "_Catch", n = 2)[, 1]
    fished_group <- str_split_fixed(fished_group, "Tot_", n = 2)[, 2]
    fished_group_names <- fun_group$Name[match(fished_group, fun_group$Code)]

    # For catch by boxes
    totcatch <- list()
    for (i in 1:length(fished_group)) {
      totcatch[[i]] <- ncvar_get(nc = fishtot_out, varid = paste0("Tot_", fished_group[i], "_Catch"))
    }
    names(totcatch) <- fished_group_names
    totcatch_df <- ldply(totcatch, data.frame)
    totcatch_df$Box <- paste("Box", rep(0:(nrow(totcatch[[1]]) - 1), length(fished_group_names)))
    totcatch_df_l <- gather(totcatch_df, key = "Time", value = "Catch", 2:(ncol(totcatch_df) - 1))
    totcatch_df_l$Time <- as.numeric(gsub("[^0-9]", "", totcatch_df_l$Time, ""))
    totcatch_df_l <- totcatch_df_l[totcatch_df_l$Time > 1, ]
    totcatch_df_l$Time <- startyear + totcatch_df_l$Time - 2

    ## Effort Plots
    effort_l <- effort %>%
      gather("Fishery", "Effort", 2:ncol(effort))
    effort_l$Time <- effort_l$Time / 365 + startyear - 1


    ## Discards Plots

    # Total discard
    discard_total_l <- gather(discard_total, "Group", "Discards", 2:ncol(discard_total))
    discard_total_l$Group <- fun_group$Name[match(discard_total_l$Group, fun_group$Code)]
    discard_total_l$Time <- discard_total_l$Time / 365 + startyear
    discard_total_l <- merge(discard_total_l, fish_biomass_year_l)
    discard_total_l$prop_disc <- discard_total_l$Discards / (discard_total_l$Biomass + discard_total_l$Discards)

    # Discard by fishery
    discard_fishery_l <- gather(discard_fishery, "Group", "Discards", 3:ncol(discard_fishery))
    discard_fishery_l$Group <- fun_group$Name[match(discard_fishery_l$Group, fun_group$Code)]
    discard_fishery_l$Fishery <- fish_groups$Name[match(discard_fishery_l$Fishery, fish_groups$Code)]
    discard_fishery_l$Time <- discard_fishery_l$Time / 365 + startyear
    discard_fishery_l <- merge(discard_fishery_l, fish_fishery_l, all.x = TRUE)
    discard_fishery_l$prop_disc <- discard_fishery_l$Discards / (discard_fishery_l$biomass + discard_fishery_l$Discards)

    dis_w_list <- list()
    dis_num_list <- list()
    catch_num_list <- list()
    catch_w_list <- list()
    land_w_list <- list()
    land_num_list <- list()
    le_list <- list()
    removalsBoxAge <- list()
    removalsBoxAgeW <- list()

    for (i in 1:length(dis_names)) {
        ## Get numbers disarded
        tempDis <- ncvar_get(nc = fish_out, varid = paste0(names_age[i], "_Discards"))
        tempDis <- apply(tempDis, c(2), sum)
        dis_num_list[[i]] <- tempDis
        ## Get numbers in stock
        tempNums <- ncvar_get(nc = nc_out, varid = paste0(names_age[i], "_Nums"))
        ## Get weight for each ageclass
        tempRN <- ncvar_get(nc = nc_out, varid = paste0(names_age[i], "_ResN"))
        tempRN[tempNums <= 1e-16] <- NA
        tempSN <- ncvar_get(nc = nc_out, varid = paste0(names_age[i], "_StructN"))
        tempSN[tempNums <= 1e-16] <- NA
        structN_2 <- apply(tempSN, c(3), mean, na.rm = TRUE)
        reserveN_2 <- apply(tempRN, c(3), mean, na.rm = TRUE)
        wgt_t <- (structN_2 + reserveN_2) * 5.7 * 20 / 1e+9
        wgt_g <- (structN_2 + reserveN_2) * 5.7 * 20 / 1e+3
        ## Get length for ageclass
        fg_group <- fun_group$Code[fishing_names[i] == fun_group$Name]
        param_a <- ab_params$a[ab_params$a_name == paste0("li_a_", fg_group)]
        param_b <- ab_params$b[ab_params$b_name == paste0("li_b_", fg_group)]
        le_list[[i]] <- (wgt_g / param_a)^(1 / param_b)

        ## Get catch
        tempCat <- ncvar_get(nc = fish_out, varid = paste0(names_age[i], "_Catch"))
        tempCat <- apply(tempCat, c(2), sum)
        catch_num_list[[i]] <- tempCat + tempDis

        ## get removals per box per ageclass
        tmplist <- NULL
        tempCatR <- ncvar_get(nc = fish_out, varid = paste0(names_age[i], "_Catch"))
        tempDisR <- ncvar_get(nc = fish_out, varid = paste0(names_age[i], "_Discards"))
        tmplist <- list(tempCatR + tempDisR)
        names(tmplist) <- names_age[i]
        tmplistW <- list(tempCatR * wgt_t + tempDisR * wgt_t)
        names(tmplistW) <- names_age[i]
        removalsBoxAge <- c(removalsBoxAge, tmplist)
        removalsBoxAgeW <- c(removalsBoxAgeW, tmplistW)

                                        # Get landings
        land_num_list[[i]] <- tempCat

                                        ## Calculated discards, catch and landings in tonnes
        if (toutfinc == toutinc & length(wgt_t) == length(tempDis)) {
            dis_w_list[[i]]   <- wgt_t * tempDis
            catch_w_list[[i]] <- wgt_t * tempCat + wgt_t * tempDis
            land_w_list[[i]]  <- wgt_t * tempCat
        } else {
            dis_w_list <- NULL
            catch_w_list <- NULL
            land_w_list <- NULL
        }
    }


    rboxageoutW <- NULL
    for(m in 1:length(removalsBoxAgeW)){
        tmp <- as.data.frame(removalsBoxAgeW[[m]])
        tmpname <- names(removalsBoxAgeW)[m]
        tmp <- cbind(tmp, box = 0:52,
                     age = as.numeric(gsub('[^0-9.-]', '', tmpname)),
                     species = gsub('[[:digit:]]+', '', tmpname))
        rboxageoutW <- rbind(rboxageoutW, tmp)
    }
    rboxageoutW <-
        rboxageoutW %>%
        pivot_longer(!c(box, age, species), values_to = 'Wremovals', names_to = 'Time') %>%
      mutate(rep(startyear:(dim(rboxageoutW)[2] - 4 + startyear), dim(rboxageoutW)[1]))
    

    names(dis_num_list) <- names_age
    names(catch_num_list) <- names_age
    names(land_num_list) <- names_age
    names(le_list) <- names_age
    if (toutfinc == toutinc & length(wgt_t) == length(tempDis)) {
        names(land_w_list) <- names_age
        names(catch_w_list) <- names_age
        names(dis_w_list) <- names_age
    }

    max_time_f <- length(fish_out$dim$t$vals)
    dis_df <- ldply(dis_num_list, data.frame)
    dis_df$Time <- rep(0:(max_time_f - 1), nrow(dis_df) / max_time_f)
    dis_df$Time <- dis_df$Time * toutfinc / 365 + startyear
    names(dis_df) <- c("Group", "Discard_numb", "Time")
    dis_df$Catch_numb <- ldply(catch_num_list, data.frame)$"X..i.."
    dis_df$Land_numb <- ldply(land_num_list, data.frame)$"X..i.."
    dis_df$Age <- as.numeric(gsub("[^0-9]", "", dis_df$Group, ""))
    dis_df$Functional_Group <- gsub("[[:digit:]]+", "", dis_df$Group)

    if (toutfinc == toutinc & length(wgt_t) == length(tempDis)) {
      dis_df$Discard_weight <- ldply(dis_w_list, data.frame)$"X..i.."
      dis_df$Land_weight    <- ldply(land_w_list, data.frame)$"X..i.."
      dis_df$Catch_weight   <- ldply(catch_w_list, data.frame)$"X..i.."
      dis_df$Length <- ldply(le_list, data.frame)$"X..i.."
    }
    if (is.null(dis_w_list)) {
      warning("toutfinc != toutinc: Catch and discard in tonnes by ageclass could not be produced")
    }
    # dis_df <- dis_df[dis_df$Time != startyear,] # Remove time 0 as no catch
  } else {
    fish_fishery_l <- NULL
    totcatch_df_l <- NULL
    fish_biomass_year <- NULL
    fish_biomass_year_l <- NULL
    fishedFish <- NULL
    effort_l <- NULL
    dis_df <- NULL
    discard_total_l <- NULL
    discard_fishery_l <- NULL
  }



    output <- list(disagg = vars, invert_vars = invert_vars, 
                   invert_mnames = invert_mnames, trace_vars = trace_vars, 
                   trace_names = trace_names, var_names = tot_num, N_names = N, 
                   max_layers = max_layers, max_time = max_time, 
                   bioagg_names = bioagg_names, rs_names = rs_names, 
                   ssb_names = ssb_names, yoy_names = yoy_names, 
                   islands = islands, rel_bio = rel_bio, tot_bio = tot_bio, 
                   ssb = ssb, yoy = yoy, structN = structN, reserveN = reserveN, 
                   totalnums = totalnums, map_base = map_base, numboxes = numboxes, 
                   fun_group = fun_group, invert_names = invert_names, 
                   invert_l = invert_l, vert_l = vert_l, ab_params = ab_params, 
                   diet_l = diet_l, tot_pred = tot_pred, erla_plots = erla_plots, 
                   toutinc = toutinc, startyear = startyear, endyear = endyear, 
                   tot_bio_v = tot_bio_v, tot_bio_i = tot_bio_i, 
                   biomass_by_box = biomass_by_box, fgnames = fun_group[, 4], 
                   fish_fishery_l = fish_fishery_l, 
                   fish_biomass_year = fish_biomass_year, 
                   fishedFish = fishedFish, dens = dens, dens_km2 = dens_km2, 
                   dens_km3 = dens_km3, yoy_nums = yoy_nums, 
                   totcatch = totcatch_df_l, 
                   fish_biomass_year_l = fish_biomass_year_l, 
                   effort_l = effort_l, dis_df = dis_df, 
                   discard_total_l = discard_total_l, 
                   discard_fishery_l = discard_fishery_l, pR_parms = pR_parms, 
                   bms = bms, naa = naa, naasp = naasp, har = har, 
                   ssbmri = ssbmri, removalsBoxAge = removalsBoxAge, 
                   rboxageoutW = rboxageoutW, ntot = ntot, caa = caa,
                   removalsBoxAgeW = removalsBoxAgeW)

  cat("### ------------ vat object created, you can now run the vat application ------------ ###\n")
  return(output)
  class(output) <- "vadt"
}
