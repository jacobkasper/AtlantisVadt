create_vadt <- function(outdir, funfile, fishfile = NULL,
                        outbgm, outbgmH, ncout, startyear, endyear,
                        toutinc, toutfinc, fishing = FALSE,
                        bmsummaries, naasummaries, harsummaries, ssbmri, ntot,
                        caa                        ){
    # constants




    ##Holly's SSB
    output.bgm <- basename(list.files(
        path = outdir,
        pattern = "^atlantis.*\\.bgm$",
        full.names = TRUE
    ))

    input.harvest <- basename(list.files(
        path = outdir,
        pattern = "^Har.*\\.prm$",
        full.names = TRUE
    ))


    input.bio <- basename(list.files(
        path = outdir,
        pattern = "^Bio.*\\.prm$",
        full.names = TRUE
    ))

    #biolprm  <- paste0('../../output/Atlantis2025_03/', input.bio)
    biolprm  <- paste0(outdir, "/", input.bio)

    source('/heima/jacob/Atlantis/otherAtlantisTools/atlantis_tools_data_processing/extract_biomass_from_nc_out/R_tools_from_ReactiveAtlantis_new.R')
    dir.outputs <- outdir
    # path save location
    dir.save <- paste0(dir.outputs,"/R_output/")
    # make this directory if it does not already exist:
    ifelse(!dir.exists(file.path(dir.save)), dir.create(file.path(dir.save)), FALSE)
    # name of groups.csv input file
    input.grps <- funfile
    # name of groups.csv input file
    #input.bio <- biolprmH
    # name of groups.csv input file
    # input.harvest <- harprmH
    # name of bgm output file
    #  output.bgm <- outbgmH
    ####################' get data

    # groups data
    dir.grps.csv <- list.files(path = outdir, #dir.inputs,
                               full.names = TRUE,
                               recursive = F,
                               pattern = input.grps)[1]
    grp <- utils::read.csv(dir.grps.csv) |>
        filter(IsTurnedOn==1)

    # biology data
    dir.bio.prm <- list.files(path = outdir,
                              full.names = TRUE,
                              recursive = F,
                              pattern = input.bio)[1]

    # harvest data
    dir.harv.prm <- list.files(path = outdir,
                               full.names = TRUE,
                               recursive = F,
                               pattern = input.harvest)[1]
    # bgm data
    dir.bgm.file <- list.files(path = outdir,
                               full.names = TRUE,
                               recursive = F,
                               pattern = output.bgm)[1]
    inf.box <- boxes.prop(dir.bgm.file) # function from ReactiveAtlantis, I have edited it to isolate what is need (see source)

    # output data from nc file
    #dir.out.nc <- list.files(path = dir.outputs,
    #                        full.names = TRUE,
    #                         recursive = F,
    #                        pattern = ".nc")[1]
    dir.out.nc <- paste0(dir.outputs, '/Out.nc')
    nc.out <- ncdf4::nc_open(dir.out.nc)

    Time   <- nc.out$dim$t$vals / (60*60*24) # convert from seconds to days


    age.grp <- grp[grp$NumCohorts  > 1,]
    age.grp <- age.grp %>%
        filter(Code != "PWN")
    unique_codes <- unique(age.grp$Code)

    # Create a list to store the results
    results <- vector("list", length(unique_codes))
    names(results) <- unique_codes

    # Loop over unique codes using dplyr to filter and apply the function
    results <- lapply(unique_codes, function(code) {
        age.grp.sub <- age.grp %>% filter(Code == code)  # Subset the data for each unique code
        # Run the bio.age function
        bio.age(age.grp = age.grp.sub,
                nc.out  = nc.out,
                inf.box = inf.box,
                Time    = Time,
                flag.return.whole.system.biomass = TRUE,
                path.bio.prm = dir.bio.prm)

    })

    # Name the results by unique codes for easy reference
    names(results) <- unique_codes
    ssbholly <- map_df(results, ~ as_tibble(.x), .id = "Code") %>%
        left_join(age.grp) %>%
        select(Time, Name, Biomass)



    ####

    nsecs <- 86400
    ndays <- 365
    g_per_ton <- 1e6
    xCN <- 5.7
    wetdry <- 20

    startyear <- startyear - 1 ##jmk to adjust for time 0 in Atlantis
    species <- c("BIRD", "FISH", "MAMMAL", "SHARK")
    tons <- function(mgN) {
        return(mgN * 5.7 * 20 / 1e9)
    }

    # Functions to aggregate arrays
    mean_agg <- function(x) {
        # Calculate mean across layers and boxes for each timestep
        result <- apply(x, 3, mean, na.rm = TRUE)
        tibble(X1 = seq_along(result), X2 = result)
    }

    sum_agg <- function(x) {
        # Calculate sum across layers and boxes for each timestep
        result <- apply(x, 3, sum, na.rm = TRUE)
        tibble(X1 = seq_along(result), X2 = result)
    }

    sum_agg_2 <- function(x) {
        # Calculate sum across layers and timesteps for each box
        result <- apply(x, 2, sum, na.rm = TRUE)
        tibble(X1 = seq_along(result), X2 = result)
    }
    cat("### ------------ Reading in data                                         ------------ ###\n")

    prod_out <- ncdf4::nc_open(paste0(outdir, ncout, "PROD.nc"))

    # Read text output files
    bio_agg <- read.table(paste0(outdir, ncout, "BoxBiomass.txt"), header = TRUE)
    yoy <- read.table(paste0(outdir, ncout, "YOY.txt"), header = TRUE)
    biomass <- read.table(paste0(outdir, ncout, "BiomIndx.txt"), header = TRUE)

    diet <- read.table(paste0(outdir, ncout, "DietCheck.txt"), header = TRUE,
                       stringsAsFactors = TRUE)

    bgm_file <- list.files(outdir, pattern = "\\.bgm$", full.names = TRUE)
    if (length(bgm_file) == 0) {
        bgm_file <- list.files(pattern = "\\.bgm$", full.names = TRUE)
    }
    bgm <- readLines(bgm_file)

    # Rename InvertType to GroupType if needed
    fun_group <- read_csv(paste0(outdir, "/", funfile), show_col_types = FALSE)

    if ("InvertType" %in% names(fun_group)) {
        fun_group <- fun_group %>% rename(GroupType = InvertType)
    }

    # Split into active and inactive groups
    fun_group_off <- fun_group %>% filter(IsTurnedOn == 0)
    fun_group <- fun_group %>% filter(IsTurnedOn == 1)

    biolprm   <- readLines(biolprm)


    if (sum(names(fun_group) == "InvertType") > 0) {
        names(fun_group)[names(fun_group) == "InvertType"] <- "GroupType"
    }
    bms <- read_csv(bmsummaries, show_col_types = FALSE) %>%
        dplyr::filter(Time <= endyear) ##att jmk
    naa <- read_csv(naasummaries, show_col_types = FALSE)
    naasp <- unique(naa$species)
    har <- read_csv(harsummaries, show_col_types = FALSE) %>%
        dplyr::filter(Time <= endyear) ##att jmk
    ssbmri <- read_csv(ssbmri, show_col_types = FALSE)
    ntot <- read_csv(ntot, show_col_types = FALSE)
    caa <- read_csv(caa, show_col_types = FALSE)


    # Define vertebrate types
    vertebrate_types <- c("FISH", "MAMMAL", "SHARK", "BIRD")

    rs_data <- fun_group %>%
        filter(GroupType %in% vertebrate_types) %>%
        mutate(across(c(Name, Code), str_trim))

    rs_names <- rs_data$Name
    rs_codes <- rs_data$Code

    invert_names <- fun_group %>%
        filter(!GroupType %in% vertebrate_types)

    # Extract NetCDF variables and dimensions
    bioagg_names <- names(bio_agg)[-c(1, 2)]
    max_tracer <- nc.out$nvars
    max_layers <- length(nc.out$dim$z$vals)
    max_time <- length(nc.out$dim$t$vals)
    var_names <- names(nc.out$var)


    # ------------------------------------- #
    # -     Length Weight Parameters      - #
    # ------------------------------------- #

    # Extract a and b parameters from biology parameter
    ab_params <-
        tibble(
            a_raw = str_subset(biolprm, "li_a"),
            b_raw = str_subset(biolprm, "li_b")
        ) %>%
        separate(a_raw, into = c("a_name", "a"), sep = "\\s+", extra = "drop", convert = TRUE) %>%
        separate(b_raw, into = c("b_name", "b"), sep = "\\s+", extra = "drop", convert = TRUE)



    # ------------------------------------- #
    # -           spatial Plots           - #
    # ------------------------------------- #

    cat("### ------------ Setting up spatial plots                                ------------ ###\n")

    # Count boxes
    numboxes <- str_count(bgm, "# Box number") %>% sum()
    # Extract box areas
    areaboxes <- bgm %>%
        str_subset("area") %>%
        tibble(raw = .) %>%
        separate(raw, into = c("box_part", "area"), sep = "area\\t") %>%
        mutate(
            box = box_part %>%
                str_remove("box") %>%
                str_remove(".$") %>%
                as.numeric(),
            area = as.numeric(area) / 1e6  # convert to km2
        ) %>%
        select(box, area) %>%
        arrange(box)

    # Extract the box vertices
    vertices <- tibble(box_line = bgm) %>%
        filter(str_detect(box_line, "box\\d+\\.vert ")) %>%  # Only lines with "vert " (with space)
        mutate(box_id = str_extract(box_line, "(?<=box)\\d+") %>% as.integer()) %>%
        select(box_id, box_line)
    # Extract latitude and longitude
    map_base <- vertices %>%
        separate(box_line, into = c("label", "x", "y", "extra"),
                 sep = "\\s+", extra = "drop") %>%
        select(boxid = box_id, x, y) %>%
        mutate(across(c(x, y), as.double))
    # Check for islands
    islands <- bgm %>%
        str_subset("botz\\t0") %>%
        str_extract("box\\d+") %>%
        str_extract("\\d+") %>%
        as.integer()

    # ------------------------------------- #
    # -          Whitin Box Plots         - #
    # ------------------------------------- #

    # Calculate biomass by box - for within-box plot
    biomass_by_box <- bio_agg %>%
        pivot_longer(cols = -c(Time, Box), names_to = "Code", values_to = "value") %>%
        left_join(fun_group %>% select(Code, Name, Index), by = "Code") %>%
        arrange(Index) %>%
        mutate(
            Name = factor(Name, levels = unique(Name)),
            Time = round(Time / 365),
            Time = if_else(Time > 0, Time + startyear, Time)
        ) %>%
        filter(Time > 0)
    # ------------------------------------- #
    # -     Interactive spatial Plots     - #
    # ------------------------------------- #

    cat("### ------------ This part takes a while                                 ------------ ###\n")
    cat("### ------------ Go grab a kleina                                        ------------ ###\n")


    # Extract variable names
    nums <- str_subset(var_names, "Nums")
    N <- var_names %>%
        str_subset("_N") %>%
        str_subset("_Nums", negate = TRUE)

    tot_num <- nums
    vert_group <- str_remove(nums, "_Nums")

    str_N <- str_subset(var_names, "StructN")
    res_N <- str_subset(var_names, "ResN")

    # Extract tracers using map functions
    vars <- vert_group %>%
        map(~ncdf4::ncvar_get(nc = nc.out, varid = paste0(.x, "_Nums"))) %>%
        set_names(paste0(vert_group, "_Nums"))
    dens <- vars %>%
        map(~{
            temp_dens <- apply(.x, c(2, 3), sum) / areaboxes$area
            temp_dens[1, ] <- 0  # set as 0 in box 0
            temp_dens
        }) %>%
        set_names(paste0(vert_group, "_Nums"))
    rn_list <- vert_group %>%
        map2(vars, ~{
            temp_rn <- ncdf4::ncvar_get(nc = nc.out, varid = paste0(.x, "_ResN"))
            temp_rn[.y <= 1e-16] <- NA
            temp_rn
        }) %>%
        set_names(paste0(vert_group, "_ResN"))
    sn_list <- vert_group %>%
        map2(vars, ~{
            temp_sn <- ncdf4::ncvar_get(nc = nc.out, varid = paste0(.x, "_StructN"))
            temp_sn[.y <= 1e-16] <- NA
            temp_sn
        }) %>%
        set_names(paste0(vert_group, "_StructN"))


    volume <- ncdf4::ncvar_get(nc = nc.out, varid = "volume")
    vol <- apply(volume, c(2, 3), sum)[, 1]

    # Helper function to process each variable
    process_density <- function(var_name, metric) {
        tempN <- ncdf4::ncvar_get(nc = nc.out, varid = var_name)
        if (length(dim(tempN)) == 3) {
            # 3D case
            totBioBox <- tempN * volume
            if (metric == "km3") {
                temp_dens <- apply(totBioBox, c(2, 3), sum) / (vol + 1e-6)
            } else {  # km2
                temp_dens <- apply(totBioBox, c(2, 3), sum) / areaboxes$area
            }
            temp_dens <- tons(temp_dens)
            temp_dens[1, ] <- 0  # set as 0 in box 0
            temp_dens[, 2:ncol(temp_dens)]  # remove time 0
        } else {
            # 2D case
            if (metric == "km3") {
                totBioBox <- tempN * areaboxes$area
                tons(totBioBox / (vol + 1e-6))
            } else {  # km2
                tons(tempN)
            }
        }
    }

    # Calculate densities
    dens_km3 <- N %>%
        map(~process_density(.x, "km3")) %>%
        set_names(N)

    dens_km2 <- N %>%
        map(~process_density(.x, "km2")) %>%
        set_names(N)

    # ------------------------------------- #
    # -   Distribution by boxes Plots     - #
    # ------------------------------------- #

    # Create Erlas plots
    nominal_dz <- ncdf4::ncvar_get(nc = nc.out, varid = "nominal_dz")
    depth_layers <- nominal_dz[, which.max(colSums(nominal_dz))]
    depth_layers <- depth_layers[-c(1, length(depth_layers))] %>%
        rev() %>%
        cumsum()

    # Option 1: Be explicit about dplyr
    depth_labels <- tibble(depth = c(0, depth_layers)) %>%
        dplyr::mutate(
                   lower = depth,
                   upper = lead(depth),
                   rn = dplyr::row_number()
               ) %>%
        dplyr::mutate(
                   label = dplyr::case_when(
                                      rn == 1 ~ paste0("0 - ", upper),
                                      is.na(upper) ~ paste0(lower, " + "),
                                      TRUE ~ paste0(lower, " - ", upper)
                                  )
               ) %>%
        dplyr::pull(label) %>%
        c("Sediment")

    # Get vertebrate codes
    vert_names <- fun_group %>%
        filter(GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")) %>%
        pull(Code)

    # Extract maturity ages
    mat_age_data <- biolprm %>%
        str_subset("_age_mat") %>%
        tibble(line = .) %>%
        mutate(
            species_id = str_remove(line, "_age_mat.*"),
            # Extract first number from the line
            age = str_extract(line, "\\d+") %>% as.numeric()
        ) %>%
        filter(species_id %in% vert_names)

    species_ids <- mat_age_data$species_id
    juvenile_age <- mat_age_data$age

    process_age_group <- function(age_vars, data_list, depth_labels, metric = "number") {

        # Sum arrays element-wise across age classes
        summed_array <- age_vars %>%
            map(~data_list[[.x]]) %>%
            reduce(`+`)  # Sum arrays: [layer, box, time]

        # Get dimensions
        n_layers <- dim(summed_array)[1]
        n_boxes <- dim(summed_array)[2]
        n_times <- dim(summed_array)[3]

        # Define layer ordering
        layer_levels <- c((n_layers - 1):1, n_layers)

        # Convert to long format - manual approach to avoid factor issues
        result <- expand.grid(
            Layer = 1:n_layers,
            Box = 1:n_boxes,
            Time = 1:n_times,
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE
        ) %>%
            as_tibble() %>%
            mutate(
                !!metric := as.vector(summed_array),
                Box = Box - 1,  # Convert to 0-indexed
                Box = paste0("Box", Box),
                Layer = factor(Layer, levels = layer_levels, labels = depth_labels)
            )

        return(result)
    }

    # Main function to create plots data for numbers
    create_abundance_plots <- function(species_ids, fun_group, juvenile_age, vars, depth_labels) {

        species_data <- tibble(
            species_id = species_ids,
            juvenile_age = juvenile_age
        ) %>%
            mutate(
                # Get species info
                species_info = map(species_id, ~{
                    fun_group %>%
                        filter(Code == .x) %>%
                        select(Name, NumCohorts) %>%
                        mutate(across(everything(), str_trim))
                }),
                spp_name = map_chr(species_info, ~.x$Name),
                num_cohorts = map_int(species_info, ~as.integer(.x$NumCohorts)),

                # Define age ranges - USE SPECIES NAME not CODE
                has_juveniles = juvenile_age > 0,
                juv_ages = map2(spp_name, juvenile_age, ~{  # Changed from species_id to spp_name
                    if (.y > 0) paste0(.x, 1:.y, "_Nums") else NULL
                }),
                ad_ages = pmap(list(spp_name, juvenile_age, num_cohorts), ~{  # Changed to spp_name
                    start_age <- if (..2 > 0) ..2 + 1 else ..2
                    paste0(..1, start_age:..3, "_Nums")
                })
            )

        # Process juveniles
        juv_plots <- species_data %>%
            filter(has_juveniles) %>%
            mutate(
                juv_data = map2(juv_ages, spp_name, ~process_age_group(.x, vars, depth_labels, "number"))
            ) %>%
            select(spp_name, juv_data) %>%
            deframe() %>%
            set_names(paste(names(.), "Juvenile"))

        # Process adults
        ad_plots <- species_data %>%
            mutate(
                ad_data = map2(ad_ages, spp_name, ~process_age_group(.x, vars, depth_labels, "number"))
            ) %>%
            select(spp_name, ad_data) %>%
            deframe() %>%
            set_names(paste(names(.), "Adult"))

        # Combine
        c(juv_plots, ad_plots)
    }


    # Companion function for biomass
    create_biomass_plots <- function(species_ids, fun_group, juvenile_age, vars,
                                     rn_list, sn_list, wetdry, xCN, depth_labels) {

        # First calculate biomass arrays for all age classes
        biomass_vars <- names(vars) %>%
            set_names() %>%
            map(~{
                group <- str_remove(.x, "_Nums")

                # Get data
                nums <- vars[[.x]]
                rn <- rn_list[[paste0(group, "_ResN")]]
                sn <- sn_list[[paste0(group, "_StructN")]]

                # Replace NA with 0
                rn[is.na(rn)] <- 0
                sn[is.na(sn)] <- 0

                # Calculate biomass: wgt = (RN + SN) * wetdry * X_CN/1000
                wgt <- (rn + sn) * wetdry * xCN / 1000
                biomass <- wgt * nums /1000/1000
                return(biomass)
            })

        # Now use the same processing function with biomass data
        species_data <- tibble(
            species_id = species_ids,
            juvenile_age = juvenile_age
        ) %>%
            mutate(
                # Get species info
                species_info = map(species_id, ~{
                    fun_group %>%
                        filter(Code == .x) %>%
                        select(Name, NumCohorts) %>%
                        mutate(across(everything(), str_trim))
                }),
                spp_name = map_chr(species_info, ~.x$Name),
                num_cohorts = map_int(species_info, ~as.integer(.x$NumCohorts)),

                # Define age ranges
                has_juveniles = juvenile_age > 0,
                juv_ages = map2(spp_name, juvenile_age, ~{
                    if (.y > 0) paste0(.x, 1:.y, "_Nums") else NULL
                }),
                ad_ages = pmap(list(spp_name, juvenile_age, num_cohorts), ~{
                    start_age <- if (..2 > 0) ..2 + 1 else ..2
                    paste0(..1, start_age:..3, "_Nums")
                })
            )

        # Process juveniles
        juv_plots <- species_data %>%
            filter(has_juveniles) %>%
            mutate(
                juv_data = map2(juv_ages, spp_name, ~process_age_group(.x, biomass_vars, depth_labels, "biomass"))
            ) %>%
            select(spp_name, juv_data) %>%
            deframe() %>%
            set_names(paste(names(.), "Juvenile"))

        # Process adults
        ad_plots <- species_data %>%
            mutate(
                ad_data = map2(ad_ages, spp_name, ~process_age_group(.x, biomass_vars, depth_labels, "biomass"))
            ) %>%
            select(spp_name, ad_data) %>%
            deframe() %>%
            set_names(paste(names(.), "Adult"))

        # Combine
        c(juv_plots, ad_plots)
    }

    # Usage:




    # Get species names in the order of species_ids
    species_name_order <- fun_group %>%
        filter(Code %in% species_ids) %>%
        mutate(Code = factor(Code, levels = species_ids)) %>%
        arrange(Code) %>%
        pull(Name) %>%
        str_trim()

    erla_plots <- create_abundance_plots(species_ids, fun_group, juvenile_age, vars, depth_labels)
    # Sort to match species_ids order, with Juvenile before Adult
    erla_plots <- erla_plots[order(
        match(str_remove(names(erla_plots), " (Juvenile|Adult)$"), species_name_order),
        ifelse(str_detect(names(erla_plots), "Juvenile$"), 1, 2)  # Juvenile=1, Adult=2
    )]

    erla_biomass <- create_biomass_plots(species_ids, fun_group, juvenile_age, vars,
                                         rn_list, sn_list, wetdry, xCN, depth_labels)
    erla_biomass <- erla_biomass[order(
        match(str_remove(names(erla_biomass), " (Juvenile|Adult)$"), species_name_order),
        ifelse(str_detect(names(erla_biomass), "Juvenile$"), 1, 2)  # Juvenile=1, Adult=2
    )]



    # --- Extract physical tracers from the ncdf4 object ---
    phy_names <- names(nc.out$var) |>
        setdiff(tot_num) |>
        str_subset("_ResN", negate = TRUE) |>
        str_subset("_StructN", negate = TRUE) |>
        setdiff(paste0(str_trim(rs_names), "_N")) |>
        setdiff("nominal_dz")

    invert_mnames <- str_subset(phy_names, "_N")
    trace_names <- setdiff(phy_names, invert_mnames)

    # Helper function to process 2D arrays
    process_2d <- function(tmp, numboxes) {
        tmp |>
            as.data.frame() |>
            mutate(Box = paste("Box", 0:(numboxes - 1))) |>
            pivot_longer(-Box, names_to = "Time", values_to = "number") |>
            mutate(Time = as.numeric(str_remove(Time, "V")))
    }

    # Helper function to process 3D arrays
    process_3d <- function(tmp, depth_labels) {
        as.data.frame.table(tmp, responseName = "number") |>
            setNames(c("Layer", "Box", "Time", "number")) |>
            mutate(
                Layer = as.integer(Layer),
                Box = paste0("Box", as.integer(Box) - 1),
                Time = as.integer(Time),
                Layer = factor(Layer,
                               levels = c(rev(seq_len(max(Layer) - 1)), max(Layer)),
                               labels = depth_labels)
            )
    }

    # Process invertebrate variables
    invert_vars <- map(invert_mnames, \(varname) {
        ncdf4::ncvar_get(nc = nc.out, varid = varname)
    }) |> set_names(invert_mnames)

    erla_plots[invert_mnames] <- imap(invert_vars, \(tmp, varname) {
        if (length(dim(tmp)) == 2 && dim(tmp)[1] == numboxes) {
            process_2d(tmp, numboxes)
        } else if (length(dim(tmp)) == 3) {
            process_3d(tmp, depth_labels)
        } else {
            NULL
        }
    })

    # Process tracer variables
    trace_vars <- map(trace_names, \(varname) {
        ncdf4::ncvar_get(nc = nc.out, varid = varname)
    }) |> set_names(trace_names)

    erla_plots[trace_names] <- imap(trace_vars, \(tmp, varname) {
        if (length(dim(tmp)) == 2 && dim(tmp)[1] == numboxes) {
            process_2d(tmp, numboxes)
        } else if (length(dim(tmp)) == 3) {
            process_3d(tmp, depth_labels)
        } else {
            NULL
        }
    })

    # --- End Erla Plots ---




    # ------------------------------------- #
    # - Reserve/Structural Nitrogen Plots - #
    # ------------------------------------- #

    cat("### ------------ Setting up aggregated diagnostic plots                  ------------ ###\n")
    # Aggregate arrays
    # Helper function to process lists
    process_list <- function(data_list, agg_fun, metric_name) {
        data_list %>%
            imap_dfr(~{
                agg_fun(.x) %>%
                    as_tibble() %>%
                    mutate(.id = .y)
            }) %>%
            mutate(
                .id = factor(.id, levels = unique(.id)),
                Time = as.numeric(as.character(X1)) * toutinc / 365 + startyear,
                Age = str_extract(.id, "\\d+") %>% as.numeric(),
                Ageclass = paste("Ageclass", Age),
                Ageclass = factor(Ageclass, levels = paste("Ageclass", 1:max(Age, na.rm = TRUE)))
            ) %>%
            filter(Time > startyear)
    }

    # Apply to each list
    structN <- process_list(sn_list, mean_agg, "structN")
    reserveN <- process_list(rn_list, mean_agg, "reserveN")
    totalnums <- process_list(vars, sum_agg, "totalnums")

    # Extract preference of building reserve weight
    pR_parms <- biolprm %>%
        str_subset("pR_") %>%
        tibble(raw = .) %>%
        separate(raw, into = c("param", "value"), sep = "\\s+", extra = "drop") %>%
        mutate(
            Code = str_remove(param, "pR_"),
            Group = fun_group$Name[match(Code, fun_group$Code)],
            pR = value
        ) %>%
        select(Group, pR)

    # ------------------------------------- #
    # -            Diet Plots             - #
    # ------------------------------------- #

    cat("### ------------ Setting up diet matrix plot                             ------------ ###\n")
    # Extract prey names as a vector (fun_group is already filtered to IsTurnedOn == 1)
    # Determine diet structure and set column names

    prey_names <- fun_group$Name

    # Build column names based on structure
    base_cols <- c("Time", "Code")
    extra_cols <- if ("Updated" %in% names(diet)) {
                      c("Cohort", "Stock", "Updated")
                  } else if ("Habitat" %in% names(diet)) {
                      "Habitat"
                  } else {
                      c("Cohort", "Stock")
                  }

    # Rename columns
    colnames(diet) <- c(base_cols, extra_cols, prey_names)

    # Process diet data with type conversion
    diet <- diet %>%
        mutate(
            Code = as.character(Code),
            Time = as.numeric(Time)  # Add this line
        ) %>%
        left_join(fun_group %>% select(Index, Code, Name), by = "Code") %>%
        arrange(Index) %>%
        mutate(Code = Name) %>%
        select(-Name, -Index)

    # Pivot to long format based on structure
    diet_l <- if ("Habitat" %in% names(diet)) {
                  diet %>%
                      pivot_longer(cols = -(Time:Habitat), names_to = "Prey", values_to = "eaten") %>%
                      dplyr::rename(Predator = Code)
              } else if ("Updated" %in% names(diet)) {
                  diet %>%
                      pivot_longer(cols = -(Time:Updated), names_to = "Prey", values_to = "eaten") %>%
                      dplyr::rename(Predator = Code)
              } else {
                  diet %>%
                      pivot_longer(cols = -(Time:Stock), names_to = "Prey", values_to = "eaten") %>%
                      dplyr::rename(Predator = Code)
              }

    # Filter and transform
    diet_l <- diet_l %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    # Summarize
    tot_pred <- diet_l %>%
       dplyr::group_by(Predator, Prey) %>%
        dplyr::summarise(Eaten = mean(eaten), .groups = "drop")

    # Convert to data.table if needed
    diet_l   <- data.table::as.data.table(diet_l)
    tot_pred <- data.table::as.data.table(tot_pred)


    cat("### ------------ Setting up summary plot                                 ------------ ###\n")


    # ------------------------------------- #
    # -          Summary Plots            - #
    # ------------------------------------- #



    ## Biomass Plots
    # Get the first Rel column position
    first_rel_col <- min(grep("^Rel", names(biomass)))

    # Split correctly
    rel_bio <- biomass %>% select(Time, starts_with("Rel"))
    tot_bio <- biomass %>% select(1:(first_rel_col - 1))

    # Biomass Long
    tot_bio_l <- tot_bio %>%
        pivot_longer(cols = -Time, names_to = "Code", values_to = "value")

    # Process vertebrates
    tot_bio_v <- tot_bio_l %>%
        filter(Code %in% rs_codes) %>%
        left_join(fun_group %>% select(Code, Index, Name), by = "Code") %>%
        arrange(Index) %>%
        mutate(Name = factor(Name, levels = unique(Name))) %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    # Process invertebrates
    tot_bio_i <- tot_bio_l %>%
        filter(!Code %in% rs_codes) %>%
        left_join(fun_group %>% select(Code, Index, Name), by = "Code") %>%
        arrange(Index) %>%
        mutate(Name = factor(Name, levels = unique(Name))) %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    # Rename columns (tot_bio already has correct column names from biomass)
    # rel_bio columns are "Time", "RelFCD", "RelFHA", ... "RelDIN"
    # No need to rename - they already have meaningful names

    # Filter and transform time
    rel_bio <- rel_bio %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    tot_bio <- tot_bio %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))


    ## YOY Plots
    # Extract KWRR parameters
    kwrr_params <- biolprm %>%
        str_subset("KWRR") %>%
        str_subset("##", negate = TRUE) %>%
        tibble(line = .) %>%
        separate(line, into = c("param", "value"), sep = "\\s+", extra = "drop") %>%
        mutate(
            group = str_extract(param, "(?<=_)[A-Z]+$"),
            kwrr = as.numeric(value)
        ) %>%
        select(group, kwrr)

    # Extract KWSR parameters
    kwsr_params <- biolprm %>%
        str_subset("KWSR") %>%
        str_subset("##", negate = TRUE) %>%
        tibble(line = .) %>%
        separate(line, into = c("param", "value"), sep = "\\s+", extra = "drop") %>%
        mutate(
            group = str_extract(param, "(?<=_)[A-Z]+$"),
            kwsr = as.numeric(value)
        ) %>%
        select(group, kwsr)

    # Combine parameters and calculate weights
    kwrr_kwsr_params <- kwrr_params %>%
        left_join(kwsr_params, by = "group") %>%
        mutate(
            weight = kwrr + kwsr,
            weightTons = weight * 20 * xCN / 1e9
        )

    # Calculate YOY numbers
    yoy_nums <- yoy %>%
        pivot_longer(cols = -Time, names_to = "species", values_to = "biomass") %>%
        mutate(
            group = str_remove(species, "\\.0$"),
            biomass_num = biomass
        ) %>%
        left_join(
            kwrr_kwsr_params %>% select(group, weightTons),
            by = "group"
        ) %>%
        mutate(numbers = biomass / weightTons) %>%
        select(Time, species, numbers) %>%
        pivot_wider(names_from = species, values_from = numbers)

    # Rename columns
    colnames(yoy) <- c("Time", rs_names)
    colnames(yoy_nums) <- c("Time", rs_names)

    yoy <- yoy %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    yoy_nums <- yoy_nums %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365))

    ## SSB PLOTS
    ssb <- ssbholly %>%
        filter(Time > 0) %>%
        mutate(Time = startyear + round(Time / 365)) %>%
        group_by(Time, Name) %>%
        summarise(Biomass = sum(Biomass, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = 'Name', values_from = 'Biomass')

    # yoy[ssb$Time< 356 & ssb[1:ncol(ssb)] == 0] <- NA # Set rec as NA before SSB is calculated
    # ssb[ssb$Time< 356 & ssb[1:ncol(ssb)] == 0] <- NA # Set SSB as NA before SSB is calculated

    # Labels
    ssb_names <- colnames(ssb)[-1]
    yoy_names <- colnames(yoy)[-1]



    # Create the production output
    prod_names <- names(prod_out$var)
    t <- prod_out$dim$t$vals
    time <- startyear + (t / 60 / 60 / 24 / 365)
    b <- prod_out$dim$b$vals
    z <- prod_out$dim$z$vals

    # Separate the vertebrate and invertebrate groups
    vert_group <- str_subset(prod_names, paste(rs_names, collapse = "|"))
    invert_group <- str_subset(prod_names, paste(invert_names$Name, collapse = "|"))

    # Helper function to process production data
    process_prod_data <- function(group_vars, prod_out, time) {
        group_vars %>%
            set_names() %>%
            map(~ncdf4::ncvar_get(prod_out, .x)) %>%
            map(colSums) %>%
            imap_dfr(~tibble(id = .y, !!!set_names(.x, as.character(time)))) %>%
            pivot_longer(cols = -id, names_to = "variable", values_to = "value")
    }

    # Read in and convert to long format
    invert_l <- process_prod_data(invert_group, prod_out, time)
    vert_l <- process_prod_data(vert_group, prod_out, time)


    # ------------------------------------- #
    # -        Fisheries Plots            - #
    # ------------------------------------- #


    if (fishing) {
        cat("### ------------ Setting up fisheries plots                              ------------ ###\n")
        # Read in Data
        fish_groups <- read_csv(paste0(outdir, fishfile), show_col_types = FALSE)
        fish_total <- read_table(paste0(outdir, ncout, "Catch.txt"), show_col_types = FALSE)
        fish_fishery <- read_table(paste0(outdir, ncout, "CatchPerFishery.txt"), show_col_types = FALSE)
        discard_total <- read_table(paste0(outdir, ncout, "Discard.txt"), show_col_types = FALSE)
        discard_fishery <- read_table(paste0(outdir, ncout, "DiscardPerFishery.txt"), show_col_types = FALSE)
        effort <- read_table(paste0(outdir, ncout, "Effort.txt"), show_col_types = FALSE)
        fish_out <- ncdf4::nc_open(paste0(outdir, ncout, "CATCH.nc"))
        fishtot_out <- ncdf4::nc_open(paste0(outdir, ncout, "TOTCATCH.nc"))

        # Get variables and remove groups that are turned off
        var_names_fished <- names(fish_out$var)

        # Filter out turned-off groups
        groups_off <- fun_group_off$Name
        var_names_fished <- var_names_fished %>%
            str_subset(paste(groups_off, collapse = "|"), negate = TRUE)

        # Extract catch and discard variable names
        catch_names <- var_names_fished %>%
            str_subset("Catch") %>%
            str_subset("_FC", negate = TRUE)

        dis_names <- var_names_fished %>%
            str_subset("Discard") %>%
            str_subset("_FC", negate = TRUE)

        # Extract fishing group names
        names_age <- str_remove(dis_names, "_Discards")
        fishing_names <- str_remove_all(names_age, "\\d+")

        # Set up fisheries files
        ts_act_cols <- which(str_detect(names(fish_total), "TsAct"))

        fish_biomass_year <- fish_total %>%
            select(1:(min(ts_act_cols) - 1)) %>%
            filter(Time > 0) %>%
            mutate(Time = startyear + (Time / 365))

        fish_tsact_year <- fish_total %>%
            select(Time, starts_with("TsAct")) %>%
            filter(Time > 0) %>%
            mutate(Time = startyear + (Time / 365))

        # Get fished species names
        fishedFish <- names(fish_biomass_year)[-1]
        fishedFish <- fun_group$Name[match(fishedFish, fun_group$Code)]

        # Rename columns
        colnames(fish_biomass_year) <- c("Time", fishedFish)
        colnames(fish_tsact_year) <- c("Time", fishedFish)
        colnames(fish_fishery) <- c("Time", "Fishery", fishedFish)

        # Convert to long format
        fish_biomass_year_l <- fish_biomass_year %>%
            pivot_longer(cols = -Time, names_to = "Group", values_to = "Biomass")

        fish_fishery_l <- fish_fishery %>%
            pivot_longer(cols = -(Time:Fishery), names_to = "Group", values_to = "biomass") %>%
            mutate(Time = round(startyear + Time / 365)) %>%
            left_join(fish_groups, by = c("Fishery" = "Code")) %>%
            select(-IsRec, -NumSubFleets, -Fishery, -Index) %>%
            rename(Fishery = Name) %>%
            relocate(Fishery, .before = Group)

        # Extract fished group names from TOTCATCH file
        fished_names <- names(fishtot_out$var)
        fished_catch <- fished_names %>%
            str_subset("_Catch") %>%
            str_subset("Avg_Catch_Sze", negate = TRUE)

        fished_group <- fished_catch %>%
            str_remove("_Catch") %>%
            str_remove("Tot_")

        fished_group_names <- fun_group$Name[match(fished_group, fun_group$Code)]


        # For catch by boxes
        # Extract total catch data
        totcatch <- fished_group %>%
            set_names(fished_group_names) %>%
            map(~ncdf4::ncvar_get(nc = fishtot_out, varid = paste0("Tot_", .x, "_Catch")))

        # Convert to long format
        totcatch_df_l <- totcatch %>%
            imap_dfr(~{
                as.data.frame(.x) %>%
                    mutate(Box = paste("Box", 0:(n() - 1)),
                           Group = .y)
            }) %>%
            pivot_longer(cols = starts_with("V"), names_to = "Time", values_to = "Catch") %>%
            mutate(
                Time = as.numeric(str_extract(Time, "\\d+")),
                Time = startyear + Time - 1
            ) %>%
            filter(Time > startyear)  # Remove time zero

        ## Effort Plots
        effort_l <- effort %>%
            pivot_longer(cols = -Time, names_to = "Fishery", values_to = "Effort") %>%
            mutate(
                Time = round(Time / 365),
                Time = Time + startyear
            ) %>%
            filter(Time > startyear)  # Remove time zero

        ## Discards Plots
        # Discard total
        discard_total_l <-
            discard_total %>%
            pivot_longer(cols = -Time, names_to = "Group", values_to = "Discards") %>%
            mutate(Group = fun_group$Name[match(Group, fun_group$Code)]) %>%
            filter(Time > 0) %>%
            mutate(Time = startyear + round(Time / 365)) %>%
            inner_join(fish_biomass_year_l) %>%  # Join on all common columns
            mutate(prop_disc = Discards / (Biomass + Discards))

        # Discard by fishery
        discard_fishery_l <- discard_fishery %>%
            pivot_longer(cols = -(Time:Fishery), names_to = "Group", values_to = "Discards") %>%
            mutate(
                Group = fun_group$Name[match(Group, fun_group$Code)],
                Fishery = fish_groups$Name[match(Fishery, fish_groups$Code)]
            ) %>%
            filter(Time > 0) %>%
            mutate(Time = startyear + (Time / 365)) %>%
            left_join(fish_fishery_l, by = c("Time", "Fishery", "Group")) %>%
            mutate(prop_disc = Discards / (biomass + Discards))

        # Process catch/discard data for each age class
        process_catch_discard <- function(age_name, fish_out, nc.out, fun_group, ab_params,
                                          fishing_names, toutinc, toutfinc) {

            # Get numbers discarded
            tempDis <- ncdf4::ncvar_get(nc = fish_out, varid = paste0(age_name, "_Discards")) %>%
                apply(2, sum)

            # Get numbers in stock
            tempNums <- ncdf4::ncvar_get(nc = nc.out, varid = paste0(age_name, "_Nums"))

            # Get weight for each ageclass
            tempRN <- ncdf4::ncvar_get(nc = nc.out, varid = paste0(age_name, "_ResN"))
            tempRN[tempNums <= 1e-16] <- NA
            tempSN <- ncdf4::ncvar_get(nc = nc.out, varid = paste0(age_name, "_StructN"))
            tempSN[tempNums <= 1e-16] <- NA

            structN_2 <- apply(tempSN, 3, mean, na.rm = TRUE)
            reserveN_2 <- apply(tempRN, 3, mean, na.rm = TRUE)
            wgt_t <- (structN_2 + reserveN_2) * 5.7 * 20 / 1e9
            wgt_g <- (structN_2 + reserveN_2) * 5.7 * 20 / 1e3

            # Get length for ageclass
            species_name <- str_remove_all(age_name, "\\d+")
            fg_group <- fun_group$Code[species_name == fun_group$Name]
            param_a <- ab_params$a[ab_params$a_name == paste0("li_a_", fg_group)]
            param_b <- ab_params$b[ab_params$b_name == paste0("li_b_", fg_group)]
            le <- (wgt_g / param_a)^(1 / param_b)

            # Get catch
            tempCat <- ncdf4::ncvar_get(nc = fish_out, varid = paste0(age_name, "_Catch")) %>%
                apply(2, sum)

            # Get removals per box per ageclass
            tempCatR <- ncdf4::ncvar_get(nc = fish_out, varid = paste0(age_name, "_Catch"))
            tempDisR <- ncdf4::ncvar_get(nc = fish_out, varid = paste0(age_name, "_Discards"))
            removals <- tempCatR + tempDisR
            removalsW <- removals * wgt_t

            # Calculate weights if time steps match
            if (toutfinc == toutinc && length(wgt_t) == length(tempDis)) {
                dis_w <- wgt_t * tempDis
                catch_w <- wgt_t * (tempCat + tempDis)
                land_w <- wgt_t * tempCat
            } else {
                dis_w <- NULL
                catch_w <- NULL
                land_w <- NULL
            }

            list(
                dis_num = tempDis,
                catch_num = tempCat + tempDis,
                land_num = tempCat,
                le = le,
                removals = removals,
                removalsW = removalsW,
                dis_w = dis_w,
                catch_w = catch_w,
                land_w = land_w
            )
        }

        # Process all age classes
        catch_discard_data <- names_age %>%
            set_names() %>%
            map(~process_catch_discard(.x, fish_out, nc.out, fun_group, ab_params,
                                       fishing_names, toutinc, toutfinc))

        # Extract components
        dis_num_list <- map(catch_discard_data, "dis_num")
        catch_num_list <- map(catch_discard_data, "catch_num")
        land_num_list <- map(catch_discard_data, "land_num")
        le_list <- map(catch_discard_data, "le")
        removalsBoxAge <- map(catch_discard_data, "removals")
        removalsBoxAgeW <- map(catch_discard_data, "removalsW")
        dis_w_list <- map(catch_discard_data, "dis_w")
        catch_w_list <- map(catch_discard_data, "catch_w")
        land_w_list <- map(catch_discard_data, "land_w")

        # Process removals by box and age with weight
        rboxageoutW <- removalsBoxAgeW %>%
            imap_dfr(~{
                as.data.frame(.x) %>%
                    mutate(
                        box = 0:52,
                        age = as.numeric(str_extract(.y, "\\d+")),
                        species = str_remove_all(.y, "\\d+")
                    )
            }) %>%
            pivot_longer(cols = -c(box, age, species),
                         names_to = "Time",
                         values_to = "Wremovals") %>%
            mutate(Time = as.numeric(str_extract(Time, "\\d+")) + startyear - 1)

        # Create dis_df
        max_time_f <- length(fish_out$dim$t$vals)

        dis_df <- dis_num_list %>%
            imap_dfr(~tibble(
                         Group = .y,
                         Discard_numb = .x,
                         Time = 0:(max_time_f - 1)
                     )) %>%
            mutate(
                Catch_numb = unlist(catch_num_list),
                Land_numb = unlist(land_num_list),
                Age = as.numeric(str_extract(Group, "\\d+")),
                Functional_Group = str_remove_all(Group, "\\d+")
            )

        # Add weight and length columns if conditions met
        if (toutfinc == toutinc && !is.null(dis_w_list[[1]])) {
            dis_df <- dis_df %>%
                mutate(
                    Discard_weight = unlist(dis_w_list),
                    Land_weight = unlist(land_w_list),
                    Catch_weight = unlist(catch_w_list),
                    Length = unlist(le_list)
                )
        } else {
            warning("toutfinc != toutinc: Catch and discard in tonnes by ageclass could not be produced")
        }

        # Filter and transform time
        dis_df <- dis_df %>%
            filter(Time > 0) %>%
            mutate(Time = Time * toutfinc / 365 + startyear)
    } else {
        # If fishing == FALSE, set all to NULL
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
                   erla_biomass = erla_biomass,
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
