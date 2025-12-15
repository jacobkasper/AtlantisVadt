#' The visualizing Atlantis diagnostic tool
#'
#' The visualizing Atlantis diagnostic tool is an interactive Shiny-based tool for model tuning and calibration. It includes various plots, both aggregated and unaggregated, that are useful for diagnostic, tuning, and visualizing output. To use \code{vadt}, the user must first run the \code{create_vadt} function which will create an object of class \code{vadt} which can be fed to the \code{vadt} function. The user may also run the \code{animate_vadt} to create the animated GIFs (this is optional)
#'
#' @param obj Object of class vat returned by create_vat
#' @param anim Directory to stored animated plot created by vat_animate function (defaults to NULL)
#' @import ggplot2
#' @import shiny
#' @importFrom scales muted
#' @importFrom stringr str_trim
#' @export
#' @seealso \code{\link{create_vadt}}, \code{\link{animate_vadt}}
#' @examples
#' \dontrun{
#' anim <- "/path/to/animinated/gifs"
#' obj <- create_vadt(
#'   outdir = "/atlantis/output_dir/",
#'   fgfile = "/atlantis/functionalgroup.csv",
#'   ncout = "output_atlantis"
#' )
#' vadt(obj, anim = NULL)
#' }
#'
vadt <- function(obj, outdir, anim = NULL) {
  shinyApp(
    ui = navbarPage(
      "vadt",
      # Starting "Welcome" Tab"
      tabPanel(
        "Welcome",
        fluidRow(column(
          12,
          h1("Visualizing Atlantis Diagnostic Tool", align = "center")
        )),
        fluidRow(column(
          10,
          h1(paste("Atlantis Output Directory:", outdir), align = "center")
        )),
        p(),
        p(),
        p(),
        fluidRow(
          column(2),
          column(
            8,
            includeMarkdown("../../AtlantisVadt/vat_diagnostic.md")#includeMarkdown("http://mareframe.github.io/vat_documentation/markdown/vat_diagnostic.md")
          ),
          column(2)
        ),
        br(),
        fluidRow(
          column(2),
          column(
            1,
            img(src = "http://mareframe.github.io/vat_documentation/images/mareframe_logo.png", height = 50)
          ),
          column(4),
          column(
            1,
            img(src = "http://mareframe.github.io/vat_documentation/images/eu.jpg", height = 50)
          ),
          column(2)
        )
      ),
      # tabPanel("Functional Groups",
      #          dataTableOutput('fun_group_atl')),
      #
      # Disaggregated Spatial Maps
      navbarMenu(
        "Spatial Plots",
        tabPanel(
          "Distribution by Box",
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              "N Without Migration",
              fluidRow(
                column(4),
                column(4, wellPanel(selectInput("erla_plot_select_2",
                  label = "Functional Group",
                  choices = names(obj$erla_plots)
                )))
              ),
              fluidRow(column(
                12,
                plotOutput("vert_erla_plot_migOff", height = "1200px")
              ))
            ),
            tabPanel(
                "Biomass Without Migration",
                fluidRow(
                    column(4),
                    column(4, wellPanel(selectInput("erla_plot_select_3",
                                                    label = "Functional Group",
                                                    choices = names(obj$erla_biomass)
                                                    )))
                ),
                fluidRow(column(
                    12,
                    plotOutput("vert_erla_plot_biomass_migOff", height = "1200px")
                ))
            ),
            tabPanel(
              "Show Migration",
              fluidRow(
                column(4),
                column(4, wellPanel(selectInput("erla_plot_select",
                  label = "Functional Group",
                  choices = names(obj$erla_plots)
                )))
              ),
              fluidRow(column(
                12,
                plotOutput("vert_erla_plot", height = "1200px")
              ))
            )
          )
        ),
        tabPanel(
          "Within a Box Distribution",
          fluidRow(
            column(4),
            column(4, wellPanel(selectInput("biomass_box_sel",
              label = "Box",
              choices = unique(obj$biomass_by_box$Box)
            )))
          ),
          fluidRow(column(
            12,
            plotOutput("within_box_plot", height = "1200px")
          ))
        ),
        tabPanel(
          "Interactive Plots",
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              "Numbers per m2",
              fluidRow(
                column(
                  4,
                  wellPanel(
                    selectInput("disagg_var",
                      label = "",
                      selected = obj$var_names[1],
                      choices = obj$var_names
                    ),
                    sliderInput("time",
                      label = "Choose a time to display",
                      min = 1,
                      step = 1,
                      max = obj$max_time,
                      value = 1,
                      round = TRUE
                    )
                  )
                ),
                column(
                  8,
                  plotOutput("map_Numb", height = "450px")
                )
              )
            ),
            tabPanel(
              "Tons per km2",
              fluidRow(
                column(
                  4,
                  wellPanel(
                    selectInput("trace_sm",
                      label = "",
                      selected = obj$N_names[1],
                      choices = obj$N_names
                    ),
                    sliderInput("trace_time",
                      label = "Choose a time to display",
                      min = 1,
                      step = 1,
                      max = (obj$max_time - 1),
                      value = 1,
                      round = TRUE
                    )
                  )
                ),
                column(
                  8,
                  plotOutput("map_Bm2", height = "450px")
                )
              )
            ),
            tabPanel(
              "Tons per m3",
              fluidRow(
                column(
                  4,
                  wellPanel(
                    selectInput("invert_sm",
                      label = "",
                      selected = obj$N_names[1],
                      choices = obj$N_names
                    ),
                    sliderInput("invert_time",
                      label = "Choose a time to display",
                      min = 1,
                      step = 1,
                      max = (obj$max_time - 1),
                      value = 1,
                      round = TRUE
                    )
                  )
                ),
                column(
                  8,
                  plotOutput("map_Bm3", height = "450px")
                )
              )
            )
          )
        ),
        tabPanel(
          "Animated Spatial Biomass (tons)",
          column(
            5,
            if (is.null(anim) == FALSE) {
              wellPanel(selectInput("aggbio",
                label = "Functional Group",
                selected = obj$bioagg_names[1],
                choices = obj$bioagg_names
              ))
            }
          ),
          column(
            7,
            if (is.null(anim) == FALSE) {
              plotOutput("agg_image", inline = TRUE, "100%", "550px")
            }
          )
        )
      ),
      tabPanel(
        "Age Disaggregated",
        navlistPanel("Unit",
          widths = c(2, 10),
          tabPanel(
            "Nitrogen (mg)",
            fluidRow(
              column(4),
              column(4, wellPanel(selectInput("sn",
                label = "Functional Group",
                choices = obj$rs_names
              )))
            ),
            fluidRow(
              column(
                6,
                plotOutput("structn", height = "450px")
              ),
              column(
                6,
                plotOutput("totalnum", height = "450px")
              )
            ),
            fluidRow(
              column(
                6,
                plotOutput("reserven", height = "450px")
              ),
              column(
                6,
                plotOutput("totalprop", height = "450px")
              )
            ),
            fluidRow(
              column(
                6,
                plotOutput("lw_plot", height = "450px")
              ),
              column(
                6,
                plotOutput("totalbio", height = "450px")
              )
            ),
            fluidRow(
              column(
                6,
                plotOutput("wetwgt", height = "450px")
              ),
              column(
                6,
                plotOutput("propbio", height = "450px")
              )
            )
          ),
          tabPanel(
            "Relative weight",
            fluidRow(
              column(4),
              column(4, wellPanel(selectInput("rel",
                label = "Functional Group",
                choices = obj$rs_names
              )))
            ),
            fluidRow(
              column(
                6,
                plotOutput("structn_rel", height = "1200px")
              ),
              column(
                6,
                plotOutput("reserven_rel", height = "1200px")
              )
            )
          ),
          tabPanel(
            "RN relative weight",
            fluidRow(
              column(4),
              column(4, wellPanel(selectInput("rel_rn",
                label = "Functional Group",
                choices = obj$rs_names
              )))
            ),
            fluidRow(
              column(
                6,
                plotOutput("rn_rel", height = "1200px")
              ),
              column(
                6,
                plotOutput("FRC", height = "1200px")
              )
            )
          ),
          tabPanel(
            "NAA",
            fluidRow(
              column(4),
              column(4, wellPanel(selectInput("snaa",
                label = "Functional Group",
                choices = obj$rs_names
              )))
            ),
            fluidRow(
              column(
                6,
                plotOutput("NumAA", height = "2400px", width = "1200px")
              )
            )
          ),
        )
      ),

      # The diagnostic plots UI

      navbarMenu(
        "Diet Data",
        tabPanel(
          "Diet by Predator",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$tot_pred) == FALSE) {
                wellPanel(selectInput("diet_pred_unagg",
                  label = "Predator",
                  choices = obj$fgnames
                ))
              }
            )
          ),
          fluidRow(column(
            12,
            if (is.null(obj$tot_pred) == FALSE) {
              plotOutput("diet_pred_plot", height = "800px")
            }
          ))
        ),
        tabPanel(
          "Diet by Predator Free",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$tot_pred) == FALSE) { ## JMK
                wellPanel(selectInput("diet_pred_unaggfree",
                  label = "Predator Free",
                  choices = obj$fgnames
                ))
              }
            )
          ),
          fluidRow(column(
            12,
            if (is.null(obj$tot_pred) == FALSE) {
              plotOutput("diet_pred_plot_free", height = "800px")
            }
          ))
        ),
        tabPanel(
          "Diet by Prey",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$tot_pred) == FALSE) {
                wellPanel(selectInput("diet_prey_unagg",
                  label = "Prey",
                  choices = obj$fgnames
                ))
              }
            )
          ),
          fluidRow(column(
            12,
            if (is.null(obj$tot_pred) == FALSE) {
              plotOutput("diet_prey_plot", height = "600px")
            }
          ))
        ),
        tabPanel(
          "Diet by Predator and Prey",
          fluidRow(
            column(2),
            column(
              4,
              if (is.null(obj$tot_pred) == FALSE) {
                wellPanel(
                  selectInput("diet_dispred",
                    label = "Predator",
                    choices = obj$fgnames
                  )
                )
              }
            ),
            column(
              4,
              if (is.null(obj$tot_pred) == FALSE) {
                wellPanel(
                  selectInput("diet_disprey",
                    label = "Prey",
                    choices = obj$fgnames
                  )
                )
              }
            ),
            column(2)
          ),
          fluidRow(
            column(1),
            column(
              5,
              if (is.null(obj$tot_pred) == FALSE) {
                plotOutput("diet_pprey", height = "600px")
              }
            )
          )
        )
      ),
      navbarMenu(
        "Biological Summaries",
        tabPanel(
          "Biomass Facet Plots",
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              "Vertebrates",
              fluidRow(
                column(4),
                column(
                  4,
                  selectInput(
                    "tot_vert_scale",
                    "Scale Type",
                    c("Free", "Fixed")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  plotOutput("tot_vert_sum", height = "800px")
                )
              )
            ),
            tabPanel(
              "Invertebrates and Other Tracers",
              fluidRow(
                column(4),
                column(
                  4,
                  selectInput(
                    "tot_invert_scale",
                    "Scale Type",
                    c("Fixed", "Free")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  plotOutput("tot_invert_sum", height = "800px")
                )
              )
            )
          )
        ),
        tabPanel(
          "Vertebrates",
          fluidRow(
            column(4),
            column(
              4,
              wellPanel(
                selectInput("ssb_var",
                  label = "Functional Group",
                  choices = obj$rs_names
                )
              )
            ),
            column(4)
          ),
          fluidRow(
            column(1),
            column(
              5,
              plotOutput("tot_map", height = "300px")
            ),
            column(
              5,
              plotOutput("ssb_map", height = "300px")
            )
          ),
          fluidRow(
            column(1),
            column(
              5,
              plotOutput("harvest", height = "300px")
            ),
            column(
              5,
              plotOutput("yoy_map", height = "300px")
            )
          ),
          fluidRow(
            column(1),
            column(
              5,
              align = "center",
              plotOutput("numbers_map", height = "300px")
            )
          )
        ),
        tabPanel(
          "Invertebrates and Other Tracers",
          fluidRow(
            column(4),
            column(
              4,
              wellPanel(
                selectInput("invert_var",
                  label = "Functional Group",
                  choices = obj$invert_names$Name
                )
              )
            ),
            column(4)
          ),
          fluidRow(
            column(1),
            column(
              5,
              plotOutput("invert_rbio", height = "300px")
            ),
            column(
              5,
              plotOutput("invert_tbio", height = "300px")
            )
          ),
          fluidRow(
            column(1),
            column(
              5,
              plotOutput("invertgraze", height = "300px")
            ),
            column(
              5,
              plotOutput("invertprod", height = "300px")
            )
          )
        )
      ),
      navbarMenu(
        "Fisheries",
        tabPanel(
          "Landings By Species",
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              "Total Landings",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput(
                        "scale",
                        "Scale Type",
                        c("Free", "Fixed")
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("fish_all", height = "1200px")
              }))
            ),
            tabPanel(
              "Total Landings By Group",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput("fish_marginal",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("fish_marginal_map", height = "550px")
              }))
            ),
            tabPanel(
              "Landings (n) by ageclass",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput("fish_age_n",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    plotOutput("fish_by_age_n", height = "800px", width = "1200px")
                  }
                )
              )
            ),
            tabPanel(
              "Landings (biomass) by ageclass",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput("fish_age",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    plotOutput("fish_by_age_w", height = "800px", width = "1200px")
                  }
                ),
              )
            ),
            tabPanel(
              "Landings (biomass) by ageclass by box",
              fluidRow(
                column(2),
                column(
                  4,
                  if (is.null(obj$rboxageoutW) == FALSE) {
                    wellPanel(
                      selectInput("fish_species_box",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(
                  4,
                  if (is.null(obj$rboxageoutW) == FALSE) {
                    wellPanel(
                      selectInput("fish_age_box",
                        label = "Age",
                        choices = 1:16
                      )
                    )
                  }
                ),
                column(2)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$rboxageoutW) == FALSE) {
                    plotOutput("fish_by_age_w_box", height = "800px", width = "1200px")
                  }
                ),
              )
            ),
            tabPanel(
              "Catch (biomass) by ageclass",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$dis_df) == FALSE) {
                    wellPanel(
                      selectInput("fish_species",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$dis_df) == FALSE) {
                    plotOutput("catch_w", height = "800px", width = "1200px")
                  }
                ),
              )
            ),
            tabPanel(
              "Catch (biomass) by ageclass free",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$dis_df) == FALSE) {
                    wellPanel(
                      selectInput("fish_speciesf",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$dis_df) == FALSE) {
                    plotOutput("catch_wf", height = "800px", width = "1200px")
                  }
                ),
              )
            ),
            tabPanel(
              "Catch (N) by ageclass",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$dis_df) == FALSE) {
                    wellPanel(
                      selectInput("fish_speciesn",
                        label = "Functional Group",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(
                column(
                  6,
                  if (is.null(obj$dis_df) == FALSE) {
                    plotOutput("catch_n", height = "800px", width = "1200px")
                  }
                ),
              )
            )
          )
        ),
        tabPanel(
          "Landings By Fisheries",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                wellPanel(
                  selectInput("fish_fishery",
                    label = "Fishery",
                    choices = as.character(unique(obj$fish_fishery_l$Fishery))
                  )
                )
              }
            ),
            column(4)
          ),
          fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
            plotOutput("fish_fishery_map", height = "1200px")
          }))
        ),
        tabPanel(
          "Landings By Boxes",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                selectInput("FishedGroups",
                  label = "Functional Group",
                  choices = unique(obj$totcatch$.id)
                )
              }
            )
          ),
          fluidRow(
            column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
              plotOutput("Catch_box", height = "1200px")
            })
          )
        ),
        tabPanel(
          "Effort By Fishery",
          fluidRow(
            column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
              plotOutput("effort", height = "800px")
            })
          )
        ),
        tabPanel(
          "Discards By Group",
          fluidRow(
            column(4),
            column(
              4,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                wellPanel(selectInput("disc_Group",
                  label = "Functional Group",
                  choices = obj$fishedFish
                ))
              }
            )
          ),
          fluidRow(
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE & is.null(obj$dis_df$Length) == FALSE) {
                plotOutput("Total_discard_w", height = "450px")
              }
            ),
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE & is.null(obj$dis_df$Length) == FALSE) {
                plotOutput("prop_disc_w", height = "450px")
              }
            )
          ),
          fluidRow(
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("Total_discard_numb", height = "450px")
              }
            ),
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("prop_disc_numb", height = "450px")
              }
            )
          ),
          fluidRow(
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("prop_age", height = "450px")
              }
            ),
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("disc_age", height = "450px")
              }
            )
          ),
          fluidRow(
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE & is.null(obj$dis_df$Length) == FALSE) {
                plotOutput("prop_length", height = "450px")
              }
            ),
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE & is.null(obj$dis_df$Length) == FALSE) {
                plotOutput("disc_length", height = "450px")
              }
            )
          ),
          fluidRow(
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("discard_numb_age", height = "450px")
              }
            ),
            column(
              6,
              if (is.null(obj$fish_fishery_l) == FALSE & is.null(obj$dis_df$Discard_weight) == FALSE) {
                plotOutput("discard_weight_age", height = "450px")
              }
            )
          )
        ),
        tabPanel(
          "Discard Summary",
          navlistPanel(
            widths = c(2, 10),
            tabPanel(
              "Total Discard By Group",
              fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("discard_group", height = "1200px")
              }))
            ),
            tabPanel(
              "Total Discard By Fishery",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput("disc_fishery",
                        label = "Fishery",
                        choices = unique(obj$discard_fishery_l$Fishery)
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("discard_fishery", height = "1200px")
              }))
            ),
            tabPanel(
              "Total Discard of a Group by Fishery",
              fluidRow(
                column(4),
                column(
                  4,
                  if (is.null(obj$fish_fishery_l) == FALSE) {
                    wellPanel(
                      selectInput("disc_group",
                        label = "Fishery",
                        choices = obj$fishedFish
                      )
                    )
                  }
                ),
                column(4)
              ),
              fluidRow(column(12, if (is.null(obj$fish_fishery_l) == FALSE) {
                plotOutput("discard_fishery_group", height = "550px")
              }))
            )
          )
        )
      )
    ),
    server = function(input, output) {

        # -----------------------------------------
        # SPATIAL PLOTS TAB
        # -----------------------------------------

        # ==============================================================================
        # SPATIAL PLOTTING HELPERS
        # ==============================================================================

        # Helper function to prepare spatial map data
        prepare_spatial_data <- function(data_matrix, var_name, time_index,
                                         numboxes, islands = NULL) {
            tmp <- data_matrix[[var_name]]
            tmp_min <- min(tmp, na.rm = TRUE)
            tmp_max <- max(tmp, na.rm = TRUE)
            tmp_mid <- min(tmp, na.rm = TRUE)

            # Extract data for specific time
            tmp_values <- tmp[, time_index]

            # Set islands to NA
            if (!is.null(islands) && is.character(islands)) {
                islands_numeric <- as.numeric(islands)
                tmp_values[islands_numeric + 1] <- NA
            }

            data.frame(
                boxid = 0:(numboxes - 1),
                value = tmp_values,
                limits = list(c(tmp_min, tmp_max)),
                midpoint = tmp_mid
            )
        }

        # Standard spatial map plot
        plot_spatial_map <- function(data, map_base) {
            req(data)
            req(nrow(data) > 0)

            map_data <- merge(map_base, data, by = "boxid")

            ggplot(map_data, aes(x = x, y = y)) +
                geom_polygon(aes(group = boxid, fill = value), colour = "black") +
                theme_bw() +
                xlab("") +
                ylab("") +
                scale_fill_gradient2(
                    limits = data$limits[[1]],
                    midpoint = data$midpoint[1],
                    low = muted("white"),
                    mid = "white",
                    high = muted("blue")
                ) +
                theme(
                    legend.title = element_blank(),
                    plot.background = element_blank()
                ) +
                scale_y_continuous(breaks = NULL) +
                scale_x_continuous(breaks = NULL)
        }

        # Faceted time series by box (for within-box and erla plots)
        plot_by_box_faceted <- function(data, y_var = "number", color_var = NULL,
                                        color_palette = NULL, ncol = 5, free_scales = FALSE) {
            req(data)
            req(nrow(data) > 0)

            # Prepare data
            if ("Box" %in% names(data) && is.character(data$Box)) {
                nrbox <- length(unique(data$Box))
                data <- data %>%
                    mutate(Box = factor(Box, levels = paste("Box", 0:(nrbox - 1))))
            }

            p <- ggplot(data, aes(y = .data[[y_var]], x = Time))

            # Add coloring by layer if specified
            if (!is.null(color_var) && color_var %in% names(data)) {
                p <- p + geom_line(aes(group = .data[[color_var]], color = .data[[color_var]]),
                                   linewidth = 1)

                # Apply custom palette if provided
                if (!is.null(color_palette)) {
                    p <- p + scale_color_manual(values = color_palette)
                }
            } else {
                p <- p + geom_line(linewidth = 1)
            }

            scales_type <- if (free_scales) "free" else "fixed"

            p +
                facet_wrap(~Box, ncol = ncol, scales = scales_type) +
                theme_atlantis() +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                ylab("") +
                xlab("Year")
        }

        # ==============================================================================
        # SPATIAL PLOT OUTPUTS
        # ==============================================================================

        # Density map (Numbers per m2)
        output$map_Numb <- renderPlot({
            spatial_data <- prepare_spatial_data(
                obj$dens, input$disagg_var, input$time,
                obj$numboxes, obj$islands
            )
            plot_spatial_map(spatial_data, obj$map_base)
        })

        # Biomass per m2 map
        output$map_Bm2 <- renderPlot({
            spatial_data <- prepare_spatial_data(
                obj$dens_km2, input$trace_sm, input$trace_time,
                obj$numboxes, obj$islands
            )
            plot_spatial_map(spatial_data, obj$map_base)
        })

        # Biomass per m3 map
        output$map_Bm3 <- renderPlot({
            spatial_data <- prepare_spatial_data(
                obj$dens_km3, input$invert_sm, input$invert_time,
                obj$numboxes, obj$islands
            )
            plot_spatial_map(spatial_data, obj$map_base)
        })

        # Within box biomass plot
        output$within_box_plot <- renderPlot({
            tmp <- obj$biomass_by_box %>%
                filter(Box == input$biomass_box_sel) %>%
                mutate(value = round(value))

            ggplot(tmp, aes(y = value, x = Time)) +
                geom_line() +
                facet_wrap(~Name, scales = "free", ncol = 5) +
                theme_bw() +
                xlab("Year") +
                scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
                ylab("")
        })

        # Vertebrate numbers by box (Erla plot)
        output$vert_erla_plot <- renderPlot({
            tmp <- obj$erla_plots[[input$erla_plot_select]] %>%
                mutate(Time = as.numeric(as.character(Time)) * obj$toutinc / 365 + obj$startyear)

            # Determine color scheme based on number of layers
            color_var <- if ("Layer" %in% names(tmp)) "Layer" else NULL
            color_palette <- if (!is.null(color_var) && nlevels(tmp$Layer) == 7) {
                                 c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
                             } else {
                                 NULL
                             }

            plot_by_box_faceted(tmp, color_var = color_var, color_palette = color_palette)
        })

        # Vertebrate numbers by box with migration off
        output$vert_erla_plot_migOff <- renderPlot({
            tmp <- obj$erla_plots[[input$erla_plot_select_2]] %>%
                mutate(
                    Box = as.numeric(str_remove(Box, "Box")),
                    Time = as.numeric(as.character(Time)) * obj$toutinc / 365 + obj$startyear
                ) %>%
                filter(Time != 1)  # Remove first time step

            # Determine color scheme
            color_var <- if ("Layer" %in% names(tmp)) "Layer" else NULL
            color_palette <- if (!is.null(color_var) && nlevels(tmp$Layer) == 7) {
                                 c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
                             } else {
                                 NULL
                             }

            plot_by_box_faceted(tmp, color_var = color_var, color_palette = color_palette)
        })

        # ==============================================================================
        # BIOMASS PLOTS (from erla_biomass)
        # ==============================================================================

        # # Vertebrate biomass by box (with migration)
        # output$vert_biomass_plot <- renderPlot({
        #     req(obj$erla_biomass)

        #     tmp <- obj$erla_biomass[[input$erla_biomass_select]] %>%
        #         mutate(Time = as.numeric(as.character(Time)) * obj$toutinc / 365 + obj$startyear)

        #     # Determine color scheme based on number of layers
        #     color_var <- if ("Layer" %in% names(tmp)) "Layer" else NULL
        #     color_palette <- if (!is.null(color_var) && nlevels(tmp$Layer) == 7) {
        #                          c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
        #                      } else {
        #                          NULL
        #                      }

        #     plot_by_box_faceted(tmp, y_var = "biomass", color_var = color_var,
        #                         color_palette = color_palette)
        # })

        # Vertebrate biomass by box (migration off)
        output$vert_erla_plot_biomass_migOff <- renderPlot({
            req(obj$erla_biomass)
            req(input$erla_plot_select_3)

            tmp <- obj$erla_biomass[[input$erla_plot_select_3]] %>%
                mutate(
                    Box = as.numeric(str_remove(Box, "Box")),
                    Time = as.numeric(as.character(Time)) * obj$toutinc / 365 + obj$startyear
                ) %>%
                filter(Time != obj$startyear)  # Remove first time step

            # Determine color scheme
            color_var <- if ("Layer" %in% names(tmp)) "Layer" else NULL
            color_palette <- if (!is.null(color_var) && nlevels(tmp$Layer) == 7) {
                                 c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
                             } else {
                                 NULL
                             }

            plot_by_box_faceted(tmp, y_var = "biomass", color_var = color_var,
                                color_palette = color_palette)
        })
        # Animated aggregated biomass
        output$agg_image <- renderImage({
            filename <- normalizePath(file.path(
                anim,
                paste(input$aggbio, "-aggbio.gif", sep = "")
            ))
            list(src = filename)
        }, deleteFile = FALSE)

        # -----------------------------------------
        # AGE DISAGGREGATED TAB
        # -----------------------------------------

        # ==============================================================================
        # HELPER FUNCTIONS FOR ATLANTIS PLOTTING
        # ==============================================================================

        # Reusable theme
        theme_atlantis <- function() {
            theme_bw() +
                theme(
                    panel.background = element_blank(),
                    legend.key = element_rect(),
                    legend.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line(linewidth = .2)
                )
        }

        # Function to prepare combined biomass data (used by multiple plots)
        prepare_biomass_data <- function(structN_data, reserveN_data, totalnums_data,
                                         sn_ids, rn_ids, totn_ids) {
            req(structN_data, reserveN_data, totalnums_data)

            dat_combined <- totalnums_data %>%
                filter(.id %in% totn_ids) %>%
                select(X1, Time, Age, Ageclass, .id_totn = .id, nums = X2) %>%
                left_join(
                    structN_data %>%
                    filter(.id %in% sn_ids) %>%
                    select(X1, Time, Age, structN = X2),  # Added Age
                    by = c("X1", "Time", "Age")
                ) %>%
                left_join(
                    reserveN_data %>%
                    filter(.id %in% rn_ids) %>%
                    select(X1, Time, Age, resN = X2),  # Added Age
                    by = c("X1", "Time", "Age")
                ) %>%
                mutate(biomass_tons = ((structN + resN) * 5.7 * 20) / 1e3 * nums / 1e6)

            req(nrow(dat_combined) > 0)
            dat_combined
        }
        # General line plot function
        plot_atlantis_line <- function(data, y_var, y_label,
                                       color_var = "Ageclass", group_var = "Ageclass",
                                       y_transform = NULL) {
            req(data)
            req(nrow(data) > 0)

            # Apply transformation if provided
            if (!is.null(y_transform)) {
                data <- data %>% mutate(plot_y = {{y_transform}}(.data[[y_var]]))
                y_var <- "plot_y"
            }

            ggplot(data, aes(y = .data[[y_var]], x = Time)) +
                geom_line(aes(group = .data[[group_var]], color = .data[[color_var]]),
                          linewidth = 2, alpha = .75) +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                ylab(y_label) +
                scale_color_viridis(discrete = TRUE) +
                theme_atlantis() +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")
        }

        # General density plot function (for proportions)
        plot_atlantis_density <- function(data, y_var, y_label, fill_var = "Ageclass") {
            req(data)
            req(nrow(data) > 0)

            ggplot(data, aes(y = .data[[y_var]], x = Time)) +
                geom_density(stat = "identity", aes(fill = .data[[fill_var]]),
                             position = "fill", alpha = .75, linewidth = .2) +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                ylab(y_label) +
                scale_fill_viridis(discrete = TRUE) +
                theme_atlantis() +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")
        }

        # Faceted line plot with reference lines (for relative changes)
        plot_atlantis_faceted <- function(data, y_var, y_label,
                                          ref_lines = c(0.8, 1.2), ncol = 1) {
            req(data)
            req(nrow(data) > 0)

            # Filter out non-finite values before plotting
            data <- data %>% filter(is.finite(.data[[y_var]]))

            p <- ggplot(data, aes(y = .data[[y_var]], x = Time)) +
                geom_line() +
                facet_wrap(~Ageclass, scales = "fixed", ncol = ncol) +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                ylab(y_label) +
                theme_atlantis() +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")

            # Add reference lines if provided
            for (line in ref_lines) {
                p <- p + geom_hline(yintercept = line, linetype = 3)  # Changed from geom_abline
            }

            p
        }
        # Calculate relative change
        calc_relative_change <- function(data, value_col = "X2") {
            data %>%
                group_by(.id) %>%
                mutate(rel = .data[[value_col]] / first(.data[[value_col]])) %>%
                ungroup()
        }

        # ==============================================================================
        # SIMPLIFIED RENDER FUNCTIONS
        # ==============================================================================

        # Structural and Reserve Nitrogen (simple line plots)
        output$structn <- renderPlot({
            sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
            dat_filtered <- obj$structN %>% filter(.id %in% sn_ids)
            plot_atlantis_line(dat_filtered, "X2", "Structural Nitrogen (mg N)")
        })

        output$reserven <- renderPlot({
            rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "")
            dat_filtered <- obj$reserveN %>% filter(.id %in% rn_ids)
            plot_atlantis_line(dat_filtered, "X2", "Reserve Nitrogen (mg N)")
        })

        # Proportion and Total Biomass
        output$propbio <- renderPlot({
            sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
            rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "")
            totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")

            dat_combined <- prepare_biomass_data(obj$structN, obj$reserveN, obj$totalnums,
                                                 sn_ids, rn_ids, totn_ids)
            plot_atlantis_density(dat_combined, "biomass_tons", "Proportion of total biomass")
        })

        output$totalbio <- renderPlot({
            sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
            rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "")
            totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")

            dat_combined <- prepare_biomass_data(obj$structN, obj$reserveN, obj$totalnums,
                                                 sn_ids, rn_ids, totn_ids)
            plot_atlantis_line(dat_combined, "biomass_tons", "Total biomass (tons)")
        })

        # Total Numbers
        output$totalnum <- renderPlot({
            totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
            dat_totn <- obj$totalnums %>% filter(.id %in% totn_ids)
            plot_atlantis_line(dat_totn, "X2", "Total numbers")
        })

        output$totalprop <- renderPlot({
            totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
            dat_totn <- obj$totalnums %>% filter(.id %in% totn_ids)
            plot_atlantis_density(dat_totn, "X2", "Proportion of total numbers")
        })

        # Length-At-Age
        output$lw_plot <- renderPlot({
            sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
            lw_data <- obj$structN %>%
                filter(.id %in% sn_ids) %>%
                mutate(wt_grams = 3.65 * X2 * 5.7 * 20 / 1000)

            fg_name <- obj$fun_group %>%
                filter(str_trim(Name) == input$sn) %>%
                pull(Code)

            param_a <- obj$ab_params %>%
                filter(str_detect(a_name, fg_name)) %>%
                pull(a)

            param_b <- obj$ab_params %>%
                filter(str_detect(b_name, fg_name)) %>%
                pull(b)

            lw_data <- lw_data %>%
                mutate(length = (wt_grams / param_a)^(1 / param_b)) %>%
                filter(!is.na(X2) | Time > 1948)

            plot_atlantis_line(lw_data, "length", "Length-At-Age (cm)")
        })


        # Wet Weight
        output$wetwgt <- renderPlot({
            sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
            rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "")

            dat_combined <- obj$reserveN %>%
                filter(.id %in% rn_ids) %>%
                left_join(
                    obj$structN %>%
                    filter(.id %in% sn_ids) %>%
                    select(X1, Time, Age, Ageclass, SN = X2),  # Added Age, Ageclass
                    by = c("X1", "Time", "Age", "Ageclass")
                ) %>%
                mutate(wetwgt = (X2 + SN) * 20 * 5.7 / 1000)

            plot_atlantis_line(dat_combined, "wetwgt", "Wet weight (g)")
        })
        # Wet weight versions (structng, reserveng, totalbiog, totalnumg, totalpropg, lw_plotg)
        # These are duplicates with input$snwet instead of input$sn - just wrap the above in functions

        # Relative changes
        output$structn_rel <- renderPlot({
            sn_ids <- paste(input$rel, 1:16, "_StructN", sep = "")
            dat_sn <- obj$structN %>%
                filter(.id %in% sn_ids) %>%
                calc_relative_change("X2")

            plot_atlantis_faceted(dat_sn, "rel", "Change in Structural weight",
                                  ref_lines = c(0.8, 1.2))
        })

        output$reserven_rel <- renderPlot({
            rn_ids <- paste(input$rel, 1:16, "_ResN", sep = "")
            dat_rn <- obj$reserveN %>%
                filter(.id %in% rn_ids) %>%
                calc_relative_change("X2")

            plot_atlantis_faceted(dat_rn, "rel", "Change in Reserve weight",
                                  ref_lines = c(0.8, 1.2))
        })

        # Healthy weight (RN relative to SN)
        output$rn_rel <- renderPlot({
            rn_ids <- paste(input$rel_rn, 1:16, "_ResN", sep = "")
            sn_ids <- paste(input$rel_rn, 1:16, "_StructN", sep = "")

            dat_combined <- obj$reserveN %>%
                filter(.id %in% rn_ids) %>%
                left_join(
                    obj$structN %>%
                    filter(.id %in% sn_ids) %>%
                    select(X1, Time, Age, Ageclass, sn = X2),
                    by = c("X1", "Time", "Age", "Ageclass")
                ) %>%
                mutate(rel = X2 / (2.65 * sn)) %>%
                filter(is.finite(rel))  # Remove NA, Inf, -Inf values

            plot_atlantis_faceted(dat_combined, "rel", "Change in healthy weight",
                                  ref_lines = c(0.72, 1))
        })

        # FRC (Fraction into Structural Nitrogen)
        output$FRC <- renderPlot({
            rn_ids <- paste(input$rel_rn, 1:16, "_ResN", sep = "")
            sn_ids <- paste(input$rel_rn, 1:16, "_StructN", sep = "")

            pR_value <- obj$pR_parms %>%
                filter(Group == input$rel_rn) %>%
                pull(pR) %>%
                as.numeric()

            dat_combined <- obj$reserveN %>%
                filter(.id %in% rn_ids) %>%
                left_join(
                    obj$structN %>%
                    filter(.id %in% sn_ids) %>%
                    select(X1, Time, Ageclass, sn = X2),
                    by = c("X1", "Time", "Ageclass")
                ) %>%
                mutate(
                    temp0 = X2 / (2.65 * sn),
                    temp1 = pmax((1 / 2.65) + pR_value * (temp0 - 1), 0),
                    temp2 = (1 / 2.65) + X2 / (2.65 * sn),
                    FRC = temp1 / temp2
                )

            plot_atlantis_faceted(dat_combined, "FRC", "Proportion into SN",
                                  ref_lines = c(0.72, 1))
        })


        # NAA plot (more complex due to conditional logic and data comparison)
        output$NumAA <- renderPlot({
            req(obj$totalnums, obj$naa)

            naa_ids <- paste(input$snaa, 1:16, "_Nums", sep = "")
            naa_totn <- obj$totalnums %>% filter(.id %in% naa_ids)
            naa.t <- obj$naa %>% filter(.id %in% naa_ids)

            # Create null template for missing age classes
            max_age <- max(naa_totn$Age, na.rm = TRUE)
            naa.t.null <- tibble(
                Time = NA,
                species = rep(input$snaa, max_age),
                Age = 1:max_age,
                numbers = NA,
                .id = paste0(input$snaa, 1:max_age, "_Nums"),
                Ageclass = paste0("Ageclass ", 1:max_age)
            )

            # Case 1: No NAA data available (naa.t is empty)
            if (nrow(naa.t) == 0) {
                # Fill in missing age classes
                naa_totn <- bind_rows(naa_totn, naa.t.null) %>%
                    tidyr::complete(Age, Time) %>%
                    filter(!is.na(Time)) %>%
                    mutate(Ageclass = if_else(!is.na(Ageclass), Ageclass, paste0("Ageclass ", Age))) %>%
                    group_by(Time, Age <= max_age) %>%
                    filter(Age <= max_age) %>%
                    ungroup()

                # Aggregate oldest age class (plus group)
                naa_totn <- bind_rows(
                    naa_totn %>% filter(Age < max_age),
                    naa_totn %>%
                    filter(Age >= max_age) %>%
                    group_by(Time) %>%
                    summarise(
                        X2 = sum(X2, na.rm = TRUE),
                        Age = max_age,
                        Ageclass = paste0("Ageclass ", max_age),
                        .groups = "drop"
                    )
                ) %>%
                    arrange(Time, Age)

                ggplot(naa_totn, aes(y = X2, x = Time)) +
                    geom_line(linewidth = 1, alpha = .75) +
                    facet_wrap(. ~ Ageclass, scales = "free_y", ncol = 2) +
                    ylab("NAA") +
                    theme_atlantis() +
                    scale_x_continuous(breaks = round(as.numeric(quantile(naa_totn$Time, probs = seq(0, 1, .2)))))+
                    guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                    xlab("Year")

            } else {
                # Case 2: NAA data available - compare model vs assessment
                max_naa_age <- max(naa.t$Age, na.rm = TRUE)

                # Fill in missing age classes for naa.t
                naa.t <- bind_rows(naa.t, naa.t.null) %>%
                    tidyr::complete(Age, Time) %>%
                    filter(!is.na(Time)) %>%
                    mutate(Ageclass = if_else(!is.na(Ageclass), Ageclass, paste0("Ageclass ", Age)))

                # Handle Capelin special case
                if (input$snaa != 'Capelin') {
                    naa.t <- naa.t %>%
                        filter(!is.na(species)) %>%
                        arrange(Time, Age)
                }

                # Aggregate model data to match NAA age range
                naa_totn <- bind_rows(
                    naa_totn %>% filter(Age < max_naa_age),
                    naa_totn %>%
                    filter(Age >= max_naa_age) %>%
                    group_by(Time) %>%
                    summarise(
                        X2 = sum(X2, na.rm = TRUE),
                        Age = max_naa_age,
                        Ageclass = paste0("Ageclass ", max_naa_age),
                        .groups = "drop"
                    )
                ) %>%
                    arrange(Time, Age)

                # Plot with both model (black) and assessment (grey) lines
                ggplot(naa_totn, aes(y = X2, x = Time)) +
                    geom_line(linewidth = 1, alpha = .75) +
                    geom_line(aes(y = numbers, x = Time), data = naa.t, color = "grey", linewidth = 1) +
                    facet_wrap(. ~ Ageclass, ncol = 2) +
                    ylab("NAA (Model=black, Assessment=grey)") +
                    theme_atlantis() +
                    scale_x_continuous(breaks = round(as.numeric(quantile(naa_totn$Time, probs = seq(0, 1, .2)))))+
                    guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                    xlab("Year")
            }
        })

        # -----------------------------------------
        # DIET DATA TAB
        # -----------------------------------------
        # Diet Predator by prey
        # ==============================================================================
        # DIET PLOTTING HELPERS
        # ==============================================================================

        # Helper function for diet plots
        plot_diet <- function(data, color_var, color_label, y_label, title,
                              facet_var = NULL, facet_scales = "fixed") {
            req(data)
            req(nrow(data) > 0)

            # Filter to only positive values
            data <- data %>% filter(eaten > 0)

            p <- ggplot(data, aes(x = Time, y = eaten, color = as.factor(.data[[color_var]]))) +
                geom_line(linewidth = 1, alpha = .75) +
                scale_color_viridis(discrete = TRUE, name = color_label) +
                xlab("Year") +
                ylab(y_label) +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                ggtitle(title) +
                theme_atlantis() +
                guides(fill = guide_legend(override.aes = list(colour = NULL)))

            # Add faceting if specified
            if (!is.null(facet_var)) {
                p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = facet_scales)
            }

            p
        }

        # Determine color variable and labels based on data structure
        get_diet_color_info <- function(data) {
            if ("Habitat" %in% names(data)) {
                list(var = "Habitat", label = "Habitat", y_label = "?")
            } else {
                list(var = "Cohort", label = "Cohort", y_label = "Proportion of Diet")
            }
        }

        # ==============================================================================
        # DIET PLOT OUTPUTS
        # ==============================================================================

        # Diet by specific predator-prey pair
        output$diet_pprey <- renderPlot({
            data_dpp <- obj$diet_l %>%
                filter(Predator == input$diet_dispred, Prey == input$diet_disprey)

            color_info <- get_diet_color_info(data_dpp)

            title <- if ("Habitat" %in% names(data_dpp)) {
                         paste0("Diet of ", data_dpp$Prey[1], " by ", data_dpp$Predator[1], " by Habitat Type")
                     } else {
                         paste0("Diet of ", data_dpp$Prey[1], " by ", data_dpp$Predator[1], " by Age Class")
                     }

            plot_diet(data_dpp,
                      color_var = color_info$var,
                      color_label = color_info$label,
                      y_label = color_info$y_label,
                      title = title)
        })

        # Diet by predator (faceted by prey)
        output$diet_pred_plot <- renderPlot({
            predConsume <- obj$diet_l %>% filter(Predator == input$diet_pred_unagg)

            color_info <- get_diet_color_info(predConsume)

            title <- if ("Habitat" %in% names(predConsume)) {
                         paste0("Diet of ", predConsume$Predator[1], " by Habitat")
                     } else {
                         paste0("Diet of ", predConsume$Predator[1], " by Age Class")
                     }

            plot_diet(predConsume,
                      color_var = color_info$var,
                      color_label = color_info$label,
                      y_label = color_info$y_label,
                      title = title,
                      facet_var = "Prey")
        })

        # Diet by predator with free scales
        output$diet_pred_plot_free <- renderPlot({
            predConsume <- obj$diet_l %>% filter(Predator == input$diet_pred_unaggfree)

            color_info <- get_diet_color_info(predConsume)

            title <- if ("Habitat" %in% names(predConsume)) {
                         paste0("Diet of ", predConsume$Predator[1], " by Habitat")
                     } else {
                         paste0("Diet of ", predConsume$Predator[1], " by Age Class")
                     }

            plot_diet(predConsume,
                      color_var = color_info$var,
                      color_label = color_info$label,
                      y_label = color_info$y_label,
                      title = title,
                      facet_var = "Prey",
                      facet_scales = "free")
        })

        # Diet by prey (faceted by predator)
        output$diet_prey_plot <- renderPlot({
            data_dpp <- obj$diet_l %>% filter(Prey == input$diet_prey_unagg)

            color_info <- get_diet_color_info(data_dpp)

            title <- if ("Habitat" %in% names(data_dpp)) {
                         paste0(data_dpp$Prey[1], " in Diet of Others by Habitat Type")
                     } else {
                         paste0(data_dpp$Prey[1], " in Diet of Others by Age Class")
                     }

            plot_diet(data_dpp,
                      color_var = color_info$var,
                      color_label = color_info$label,
                      y_label = color_info$y_label,
                      title = title,
                      facet_var = "Predator")
        })

      # -----------------------------------------
      # SUMMARIES TAB
      # -----------------------------------------

        ## Total biomass trellis
        # ==============================================================================
        # BIOMASS & ASSESSMENT PLOTTING HELPERS
        # ==============================================================================

        # Standard time series plot with optional comparison line and reference line
        plot_timeseries <- function(data, x_var = "Time", y_var, y_label = "",
                                    title = "", comparison_data = NULL,
                                    comparison_y = NULL, ref_line = NULL,
                                    expand_y_zero = TRUE, x_breaks = NULL) {
            req(data)
            req(nrow(data) > 0)

            p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
                geom_line() +
                geom_point(shape = 21, size = 0.5) +
                ylab(y_label) +
                xlab("Year") +
                ggtitle(title) +
                theme_bw()

            # Add comparison line (grey) if provided
            if (!is.null(comparison_data) && !is.null(comparison_y)) {
                p <- p +
                    geom_line(data = comparison_data, aes(y = .data[[comparison_y]]), color = "grey") +
                    geom_point(data = comparison_data, aes(y = .data[[comparison_y]]),
                               color = "grey", shape = 21, size = 0.5)
            }

            # Add reference vertical line if provided
            if (!is.null(ref_line)) {
                p <- p + geom_vline(xintercept = ref_line, linetype = 3)
            }

            # Expand y to zero
            if (expand_y_zero) {
                p <- p + expand_limits(y = 0)
            }

            # Set x-axis breaks
            if (!is.null(x_breaks)) {
                p <- p + scale_x_continuous(breaks = x_breaks)
            } else {
                p <- p + scale_x_continuous(breaks = round(as.numeric(quantile(data[[x_var]], probs = seq(0, 1, .2), na.rm = TRUE))))
            }

            p
        }

        # Faceted biomass plot
        plot_biomass_faceted <- function(data, scales = "fixed", ncol = 4,
                                         comparison_data = NULL, x_breaks = NULL) {
            req(data)
            req(nrow(data) > 0)

            p <- ggplot(data, aes(x = Time, y = value)) +
                geom_line() +
                facet_wrap(~Name, ncol = ncol, scales = scales) +
                xlab("Year") +
                ylab("Total Biomass") +
                theme_bw() +
                expand_limits(y = 0)

            # Add comparison data if provided and scales are free
            if (!is.null(comparison_data) && scales == "free") {
                p <- p + geom_line(data = comparison_data, aes(y = value, x = Time), color = "grey")
            }

            # Set x-axis breaks
            if (!is.null(x_breaks)) {
                p <- p + scale_x_continuous(breaks = x_breaks)
            } else {
                p <- p + scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2)))))
            }

            p
        }

        # ==============================================================================
        # BIOMASS PLOT OUTPUTS
        # ==============================================================================

        # Total vertebrate biomass trellis
        output$tot_vert_sum <- renderPlot({
            bm_mri <- obj$bms %>%
                pivot_longer(!Time, names_to = "Name", values_to = "value")

            x_breaks <- seq(1948, obj$endyear, 10)
            scales <- if (input$tot_vert_scale == "Fixed") "fixed" else "free"
            comparison <- if (input$tot_vert_scale == "Fixed") NULL else bm_mri

            plot_biomass_faceted(obj$tot_bio_v, scales = scales,
                                 comparison_data = comparison, x_breaks = x_breaks)
        })

        # Total invertebrate biomass trellis
        output$tot_invert_sum <- renderPlot({
            scales <- if (input$tot_invert_scale == "Fixed") "fixed" else "free"
            plot_biomass_faceted(obj$tot_bio_i, scales = scales)
        })

        # Harvest biomass
        output$harvest <- renderPlot({
            landings <- obj$fish_biomass_year %>%
                select(Time, Landings = all_of(input$ssb_var))

            har_summary <- obj$har %>%
                filter(Group == input$ssb_var) %>%
                group_by(Time) %>%
                summarise(biomass = sum(biomass), .groups = "drop")

            plot_timeseries(landings, y_var = "Landings", y_label = "Landings (tons)",
                            comparison_data = har_summary, comparison_y = "biomass",
                            ref_line = 2000,
                            x_breaks = round(as.numeric(quantile(obj$tot_bio$Time, probs = seq(0, 1, .1)))))
        })

        # Total biomass map
        output$tot_map <- renderPlot({
            # Add missing columns with zeros
            bms_missing <- setdiff(names(obj$tot_bio), names(obj$bms))
            obj$bms[bms_missing[!is.na(bms_missing)]] <- 0

            model_data <- obj$tot_bio %>%
                select(Time, value = all_of(input$ssb_var))

            comparison_data <- obj$bms %>%
                select(Time, value = all_of(input$ssb_var))

            plot_timeseries(model_data, y_var = "value", title = "Total Biomass",
                            comparison_data = comparison_data, comparison_y = "value",
                            ref_line = 2000,
                            x_breaks = round(as.numeric(quantile(obj$tot_bio$Time, probs = seq(0, 1, .2)))))
        })

        # SSB map
        output$ssb_map <- renderPlot({
            tmp <- obj$ssb %>%
                select(Time, Group = all_of(input$ssb_var)) %>%
                filter(Time >= first(Time) + 1)

            # Add missing columns with zeros
            ssb_missing <- setdiff(names(obj$ssb), names(obj$ssbmri))
            obj$ssbmri[ssb_missing] <- 0

            comparison_data <- obj$ssbmri %>%
                select(Time, value = all_of(input$ssb_var))

            p <- plot_timeseries(tmp, y_var = "Group", title = "Spawning Stock Biomass",
                                 comparison_data = comparison_data, comparison_y = "value",
                                 ref_line = 2000)

            # Add confidence ribbon if available
            lower_col <- paste0(input$ssb_var, "l")
            upper_col <- paste0(input$ssb_var, "h")

            if (lower_col %in% names(obj$ssbmri) && upper_col %in% names(obj$ssbmri)) {
                p <- p + geom_ribbon(
                             data = obj$ssbmri,
                             aes(x = Time, ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
                             inherit.aes = FALSE,
                             fill = 'blue',
                             alpha = 0.1
                         )
            }

            p
        })

        # YOY map
        output$yoy_map <- renderPlot({
            tmp <- obj$yoy_nums %>%
                select(Time, Group = all_of(input$ssb_var)) %>%
                filter(Time >= first(Time) + 1)

            plot_timeseries(tmp, y_var = "Group", title = "Number of recruits")
        })

        # Numbers map
        output$numbers_map <- renderPlot({
            n_map <- obj$totalnums %>%
                mutate(
                    species = str_remove_all(.id, "\\d+|_Nums"),
                    numbers = X2
                ) %>%
                filter(species == input$ssb_var) %>%
                group_by(species, Time) %>%
                summarise(n = sum(numbers), .groups = "drop")

            # Add missing columns with zeros
            ntot_missing <- setdiff(unique(n_map$species), names(obj$ntot))
            obj$ntot[ntot_missing] <- 0

            comparison_data <- obj$ntot %>%
                select(Time, value = all_of(input$ssb_var))

            ggplot(n_map, aes(y = n, x = Time)) +
                geom_line() +
                geom_point(data = comparison_data, aes(y = value, x = Time),
                           color = "darkgrey", size = 3) +
                ylab("Numbers") +
                xlab("Year") +
                ggtitle("Abundance") +
                expand_limits(y = 0) +
                theme_bw()
        })

        # ==============================================================================
        # INVERTEBRATE PLOTS
        # ==============================================================================

        # Invertebrate relative biomass
        output$invert_rbio <- renderPlot({
            invert_rbio <- obj$rel_bio %>%
                select(Time, matches(input$invert_var)) %>%
                mutate(Biomass = rowSums(select(., -Time))) %>%
                select(Time, Biomass) %>%
                mutate(Biomass = Biomass / first(Biomass))

            plot_timeseries(invert_rbio, y_var = "Biomass", title = "Relative Biomass")
        })

        # Invertebrate total biomass
        output$invert_tbio <- renderPlot({
            invert_tbio <- obj$tot_bio %>%
                select(Time, matches(input$invert_var)) %>%
                mutate(Biomass = rowSums(select(., -Time))) %>%
                select(Time, Biomass)

            plot_timeseries(invert_tbio, y_var = "Biomass", title = "Total Biomass")
        })

        # Invertebrate grazing
        output$invertgraze <- renderPlot({
            graze_dat <- obj$invert_l %>%
                filter(str_detect(id, input$invert_var), str_detect(id, "Grazing")) %>%
                group_by(variable) %>%
                summarise(value = sum(value), .groups = "drop") %>%
                mutate(Time = as.numeric(as.character(variable)))

            plot_timeseries(graze_dat, x_var = "Time", y_var = "value", title = "Grazing")
        })

        # Invertebrate production
        output$invertprod <- renderPlot({
            prod_dat <- obj$invert_l %>%
                filter(str_detect(id, input$invert_var), str_detect(id, "Prodn")) %>%
                group_by(variable) %>%
                summarise(value = sum(value), .groups = "drop") %>%
                mutate(Time = as.numeric(as.character(variable)))

            plot_timeseries(prod_dat, x_var = "Time", y_var = "value", title = "Production")
        })
        # -----------------------------------------
        # FISHERIES TAB
        # -----------------------------------------

      output$fish_all <- renderPlot({
        tmp <-
          obj$har %>%
          group_by(Time, Group) %>%
          dplyr::mutate(biomass = sum(biomass)) %>%
          select(-Fishery) %>%
          distinct()

        if (input$scale == "Free") {
          ggplot(data = obj$fish_biomass_year_l[obj$fish_biomass_year_l$Biomass > 0, ],
                 aes(y = Biomass, x = Time)) +
            geom_line() +
            geom_line(data = tmp, aes(y = biomass, x = Time), color = "grey") +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$fish_biomass_year_l$Time, probs = seq(0, 1, .1))))) +
            facet_wrap(~Group, scales = "free", ncol = 5) +
            theme_bw() +
            xlab("Time") +
            ylab("Landings (tons)")
        } else {
          ggplot(data = obj$fish_biomass_year_l[obj$fish_biomass_year_l$Biomass > 0, ],
                 aes(y = Biomass, x = Time)) +
            geom_line() +
            geom_line(data = tmp, aes(y = biomass, x = Time), color = "grey") +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$fish_biomass_year_l$Time,
                                                                  probs = seq(0, 1, .1))))) +
            facet_wrap(~Group, ncol = 5) +
            theme_bw() +
            xlab("Time") +
            ylab("Landings (tons)")
        }
      })




      output$fish_marginal_map <- renderPlot({
        ofby <-
          tibble(obj$fish_biomass_year) %>%
          mutate(Time = Time-1) %>%
          filter(Time >= obj$startyear)
        ggplot(aes(y = ofby[[match(input$fish_marginal,
                                                    names(ofby))]], x = Time),
               data = ofby) +
          geom_line() +
          scale_x_continuous(breaks = round(as.numeric(quantile(ofby$Time, probs = seq(0, 1, .1))))) +theme_bw() +
          ylab("Landings (tons)") +
          xlab("Year")
      })

      # Catch by ageclass

      output$fish_by_age_n <- renderPlot({
          tmp <-
        left_join(
            as_tibble(obj$dis_df[obj$dis_df$Functional_Group == input$fish_age_n, ]) %>%
              mutate(Age = as.factor(Age)) %>%
              select(Time, Functional_Group, Age, Land_numb) %>%
          mutate(Time = Time-1) %>% ##JMK bug
          filter(Time >= obj$startyear),
            as_tibble(obj$caa) %>%
              filter(species == input$fish_age_n) %>%
              dplyr::rename(obscaa = catch) %>%
              dplyr::rename(Functional_Group = species) %>%
              mutate(Age = as.factor(Age))
        )

        ggplot(data = tmp, aes(y = Land_numb, x = Time)) +
          geom_line(linewidth = 1, alpha = .75) +
          geom_line(aes(y = obscaa, x = Time), col = "grey") +
          facet_wrap(. ~ Age, ncol = 5, scales = "free_y") +
          scale_color_viridis(discrete = TRUE) +
          ylab("Landings (numbers)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

     output$fish_by_age_w <- renderPlot({
        tmp <- obj$dis_df[obj$dis_df$Functional_Group == input$fish_age, ] %>%
          mutate(Time = Time-1) %>%
          filter(Time >= obj$startyear) %>%
          mutate(Age = as.factor(Age))

        ggplot(data = tmp, aes(y = Land_weight, x = Time)) +
          geom_line(linewidth = 1, alpha = .75) +
          facet_wrap(. ~ Age, ncol = 5, scales = "free_y") +
          scale_color_viridis(discrete = TRUE) +
          ylab("Landings (Tons)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          )  +
          xlab("Year")
      })

      ## removals by age and box W
      output$fish_by_age_w_box <- renderPlot({
        tmp <-
          obj$rboxageoutW[obj$rboxageoutW$species == input$fish_species_box &
            obj$rboxageoutW$age == input$fish_age_box, ]

          ggplot(tmp) +
          geom_line(aes(x = Time, y = Wremovals)) +
          facet_wrap(~box) +
          theme_bw() +
          ylab("Landings (Tons)") +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      output$catch_w <- renderPlot({
        tmp <- left_join(
          as_tibble(obj$dis_df[obj$dis_df$Functional_Group == input$fish_species, ]) %>%
            mutate(Age = as.factor(Age)) %>%
            mutate(Time = Time-1) %>%
            filter(Time >= obj$startyear) %>%
            select(Time, Functional_Group, Age, Catch_weight),
          as_tibble(obj$caa) %>%
            filter(species == input$fish_species) %>%
            dplyr::rename(bmaa = biomassaa) %>%
            dplyr::rename(Functional_Group = species) %>%
            mutate(Age = as.factor(Age))
        )
        ggplot(data = tmp, aes(y = Catch_weight, x = Time)) +
          geom_line(linewidth = 1, alpha = .75) +
          geom_line(aes(y = bmaa, x = Time), col = "grey") +
          facet_wrap(. ~ Age, ncol = 5) + # , scales = "free_y") +
          scale_color_viridis(discrete = TRUE) +
          ylab("Catch (harvest + discards) (Tons)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          )  +
          xlab("Year")
      })

      output$catch_wf <- renderPlot({
        tmp <- left_join(
          as_tibble(obj$dis_df[obj$dis_df$Functional_Group == input$fish_speciesf, ]) %>%
            mutate(Age = as.factor(Age))  %>%
            mutate(Time = Time-1) %>%
            filter(Time >= obj$startyear) %>%
            select(Time, Functional_Group, Age, Catch_weight),
          as_tibble(obj$caa) %>%
            filter(species == input$fish_speciesf) %>%
            dplyr::rename(bmaa = biomassaa) %>%
            dplyr::rename(Functional_Group = species) %>%
            mutate(Age = as.factor(Age))
        )
        ggplot(data = tmp, aes(y = Catch_weight, x = Time)) +
          geom_line(linewidth = 1, alpha = .75) +
          geom_line(aes(y = bmaa, x = Time), col = "grey") +
          facet_wrap(. ~ Age, ncol = 5, scales = "free_y") +
          scale_color_viridis(discrete = TRUE) +
          ylab("Catch (harvest + discards) (Tons)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      output$catch_n <- renderPlot({
        tmp <-
          as_tibble(obj$dis_df[obj$dis_df$Functional_Group == input$fish_speciesn, ]) %>%
          mutate(Age = Age)  %>%
          mutate(Time = Time-1) %>%
          filter(Time >= obj$startyear) %>%
          select(Time, Functional_Group, Age, Catch_numb)
        tmpobs <-
          as_tibble(obj$caa) %>%
          filter(species == input$fish_speciesn) %>%
          dplyr::rename(obscaa = catch) %>%
          dplyr::rename(Functional_Group = species) %>%
          mutate(Age = Age) %>%
          filter(Time <= obj$endyear)
        tmp <-
          full_join(
            bind_rows(
              tmp %>%
                filter(Age < max(tmpobs$Age)),
              tmp %>%
                filter(Age >= max(tmpobs$Age)) %>%
                group_by(Time) %>%
                dplyr::mutate(Catch_numb = sum(Catch_numb)) %>%
                filter(Age == max(tmpobs$Age))
            ) %>%
              arrange(Time, Age),
            tmpobs
          ) %>%
          replace(is.na(.), 0)

        ggplot(data = tmp, aes(y = Catch_numb, x = Time)) +
          geom_line(linewidth = 1, alpha = .75) +
          geom_line(aes(y = obscaa, x = Time), col = "grey") +
          facet_wrap(. ~ Age, ncol = 5, scales = "free_y") +
          scale_color_viridis(discrete = TRUE) +
          ylab("Catch (harvest + discards) (N)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      # Catch by Fishery

        # ==============================================================================
        # FISHERIES PLOTTING HELPERS
        # ==============================================================================

        # Prepare fisheries data with optional time adjustment and filtering
        prepare_fish_data <- function(data, species_col, species_value,
                                      adjust_time = TRUE, filter_time = TRUE) {
            result <- data %>%
                filter(.data[[species_col]] == species_value) %>%
                mutate(Age = as.factor(Age))

            if (adjust_time) {
                result <- result %>% mutate(Time = Time - 1)
            }

            if (filter_time) {
                result <- result %>% filter(Time >= obj$startyear)
            }

            result
        }

        # Join fisheries model data with observed catch-at-age data
        join_with_caa <- function(model_data, species, value_col, obs_col = "catch") {
            obs_data <- obj$caa %>%
                filter(species == !!species) %>%
                rename(
                    obs_value = !!obs_col,
                    Functional_Group = species
                ) %>%
                mutate(Age = as.factor(Age))

            left_join(model_data, obs_data, by = c("Time", "Age", "Functional_Group"))
        }

        # Standard faceted fisheries plot
        plot_fisheries_faceted <- function(data, y_var, y_label,
                                           obs_var = NULL, scales = "free_y", ncol = 5) {
            req(data)
            req(nrow(data) > 0)

            p <- ggplot(data, aes(y = .data[[y_var]], x = Time)) +
                geom_line(linewidth = 1, alpha = .75)

            # Add observed data line if provided
            if (!is.null(obs_var) && obs_var %in% names(data)) {
                p <- p + geom_line(aes(y = .data[[obs_var]]), color = "grey")
            }

            p <- p +
                facet_wrap(. ~ Age, ncol = ncol, scales = scales) +
                scale_color_viridis(discrete = TRUE) +
                ylab(y_label) +
                theme_atlantis() +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .2))))) +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")

            p
        }

        # Aggregate fisheries plot (multiple groups)
        plot_fisheries_aggregate <- function(data, comparison_data = NULL,
                                             scales = "free", ncol = 5) {
            req(data)
            req(nrow(data) > 0)

            # Filter positive values
            data <- data %>% filter(Biomass > 0)

            p <- ggplot(data, aes(y = Biomass, x = Time)) +
                geom_line()

            # Add comparison data if provided
            if (!is.null(comparison_data)) {
                p <- p + geom_line(data = comparison_data, aes(y = biomass, x = Time), color = "grey")
            }

            p <- p +
                scale_x_continuous(breaks = round(as.numeric(quantile(data$Time, probs = seq(0, 1, .1))))) +
                facet_wrap(~Group, scales = scales, ncol = ncol) +
                theme_bw() +
                xlab("Time") +
                ylab("Landings (tons)")

            p
        }


        plot_fisheries_summary <- function(data, species, y_var, y_label,
                                           comparison_data = NULL, comparison_y = NULL,
                                           y_limits = NULL) {
            req(data)

            tmp <- data %>%
                filter(Functional_Group == species) %>%
                group_by(Time) %>%
                summarise(value = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")

            p <- ggplot(tmp, aes(y = value, x = Time)) +
                geom_line() +
                theme_bw() +
                ylab(y_label) +
                xlab("Time")

            # Add comparison line if provided
            if (!is.null(comparison_data) && !is.null(comparison_y)) {
                p <- p + geom_line(data = comparison_data, aes(y = .data[[comparison_y]]), color = "grey")
            }

            # Set y limits if provided
            if (!is.null(y_limits)) {
                p <- p + scale_y_continuous(limits = y_limits)
            }

            p
        }

        # Plot by age (line plot colored by age)
        plot_by_age_colored <- function(data, species, y_var, y_label) {
            req(data)

            tmp <- data %>%
                filter(Functional_Group == species) %>%
                mutate(Age = as.factor(Age))

            req(nrow(tmp) > 0)

            ggplot(tmp, aes(y = .data[[y_var]], x = Time,
                            group = factor(Age), color = factor(Age))) +
                geom_line(linewidth = 2, alpha = .75) +
                scale_color_viridis(discrete = TRUE, name = "Age") +
                ylab(y_label) +
                theme_atlantis() +
                scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")
        }

        # Plot aggregate by age (summary statistics)
        plot_age_summary <- function(data, species, agg_vars, y_var, x_var = "Age",
                                     y_label, x_label, type_var = NULL) {
            req(data)

            tmp <- data %>%
                filter(Functional_Group == species) %>%
                group_by(Age) %>%
                summarise(across(all_of(agg_vars), ~sum(.x, na.rm = TRUE)), .groups = "drop")

            if (!is.null(type_var)) {
                tmp <- tmp %>%
                    pivot_longer(cols = all_of(type_var), names_to = "type", values_to = y_var)

                p <- ggplot(tmp, aes(y = .data[[y_var]], x = .data[[x_var]],
                                     group = type, color = type)) +
                    geom_line() +
                    scale_colour_discrete(
                        name = " ",
                        breaks = type_var,
                        labels = str_replace(type_var, "Total_", "Total ")
                    )
            } else {
                p <- ggplot(tmp, aes(y = .data[[y_var]], x = .data[[x_var]])) +
                    geom_line()
            }

            p + theme_bw() +
                ylab(y_label) +
                xlab(x_label)
        }



        # ==============================================================================
        # FISHERIES PLOT OUTPUTS
        # ==============================================================================

        # Total landings across all groups
        output$fish_all <- renderPlot({
            har_summary <- obj$har %>%
                group_by(Time, Group) %>%
                summarise(biomass = sum(biomass), .groups = "drop")

            scales <- if (input$scale == "Free") "free" else "fixed"

            plot_fisheries_aggregate(obj$fish_biomass_year_l,
                                     comparison_data = har_summary,
                                     scales = scales)
        })

        # Total landings by group (single species)
        output$fish_marginal_map <- renderPlot({
            ofby <- obj$fish_biomass_year %>%
                mutate(Time = Time - 1) %>%
                filter(Time >= obj$startyear) %>%
                select(Time, value = all_of(input$fish_marginal))

            plot_timeseries(ofby, y_var = "value", y_label = "Landings (tons)",
                            x_breaks = round(as.numeric(quantile(ofby$Time, probs = seq(0, 1, .1)))))
        })

        # Landings (numbers) by age class
        output$fish_by_age_n <- renderPlot({
            tmp <- prepare_fish_data(obj$dis_df, "Functional_Group", input$fish_age_n) %>%
                select(Time, Functional_Group, Age, Land_numb) %>%
                join_with_caa(input$fish_age_n, "Land_numb", "catch")

            plot_fisheries_faceted(tmp, y_var = "Land_numb", y_label = "Landings (numbers)",
                                   obs_var = "obs_value")
        })

        # Landings (biomass) by age class
        output$fish_by_age_w <- renderPlot({
            tmp <- prepare_fish_data(obj$dis_df, "Functional_Group", input$fish_age) %>%
                select(Time, Functional_Group, Age, Land_weight)

            plot_fisheries_faceted(tmp, y_var = "Land_weight", y_label = "Landings (Tons)")
        })

        # Landings by age and box
        output$fish_by_age_w_box <- renderPlot({
            tmp <- obj$rboxageoutW %>%
                filter(species == input$fish_species_box, age == input$fish_age_box)

            ggplot(tmp, aes(x = Time, y = Wremovals)) +
                geom_line() +
                facet_wrap(~box) +
                theme_atlantis() +
                ylab("Landings (Tons)") +
                guides(fill = guide_legend(override.aes = list(colour = NULL))) +
                xlab("Year")
        })

        # Catch (biomass) by age - fixed scales
        output$catch_w <- renderPlot({
            tmp <- prepare_fish_data(obj$dis_df, "Functional_Group", input$fish_species) %>%
                select(Time, Functional_Group, Age, Catch_weight) %>%
                join_with_caa(input$fish_species, "Catch_weight", "biomassaa")

            plot_fisheries_faceted(tmp, y_var = "Catch_weight",
                                   y_label = "Catch (harvest + discards) (Tons)",
                                   obs_var = "obs_value", scales = "fixed")
        })

        # Catch (biomass) by age - free scales
        output$catch_wf <- renderPlot({
            tmp <- prepare_fish_data(obj$dis_df, "Functional_Group", input$fish_speciesf) %>%
                select(Time, Functional_Group, Age, Catch_weight) %>%
                join_with_caa(input$fish_speciesf, "Catch_weight", "biomassaa")

            plot_fisheries_faceted(tmp, y_var = "Catch_weight",
                                   y_label = "Catch (harvest + discards) (Tons)",
                                   obs_var = "obs_value")
        })

        # Catch (numbers) by age
        output$catch_n <- renderPlot({
            tmp <- prepare_fish_data(obj$dis_df, "Functional_Group", input$fish_speciesn) %>%
                select(Time, Functional_Group, Age, Catch_numb)

            tmpobs <- obj$caa %>%
                filter(species == input$fish_speciesn, Time <= obj$endyear) %>%
                rename(obscaa = catch, Functional_Group = species)

            # Aggregate oldest age class to match observations
            max_obs_age <- max(tmpobs$Age, na.rm = TRUE)

            tmp <- bind_rows(
                tmp %>% filter(Age < max_obs_age),
                tmp %>%
                filter(Age >= max_obs_age) %>%
                group_by(Time) %>%
                summarise(
                    Catch_numb = sum(Catch_numb),
                    Age = max_obs_age,
                    Functional_Group = first(Functional_Group),
                    .groups = "drop"
                )
            ) %>%
                arrange(Time, Age) %>%
                full_join(tmpobs, by = c("Time", "Age", "Functional_Group")) %>%
                replace_na(list(Catch_numb = 0, obscaa = 0))

            plot_fisheries_faceted(tmp, y_var = "Catch_numb",
                                   y_label = "Catch (harvest + discards) (N)",
                                   obs_var = "obscaa")
        })



        # Landings by fishery
        output$fish_fishery_map <- renderPlot({
            tmp <- obj$fish_fishery_l %>% filter(Fishery == input$fish_fishery)
            mri_har <- obj$har %>% filter(Fishery %in% input$fish_fishery)

            ggplot(tmp, aes(y = biomass, x = Time)) +
                geom_line() +
                geom_line(data = mri_har, aes(y = biomass, x = Time), color = "grey") +
                facet_wrap(~Group, ncol = 5, scales = "free_y") +
                scale_x_continuous(breaks = round(as.numeric(quantile(obj$fish_fishery_l$Time, probs = seq(0, 1, .2))))) +
                theme_bw() +
                xlab("Time") +
                ylab("Landings (tons)")
        })

        # Catch by box
        output$Catch_box <- renderPlot({
            tmp <- obj$totcatch %>%
                filter(.id == input$FishedGroups) %>%
                mutate(Box = factor(Box, levels = unique(Box)[order(as.numeric(str_remove(unique(Box), "Box ")))]))

            ggplot(tmp, aes(y = Catch, x = Time)) +
                facet_wrap(~Box, ncol = 5) +
                theme_bw() +
                geom_line(linewidth = 1) +
                ylab("Landings (tons)") +
                xlab("Year")
        })

        # Effort by fishery
        output$effort <- renderPlot({
            ggplot(obj$effort_l, aes(y = Effort, x = Time)) +
                geom_line() +
                facet_wrap(. ~ Fishery, ncol = 5, scales = "free") +
                theme_bw() +
                ylab("Effort (days)") +
                xlab("Year")
        })

        # ==============================================================================
        # DISCARD PLOTS
        # ==============================================================================

        # Total discards (weight)
        output$Total_discard_w <- renderPlot({
            plot_fisheries_summary(obj$dis_df, input$disc_Group, "Discard_weight",
                                   "Total discarded (tons)")
        })

        # Proportion discarded (weight)
        output$prop_disc_w <- renderPlot({
            tmp <- obj$dis_df %>%
                filter(Functional_Group == input$disc_Group) %>%
                group_by(Time) %>%
                summarise(
                    Total_Discard = sum(Discard_weight, na.rm = TRUE),
                    Total_Catch = sum(Catch_weight, na.rm = TRUE),
                    prop_disc = Total_Discard / (Total_Catch + 1e-06),
                    .groups = "drop"
                )

            ggplot(tmp, aes(y = prop_disc, x = Time)) +
                geom_line() +
                theme_bw() +
                ylab("Proportion discarded by weight") +
                xlab("Time") +
                scale_y_continuous(limits = c(0, 1))
        })

        # Total discards (numbers)
        output$Total_discard_numb <- renderPlot({
            plot_fisheries_summary(obj$dis_df, input$disc_Group, "Discard_numb",
                                   "Total discarded (numbers)")
        })

        # Proportion discarded (numbers)
        output$prop_disc_numb <- renderPlot({
            tmp <- obj$dis_df %>%
                filter(Functional_Group == input$disc_Group) %>%
                group_by(Time) %>%
                summarise(
                    Total_Discard = sum(Discard_numb, na.rm = TRUE),
                    Total_Catch = sum(Catch_numb, na.rm = TRUE),
                    prop_disc = Total_Discard / (Total_Catch + 1e-06),
                    .groups = "drop"
                )

            ggplot(tmp, aes(y = prop_disc, x = Time)) +
                geom_line() +
                theme_bw() +
                ylab("Proportion discarded by numbers") +
                xlab("Time") +
                scale_y_continuous(limits = c(0, 1))
        })

        # Discards by age
        output$disc_age <- renderPlot({
            plot_age_summary(obj$dis_df, input$disc_Group,
                             agg_vars = c("Total_Land" = "Land_numb", "Total_Catch" = "Catch_numb"),
                             y_var = "numbers", y_label = "Numbers", x_label = "Ageclass",
                             type_var = c("Total_Catch", "Total_Land"))
        })

        # Proportion discarded by age
        output$prop_age <- renderPlot({
            tmp <- obj$dis_df %>%
                filter(Functional_Group == input$disc_Group) %>%
                group_by(Age) %>%
                summarise(
                    Total_Discard = sum(Discard_numb, na.rm = TRUE),
                    Total_Catch = sum(Catch_numb, na.rm = TRUE),
                    prop_disc = Total_Discard / Total_Catch,
                    .groups = "drop"
                )

            ggplot(tmp, aes(y = prop_disc, x = Age)) +
                geom_line() +
                theme_bw() +
                ylab("Proportion discarded") +
                xlab("Ageclass")
        })

        # Discards by length
        output$disc_length <- renderPlot({
            tmp <- obj$dis_df %>%
                filter(Functional_Group == input$disc_Group) %>%
                group_by(Age) %>%
                summarise(
                    Total_Land = sum(Land_numb, na.rm = TRUE),
                    Total_Catch = sum(Catch_numb, na.rm = TRUE),
                    Length = mean(Length, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                pivot_longer(cols = c(Total_Land, Total_Catch),
                             names_to = "type", values_to = "numbers")

            ggplot(tmp, aes(y = numbers, x = Length, group = type, color = type)) +
                geom_line() +
                theme_bw() +
                ylab("Numbers") +
                xlab("Length (cm)") +
                scale_colour_discrete(
                    name = " ",
                    breaks = c("Total_Catch", "Total_Land"),
                    labels = c("Total catch", "Total landings")
                )
        })

        # Proportion discarded by length
        output$prop_length <- renderPlot({
            tmp <- obj$dis_df %>%
                filter(Functional_Group == input$disc_Group) %>%
                group_by(Age) %>%
                summarise(
                    Total_Discard = sum(Discard_numb, na.rm = TRUE),
                    Total_Catch = sum(Catch_numb, na.rm = TRUE),
                    Length = mean(Length, na.rm = TRUE),
                    prop_disc = Total_Discard / Total_Catch,
                    .groups = "drop"
                )

            ggplot(tmp, aes(y = prop_disc, x = Length)) +
                geom_line() +
                theme_bw() +
                ylab("Proportion discarded") +
                xlab("Length (cm)")
        })

        # Discards by age over time (numbers)
        output$discard_numb_age <- renderPlot({
            plot_by_age_colored(obj$dis_df, input$disc_Group, "Discard_numb",
                                "Total discarded (numbers)")
        })

        # Discards by age over time (weight)
        output$discard_weight_age <- renderPlot({
            plot_by_age_colored(obj$dis_df, input$disc_Group, "Discard_weight",
                                "Total discarded (tons)")
        })

        # Discards by group
        output$discard_group <- renderPlot({
            ggplot(obj$discard_total_l, aes(y = Discards, x = Time)) +
                facet_wrap(~Group, scales = "fixed", ncol = 5) +
                theme_bw() +
                geom_line(linewidth = 1, alpha = .75) +
                xlab("Year") +
                ylab("Discard (tons)")
        })

        # Discards by fishery
        output$discard_fishery <- renderPlot({
            tmp <- obj$discard_fishery_l %>% filter(Fishery == input$disc_fishery)

            ggplot(tmp, aes(y = Discards, x = Time)) +
                geom_line() +
                facet_wrap(~Group, scales = "fixed", ncol = 5) +
                theme_bw() +
                xlab("Year") +
                ylab("Discard (tons)") +
                scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2)))))
        })

        # Discards by group across fisheries
        output$discard_fishery_group <- renderPlot({
            tmp <- obj$discard_fishery_l %>% filter(Group == input$disc_group)

            ggplot(tmp, aes(y = Discards, x = Time)) +
                geom_line() +
                facet_wrap(~Fishery, scales = "fixed", ncol = 5) +
                theme_bw() +
                xlab("Year") +
                ylab("Discard (tons)")
        })
    }
  )
}
