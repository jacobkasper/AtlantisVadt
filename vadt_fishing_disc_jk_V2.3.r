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
            includeMarkdown("http://mareframe.github.io/vat_documentation/markdown/vat_diagnostic.md")
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
              "Without Migration ",
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
      # SPATIAL PLOTS
      # -----------------------------------------

      # output$fun_group_atl = renderDataTable({
      #   datatable(obj$fun_group, rownames = FALSE)
      # })
      #
      output$map_Numb <- renderPlot({
        tmp <- obj$dens[[input$disagg_var]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- min(tmp)
        tmp <- obj$dens[[input$disagg_var]][, input$time]

        # Plot islands with a different color
        if (is.character(obj$islands)) {
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)

        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() +
          xlab("") +
          ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), midpoint = tmp.mid, low = muted("white"), mid = "white", high = muted("blue")) +
          theme(legend.title = element_blank(), plot.background = element_blank()) +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = NULL)
      })


      # Plot Biomass per m2
      output$map_Bm2 <- renderPlot({
        tmp <- obj$dens_km2[[input$trace_sm]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- min(tmp)
        tmp <- obj$dens_km2[[input$trace_sm]][, input$trace_time]

        # Plot islands with a different color
        if (is.character(obj$islands)) {
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)

        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() +
          xlab("") +
          ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), midpoint = tmp.mid, low = muted("white"), mid = "white", high = muted("blue")) +
          theme(legend.title = element_blank(), plot.background = element_blank()) +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = NULL)
      })


      # Plot biomass per m3
      output$map_Bm3 <- renderPlot({
        tmp <- obj$dens_km3[[input$invert_sm]]
        tmp.min <- min(tmp)
        tmp.max <- max(tmp)
        tmp.mid <- min(tmp)
        tmp <- obj$dens_km3[[input$invert_sm]][, input$invert_time]

        # Plot islands with a different color
        if (is.character(obj$islands)) {
          islands <- as.numeric(obj$islands)
          tmp[islands + 1] <- NA
        }
        data_tmp <- data.frame(boxid = 0:(obj$numboxes - 1), tmp)

        unagg_map_data <- merge(obj$map_base, data_tmp)
        ggplot(data = unagg_map_data, aes(x = x, y = y)) +
          geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
          theme_bw() +
          xlab("") +
          ylab("") +
          scale_fill_gradient2(limits = c(tmp.min, tmp.max), midpoint = tmp.mid, low = muted("white"), mid = "white", high = muted("blue")) +
          theme(legend.title = element_blank(), plot.background = element_blank()) +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = NULL)
      })





      output$within_box_plot <- renderPlot({
        tmp <- subset(obj$biomass_by_box, Box == input$biomass_box_sel)
        ggplot(aes(y = round(value), x = Time), data = tmp) +
          geom_line() +
          facet_wrap(~Name, scales = "free", ncol = 5) +
          theme_bw() +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
          ylab("")
      })

      ## Erla's Vertebrate Number Plots
      output$vert_erla_plot <- renderPlot({
        tmp <- obj$erla_plots[[input$erla_plot_select]]
        tmp$Time <- as.numeric(as.character(tmp$Time)) * obj$toutinc / 365 + obj$startyear
        nrbox <- length(unique(tmp$Box))
        tmp <- within(tmp, Box <- factor(Box, levels = paste("Box", 0:(nrbox - 1)))) ## Order the graphs by box number
        if (is.null(tmp$Layer)) {
          ggplot(data = tmp, aes(y = number, x = Time)) +
            geom_line(linewidth = 1) +
            ylab("") +
            xlab("Year") +
            facet_wrap(~Box, ncol = 5) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
            theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2))
        } else {
          if (nlevels(tmp$Layer) == 7) {
            cbpalette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
            ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) +
              geom_line(linewidth = 1) +
              ylab("") +
              xlab("Year") +
              facet_wrap(~Box, ncol = 5) +
              scale_color_manual(values = cbpalette) +
              theme_bw() +
              guides(fill = guide_legend(override.aes = list(colour = NULL))) +
              scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
              theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2))
          } else {
            ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) +
              geom_line(linewidth = 1) +
              ylab("") +
              xlab("Year") +
              facet_wrap(~Box, ncol = 5) +
              theme_bw() +
              guides(fill = guide_legend(override.aes = list(colour = NULL))) +
              scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
              theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2))
          }
        }
      })

      ## Migration OFF
      output$vert_erla_plot_migOff <- renderPlot({
        tmp <- obj$erla_plots[[input$erla_plot_select_2]]
        tmp$number[tmp$Box == "Box 0"] <- 0
        tmp <- tmp[tmp$Time != 1, ]
        tmp$Time <- as.numeric(as.character(tmp$Time)) * obj$toutinc / 365 + obj$startyear
        nrbox <- length(unique(tmp$Box))
        tmp <- within(tmp, Box <- factor(Box, levels = paste("Box", 0:(nrbox - 1)))) ## Order the graphs by box number
        if (is.null(tmp$Layer)) {
          ggplot(data = tmp, aes(y = number, x = Time)) +
            geom_line(linewidth = 1) +
            ylab("") +
            xlab("Year") +
            facet_wrap(~Box, ncol = 5) + ## , scales = 'free') + ##jacob
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            )
        } else {
          if (nlevels(tmp$Layer) == 7) {
            cbpalette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#74add1", "#4575b4", "#313695")
            ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) +
              geom_line(linewidth = 1) +
              ylab("") +
              xlab("Year") +
              facet_wrap(~Box, ncol = 5) + ## , scales = 'free') +##jacob
              scale_color_manual(values = cbpalette) +
              theme_bw() +
              guides(fill = guide_legend(override.aes = list(colour = NULL))) +
              scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
              theme(
                panel.background = element_blank(), legend.key = element_rect(),
                legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_blank(), axis.line = element_line(linewidth = .2)
              )
          } else {
            ggplot(data = tmp, aes(y = number, x = Time, group = Layer, color = Layer)) +
              geom_line(linewidth = 1) +
              ylab("") +
              xlab("Year") +
              facet_wrap(~Box, ncol = 5) + ## , scales = 'free') +##jacob
              theme_bw() +
              guides(fill = guide_legend(override.aes = list(colour = NULL))) +
              scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
              theme(
                panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(),
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.line = element_line(linewidth = .2)
              )
          }
        }
      })




      output$agg_image <- renderImage(
        {
          filename <- normalizePath(file.path(
            anim,
            paste(input$aggbio, "-aggbio.gif", sep = "")
          ))
          list(src = filename)
        },
        deleteFile = FALSE
      )

      # -----------------------------------------
      # AGE DISAGGREGATED TAB
      # -----------------------------------------


      # Structural nitrogen
      output$structn <- renderPlot({
        sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        ggplot(data = dat_sn, aes(y = V1, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_sn$Time, probs = seq(0, 1, .2))))) +
          ylab("Structural Nitrogen (mg N)") +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      # Reserve nitrogen
      output$reserven <- renderPlot({
        rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "")
        dat_rn <- subset(obj$reserve, .id %in% rn_ids)
        ggplot(data = dat_rn, aes(y = V1, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Reserve Nitrogen (mg N)") +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +  jk 2/23
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)) +
          xlab("Year")
      })




      # Proportion Biomass
      output$propbio <- renderPlot({
        sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
        sn <- subset(obj$structN, .id %in% sn_ids)
        rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "") ## changed $rn to $sn ##here
        rn <- subset(obj$reserveN, .id %in% rn_ids)
        totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
        dat_tn <- subset(obj$totalnums, .id %in% totn_ids)
        dat_tn$V1 <- ((sn$V1 + rn$V1) * 5.7 * 20) / 1e3 * dat_tn$V1 / 1e6 ## rn$V1 is not working

        ggplot(data = dat_tn, aes(y = V1, x = Time)) +
          #          geom_line(aes(color = Ageclass), linewidth = 2, alpha = .75) +
          geom_density(stat = "identity", aes(fill = Ageclass), position = "fill", alpha = .75, linewidth = .2) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_tn$Time, probs = seq(0, 1, .2))))) +
          ylab("Proportion of total biomass") +
          scale_fill_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +  jk 2/23
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2, color = "black")
          ) +
          xlab("Year")
      })


      # Total Biomass
      output$totalbio <- renderPlot({
        sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
        sn <- subset(obj$structN, .id %in% sn_ids)
        rn_ids <- paste(input$sn, 1:16, "_ResN", sep = "") ## changed $rn to $sn ##here
        rn <- subset(obj$reserveN, .id %in% rn_ids)
        totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
        dat_tn <- subset(obj$totalnums, .id %in% totn_ids)
        dat_tn$V1 <- ((sn$V1 + rn$V1) * 5.7 * 20) / 1e3 * dat_tn$V1 / 1e6 ## rn$V1 is not working

        ggplot(dat_tn, aes(y = V1, x = Time, group = Ageclass, color = Ageclass)) +
          geom_line(linewidth = 2, alpha = .75) +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) + jk 2/23
          ylab("Total biomass (tons)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_tn$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })




      # Tot number
      output$totalnum <- renderPlot({
        totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time, group = Ageclass, color = Ageclass)) +
          geom_line(linewidth = 2, alpha = .75) +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) + jk 2/23
          ylab("Total numbers") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_totn$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })







      # Total Prop
      output$totalprop <- renderPlot({
        totn_ids <- paste(input$sn, 1:16, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time)) +
          geom_density(stat = "identity", aes(fill = Ageclass), position = "fill", alpha = .75, linewidth = .2) +
          scale_fill_viridis(discrete = TRUE) +
          ylab("Proportion of total numbers") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_totn$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      ## Length-At-Age plot
      output$lw_plot <- renderPlot({
        mfloor <- function(x, base) {
          base * floor(x / base)
        }
        mceiling <- function(x, base) {
          base * ceiling(x / base)
        }
        sn_ids <- paste(input$sn, 1:16, "_StructN", sep = "")
        lw_data <- subset(obj$structN, .id %in% sn_ids)
        lw_data$wt_grams <- 3.65 * lw_data$V1 * 5.7 * 20 / 1000
        fg_name <- obj$fun_group[str_trim(obj$fun_group$Name) == input$sn, 1]
        param_a <- obj$ab_params[grep(fg_name, obj$ab_params$a_name), 2]
        param_b <- obj$ab_params[grep(fg_name, obj$ab_params$b_name), 4]
        lw_data$length <- (lw_data$wt_grams / param_a)^(1 / param_b)

        ggplot(data = lw_data, aes(y = length, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(lw_data$Time, probs = seq(0, 1, .2))))) +
          ylab("Length-At-Age (cm) 1") +
          scale_y_continuous(breaks = seq(mfloor(min(lw_data$length), 5), mceiling(max(lw_data$length), 5), 5)) +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2, color = "black")) +
          xlab("Year")
      })


      output$wetwgt <- renderPlot({
        group <- input$sn
        sn_ids <- paste(group, 1:16, "_StructN", sep = "")
        rn_ids <- paste(group, 1:16, "_ResN", sep = "")
        dat_rn <- subset(obj$reserve, .id %in% rn_ids)
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        dat_rn$SN <- dat_sn$V1
        dat_rn$wetwgt <- (dat_rn$V1 + dat_rn$SN) * 20 * 5.7 / 1000

        ggplot(data = dat_rn, aes(y = wetwgt, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Wet weight (g)") +
          scale_color_viridis(discrete = TRUE) + ## scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2, color = "black")) +
          xlab("Year")
      })

      output$structng <- renderPlot({
        sn_ids <- paste(input$snwet, 1:16, "_StructN", sep = "")
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        ggplot(data = dat_sn, aes(y = (V1 * 5.7 * 20) / 100, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_sn$Time, probs = seq(0, 1, .2))))) +
          ylab("Wet Weight (g)") +
          scale_color_viridis(discrete = TRUE) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)) +
          xlab("Year")
      })

      # Reserve nitrogen
      output$reserveng <- renderPlot({
        rn_ids <- paste(input$snwet, 1:16, "_ResN", sep = "")
        dat_rn <- subset(obj$reserve, .id %in% rn_ids)
        ggplot(data = dat_rn, aes(y = (V1 * 5.7 * 20) / 100, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Wet Weight (g)") +
          scale_color_viridis(discrete = TRUE) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })


      # Total Biomass
      output$totalbiog <- renderPlot({
        sn_ids <- paste(input$snwet, 1:16, "_StructN", sep = "")
        sn <- subset(obj$structN, .id %in% sn_ids)
        rn_ids <- paste(input$snwet, 1:16, "_ResN", sep = "") ## jk 2/23 from "_ReserveN"
        rn <- subset(obj$reserveN, .id %in% rn_ids) ## changed from structN and sn_ids
        totn_ids <- paste(input$snwet, 1:16, "_Nums", sep = "")
        dat_tn <- subset(obj$totalnums, .id %in% totn_ids)
        dat_tn$V1 <- ((rn$V1 + sn$V1) * 5.7 * 20) / 1e3 * dat_tn$V1 / 1e6
        ggplot(data = dat_tn, aes(y = V1, x = Time)) +
          geom_line(aes(color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_tn$Time, probs = seq(0, 1, .2))))) +
          ylab("Total Biomass (Tons)") +
          scale_color_viridis(discrete = TRUE) + ##          scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2, color = "black")
          ) +
          xlab("Year")
      })



      # Tot number
      output$totalnumg <- renderPlot({
        totn_ids <- paste(input$snwet, 1:16, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time, group = .id, color = Ageclass)) +
          geom_line(linewidth = 2, alpha = .75) +
          scale_color_viridis(discrete = TRUE) + ##          scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          ylab("Total numbers") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_totn$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)) +
          xlab("Year")
      })


      # Total Prop
      output$totalpropg <- renderPlot({
        totn_ids <- paste(input$snwet, 1:16, "_Nums", sep = "")
        dat_totn <- subset(obj$totalnums, .id %in% totn_ids)
        ggplot(dat_totn, aes(y = V1, x = Time)) +
          geom_density(stat = "identity", aes(fill = Ageclass), position = "fill", alpha = .75, linewidth = .2) +
          scale_fill_viridis(discrete = TRUE) + ##          scale_fill_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          ylab("Proportion of total numbers") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_totn$Time, probs = seq(0, 1, .2))))) +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)) +
          xlab("Year")
      })

      ## Length-At-Age plot
      output$lw_plotg <- renderPlot({
        mfloor <- function(x, base) {
          base * floor(x / base)
        }
        mceiling <- function(x, base) {
          base * ceiling(x / base)
        }
        sn_ids <- paste(input$snwet, 1:16, "_StructN", sep = "")
        lw_data <- subset(obj$structN, .id %in% sn_ids)
        lw_data$wt_grams <- 3.65 * lw_data$V1 * 5.7 * 20 / 1000
        fg_name <- obj$fun_group[str_trim(obj$fun_group$Name) == input$snwet, 1]
        param_a <- obj$ab_params[grep(fg_name, obj$ab_params$a_name), 2]
        param_b <- obj$ab_params[grep(fg_name, obj$ab_params$b_name), 4]
        lw_data$length <- (lw_data$wt_grams / param_a)^(1 / param_b)

        ggplot(data = lw_data, aes(y = length, x = Time)) +
          geom_line(aes(group = Ageclass, color = Ageclass), linewidth = 2, alpha = .75) +
          scale_x_continuous(breaks = round(as.numeric(quantile(lw_data$Time, probs = seq(0, 1, .2))))) +
          scale_y_continuous(breaks = seq(mfloor(min(lw_data$length), 5), mceiling(max(lw_data$length), 5), 5)) +
          ylab("Length-At-Age (cm) 2") +
          scale_color_viridis(discrete = TRUE) + ##          scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2, color = "black")) +
          xlab("Year")
      })



      # Relative change in weight

      # Structural nitrogen
      output$structn_rel <- renderPlot({
        sn_ids <- paste(input$rel, 1:16, "_StructN", sep = "")
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        dat_sn$rel <- NA
        for (i in as.character(unique(dat_sn$.id))) {
          dat_sn$rel[dat_sn$.id == i] <- dat_sn$V1[dat_sn$.id == i] / dat_sn$V1[dat_sn$.id == i & dat_sn$X1 == 1]
        }
        ggplot(data = dat_sn, aes(y = rel, x = Time)) +
          geom_line() +
          facet_wrap(~Ageclass, scales = "fixed", ncol = 1) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_sn$Time, probs = seq(0, 1, .2))))) +
          ylab("Change in Structural weight") +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year") +
          geom_abline(slope = 0, intercept = 1.2, linetype = 3) +
          geom_abline(slope = 0, intercept = 0.8, linetype = 3)
      })

      # Reserve nitrogen
      output$reserven_rel <- renderPlot({
        rn_ids <- paste(input$rel, 1:16, "_ResN", sep = "")
        dat_rn <- subset(obj$reserveN, .id %in% rn_ids)
        dat_rn$rel <- NA
        for (i in as.character(unique(dat_rn$.id))) {
          dat_rn$rel[dat_rn$.id == i] <- dat_rn$V1[dat_rn$.id == i] / dat_rn$V1[dat_rn$.id == i & dat_rn$X1 == 1]
        }
        ggplot(data = dat_rn, aes(y = rel, x = Time)) +
          geom_line() +
          facet_wrap(~Ageclass, scales = "fixed", ncol = 1) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Change in Reserve weight") +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year") +
          geom_abline(slope = 0, intercept = 1.2, linetype = 3) +
          geom_abline(slope = 0, intercept = 0.8, linetype = 3)
      })


      # Reserve nitrogen relative
      output$rn_rel <- renderPlot({
        rn_ids <- paste(input$rel_rn, 1:16, "_ResN", sep = "")
        sn_ids <- paste(input$rel_rn, 1:16, "_StructN", sep = "")
        dat_rn <- subset(obj$reserveN, .id %in% rn_ids)
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        dat_rn$sn <- dat_sn$V1
        dat_rn$rel <- dat_rn$V1 / (2.65 * dat_rn$sn)
        ggplot(data = dat_rn, aes(y = rel, x = Time)) +
          geom_line() +
          facet_wrap(~Ageclass, scales = "fixed", ncol = 1) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Change in healthy weight") +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year") +
          geom_abline(slope = 0, intercept = 1, linetype = 3) +
          geom_abline(slope = 0, intercept = 0.72, linetype = 3)
      })


      # Partitioning of weight
      output$FRC <- renderPlot({
        rn_ids <- paste(input$rel_rn, 1:16, "_ResN", sep = "")
        sn_ids <- paste(input$rel_rn, 1:16, "_StructN", sep = "")
        dat_rn <- subset(obj$reserveN, .id %in% rn_ids)
        dat_sn <- subset(obj$structN, .id %in% sn_ids)
        dat_rn$sn <- dat_sn$V1
        dat_rn$pR <- as.numeric(as.character(obj$pR_parms[obj$pR_parms$Group == input$rel_rn, "pR"]))
        dat_rn$temp0 <- dat_rn$V1 / (2.65 * dat_rn$sn)
        dat_rn$temp1 <- (1 / 2.65) + dat_rn$pR * (dat_rn$temp0 - 1)
        dat_rn$temp1[dat_rn$temp1 < 0] <- 0
        dat_rn$temp2 <- (1 / 2.65) + dat_rn$V1 / (2.65 * dat_rn$sn)
        dat_rn$FRC <- dat_rn$temp1 / dat_rn$temp2
        ggplot(data = dat_rn, aes(y = FRC, x = Time)) +
          geom_line() +
          facet_wrap(~Ageclass, scales = "fixed", ncol = 1) +
          scale_x_continuous(breaks = round(as.numeric(quantile(dat_rn$Time, probs = seq(0, 1, .2))))) +
          ylab("Proportion into SN") +
          theme_bw() +
          guides(fill = guide_legend(override.aes = list(colour = NULL))) +
          theme(
            panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year") +
          geom_abline(slope = 0, intercept = 1, linetype = 3) +
          geom_abline(slope = 0, intercept = 0.72, linetype = 3)
      })


      ## NAA ###this should be corrected, right now hard coded for only cod, haddock and saithe having NAA
      output$NumAA <- renderPlot({
        naa_ids <- paste(input$snaa, 1:16, "_Nums", sep = "")
        naa_totn <- subset(obj$totalnums, .id %in% naa_ids)
        naa.t <- subset(obj$naa, .id %in% naa_ids)
        naa.t.null <- tibble(
          Time = NA,
          species = rep(input$snaa, max(naa_totn$Age)),
          Age = 1:max(naa_totn$Age),
          numbers = NA,
          .id = paste0(input$snaa, 1:max(naa_totn$Age), "_Nums"),
          Ageclass = paste0("Ageclass ", 1:max(naa_totn$Age))
        )

        if (dim(naa.t)[1] == 0) {
          naa_totn
          tmp <-
            left_join(expand(bind_rows(naa_totn, naa.t.null), Age, Time), naa_totn) %>%
            filter(!is.na(Time)) %>%
            mutate(Ageclass = ifelse(!is.na(Ageclass), Ageclass, paste0("Ageclass ", Age)))
          naa_totn <- bind_rows(
            naa_totn %>%
              filter(Age < max(tmp$Age)),
            naa_totn %>%
              filter(Age >= max(tmp$Age)) %>%
              group_by(Time) %>%
              dplyr::mutate(V1 = sum(V1)) %>%
              filter(Age == max(tmp$Age))
          )

          ggplot(naa_totn, aes(y = V1, x = Time)) +
            geom_line(linewidth = 1, alpha = .75) +
            facet_wrap(. ~ as.factor(Ageclass), scales = "free_y", ncol = 2) +
            ylab("NAA") +
            theme_bw() +
            scale_x_continuous(
              breaks =
                round(as.numeric(quantile(naa_totn$Time,
                  probs = seq(0, 1, .2)
                )))
            ) +
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
        } else {
          naa.t <-
            left_join(expand(bind_rows(naa.t, naa.t.null), Age, Time), naa.t) %>%
            filter(!is.na(Time)) %>%
            mutate(Ageclass = ifelse(!is.na(Ageclass), Ageclass, paste0("Ageclass ", Age)))
          naa_totn <- bind_rows(
            naa_totn %>%
              filter(Age < max(naa.t$Age)),
            naa_totn %>%
              filter(Age >= max(naa.t$Age)) %>%
              group_by(Time) %>%
              dplyr::mutate(V1 = sum(V1)) %>%
              filter(Age == max(naa.t$Age))
          ) %>%
            arrange(Time, Age)
          naa.t <- if(input$snaa == 'Capelin') {naa.t} else{
            naa.t %>%
              filter(!is.na(species)) %>%
              arrange(Time, Age)
          }
          
          naa_totn <-
            bind_rows(
              naa_totn %>%
                select(-X1) %>%
                filter(Age < range(naa.t$Age)[2]),
              naa_totn %>%
                select(-X1) %>%
                filter(Age >= range(naa.t$Age)[2]) %>%
                group_by(Time) %>%
                dplyr::mutate(V1 = sum(V1)) %>%
                filter(Age == range(naa.t$Age)[2])
            ) %>%
            arrange(Time, Age)

          ggplot(naa_totn, aes(y = V1, x = Time)) +
            geom_line(linewidth = 1, alpha = .75) +
            geom_line(aes(y = numbers, x = Time), data = naa.t, color = "grey") +
            facet_wrap(. ~ as.factor(Ageclass), ncol = 2) + ##scales = "free_y",
            ylab("NAA") +
            theme_bw() +
            scale_x_continuous(
              breaks =
                round(as.numeric(quantile(naa_totn$Time,
                  probs = seq(0, 1, .2)
                )))
            ) +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(),
              legend.background = element_blank(),
              #panel.grid.major = element_blank(),
              #panel.grid.minor = element_blank(),
              #panel.border = element_blank(),
              axis.line = element_line(linewidth = .2)
            ) +
            xlab("Year")
        } # naa.t <-
        #   left_join(expand(bind_rows(naa.t, naa.t.null), Age, Time), naa.t) %>%
        #   filter(!is.na(Time)) %>%
        #   mutate(Ageclass = ifelse(!is.na(Ageclass), Ageclass, paste0('Ageclass ', Age)))
        # naa_totn <- bind_rows(naa_totn %>%
        #                         filter(Age < max(naa.t$Age)),
        #                       naa_totn %>%
        #                         filter(Age >= max(naa.t$Age)) %>%
        #                         group_by(Time) %>%
        #                         dplyr::mutate(V1 = sum(V1)) %>%
        #                         filter(Age == max(naa.t$Age)))
        #
        # ggplot(naa_totn, aes(y = V1, x = Time)) +
        #   geom_line(linewidth = 1, alpha = .75) +
        #   geom_line(aes(y = numbers, x = Time), data = naa.t, color = "grey") +
        #   facet_wrap(. ~ as.factor(Ageclass), scales = "free_y", ncol = 2) +
        #   ylab("NAA") +
        #   theme_bw() +
        #   scale_x_continuous(breaks =
        #                        round(as.numeric(quantile(naa_totn$Time,
        #                                                  probs = seq(0, 1, .2))))) +
        #   guides(fill = guide_legend(override.aes = list(colour = NULL))) +
        #   theme(panel.background = element_blank(),
        #         legend.background = element_blank(),
        #         panel.grid.major = element_blank(),
        #         panel.grid.minor = element_blank(),
        #         panel.border = element_blank(),
        #         axis.line = element_line(linewidth = .2)) +
        #   xlab("Year")
      })


      # DIET DATA TAB
      # -----------------------------------------
      # Diet Predator by prey
      output$diet_pprey <- renderPlot({
        data_dpp <- subset(obj$diet_l, Predator == input$diet_dispred & Prey == input$diet_disprey)
        if (any(names(data_dpp) == "Habitat")) {
          ggplot(data = data_dpp, aes(x = Time, y = eaten, color = as.factor(Habitat))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Habitat") +
            xlab("Year") +
            ylab("?") +
            scale_x_continuous(breaks = round(as.numeric(quantile(data_dpp$Time, probs = seq(0, 1, .2))))) +
            ggtitle(paste("Diet of ", data_dpp[1, "Prey"], " by ", data_dpp[1, 1], " by Habitat Type", sep = "")) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2))
        } else {
          ggplot(data = data_dpp, aes(x = Time, y = eaten, color = as.factor(Cohort))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Cohort") +
            xlab("Year") +
            ylab("Proportion of Diet") +
            scale_x_continuous(breaks = round(as.numeric(quantile(data_dpp$Time, probs = seq(0, 1, .2))))) +
            ggtitle(paste("Diet of ", data_dpp[1, 5], " by ", data_dpp[1, 1], " by Age Class", sep = "")) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2))
        }
      })



      # Diet by Predator
      output$diet_pred_plot <- renderPlot({
        predConsume <- subset(obj$diet_l, Predator == input$diet_pred_unagg)
        if (any(names(predConsume) == "Habitat")) {
          ggplot(data = predConsume[predConsume$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Habitat))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Habitat") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(predConsume$Time, probs = seq(0, 1, .2))))) +
            ylab("?") +
            ggtitle(paste("Diet of ", predConsume[[1]][1], " by Habitat", sep = "")) +
            facet_wrap(~Prey) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            )
        } else {
          ggplot(data = predConsume[predConsume$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Cohort))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Cohort") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(predConsume$Time, probs = seq(0, 1, .2))))) +
            ylab("Proportion of Diet") +
            ggtitle(paste("Diet of ", predConsume$Predator[1], " by Age Class", sep = "")) +
            facet_wrap(~Prey) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            )
        }
      })




      # Diet by Predator free JMK
      output$diet_pred_plot_free <- renderPlot({
        predConsume <- subset(obj$diet_l, Predator == input$diet_pred_unaggfree)
        if (any(names(predConsume) == "Habitat")) {
          ggplot(data = predConsume[predConsume$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Habitat))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Habitat") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(predConsume$Time, probs = seq(0, 1, .2))))) +
            ylab("?") +
            ggtitle(paste("Diet of ", predConsume[[1]][1], " by Habitat", sep = "")) +
            facet_wrap(~Prey, scales = "free") +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            )
        } else {
          ggplot(data = predConsume[predConsume$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Cohort))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Cohort") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(predConsume$Time, probs = seq(0, 1, .2))))) +
            ylab("Proportion of Diet") +
            ggtitle(paste("Diet of ", predConsume$Predator[1], " by Age Class", sep = "")) +
            facet_wrap(~Prey, scales = "free") +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            )
        }
      })



      # Diet by Prey
      output$diet_prey_plot <- renderPlot({
        data_dpp <- subset(obj$diet_l, Prey == input$diet_prey_unagg)
        if (any(names(data_dpp) == "Habitat")) {
          ggplot(data = data_dpp[data_dpp$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Habitat))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Habitat") +
            xlab("Year") +
            ylab("?") +
            ggtitle(paste(data_dpp$Prey[1], " in Diet of Others by Habitat Type", sep = "")) + ## data_dpp[[1]][4]
            facet_wrap(~Predator) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            ) +
            scale_x_continuous(breaks = round(as.numeric(quantile(data_dpp$Time, probs = seq(0, 1, .2)))))
        } else {
          ggplot(data = data_dpp[data_dpp$eaten > 0, ], aes(x = Time, y = eaten, color = as.factor(Cohort))) +
            geom_line(linewidth = 1, alpha = .75) +
            scale_color_viridis(discrete = TRUE, name = "Cohort") +
            xlab("Year") +
            ylab("Proportion of Diet") +
            ggtitle(paste(data_dpp$Prey[1], " in Diet of Others by Age Class", sep = "")) + ## data_dpp[[1]][4]
            facet_wrap(~Predator) +
            theme_bw() +
            guides(fill = guide_legend(override.aes = list(colour = NULL))) +
            theme(
              panel.background = element_blank(), legend.key = element_rect(), legend.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(linewidth = .2)
            ) +
            scale_x_continuous(breaks = round(as.numeric(quantile(data_dpp$Time, probs = seq(0, 1, .2)))))
        }
      })



      # -----------------------------------------
      # SUMMARIES TAB
      # -----------------------------------------

      ## Total biomass trellis
      bm_mri <-
        as_tibble(obj$bms) %>%
        pivot_longer(!Time, names_to = "Name", values_to = "value")

      output$tot_vert_sum <- renderPlot({
        if (input$tot_vert_scale == "Fixed") {
          ggplot(aes(x = Time, y = value), data = obj$tot_bio_v) +
            geom_line() +
            facet_wrap(~Name, ncol = 4, scales = "fixed") +
            xlab("Year") +
            ylab("Total Biomass") +
            theme_bw() +
            expand_limits(y = 0) +
            scale_x_continuous(breaks = seq(1948, obj$endyear, 10))
          # scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio_v$Time, probs = seq(0, 1, .2)))))
        } else {
          ggplot(aes(x = Time, y = value), data = obj$tot_bio_v) +
            geom_line() +
            facet_wrap(~Name, ncol = 4, scales = "free") +
            xlab("Year") +
            expand_limits(y = 0) +
            ylab("Total Biomass") +
            theme_bw() +
            geom_line(aes(y = value, x = Time),
              data = bm_mri, color = "grey"
            ) +
            scale_x_continuous(breaks = seq(1948, obj$endyear, 10))
          # scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio_v$Time, probs = seq(0, 1, .2)))))
        }
      })

      output$tot_invert_sum <- renderPlot({
        if (input$tot_invert_scale == "Fixed") {
          ggplot(aes(x = Time, y = value), data = obj$tot_bio_i) +
            geom_line() +
            facet_wrap(~Name, ncol = 4, scales = "fixed") +
            xlab("Year") +
            ylab("Total Biomass") +
            theme_bw() +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio_i$Time, probs = seq(0, 1, .2)))))
        } else {
          ggplot(aes(x = Time, y = value), data = obj$tot_bio_i) +
            geom_line() +
            facet_wrap(~Name, ncol = 4, scales = "free") +
            xlab("Year") +
            ylab("Total Biomass") +
            theme_bw() +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio_i$Time, probs = seq(0, 1, .2)))))
        }
      })

      # harvest biomass map
      output$harvest <- renderPlot({
        group <- input$ssb_var
        landings <- obj$fish_biomass_year[, c("Time", group)]
        names(landings) <- c("Time", "Landings")
        #landings <- landings[2:nrow(landings), ]
        landings$Time <- round(landings$Time)
        landings$Landings <- landings$Landings[c(2:nrow(landings), NA) ]
        landings <- landings[1:(nrow(landings)-1), ]
        tmp <- as_tibble(obj$har[obj$har$Group == input$ssb_var, ]) %>%
          group_by(Time) %>%
          dplyr::summarize(biomass = sum(biomass))
        ggplot(aes(y = Landings, x = Time), data = landings) +
          geom_line() +
          geom_point(shape = 21, size = 0.5) +
          expand_limits(y = 0) +
          geom_line(
            data = tmp, aes(y = biomass, x = Time), color = "grey"
          ) +
          geom_point(data = tmp, aes(y = biomass, x = Time), color = "grey", shape = 21, size = 0.5) +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio$Time, probs = seq(0, 1, .1))))) +
          ylab("Landings (tons)") +
          xlab("Year") +
          geom_vline(xintercept = 2000, linetype =  3)
      })



      # Total biomass map
      output$tot_map <-
        renderPlot({
          bms.mis <- setdiff(names(obj$tot_bio), names(obj$bms))
          bms.mis <- bms.mis[!is.na(bms.mis)]
          obj$bms[bms.mis] <- 0
          ggplot(aes(y = obj$tot_bio[[match(input$ssb_var, names(obj$tot_bio))]], x = Time), data = obj$tot_bio) +
            geom_line() +
            geom_line(aes(y = obj$bms[[match(input$ssb_var, names(obj$bms))]], x = Time), data = obj$bms, color = "grey") +
            ylab("") +
            theme_bw() +
            ggtitle("Total Biomass") +
            expand_limits(y = 0) +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$tot_bio$Time, probs = seq(0, 1, .2))))) +
            geom_vline(xintercept = 2000, linetype =  3)
        })



      # SSB map
      output$ssb_map <- renderPlot({
        tmp <- obj$ssb[, c("Time", input$ssb_var)]
        ssb.mis <- setdiff(names(obj$ssb), names(obj$ssbmri))
        obj$ssbmri[ssb.mis] <- 0
        names(tmp) <- c("Time", "Group")
        stTime <- tmp$Time[1]
        tmp <- tmp[tmp$Time >= (stTime + 1), ]
        if(is.null(eval(parse(text = paste0('obj$ssbmri$', input$ssb_var, 'l'))))){
          ggplot(aes(y = Group, x = Time), data = tmp) +
            geom_line() +
            geom_line(aes(y = obj$ssbmri[[match(input$ssb_var, names(obj$ssbmri))]], x = Time), data = obj$ssbmri, color = "grey") +
            expand_limits(y = 0) +
            ylab("") +
            theme_bw() +
            ggtitle("Spawning Stock Biomass") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$ssb$Time, probs = seq(0, 1, .2), na.rm = TRUE))))+
            geom_vline(xintercept = 2000, linetype =  3) 
        } else {
          ggplot(aes(y = Group, x = Time), data = tmp) +
            geom_line() +
            geom_line(aes(y = obj$ssbmri[[match(input$ssb_var, names(obj$ssbmri))]], x = Time), data = obj$ssbmri, color = "grey") +
            geom_ribbon(aes(x = Time,
                            ymin = eval(parse(text = paste0('obj$ssbmri$', input$ssb_var, 'l'))), 
                            ymax = eval(parse(text = paste0('obj$ssbmri$', input$ssb_var, 'h')))
            ), 
            data = obj$ssbmri,
            inherit.aes = FALSE, 
            fill = 'blue',
            alpha=0.1) +
            expand_limits(y = 0) +
            ylab("") +
            theme_bw() +
            ggtitle("Spawning Stock Biomass") +
            xlab("Year") +
            scale_x_continuous(breaks = round(as.numeric(quantile(obj$ssb$Time, probs = seq(0, 1, .2), na.rm = TRUE))))+
            geom_vline(xintercept = 2000, linetype =  3) 
        }  
        # ggplot(aes(y = Group, x = Time), data = tmp) +
        #   geom_line() +
        #   geom_line(aes(y = obj$ssbmri[[match(input$ssb_var, names(obj$ssbmri))]], x = Time), data = obj$ssbmri, color = "grey") +
        #   expand_limits(y = 0) +
        #   ylab("") +
        #   theme_bw() +
        #   ggtitle("Spawning Stock Biomass") +
        #   xlab("Year") +
        #   scale_x_continuous(breaks = round(as.numeric(quantile(obj$ssb$Time, probs = seq(0, 1, .2), na.rm = TRUE))))+
        #   geom_vline(xintercept = 2000, linetype =  3)
        })

      # YOY map
      output$yoy_map <- renderPlot({
        tmp <- obj$yoy_nums[, c("Time", input$ssb_var)]
        names(tmp) <- c("Time", "Group")
        stTime <- tmp$Time[1]
        tmp <- tmp[tmp$Time >= (stTime + 1), ]
        ggplot(aes(y = Group, x = Time), data = tmp) +
          ylab("") +
          geom_line() +
          expand_limits(y = 0) +
          theme_bw() +
          ggtitle("Number of recruits") +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(obj$yoy_nums$Time, probs = seq(0, 1, .2), na.rm = TRUE))))
      })

      ## Numbers map
      output$numbers_map <- renderPlot({
        n.map <-
          as_tibble(obj$totalnums) %>%
          mutate(.id = gsub("[[:digit:]]+", "", .id)) %>%
          mutate(
            species = gsub("_Nums", "", .id),
            numbers = V1
          ) %>%
          select(species, Time, numbers) %>%
          group_by(species, Time) %>%
          dplyr::mutate(n = sum(numbers)) %>%
          select(Time, species, n) %>%
          distinct() %>%
          filter(species == input$ssb_var)
        tmp <- obj$ntot[, c("Time", input$ssb_var)]
        ntot.miss <- setdiff(names(unique(obj$n.map$species)), names(obj$ntot))
        obj$ntot[ntot.miss] <- 0

        ggplot(aes(y = n, x = Time), data = n.map[n.map$species == input$ssb_var, ]) +
          ylab("Numbers") +
          geom_line() +
          expand_limits(y = 0) +
          theme_bw() +
          geom_point(aes(
            y = obj$ntot[[match(input$ssb_var, names(obj$ntot))]],
            x = Time
          ), data = obj$ntot, color = "darkgrey", size = 3) +
          ggtitle("Abundance") +
          xlab("Year")
      })

      # Invertebrate rel plots
      output$invert_rbio <- renderPlot({
        invert_rbio <- obj$rel_bio[, c(1, grep(input$invert_var, names(obj$rel_bio)))] %>%
          mutate(Biomass = rowSums(select(., -Time))) %>%
          select(Time, Biomass) %>%
          mutate(Biomass = Biomass / Biomass[1])
        ggplot(aes(y = Biomass, x = Time), data = invert_rbio) +
          geom_line() +
          ylab("") +
          theme_bw() +
          ggtitle("Relative Biomass") +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(invert_rbio$Time, probs = seq(0, 1, .2)))))
      })

      output$invert_tbio <- renderPlot({
        invert_tbio <- obj$tot_bio[, c(1, grep(input$invert_var, names(obj$rel_bio)))] %>%
          mutate(Biomass = rowSums(select(., -Time))) %>%
          select(Time, Biomass)
        #        colnames(invert_tbio) <- c("Time", "Biomass")
        ggplot(aes(y = Biomass, x = Time), data = invert_tbio) +
          geom_line() +
          ylab("") +
          theme_bw() +
          ggtitle("Total Biomass") +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(invert_tbio$Time, probs = seq(0, 1, .2)))))
      })

      output$invertgraze <- renderPlot({
        graze_dat <- obj$invert_l[grep(input$invert_var, obj$invert_l$id), ]
        graze_dat <-
          graze_dat[grep("Grazing", graze_dat$id), ] %>%
          group_by(variable) %>%
          dplyr::mutate(value = sum(value)) %>%
          select(-id) %>%
          distinct()
        ggplot(aes(y = value, x = as.numeric(as.character(variable))), data = graze_dat) +
          geom_line() +
          xlab("Time") +
          ylab("") +
          theme_bw() +
          ggtitle("Grazing") +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(as.numeric(as.character(graze_dat$variable)), probs = seq(0, 1, .2)))))
      })

      output$invertprod <- renderPlot({
        prod_dat <- obj$invert_l[grep(input$invert_var, obj$invert_l$id), ]
        prod_dat <-
          prod_dat[grep("Prodn", prod_dat$id), ] %>%
          group_by(variable) %>%
          dplyr::mutate(value = sum(value)) %>%
          select(-id) %>%
          distinct()
        ggplot(aes(y = value, x = as.numeric(as.character(variable))), data = prod_dat) +
          geom_line() +
          xlab("Time") +
          ylab("") +
          theme_bw() +
          ggtitle("Production") +
          xlab("Year") +
          scale_x_continuous(breaks = round(as.numeric(quantile(as.numeric(as.character(prod_dat$variable)), probs = seq(0, 1, .2)))))
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
        ofby1 <- 
          tibble(obj$fish_biomass_year_l) %>% 
          mutate(Time = Time-1) %>% 
          filter(Time >= obj$startyear)
        
        if (input$scale == "Free") {
          ggplot(data = ofby1[ofby1$Biomass > 0, ], aes(y = Biomass, x = Time)) +
            geom_line() +
            geom_line(data = tmp, aes(y = biomass, x = Time), color = "grey") +
            scale_x_continuous(breaks = round(as.numeric(quantile(ofby1$Time, probs = seq(0, 1, .1))))) +
            facet_wrap(~Group, scales = "free", ncol = 5) +
            theme_bw() +
            xlab("Time") +
            ylab("Landings (tons)")
        } else {
          ggplot(data = ofby1[ofby1$Biomass > 0, ], aes(y = Biomass, x = Time)) +
            geom_line() +
            geom_line(data = tmp, aes(y = biomass, x = Time), color = "grey") +
            scale_x_continuous(breaks = round(as.numeric(quantile(ofby1$Time, probs = seq(0, 1, .1))))) +
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
        #     tmp <- obj$dis_df[obj$dis_df$Functional_Group == input$fish_age_n, ]
        #     tmp$Age <- as.factor(tmp$Age)
        #     ggplot(data = tmp, aes(y = Land_numb, x = Time)) +
        #         geom_line(linewidth = 1, alpha = .75) +
        #         facet_wrap(.~Age, ncol = 5, scales = "free_y") +
        #         scale_color_viridis(discrete = TRUE) +
        #         ylab("Landings (numbers)") +
        #         theme_bw() +
        #         scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
        #         guides(fill = guide_legend(override.aes = list(colour = NULL))) +
        #         theme(panel.background = element_blank(), legend.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #               panel.border = element_blank(), axis.line = element_line(linewidth = .2)) +
        #         xlab("Year")
        #
  tmp <-
        left_join(
            as_tibble(obj$dis_df[obj$dis_df$Functional_Group == input$fish_age_n, ]) %>%
              mutate(Age = as.factor(Age)) %>%
              select(Time, Functional_Group, Age, Land_numb) %>% 
          mutate(Time = Time-1) %>% 
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
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(),
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
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(),
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
          ## scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
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
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(),
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
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(),
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
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            axis.line = element_line(linewidth = .2)
          ) +
          xlab("Year")
      })

      # Catch by Fishery


      output$fish_fishery_map <- renderPlot({
        tmp <- subset(obj$fish_fishery_l, Fishery == input$fish_fishery)
        mri.har <- subset(obj$har, Fishery %in% input$fish_fishery)
        ggplot(data = tmp, aes(y = biomass, x = Time)) + ## [tmp$biomass > 0, ]
          geom_line() +
          geom_line(data = mri.har, aes(y = biomass, x = Time), color = "grey") +
          facet_wrap(~Group, ncol = 5, scales = "free_y") +
          scale_x_continuous(breaks = round(as.numeric(quantile(obj$fish_fishery_l$Time, probs = seq(0, 1, .2))))) +
          theme_bw() +
          xlab("Time") +
          ylab("Landings (tons)")
      })

      output$Catch_box <- renderPlot({
        tmp <- obj$totcatch[obj$totcatch$".id" == input$FishedGroups, ]
        nrbox <- length(unique(tmp$Box))
        tmp <- within(tmp, Box <- factor(Box, levels = paste("Box", 0:(nrbox - 1)))) ## Order the graphs by box number
        ggplot(data = tmp, aes(y = Catch, x = Time)) + ## [tmp$Catch > 0, ]
          facet_wrap(~Box, ncol = 5) +
          theme_bw() +
          geom_line(linewidth = 1) +
          ylab("Landings (tons)") +
          xlab("Year")
      })


      output$effort <- renderPlot({
        ggplot(data = obj$effort_l, aes(y = Effort, x = Time)) +
          geom_line() +
          facet_wrap(. ~ Fishery, ncol = 5, scales = "free") +
          theme_bw() +
          ylab("Effort (days)") +
          xlab("Year")
      })

      # Discards plots

      output$Total_discard_w <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Time) %>%
          dplyr::summarise(Total_Discard = sum(Discard_weight))
        ggplot(data = tmp, aes(y = Total_Discard, x = Time)) +
          geom_line() +
          theme_bw() +
          ylab("Total discarded (tons)") +
          xlab("Time")
      })

      output$prop_disc_w <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Time) %>%
          dplyr::summarise(
            Total_Discard = sum(Discard_weight),
            Total_Catch = sum(Catch_weight),
            prop_disc = Total_Discard / (Total_Catch + 1e-06)
          )
        ggplot(data = tmp, aes(y = prop_disc, x = Time)) +
          geom_line() +
          theme_bw() +
          ylab("Proportion discarded by weight") +
          xlab("Time") +
          scale_y_continuous(limits = c(0, 1))
      })

      output$Total_discard_numb <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Time) %>%
          dplyr::summarise(Total_Discard = sum(Discard_numb))
        ggplot(data = tmp, aes(y = Total_Discard, x = Time)) +
          geom_line() +
          theme_bw() +
          ylab("Total discarded (numbers)") +
          xlab("Time")
      })

      output$prop_disc_numb <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Time) %>%
          dplyr::summarise(
            Total_Discard = sum(Discard_numb),
            Total_Catch = sum(Catch_numb),
            prop_disc = Total_Discard / (Total_Catch + 1e-06)
          )
        ggplot(data = tmp, aes(y = prop_disc, x = Time)) +
          geom_line() +
          theme_bw() +
          ylab("Proportion discarded by numbers") +
          xlab("Time") +
          scale_y_continuous(limits = c(0, 1))
      })

      output$disc_age <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Age) %>%
          dplyr::summarise(
            Total_Land = sum(Land_numb),
            Total_Catch = sum(Catch_numb)
          )
        tmp_l <- gather(tmp, "type", "numbers", 2:3)
        ggplot(data = tmp_l, aes(
          y = numbers, x = Age,
          group = type, color = type
        )) +
          geom_line() +
          theme_bw() +
          ylab("Numbers") +
          xlab("Ageclass") +
          scale_colour_discrete(
            name = " ",
            breaks = c("Total_Catch", "Total_Land"),
            labels = c("Total catch", "Total landings")
          )
      })


      output$prop_age <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Age) %>%
          dplyr::summarise(
            Total_Discard = sum(Discard_numb),
            Total_Catch = sum(Catch_numb),
            prop_disc = Total_Discard / Total_Catch
          )
        ggplot(data = tmp, aes(y = prop_disc, x = Age)) +
          geom_line() +
          theme_bw() +
          ylab("Proportion discarded") +
          xlab("Ageclass")
      })


      output$disc_length <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Age) %>%
          dplyr::summarise(
            Total_Land = sum(Land_numb),
            Total_Catch = sum(Catch_numb),
            Length = mean(Length)
          )
        tmp_l <- gather(tmp, "type", "numbers", 2:3)
        ggplot(data = tmp_l, aes(
          y = numbers,
          x = Length, group = type, color = type
        )) +
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


      output$prop_length <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          group_by(Age) %>%
          dplyr::summarise(
            Total_Discard = sum(Discard_numb),
            Total_Catch = sum(Catch_numb),
            Length = mean(Length),
            prop_disc = Total_Discard / Total_Catch
          )
        ggplot(data = tmp, aes(y = prop_disc, x = Length)) +
          geom_line() +
          theme_bw() +
          ylab("Proportion discarded") +
          xlab("Length (cm)")
      })



      output$discard_numb_age <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          mutate(age = as.factor(Age))
        ggplot(data = tmp, aes(
          y = Discard_numb, x = Time, group = factor(Age),
          color = factor(Age)
        )) +
          geom_line(linewidth = 2, alpha = .75) +
          scale_color_viridis(discrete = TRUE) + # scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          ylab("Total discarded (numbers)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
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


      output$discard_weight_age <- renderPlot({
        tmp <- obj$dis_df %>%
          dplyr::filter(Functional_Group == input$disc_Group) %>%
          mutate(age = as.factor(Age))
        ggplot(data = tmp, aes(
          y = Discard_weight,
          x = Time,
          group = factor(Age),
          color = factor(Age)
        )) +
          geom_line(linewidth = 2, alpha = .75) +
          scale_color_viridis(discrete = TRUE) + # scale_color_brewer(name = "Ageclass", type = "div", palette = 5, labels = 1:16) +
          ylab("Total discarded (tons)") +
          theme_bw() +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2))))) +
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


      output$discard_group <- renderPlot({
        ggplot(data = obj$discard_total_l, aes(y = Discards, x = Time)) +
          facet_wrap(~Group, scales = "fixed", ncol = 5) +
          theme_bw() +
          geom_line(linewidth = 1, alpha = .75) +
          xlab("Year") +
          ylab("Discard (tons)")
      })

      output$discard_fishery <- renderPlot({
        tmp <- obj$discard_fishery_l %>%
          dplyr::filter(Fishery == input$disc_fishery)
        ggplot(aes(y = Discards, x = Time), data = tmp) +
          geom_line() +
          facet_wrap(~Group, scales = "fixed", ncol = 5) +
          theme_bw() +
          xlab("Year") +
          ylab("Discard (tons)") +
          scale_x_continuous(breaks = round(as.numeric(quantile(tmp$Time, probs = seq(0, 1, .2)))))
      })

      output$discard_fishery_group <- renderPlot({
        tmp <- obj$discard_fishery_l %>%
          dplyr::filter(Group == input$disc_group)
        ggplot(aes(y = Discards, x = Time), data = tmp) +
          geom_line() +
          facet_wrap(~Fishery, scales = "fixed", ncol = 5) +
          theme_bw() +
          xlab("Year") +
          ylab("Discard (tons)")
      })
    }
  )
}
