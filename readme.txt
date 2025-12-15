# VADT - Visualising Atlantis Diagnostic Toolbox

The Visualising [Atlantis](http://atlantis.cmar.csiro.au/) Toolbox (VADT) was created by Christopher David Desjardins at the [Science Institute at the University of Iceland](http://raunvis.hi.is/rhformE.php) as part of the [EU MareFrame project](http://www.mareframe.eu/) with code contributions from:

- Vidette McGregor at [NIWA](https://www.niwa.co.nz/)
- Erla Sturludottir at the [Agricultural University of Iceland](https://www.lbhi.is/en)
- Jacob Kasper at the Marine and Freshwater Research Institute (https://www.hafogvatn.is/)

VADT is released under the [GPL v3 or later](http://www.gnu.org/copyleft/gpl.html). Source code is available at Source code is available at [https://github.com/jacobkasper/AtlantisVadt](https://github.com/jacobkasper/AtlantisVadt).

## Recent Updates (2025)

The codebase has been significantly modernized and refactored:

- **Converted to modern tidyverse**: Replaced legacy `plyr`, `reshape2`, and base R code with `dplyr`, `tidyr`, and `purrr`
- **Improved data processing**: Streamlined NetCDF data extraction and array manipulation
- **Modular plotting functions**: Created reusable helper functions for consistent visualization across all tabs
- **Enhanced error handling**: Added `req()` checks and better data validation throughout
- **Better performance**: Optimized data pipelines and reduced code duplication (~40% code reduction in plotting sections)
- **Added new features**: Biomass by box visualizations (with/without migration)

## Features

VADT provides comprehensive visualization and analysis tools for Atlantis ecosystem model outputs:

### Spatial Plots
- **Distribution by Box**: Within and across box distributions for functional groups by age class
- **Spatial Maps**: Interactive density and biomass maps (per m², per m³)
- **Migration Analysis**: Compare distributions with and without migration
- **Biomass by Box**: Time series of biomass distribution across spatial boxes
- **Animated Plots**: GIF animations of spatial biomass dynamics

### Age-Disaggregated Plots
For vertebrate functional groups:
- **Structural & Reserve Nitrogen**: Nitrogen pools by age class over time
- **Biomass**: Total and proportional biomass by age class
- **Numbers**: Total abundance and proportional distribution by age class
- **Weight Metrics**: Wet weight and relative weight changes
- **Length-at-Age**: Growth trajectories by age class
- **Condition Indices**: Relative health metrics and partitioning coefficients

### Diet Data
- **Predator-Prey Interactions**: Consumption patterns by predator and prey
- **Temporal Dynamics**: Diet composition changes over time
- **Habitat/Cohort Breakdown**: Diet data by habitat type or age cohort
- **Faceted Views**: Multiple predators or prey displayed simultaneously

### Biological Summaries
- **Total Biomass**: Vertebrate and invertebrate biomass time series
- **Assessment Comparisons**: Model vs. observed data (grey reference lines)
- **Spawning Stock Biomass (SSB)**: With confidence intervals when available
- **Recruitment**: Young-of-year (YOY) abundance
- **Numbers-at-Age (NAA)**: Age structure comparison with stock assessments
- **Invertebrate Metrics**: Relative biomass, grazing, and production

### Fisheries
- **Total Landings**: Aggregate and by functional group
- **Catch by Age**: Numbers and biomass at age with assessment data overlay
- **Spatial Distribution**: Catch by box and fishery
- **Effort**: Fishing effort time series by fleet
- **Discards**: Total discards, proportions, and age/length composition
- **Fishery-Specific**: Landings and discards by fleet

## Reference Data

VADT can display reference plots comparing model outputs to observed data:

- **Biomass & SSB**: Stock assessment estimates with confidence intervals
- **Numbers-at-Age (NAA)**: Age structure from surveys
- **Harvest**: Observed catch data
- **Catch-at-Age**: Age-specific catch from assessments

Reference data templates are provided in the `observation_templates/` directory. Observed data is displayed as grey lines/points for easy comparison.

**Note**: ForVADT reference indices are Iceland-specific and may not be applicable to other regions.

## Technical Details

### Dependencies
VADT uses modern R packages:
- **Data manipulation**: `dplyr`, `tidyr`, `purrr`, `stringr`
- **Visualization**: `ggplot2`, `viridis`, `scales`
- **Spatial data**: `sf`, `terra`
- **NetCDF processing**: `ncdf4`
- **Shiny framework**: `shiny`, `shinyWidgets`

### Data Processing
- Efficient NetCDF file reading with lazy evaluation
- Array processing using vectorized operations
- Proper handling of spatial dimensions (layer, box, time)
- Memory-optimized data structures for large model outputs

### Code Architecture
- **Modular helper functions**: Reusable plotting and data processing functions
- **Consistent theming**: Unified `theme_atlantis()` across all visualizations
- **Error handling**: Comprehensive data validation and user feedback
- **Performance**: Optimized pipelines for responsive Shiny interface

## Installation
```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("jacobkasper/AtlantisVadt")
```

## Usage
```r
library(vadt)

# Launch the Shiny application
# The app will guide you through loading Atlantis output files
vat::launch_vat()
```

## File Requirements

VADT expects standard Atlantis output files:
- `output.nc` - Main output NetCDF file
- `outputCatch.nc` - Fisheries catch data (if applicable)
- `*.bgm` - Box geometry file
- `*.prm` - Parameter files (biological, fishing, etc.)
- `groups.csv` - Functional groups definition

Optional reference data files (see `observation_templates/`):
- Biomass summaries
- SSB estimates
- Numbers-at-age
- Harvest data
- Catch-at-age

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with clear description of changes

## Citation

If you use VADT in your research, please cite:
```
Desjardins, C.D., McGregor, V., Sturludottir, E., Kasper, J. (2025). 
VADT: Visualising Atlantis Toolbox. 
https://github.com/jacobkasper/AtlantisVadt
```

## Support

For questions, issues, or feature requests, please open an issue on GitHub:
https://github.com/jacobkasper/AtlantisVadt/issues

## License

GPL v3 or later. See LICENSE file for details.

## Acknowledgments

This work was supported by the EU MareFrame project (FP7-KBBE-2013-7, Grant 613571).
Additional development supported by the Agricultural University of Iceland and the Marine and Freshwater Research Institute of Iceland.