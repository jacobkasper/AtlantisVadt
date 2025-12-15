# VAT - Visualising Atlantis Toolbox

The Visualising [Atlantis](http://atlantis.cmar.csiro.au/) and Diagnostic Toolbox (`vadt`) was created by Christopher David Desjardins at the [Science Institute at the University of Iceland](http://raunvis.hi.is/rhformE.php) as part of the [EU MareFrame project](http://www.mareframe-fp7.org/) with code contributions from:

- Vidette McGregor at [NIWA](http://www.niwa.co.nz/)
- Erla Sturludottir at the [Science Institute at the University of Iceland](http://raunvis.hi.is/rhformE.php)
- Jacob Kasper at the [Marine and Freshwater Research Institute (MFRI), Iceland](https://www.hafogvatn.is/en)

`vadt` is released under the [GPL v3 or later](http://www.gnu.org/copyleft/gpl.html). Source code is available at [https://github.com/jacobkasper/AtlantisVadt](https://github.com/jacobkasper/AtlantisVadt).

## Recent Updates (2025)

The codebase has been modernized with tidyverse conventions, improved data processing pipelines, modular plotting functions, and enhanced performance (~40% code reduction in plotting sections). New features include biomass by box visualizations with/without migration.

## Current Features

**VADT provides comprehensive visualization for Atlantis ecosystem model outputs:**

### Spatial Analysis
- Within and across box distributions by functional group and age class
- Interactive density and biomass maps (per m², per m³)
- Migration analysis comparing distributions with/without movement
- Animated spatial biomass dynamics

### Age-Disaggregated Analysis
- Structural & reserve nitrogen pools over time
- Biomass and abundance by age class (total and proportional)
- Length-at-age trajectories and weight metrics
- Condition indices and health metrics

### Ecological Interactions
- Diet composition by predator and prey
- Temporal dynamics of predator-prey relationships
- Habitat and cohort-specific consumption patterns

### Stock Assessment & Fisheries
- Biomass, SSB, and recruitment time series
- Numbers-at-age with stock assessment comparisons (grey reference lines)
- Landings and catch by species, fishery, age, and spatial box
- Effort dynamics and discard analysis
- Age and length composition of catches

### Reference Data Integration
Model outputs can be compared with observed data from stock assessments. Reference data templates are available in `observation_templates/`. Observed estimates are displayed as grey lines/points for easy visual comparison.

## Getting Started

To start the application:
1. Click a tab (e.g., **Age Disaggregated**, **Spatial Plots**, **Biological Summaries**, **Fisheries**)
2. Select the functional group or variable of interest
3. Some plots have additional navigation tabs on the left side of the page
4. Adjust scale options (Fixed/Free) where available for optimal visualization

**Note:** The first load may take a moment as data is processed. Subsequent interactions will be faster.