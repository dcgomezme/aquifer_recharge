[![DOI](https://zenodo.org/badge/655962123.svg)](https://doi.org/10.5281/zenodo.14286424)

# Assessment of Aquifer Recharge Estimation Using Empirical Methodologies

This project implements methodologies to estimate the potential aquifer recharge in the Lebrija river basin, Santander, Colombia. The code is derived from the research presented in the thesis:

> **"Assessment of methodologies to estimate aquifer recharge in a Tropical Climate Zone"**  
> BSc Thesis in Civil Engineering  
> **Author**: David Camilo Gómez Medina  
> **Advisors**: Adriana Patricia Piña Fulano, Leonardo David Donado Garzón  
> Universidad Nacional de Colombia, 2023

## Overview

Aquifer recharge quantification is crucial for effective water resource management, especially in tropical regions. This project provides an R-based implementation of the methodologies discussed in the thesis. It serves as an alternative to traditional Geographic Information System (GIS) software, enabling users to process data and generate insights about aquifer recharge.

The code primarily focuses on:

1. **Empirical Methodologies**:
   - Chaturvedi formula
   - Modified Chaturvedi formula
   - Turc methodology
   - Gunther Schosinsky's method

2. **Geospatial Data Handling**:
   - Precipitation, temperature, land use, and soil texture data processing.
   - Map algebra for spatial analysis.

3. **Climate Impact Analysis**:
   - Influence of La Niña and El Niño on aquifer recharge in tropical zones.

## Features

- Calculation of potential aquifer recharge using various empirical methods.
- Analysis of hydrometeorological data for a 20-year period (2000–2020).
- Support for climatic events' analysis (e.g., ENSO phenomena).

## Requirements

- **R Language**: The project relies on the R programming language.
- **Libraries**:
  - `tidyverse`
  - `sf` (for geospatial data)
  - `raster`
  - `sp`

Ensure you have these libraries installed before running the script.

## Data Sources

The hydrometeorological data, digital elevation models, and land use information used in this project are sourced from:
- Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM)
- Geological and cartographic data from public repositories.

## Usage

1. Clone this repository:
   ```bash
   git clone https://github.com/your-repo-name/aquifer-recharge-estimation.git
   cd aquifer-recharge-estimation
   ```

2. Install the required R libraries:
   ```R
   install.packages(c("tidyverse", "sf", "raster", "sp"))
   ```

3. Run the script:
   ```R
   source("R_Tesis.R")
   ```

4. Follow the instructions within the script to load and process your data.

## Results

The code outputs maps and statistical data detailing:
- Potential aquifer recharge across the Lebrija basin.
- Effects of climatic variations (La Niña, El Niño) on recharge patterns.

## License

This project is open-source under the [MIT License](LICENSE).

## Acknowledgments

- Universidad Nacional de Colombia, Facultad de Ingeniería.
- Grupo de Hidrodinámica del Medio Natural (HYDS).
- Advisors and collaborators of the thesis project.
