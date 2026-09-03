
# Updated:

# Description

This repository contains code to conduct a material flow analysis model for California plastics by industry sector, as well as code to run this model in an interactive RShiny website.

# Repository Structure

```
ca-mfa
  |__ data
    |__ raw
    |__ static
  |__ functions
  |__ mfa_app
        |__ www
  |__ archive
       |__ tests
```

# Data

All data files required to launch the app and run the model can be found in the data folder


```

data
 |__ raw
      |__ plastic_sector_classification.csv
      |__ scaled_na_consumption.csv
 |__ static
      |__ ca_consum_bau_default.rds
      |__ ca_rr_pack.csv
      |__ ca_rr.csv
      |__ emission_factors_clean.csv
      |__ incineration_clean.csv
      |__ lifetimes_clean.csv


```


# Previous description:

# Description

This repository contains code to conduct a material flow analysis for California plastics by resin type and industry sector.   

# Overview

This ReadMe contains the following information (click to jump directly to a section):

[Repository Structure](#repository-structure): brief overview of repository structure
[Data](#data): description of current datasets and where to get them 
[Repository Details](#repository-details): brief descriptions of all the scripts

# Repository Structure

The repository uses the following basic structure: 

```
ca-mfa
  |__ data
    |__ raw
    |__ processed
    |__ output
  |__ figures
  |__ R
  
```



# Data 

The data folder is on the gitignore, I didn't want to mess with all the files currently in the project data folder on Drive so I made a new one called `github-data` which has all the datasets and versions needed to run this code. 
All datasets can be downloaded from [Google drive](https://drive.google.com/drive/folders/1x9pYRdLlX-eGH1cVZbEhjAm3xq-HIcIC?usp=drive_link) just rename github-data to data so the filepaths work. 

As of now the key datasets are: 

```
data
  |__ raw
    |__ 2024_12_16_USEEIO_and_NAICS_crosswalk_2017_schema.xlsx: provides information on the BEA industry codes required to relate the detailed codes to their summary code   
    |__ plastic_sector_classification.xlsx: current version of the classification of each of the 400+ detailed BEA industry codes into plastic-relevant sectors    
    |__ CAEEIO_326_output_2012_2020_v4.xlsx: current version of CA EEIO model, includes full Leontief results in the "Deflated plastic intensity" tab and power series results for 2012-2020 
  |__ processed
    |__ industry_to_plastic_sector.csv: proportions to allocate each of the 73 sectors to one or more plastic sectors; output of 01_classify_sectors
  |__ output
    |__ CA Plastic MFA Model v14.xlsx: current MFA model output, from Roland not calculated in this repository, used to make most of the figures and get the top-line avoided production and GHG results    
    |__ CA_EEIO_2012_2020_power_series_by_plastic_sectors: IO model output using the power series approach; output of 02_io_model 

```  

# Repository Details

All of the relevant code is contained in the `R` folder. The order of running scripts should be as follows: 

  - `00-download_state_models.R`: some code to pull the models for all 50 states from the EPA website   
  - `01_classify_sectors`: this uses the plastic sector classification input file to calculate the proportions for how to allocate consumption from each of the 73 industry sectors into plastic-relevant sectors   
  - `02_io_model`: use the power series approach to estimate annual consumption 2012-2020 using the proportions calculated in `01_classify_sectors` and the EPA state-level IO model   
    - **NOTE: this is where we will need to update the code to generate power series estimates for all other years**
  - `03_figures`: code used to make any figures (currently just CLC presentation figures) save in the `figures` folder   
  - `_archived`: this is previous methods for plastic sector classification and the full Leontief results (not the power series approach) keeping it here for now   