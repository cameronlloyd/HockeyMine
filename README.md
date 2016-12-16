# HockeyMine
Uses the following R packages:
* config
* html
* rvest
* ggplot2
* caret
* lattice
* tree
* randomForest
* gbm

# Instructions
* Set main directory as working directory in R Studio
* Perform any or all steps mentioned below
* Test outputs of my scripts are stored in all 'Results' folder.

## Web Scraping
All scripts are contained in ./WebScrape
* scrapeTeam.R - Scrapes team schedule from site (this file was lost in system error)
* scrapeMatchups.R - Appends more statistics to each record in teams schedule.  Primarily matchup statistics
* mergeSchedules.R - Merge all matchups to one dataset 'Matchups.csv' (stored in ~/Data)

All schedules will be stored in ./TeamSchedules

## Preprocessing
All scripts are contained in ./Preprocessing
* Preprocessing.R - used for all dataset filtering.  (Results stored in ~/Data)
* Exploration - used to retrieve useful statistics of dataset.

Each script has two parts.  Boolean variables (part1) near bottom of script specify which parts to run

## Model Eval
All scripts are contained in ./ModelBuild
* ModelBuild.R - Does splitting, model eval, and model building

Boolean variables (needSplit, evalModels and CreateModel) near bottom of script to specify if all tasks need done
