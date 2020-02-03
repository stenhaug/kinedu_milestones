# cogsci submission (2/2/2020)

This repo produces the cogsci paper submission, "The latent factor structure of developmental change in early childhood," which can be reproduced as follows:

1. [data-clean/data-clean.Rmd](data-clean/data-clean.Rmd) is the code that takes the data in data-raw and produces the rds files in data-clean (these are <100mb so they are included in this repo)

2. Each of the Rmd files in data-models (models_exploratory.Rmd, models_bifactor.Rmd, and age_partitioned_models.Rmd) contains the code to produce the models used in the paper. However, these take about one day to fit on a local machine and so we have uploaded the saved models that we fit to [figshare](https://figshare.com/projects/The_latent_factor_structure_of_developmental_change_in_early_childhood/75189)

3. cogsci/cogsci.Rmd is the main file for the paper submission
