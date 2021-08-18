# Repository overview

This repository provides all the analysis code for the paper **"SARS-CoV-2 antibody seroprevalence and associated risk factors in an urban district in Cameroon"**.

All tabular, graphical and other analytic outputs in the paper can be reproduced by knitting the `figures_for_paper.Rmd` file in the home directory.
This Rmd file will source analytics scripts in the /scripts folder.

All data referenced in the paper is provided in the /data folder.

All maps used during the study are in the /maps folder.

The study flowchart is in the /plots folder.

The study questionnaire (*study_questionnaire.pdf*) is included in the home directory.

# Study dataset

A minimal, anonymized version of the dataset is provided in this repository.
This is the *yaounde_covid_seroprev_dataset.csv file* in the \\*data* folder.
Each column in the CSV corresponds to a question in the study questionnaire.

The original dataset (exported from KoboCollect) was modified in the following ways:

1\.
Most variables from questionnaire sections 10 to 14, which were not addressed in the seroprevalence paper, are not included.

2\.
The address variable, \`loc_hhld_addr\`, was removed, and replaced with the statement "data removed to protect privacy"

3\.
The date of birth variable, \`dt_birth\` was similarly removed, although the age at survey (in years) remains available.

4\.
Dates of deaths of household members were truncated to the first day of the month of death.
(e.g. 2020-06-30 to 2020-06-01)

5\.
GPS coordinates were de-localized by adding ≤ 0.001 degrees (with the R code \`sample(seq(-0.001, 0.001, by = 0.0001))\`) of random noise to the latitude and longitude, and ≤ 50 metres ( with the R code \`sample( seq(-25, 25, by = 1))\`) of random noise to the altitudes.

## Variable names

For naming variables, we use Emily Riederer's controlled vocabulary recommendations.

See here: [<https://emilyriederer.netlify.app/post/column-name-contracts/>]([https://emilyriederer.netlify.app/post/column-name-contracts/){.uri}.](https://emilyriederer.netlify.app/post/column-name-contracts/){.uri}.)

The data dictionary (*data/data_dictionary.xlsx*) contains a spreadsheet which shows which variable name corresponds to which survey question.
To understand what each variable represents, the user is also encouraged to consult the original questionnaire, which is also present in this repository (*study_questionnaire.pdf*).

# Package versions and reproducibility

Package versions for this repository are controlled with the `renv` package.
After cloning the repository, the user should run the function `renv::restore()` to install the package versions saves in the *renv.lock* file.
This will ensure that the repository code is fully reproducible and should run without bugs.
