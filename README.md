---
title: "README"
output: html_document
---

## Overview
This project contains files to run the simulations of the ReMoTe model. This deterministic simulation considers to role of the ARI and potential protection against progression to active disease from reinfection. It is based on a synthetic population on one million individuals with age structure similar to South Africa. This work has been published in [`Clinical Infectious Diseases`](https://pubmed.ncbi.nlm.nih.gov/35666515/).

## Explanation of files
Following is an explanation of the files in the project:

1. utils.R. This has all the functions needed to run the simulations.
2. runSimulation_homogeneous_protection.RMD. This Markdown file has detailed code to run the scenario when there is homogeneous risk of infection and repeat infection confers protection again subsequent risk of progression to disease.  
3. runSimulation_homogeneous_noprotection.RMD. This Markdown file has detailed code to run the scenario when there is homogeneous risk of infection and repeat infection confers no protection again subsequent risk of progression to disease.  
4. runSimulation_heterogeneous_protection.RMD. This Markdown file has detailed code to run the scenario when there is is a high risk (20% of population) and low risk population with a six fold difference in ARI between high and low risk populations and repeat infection confers protection again subsequent risk of progression to disease.  
5. runSimulation_heterogeneouss_noprotection.RMD. This Markdown file has detailed code to run the scenario when there is is a high risk (20% of population) and low risk population with a six fold difference in ARI between high and low risk populations and repeat infection confers no protection again subsequent risk of progression to disease. 
6. runSensitivityAnalyses.R. This runs all the sensitivity analyses reported in the supplement. Sensitivity Analyses.RMD is the R Markdown version of this.
7. simDoddARI.R. This runs the simulations assuming the observed ARIs reported in Dodd (i.e. without adjusting the ARIs)
8. Figures.R. This generates all the figures in the main text of the paper.
8. runSimulations.RMD. This markdown file is a condensed version of all the simulations. This is not as updated as each simulation specific to the amin scnearios reported in the paper.

Files 2-5 output data used in the Figures.R file.
