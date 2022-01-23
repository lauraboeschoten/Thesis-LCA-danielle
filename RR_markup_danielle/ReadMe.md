---
title: "ReadMe"
author: "Daniëlle Remmerswaal"
date: "23-1-2022"
output: 
  html_document:
    keep_md: yes
    toc: false
    number_sections: yes
    
  
---




This repository contains all necessary files to replicate the (MILC part of the) simulation study for the MSc thesis "Comparing MILC and tree-MILC for estimating and correcting multiple types of errors in combined datasets" by Daniëlle Remmerswaal. Software requirements are R (http://www.r-project.org}) and R-package poLCA (version 1.4.1, https://cran.r-project.org/web/packages/poLCA/index.html). \

Here you can find an overview of all the files in this research repository.

| Files/Folders          | Description   |
| -----------------      | ------------- |
|Execute.R              |R-script to run the complete simulation study (step 1-5)|
|1_SimulateData.R       |R-script to simulate data|
|2_BootstrapData.R      |R-script to bootstrap data|
|3_LCmodel.R            |R-script to apply LC model|
|4_Imputations.R        |R-script to draw imputations|
|5_Results.R            |R-script to calculate results|
|RR MILC Markup Danielle.pdf      |Background about the simulation study can be found in this research report|
|RR MILC Markup Danielle.zip      |All necessary files to create the report yourself|


To run the simulation study you can either run each step seperately and chronologically (1 to 5), or run solely the script "Execute.R" in which all scripts are combined. 

**Steps**

1. Simulate the data

2. Sample *m* bootstrap samples

3. Apply LC model on each bootstrap sample

4. Draw *m* imputations on basis of the posterior membership probabilities

5. Calculate results, the pooled mean group sizes



\
For any help with the files in this archive, please contact Daniëlle Remmerswaal (d.m.remmerswaal@uu.nl).


