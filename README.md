# Santiago Data Analysis with R

Welcome to the _Santiago_ Data Analysis _R_-Package version 13.01.2021.

## General  
In this repository you will find a number of scripts that can be used in combination with the newest version of the [_SANitation sysTem Alternative GeneratOr (Santiago)_](https://github.com/santiago-sanitation-systems/Santiago.jl), a Julia package also available on github.
The aim is to provide basic support for the analysis of _Santiago_ outputs in R using ggplot2. 
We are continuousely updating this repository. Thus, please download the newest version frequently and contact us in case of any encountered problem.

## Content
This repository contains three _R_ Scripts:  
* _Santiago-Data-Prep.R_ --> This script helps you to load the data outputs from _Santiago_ and to convert it into a a dataframe for easy handling in _R_. You only need to do this once for new data or when the outputs from your _Santiago_ run have been updated. After using this script. The dataframew are stored as RData and can be loaded directly.
* _Santiago-Data-Helpers.R_ --> This scripts contains helper functions and variables (colours, labels etc.) for plotting. It is called from the third script.
* _Santiago-Data-Plots.R_ --> This script contains the code for the different plots.
* _Santiago-Data-Helpers-Zones.R_ --> This scripts contains helper functions and variables (colours, labels etc.) for plotting. It is called from the third script.
* _Santiago-Data-Plots-Zones.R_ --> This script contains the code to plot.

## Available Plots using _Santiago-Data-Plots.R_
### 1 - Plotting Technology Appropriateness Scores (TAS)
* p1.1 --> TAS - Influence of criteria on TAS, boxplot of TAS and criteria scores grouped by functional group
* p1.2 --> TAS - Overview on all TAS per technology
* p1.3 --> TAS - Detailed appropraiteness profiles for all technologies
* p1.4 --> SAS & RR - Overview of SAS and resource recovery potential of selected systems
### _Optional_ 2 - Plotting System Appropriateness Scores (SAS)
* p2.1 --> SAS - Boxplot of all SAS grouped by templates and selected systems
* p2.2 --> SAS - Jitterplot of SAS versus resource recovery per substance and selected sytems
### _Optional_ 3 - Plotting Resource Recovery (RR) potentials
* p3.1 --> RR - Density plot for recovery for all four substances: total phosphosrus (TP), total nitrogen (TN), total solids (TS), and water (H2O)
* p3.2 --> RR - Density plot for recovery for all four substances, grouped by source
* p3.3 --> RR - Boxplot of recovery for all substances grouped per template, colored by source.
* p3.4 --> RR - Boxplot of all recovery ratio and losses grouped by source 
* p3.5 --> RR - Recovery versus length of systems for all substance, grouped by system templates
* p3.6 --> RR - Accumulated revoery versus length of systems, coloured by system template
* p3.7 --> RR - Standard deviation of recovery against recovery, coloured by system template

## Available Plots using _Santiago-Data-Plots-Zones.R_
### 1 - Plotting Technology Appropriateness Scores (TAS) per zone
* p1.2 --> TAS - Overview on all TAS per technology and per zone
* p1.3 --> TAS - Detailed appropriateness profiles per zone for all technologies and zones
* p1.3U --> TAS - Detailed appropriateness profiles per zone for all technologies from functional group "User interface"
* p1.3S --> TAS - Detailed appropriateness profiles per zone for all technologies from functional group "Storage & Containment"
* p1.3C --> TAS - Detailed appropriateness profiles per zone for all technologies from functional group "Conveyance"
* p1.3T --> TAS - Detailed appropriateness profiles per zone for all technologies from functional group "(Semi-)Centralized Treatment"
* p1.3D --> TAS - Detailed appropriateness profiles per zone for all technologies from functional group "Disposal/Reuse"

## Usage:
### Installation
Create a folder on your computer called __santiago-sanitation-systems__. This is ideally also the root folder of your _Santiago-runfolder_ where you run the _Santiago_ runfile/package and stored your _Santiago_ input and output data (see below for the recommended folder structure).

### Input data
First of all, you need the required _Santiago_ outputs. These are two csv files and two json files:
* (runName)_allSys.csv
* (runName)_selectedSys.csv
* (runName)_TAS.json
* (runName)_TAS_Components.json

How to define the runName and to export these files is explained in the best practice runfile in the [Santiago Wiki > Data Analysis with R](https://github.com/santiago-sanitation-systems/Santiago.jl/wiki).

### Scripts
Then, you need to have all the scripts and paths on your computer set correctly. We wrote the scripts in a way, that they should run automatically without mayor changes if you have the following folder structure somewhere on your computer:
* __santiago-sanitation-systems__ (_ROOT FOLDER - create this folder yourself somewhere_)
 > * __Santiago-data-analysis__ (_SUBFOLDER1 - this is the folder you downloaded from github_)
   > * >Santiago-Data-Helpers.R
   > * >Santiago-Data-Plots.R
   > * >Santiago-Data-Prep.R
   > * >Santiago-Data-Helpers-Zones.R
   > * >Santiago-Data-Plots-Zones.R
 > * __Santiago-runfolder__ (_SUBFOLDER2 - this is the folder you created when you started working with Santiago_)
   > * >3.1-Best-Practice-Runfile.jl (_this file was initially downloaded from the Santiago Wiki_)
   > * >input (_generated when you run the runfile_)
   > * >Manifest.toml (_generated when you run the runfile_)
   > * >output (_generated when you run the runfile_)
   >   * >runname (_generated when you run the runfile_)
   > * >Project.toml (_generated when you run the runfile_)

### Using the data and the scripts
The usage is then as follows:

1) Run the script _Santiago-Data-Prep.R_ once and store the resulting dataframes as RData files in your runfolder. You only need to do this if you use the data for the first time or if your _Santiago_ output data has been changed (e.g. changes in appropriatness scores). If your data remains the same, you can just use the _Santiago-Data-Helpers.R_ script to load the previously calculated Rdata files (calculated with _Santiago-Data-Prep.R_ in a previous step). Specify if you want to conduct an analysis per zone and, where appropriate, define your zone's and folder's names. Refer to the [Santiago.jl.wiki](https://github.com/santiago-sanitation-systems/Santiago.jl/wiki) for further information about the demarcation of the zones.

2) Use _Santiago-Data-Plots.R_. This file automatically calls the helper file.  The plots are calculated and stored as a variable (e.g. "p3.3x") and in a later step also exported as PDF in the runfolder. Use `view(p.3.3x)` to view the plot in your Editor.
Plots of the first section (p1.1 to p1.4) are sufficient to draw conclusions on your Santiago's application. Further available plots are intended to a detailed and deep analysis of the results and might be overwhelming.

3) Use _Santiago-Data-Plots-Zones.R_ when you have run _Santiago-Data-Prep.R_ for every zones of interest. This file automatically calls the Helper-Zone file.  The plots are calculated and stored as a variable (e.g. "p1.3U") and in a later step also exported as PDF in the runfolder. Use `view(p1.3U)` to view the plot in your Editor. 


### Have Fun!
