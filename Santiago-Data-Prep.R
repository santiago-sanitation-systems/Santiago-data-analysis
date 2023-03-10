## First clear your R Memory
rm(list=ls()) 

## load libraries
library(stringr)
library(rjson)
library(dplyr)
library(reshape2)
library(reshape)

## Set the working directory to the current directory
sourcedir=dirname(rstudioapi::getSourceEditorContext()$path)
setwd=sourcedir

## Insert the runname you used for Santiago
runname = "bestpractice_example"

## Set TRUE if you are applying Santiago in different sector of the same city (zones)
analysisperzone = FALSE
## insert the name of the zone (ignore it if you are not conducting an analysis per zone)
zonename= "A"
## chose a name for a new folder in your output folder where all data and plot will be saved
## don't change this name for different zones of the same city
folder_zone="bestpractice_example_ZoneAnalysis"

### define the path to your output folder from this run
rundir<-file.path("../Santiago-runfolder/output", runname)
## create a directory for analysis per zone
dir.create(file.path("../Santiago-runfolder/output", paste(runname,"analysis_per_zone", sep = "_")), showWarnings = FALSE)
zonedir=(file.path("../Santiago-runfolder/output", paste(folder_zone)))

## read data 
  ## read in allSys
  props <- read.csv(file.path(rundir, paste(runname, "_properties_allSys.csv", sep = "")), sep=",", header=T)

  ## read in selectedSystems
  selectedSystems <- read.csv(file.path(rundir, paste(runname, "_selectedSys.csv", sep = "")), sep=",", header=T)
  
  ## read in and convert TAS
  tas_fg <- fromJSON(file = file.path(rundir, paste(runname, "_TAS_FG.json", sep = "")))
  tas_df <- as.data.frame(t(rbind(as.data.frame(tas_fg$tas), as.data.frame(tas_fg$FG))))
  colnames(tas_df) <- c("TAS", "FG")
  tas_df$TAS <- as.numeric(tas_df$TAS)

  ## read in and convert TAS Components
  tas_components_list <- fromJSON(file = file.path(rundir, paste(runname, "_TAS_Components.json", sep = "")))
  melted_tas_comp <- melt(tas_components_list)
  colnames(melted_tas_comp) <- c("value", "attribute", "tech")
  tas_components_df <- dcast(melted_tas_comp, tech ~ attribute)
  remove(tas_components_list, melted_tas_comp)
  
  if (analysisperzone){
    ## add column "zone"
    tas_components_df$zone <-zonename
    ## Merge TAS and TAS Components
    tas_components_df <- merge(tas_components_df, tas_df, by.x = "tech", by.y = 0)
    #create long data frame
    tas_components_df_long = melt(tas_components_df, id=c("tech", "FG", "zone"))
    ## add column "zone"
    props$zone <-zonename
  } else {
    ## Merge TAS and TAS Components
    tas_components_df <- merge(tas_components_df, tas_df, by.x = "tech", by.y = 0)
    #create long data frame
    tas_components_df_long = melt(tas_components_df, id=c("tech", "FG"))
    }
  end
  
## add column "selected" with boolean expression checking if system is in selectedSystems
props$selected <- props$ID %in% selectedSystems$ID

## factor sources for setting order
props$source <- factor(props$source, levels=str_sort(unique(props$source), numeric = TRUE))

### factor template names for setting order
template_c2=str_sort(unique(props$template), numeric = TRUE)
props$template <- factor(props$template,levels=template_c2)
## rename templates with line breaks
for(i in levels(props$template)){
  levels(props$template)[levels(props$template)==i] <- paste(strwrap(i, width=27), collapse=" \n")
}

## make templates short names
props$template_nb <- gsub(". .*$","",props$template)

## add number of SanSys within a each template
n_in_template=NULL
for (template in levels(props$template)) {
  number.of.sys.in.template <- sum(props$template == template, na.rm=TRUE)
  n_in_template = rbind(n_in_template, data.frame(template, number.of.sys.in.template))
}
props$number_in_temp<-NA
for (tem in n_in_template$template) {
  props$number_in_temp[props$template==tem] <- n_in_template$number.of.sys.in.template[n_in_template$template==tem]
}


## calculate "accumulated recoveries"
props <- within(props, recovery_ratio_accumulated_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                                                             recovery_ratio_water_mean + recovery_ratio_totalsolids_mean))
props <- within(props, recovery_ratio_accumulated_balanced_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                  recovery_ratio_water_mean + recovery_ratio_totalsolids_mean)/4)

props <- within(props, recovery_ratio_accumulated_sd <- (recovery_ratio_phosphor_sd + recovery_ratio_nitrogen_sd +
                                                             recovery_ratio_water_sd + recovery_ratio_totalsolids_sd))
props <- within(props, recovery_ratio_accumulated_balanced_sd <- (recovery_ratio_phosphor_sd + recovery_ratio_nitrogen_sd +
                                                                      recovery_ratio_water_sd + recovery_ratio_totalsolids_sd)/4)

## calculate "accumulated losses"
props <- within(props, phosphor_losses_ratio_accumulated_mean <- lost_phosphor_air.loss_mean + lost_phosphor_soil.loss_mean + lost_phosphor_water.loss_mean)
props <- within(props, nitrogen_losses_ratio_accumulated_mean <- lost_nitrogen_air.loss_mean + lost_nitrogen_soil.loss_mean + lost_nitrogen_water.loss_mean)
props <- within(props, totalsolids_losses_ratio_accumulated_mean <- lost_totalsolids_air.loss_mean + lost_totalsolids_soil.loss_mean + lost_totalsolids_water.loss_mean)
props <- within(props, water_losses_ratio_accumulated_mean <- lost_water_air.loss_mean + lost_water_soil.loss_mean + lost_water_water.loss_mean)

## transform liters to m3
props$entered_water <- props$entered_water/1000
props$recovered_water_mean <- props$recovered_water_mean/1000
props$recovered_water_sd <- props$recovered_water_sd/1000
props$lost_water_air.loss_mean <- props$lost_water_air.loss_mean/1000
props$lost_water_air.loss_sd <- props$lost_water_air.loss_sd/1000
props$lost_water_soil.loss_mean <- props$lost_water_soil.loss_mean/1000
props$lost_water_soil.loss_sd <- props$lost_water_soil.loss_sd/1000
props$lost_water_water.loss_mean <- props$lost_water_water.loss_mean/1000
props$lost_water_water.loss_sd <- props$lost_water_water.loss_sd/1000


## save Props.RData and TAS.RData for future calculations
saveRDS(props, file=(file.path(rundir, paste(runname, "props.Rdata", sep = "_"))))
saveRDS(tas_components_df, file=(file.path(rundir, paste(runname, "tas_props.Rdata", sep = "_"))))
saveRDS(tas_components_df_long, file=(file.path(rundir, paste(runname, "tas_long_props.Rdata", sep = "_"))))

if (analysisperzone){
  saveRDS(props, file=(file.path(zonedir, paste(runname, zonename, "prop.Rdata", sep = "_"))))
  saveRDS(tas_components_df, file=(file.path(zonedir, paste(runname, zonename, "tas_props.Rdata", sep = "_"))))
  saveRDS(tas_components_df_long, file=(file.path(zonedir, paste(runname, zonename, "tas_long_props.Rdata", sep = "_"))))}
end 

