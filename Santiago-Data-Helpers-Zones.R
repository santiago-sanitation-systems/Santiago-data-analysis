# # Helpers for Plots

## Load necessary Libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)
library(doBy)
library(randomcoloR)
library(ggrepel)


## Load Props.RData file and TAS files calculated with "Santiago-Data-Prep.R"
# Get the names of all files ending with tas_props.Rdata in the directory
file_names <- list.files(rundir, pattern = "prop.Rdata$", full.names = TRUE)
# Initialize an empty data frame to store the combined data
props <- data.frame()
# Loop through the file names and read them into data frames, then combine them using rbind()
for (i in seq_along(file_names)) {
  df <- readRDS(file_names[i])
  props <- rbind(props, df)
}

# Get the names of all files ending with tas_props.Rdata in the directory
file_names <- list.files(rundir, pattern = "tas_props.Rdata$", full.names = TRUE)
# Initialize an empty data frame to store the combined data
tas_components_df <- data.frame()
# Loop through the file names and read them into data frames, then combine them using rbind()
for (i in seq_along(file_names)) {
  df <- readRDS(file_names[i])
  tas_components_df <- rbind(tas_components_df, df)
}

# Get the names of all files ending with tas_long_props.Rdata in the directory
file_names <- list.files(rundir, pattern = "tas_long_props.Rdata$", full.names = TRUE)
# Initialize an empty data frame to store the combined data
tas_components_df_long <- data.frame()
# Loop through the file names and read them into data frames, then combine them using rbind()
for (i in seq_along(file_names)) {
  df <- readRDS(file_names[i])
  tas_components_df_long <- rbind(tas_components_df_long, df)
}


## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(rundir,"/Santiago-R-Plots"), showWarnings = FALSE)
plotdir=(file.path(rundir,"/Santiago-R-Plots"))
rundir<-file.path("../Santiago-runfolder/output", runname)
##################################################################
# # # colours and names

## define source colors randomly
source_cols <- levels(props$source)
for (i in 1:length(source_cols)) {
  source_cols[i]<-randomColor()
}
remove(i)
## define source names
source_labs=levels(props$source)
source_names=levels(props$source)

## define template colors with randomly assigned colors
#template_cols <- levels(props$template)
#for (i in 1:length(template_cols)) {
#    template_cols[i]<-randomColor()
#}
#remove(i)

# define template names
template_names=levels(props$template)
template_names_short=str_sort(unique(props$template_nb), numeric = TRUE)

## define functional group colour manually
fgcol=c(U="#BF0D0D", S="#F29100", C="#FFD400", T="#76B742", D="#00AECA")


## Function for multiple plots with shared ledgend ------------------
  grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
    grid.newpage()
    grid.draw(combined)
  
   # return gtable invisibly
    invisible(combined)
  }




