## clear memory
rm(list=ls()) 

## Set the working directory to the current directory
sourcedir=dirname(rstudioapi::getSourceEditorContext()$path)
setwd=sourcedir
## Insert the runname you used for Santiago
runname = "bestpractice_example"
### define the path to your output folder from this run
rundir<-file.path("../Santiago-runfolder/output", runname)
## Run the helpers script. It defines important variables and also loads the prepared data from the _Prep_ script.
source("Santiago-Data-Helpers.R")

##########################################################################
### ---- Basics ----
## Number of SanSys within Source within a Template ------------------------
  n_tem_source=NULL
  for (template in levels(props$template)) {
    for (source in levels(props$source)) {
      n <- sum(props$source == source & props$template == template)
      #sum(props$source == levels(props$source)[1] & props$template == levels(props$template)[1])
      n_tem_source = rbind(n_tem_source, data.frame(template, source, n))
    }
  }
  write.table(n_tem_source, file.path(plotdir, "n_tem_source.csv"), sep = ";", row.names=F)

  # dataframe with selected systems ----  
  props_selected = props[props$selected == "TRUE", ]
  props_selected_melted<-data.frame(props_selected$template, props_selected$ID, props_selected$source, props_selected$sysappscore,
                                    props_selected$recovery_ratio_phosphor_mean,  props_selected$recovery_ratio_phosphor_sd, 
                                    props_selected$recovery_ratio_nitrogen_mean,  props_selected$recovery_ratio_nitrogen_sd,
                                    props_selected$recovery_ratio_totalsolids_mean,  props_selected$recovery_ratio_totalsolids_sd,
                                    props_selected$recovery_ratio_water_mean,  props_selected$recovery_ratio_water_sd,
                                                                      props_selected$recovered_water_mean,  props_selected$recovered_water_sd)
  write.table(props_selected_melted, file.path(plotdir, "props_selected_melted.csv"), sep = ";", row.names=F)

  #  mean length per template ----
  l_temp=NULL
  for (template in levels(props$template)) {
    n <- round(mean(props$ntechs[props$template == template]),1)
    l_temp = rbind(l_temp, data.frame(template, n))
  }
  write.table(l_temp, file.path(plotdir, "l_temp.csv"), sep = ";", row.names=F)

##########################################################################
### ---- 1 - Plotting Technology Appropriateness Scores (TAS) ----
## ---- p1.1 --> TAS - Influence of criteria on TAS, boxplot of TAS and criteria scores grouped by functional group  #### 

## General Labels
xtxt1.1 <- expression(paste("distribution of appropraiteness score, single attribute scores and aggregated technology appropriateness score (TAS)"))
ytxt1.1 <- expression(paste("score"))

p1.1 <- ggplot(tas_components_df_long, aes(x=variable, y=value, fill=FG)) +
  geom_boxplot(show.legend=F, varwidth= FALSE, lwd=0.25, colour = "#6F6F6E", width=0.5, alpha=0.8) +
  scale_fill_manual(values = fgcol)+
  facet_wrap( ~ FG, ncol=3) +
  theme_minimal() + 
  ggtitle("Functional Groups")+
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 15)))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E", angle = 90, vjust = 1, hjust=1),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  xlab(xtxt1.1) +
  ylab(ytxt1.1) +
  theme(panel.border=element_rect(colour="grey", fill=NA)) 
p1.1

## save for screen view and ppt
ggsave(file.path(plotdir, "p1_1_techappprofiles.png"), p1.1, unit="cm", width=19, height = 14, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_1_techappprofiles.pdf"), p1.1, unit="cm", width=19, height = 24, dpi=1000, device="pdf")

## ---- p1.2 --> TAS - Overview on all TAS per technology ####

## factor fgs for setting order
tas_components_df$FG <- factor(tas_components_df$FG, levels=c('U','S','C','T','D'))

p1.2 <- ggplot(tas_components_df, aes(x=tech,y=TAS) )+
  geom_point(size=2, aes(colour=FG), alpha=0.8, stroke=0.5) +
  scale_color_manual(values=fgcol)+
  scale_shape_manual(values=c(2, 0, 1))+
  theme_minimal() +
  guides(colour = guide_legend(title="Functional group", order=1, ncol=5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=7.5, colour = "#6F6F6E",  angle = 80, hjust = 1),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "top",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x ="Technology", y = "Technology appropriateness score (TAS)")
p1.2

## save for screen view and ppt
ggsave(file.path(plotdir, "p1_2_allTAS.png"), p1.2, unit="cm", width=19, height = 12, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_2_allTAS.pdf"), p1.2, unit="cm", width=19, height = 12, dpi=1000, device="pdf")

## ---- p1.3 --> TAS - Detailed appropraiteness profiles for all technologies ----

xtxt1.3 <- expression(paste("Attribute Appropriateness Scores followed by Technology Appropriateness Score ", italic("(TAS)")))
ytxt1.3 <- expression(paste("score"))

p1.3 <- ggplot(tas_components_df_long, aes(x=variable, y=value, fill=tech)) +
  geom_boxplot(show.legend=F) +
  #scale_fill_manual(values = techcols) +
  facet_wrap( ~ tech, ncol=5) +
  theme_minimal() + 
  ggtitle("Technologies")+
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=6, colour = "#6F6F6E", angle = 80, vjust = 1, hjust=1),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=6, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  xlab(xtxt1.3) +
  ylab(ytxt1.3) +
  theme(panel.border=element_rect(colour="grey", fill=NA)) 
p1.3

## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_alltechappprofiles.png"), p1.3, unit="cm", width=25, height = 40, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_alltechappprofiles.pdf"), p1.3, unit="cm", width=25, height = 40, dpi=1000, device="pdf")


## ---- p1.4 --> Comparison of selected system (overview's profiles) ----

xtxt1.4 <- expression(paste("Decision table for preselected sanitation system"))
ytxt1.4 <- expression(paste("score"))


# Create grouped barplot for Resource recovery potential
# Create a data frame with the required columns
df <- data.frame(props_selected[,c("ID","recovery_ratio_nitrogen_mean", "recovery_ratio_phosphor_mean", "recovery_ratio_totalsolids_mean", "recovery_ratio_water_mean")])
#create a dataframe with standard deviations
sf <- data.frame(props_selected[,c("ID", "recovery_ratio_nitrogen_sd", "recovery_ratio_phosphor_sd", "recovery_ratio_totalsolids_sd", "recovery_ratio_water_sd")])
# Melt the data into long format
df.m <- melt(df, id.vars = "ID")
sf.m <- melt(sf,id.vars = "ID") #must check if it gives correct std values :in order by default or must we join them somehow
# Create name for the facet
my_labeller <- function(labels) {return("Resource recovery potential")}

# Plot the data using ggplot2
  p1.4rr <- ggplot(df.m,
        aes(x = value, y = variable, fill = variable)) +
        geom_col( position = "dodge", show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.f%% \u00B1 %.f%%", value*100, sf.m$value*100 )), hjust = -.05)+
        scale_x_continuous(labels = scales::percent_format(), lim = c(0, 1.2))+
        scale_y_discrete(labels = c("N","P","TSS","H2O"))+ #works if its always the same 4
        facet_wrap(~ ID, ncol=dim(props_selected)[1], labeller = my_labeller)+ #added dim(props_selected)[1] instead of 8
        #facet_grid(rows = vars(template), cols = vars(source)) +
        #labs(title = "Resource Recovery Potential ") +
        #scale_fill_discrete(name = "Variable") +
        theme(strip.background = element_rect(fill = "grey85", colour = "grey20"),
          panel.grid = element_line(colour = "grey92"), 
          panel.border     = element_rect(fill = NA, colour = "grey20"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "white",colour = NA) )
p1.4rr


#create df with only the SAS and melt it into long format
df2 <- data.frame(props_selected[,c("ID","sysappscore")])
df2.m <- melt(df2,id.vars = "ID")
# Create name for the facet
my_labeller2 <- function(labels) {return("System Appropriateness Score")}

# Create grouped barplot SAS
p1.4SAS <- ggplot(df2.m,
        aes(x = value, y = variable, fill = variable)) +
        geom_bar(stat = "identity", position = "dodge")+
        geom_text(aes(label = sprintf("%.f%%", signif(value)* 100)), hjust = -.05)+
        scale_x_continuous(labels = scales::percent_format(), lim = c(0, 1.2))+
        facet_wrap(~ ID, ncol=dim(props_selected)[1], labeller = my_labeller2)+
        scale_fill_discrete(guide="none")+
        theme(strip.background = element_rect(fill = "grey85", colour = "grey20"),
              #strip.text = element_blank(),
              panel.grid = element_line(colour = "grey92"), 
              panel.border     = element_rect(fill = NA, colour = "grey20"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = "white",colour = NA)
              )
p1.4SAS

#Put the system as header
df3 <- data.frame(props_selected[,c("ID","template")])
df3.m <- melt(df3,id.vars = "ID")
p1.4text <- ggplot(df3.m,
                  aes(x = 0, y = variable)) +
                  geom_text(aes(label = value), hjust = "center",vjust = "center")+
                  scale_fill_discrete(guide="none")+
                  facet_wrap(~ ID, ncol=dim(props_selected)[1]) +
                  theme(strip.background =element_rect(fill = "grey85", colour = "grey20"),
                        panel.grid = element_blank(), 
                        panel.border     = element_rect(fill = NA, colour = "grey20"),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        panel.background = element_rect(fill = "white",colour = NA)
                  )                
p1.4text
#add the graphics together in one figure
p1.4 <- p1.4text + p1.4SAS + p1.4rr + plot_layout(ncol = 1, heights = c(1,1,4))
p1.4
## save the plot in your dedicated folder
ggsave(file.path(plotdir, "p1_4_SAS_RR.png"), p1.4, unit="cm", width=70, height = 20, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_4_SAS_RR.png"), p1.4, unit="cm", width=70, height = 20, dpi=1000, device="pdf")


##########################################################################
### ---- 2 - Plotting System Appropriateness Scores (SAS) ----
## ---- p2.1 --> SAS - Boxplot of all SAS grouped by templates and selected systems ----

## Calculate Median of System Appropriateness Score
dataMedian_sysappscore <- summaryBy(sysappscore ~ template, data = props, FUN = list(median))
dataMedian_sysappscore$sysappscore.median <- round(dataMedian_sysappscore$sysappscore.median,2)

## Comment: You might have run this plot a few times to get a position of labels without overlap
p2.1 <- ggplot(data=props, aes(x=template, y=sysappscore))+
  geom_point(aes(color=source), alpha=0.5, size=1.2, position = position_jitter())+
  scale_colour_manual(values=source_cols, labels = source_labs, "Source", 
                      guide=guide_legend(override.aes = list(size=2, alpha= 1), nrow=4,byrow=FALSE))+
  geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
  geom_text(data = dataMedian_sysappscore, aes(template, sysappscore.median, label = sysappscore.median), size = 3, 
            position = position_dodge(width = 0.2), vjust = -0.75, check_overlap = TRUE)+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE, alpha=1)+
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=3) +
  guides(fill=guide_legend(ncol=4))+
  labs(x="Template", y= "SAS") +
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=6, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  theme(axis.title.x=element_blank())+
  scale_x_discrete(labels=template_names_short) +
  ggtitle("System appropriateness overview per template") 
p2.1

## save for screen view and ppt
ggsave(file.path(plotdir, "p2_1_SAS_template.png"), p2.1, unit="cm", width=19, height = 14, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p2_1_SAS_template.pdf"), p2.2, unit="cm", width=19, height = 14, dpi=1000, device="pdf")


## ---- p2.2 --> SAS - Jitterplot of SAS versus resource recovery per substance and selected sytems ----

p2.2p <- ggplot(data=props, aes(x=sysappscore, y= recovery_ratio_phosphor_mean)) +
  geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
  scale_fill_manual(values=template_cols, labels = template_names_short, str_wrap("Templates of Selected Systems", 20)) +
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=2) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha= 1)), fill=guide_legend(nrow=2,byrow=FALSE))+
  theme_minimal() +
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
  ggtitle("Phosphorus Recovery")

p2.2n <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_nitrogen_mean )) +
  geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
  scale_fill_manual(values=template_cols, labels = template_names_short, str_wrap("Templates of selected systems", 20)) +
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha= 1)), fill=guide_legend(nrow=2,byrow=FALSE))+
  theme_minimal() +
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
  ggtitle("Nitrogen Recovery")

p2.2ts <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_totalsolids_mean )) +
  geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
  scale_fill_manual(values=template_cols, labels = template_names_short, str_wrap("Templates of selected systems", 20)) +
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
  theme_minimal() +
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
  ggtitle("Total Solids Recovery")

p2.2h2o <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_water_mean )) +
  geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
  scale_fill_manual(values=template_cols, labels = template_names_short, str_wrap("Templates of selected systems", 20)) +
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
  theme_minimal() +
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
  ggtitle("Water Recovery Ratio")

p2.2h2om <- ggplot(data=props, aes(x=sysappscore, y=recovered_water_mean)) +
  geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
  geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
  scale_fill_manual(values=template_cols, labels = template_names_short, str_wrap("Templates of selected systems", 20)) +
  geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                  arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=2) +
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
  theme_minimal() +
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  labs(x = "System Appropriateness Score (SAS)", y="m3/year*person")+
  ggtitle("Water Recovery Volume")

## Create one Plot with shared legends from all plots 
## Use `p2.2` if you want to plot water recovery ratios
## If you want to plot water recovery volumes use `p2.2_h2om`
#p2.2 <-  grid_arrange_shared_legend(p2.2p, p2.2n, p2.2ts, p2.2h2o, ncol= 2, nrow=2, position="bottom")
p2.2_h2om <-  grid_arrange_shared_legend(p2.2p, p2.2n, p2.2ts, p2.2h2om, ncol= 2, nrow=2, position="bottom")

## save for screen view and ppt
ggsave(file.path(plotdir, "p2_2_SAS_recovery.png"), p2.2_h2om, unit="cm", width=19, height = 14, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p2_2_SAS_recovery.pdf"), p2.2_h2om, unit="cm", width=19, height = 14, dpi=1000, device="pdf")


##########################################################################
### ---- 3 - Plotting Resource Recovery (RR) potentials ----
## ---- p3.1 --> RR - Density plot for recovery for all four substances: total phosphosrus (TP), total nitrogen (TN), total solids (TS), and water (H2O) ----

## Labels
labstxt3.1<- expression(paste("Substance"))

p3.1p <- ggplot(props, aes(x=recovery_ratio_phosphor_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_fill_manual(labstxt3.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Phosphorus recovery")

p3.1n <- ggplot(props, aes(x=recovery_ratio_nitrogen_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_fill_manual(labstxt3.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Nitrogen recovery")

p3.1ts <- ggplot(props, aes(x=recovery_ratio_totalsolids_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_fill_manual(labstxt3.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Total solids recovery")

p3.1h2o <- ggplot(props, aes(x=recovery_ratio_water_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_fill_manual(labstxt3.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Water recovery")

p3.1h2om <- ggplot(props, aes(x=recovered_water_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_y_sqrt(labels = scientific)+
  scale_fill_manual(labstxt3.1)+
  # coord_cartesian(ylim=c(0,0.0075))+
  #  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  labs(x=expression(paste("volume [", m^3,year^-1,"]")), y= "density") +
  scale_x_continuous(labels = scientific) +
  ggtitle("Water recovery volume")

p3.1acc <- ggplot(props, aes(x=recovery_ratio_accumulated_balanced_mean)) + 
  geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
  scale_fill_manual(labstxt3.1)+
  # coord_cartesian(ylim=c(0,0.0075))+
  #  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Accumulated balanced recovery")

p3.1_h2om <-  grid.arrange(p3.1p, p3.1n, p3.1ts, p3.1h2om, ncol= 2, nrow=2)
p3.1_full<-  grid.arrange(p3.1p, p3.1n, p3.1ts, p3.1h2o, p3.1h2om, p3.1acc, ncol= 3, nrow=2)

## save for screen view and ppt
ggsave(file.path(plotdir, "p3_1_recovery_densityplot.png"), p3.1_h2om , unit="cm", width=19, height = 14, dpi=1000, device="png")
ggsave(file.path(plotdir, "p3_1_recovery_densityplot_full.png"), p3.1_full , unit="cm", width=19, height = 14, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p3_1_recovery_densityplot.pdf"), p3.1_h2om , unit="cm", width=19, height = 14, dpi=1000, device="pdf")
#ggsave(file.path(plotdir, "p3_1_recovery_densityplot_full.pdf"), p3.1_full , unit="cm", width=19, height = 14, dpi=1000, device="pdf")


## ---- p3.2 --> RR - Density plot for recovery for all four substances, grouped by source ----------

# Label
labstxt3.2<- expression(paste("Source"))

p3.2p <- ggplot(props, aes(x=recovery_ratio_phosphor_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Phosphorus recovery")

p3.2n <- ggplot(props, aes(x=recovery_ratio_nitrogen_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Nitrogen recovery")

p3.2ts <- ggplot(props, aes(x=recovery_ratio_totalsolids_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Total solids recovery")

p3.2h2o <- ggplot(props, aes(x=recovery_ratio_water_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Water recovery")

p3.2h2om <- ggplot(props, aes(x=recovered_water_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  # coord_cartesian(ylim=c(0,0.0075))+
  #  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  labs(x=expression(paste("volume [", m^3,year^-1,"]")), y= "density") +
  scale_x_continuous(labels = scientific) +
  ggtitle("Water recovery volume")

p3.2acc <- ggplot(props, aes(x=recovery_ratio_accumulated_balanced_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt3.2)+
  # coord_cartesian(ylim=c(0,0.0075))+
  #  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(#text=element_text(family="Arial"),
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="ratio [%]", y= "density") +
  ggtitle("Accumulated balanced recovery")


p3.2_h2om <-  grid_arrange_shared_legend(p3.2p, p3.2n, p3.2ts, p3.2h2om, ncol= 2, nrow=2, position= "bottom")
p3.2_full<- grid_arrange_shared_legend(p3.2p, p3.2n, p3.2ts, p3.2h2o, p3.2h2om, p3.2acc, ncol= 3, nrow=2)


## save for screen view and ppt
ggsave(file.path(plotdir, "p3_2_recovery_densityplot_sources.png"), p3.2_h2om , unit="cm", width=19, height = 14, dpi=1000, device="png")
ggsave(file.path(plotdir, "p3_2_recovery_densityplot_sources_full.png"), p3.2_full , unit="cm", width=19, height = 14, dpi=1000, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p3_2_recovery_densityplot_sources.pdf"), p3.2_h2om , unit="cm", width=19, height = 14, dpi=1000, device="pdf")
#ggsave(file.path(plotdir, "p3_2_recovery_densityplot_sources_full.pdf"), p3.2_full , unit="cm", width=19, height = 14, dpi=1000, device="pdf")


## ---- p3.3 --> RR - Boxplot of recovery for all substances grouped per template, colored by source ----
# 
## General Labels:
xtxt3.3 <- expression(paste("System template"))
labstxt3.3 <- expression(paste("Source"))
# 
## Preparation: Calculate Median of recovery ratios per template
dataMedian_template_p <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_phosphor_mean),2))
dataMedian_template_n <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_nitrogen_mean),2))
dataMedian_template_ts <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_totalsolids_mean),2))
dataMedian_template_h2o <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_water_mean),2))
dataMedian_template_h2om <- summarise(group_by(props, template), MD = round(median(recovered_water_mean),2))
dataMedian_template_acc <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_accumulated_mean),2))
# 

## Phosphor Recovery Ratio per Template
  p3.3p <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_phosphor_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_p, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
    labs(x=xtxt3.3, y= "ratio [-]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    ylim(0,100)+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Phosphorus Recovery")
  
## Nitrogen Recovery Ratio per Template 
  p3.3n <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_nitrogen_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_n, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
    labs(x=xtxt3.3, y= "ratio [-]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    ylim(0,100)+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Nitrogen Recovery")
  
##  Total Solids Recovery Ratio per Template
  p3.3ts <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_totalsolids_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_ts, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
    labs(x=xtxt3.3, y= "ratio [-]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    ylim(0,100)+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Total Solids Recovery")
  
##  Water Recovery Ratio per Template
  
  p3.3h2o <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_water_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_h2o, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
    labs(x=xtxt3.3, y= "ratio [-]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    ylim(0,100)+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Water Recovery Ratio")
  
## recovered Water per Template 
  
  p3.3h2om <- ggplot(data=props, aes(x=template, y=recovered_water_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_h2om, aes(template, MD, label = MD), size = 3, position = position_dodge(width = 0.9), vjust = -0.25)+
    labs(x=xtxt3.3, y= "recovered amount [m3/year*person]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Water Recovery Volume")
  
##  Accumulated Recovery Ratio per Template 
  
  p3.3acc <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_accumulated_mean))+
    geom_point(aes(color=source), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.3)+
    geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian_template_acc, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
    labs(x=xtxt3.3, y= "sum of ratio [-]") +
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
          panel.grid =   element_line(colour = "#ECECEC", size=0.25),
          plot.title = element_text(size = 9, face = "bold"),
          axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
          axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
          strip.text = element_text(size=8, face="bold"),
          legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
          legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    scale_x_discrete(labels = template_names_short)+
    theme(axis.text.x = element_text(size=8),
          legend.position = "right")+
    ggtitle("Accumulated Recovery")
  
  ## combine multiple plots
  p3.3_h2om <-  grid_arrange_shared_legend(p3.3p, p3.3n, p3.3ts, p3.3h2om, ncol= 1, nrow=4, position= "bottom")
  p3.3_full <-  grid_arrange_shared_legend(p3.3p, p3.3n, p3.3ts, p3.3h2o, p3.3h2om, p3.3acc, ncol= 1, nrow=6, position= "bottom")
 
  ## save for screen view and ppt
  ggsave(file.path(plotdir, "p3_3_recovery_per_template_boxplot.png"), p3.3_h2om , unit="cm", width=19, height = 19, dpi=1000, device="png")
  ggsave(file.path(plotdir, "p3_3_recovery_per_template_boxplot_full.png"), p3.3_full , unit="cm", width=19, height = 19, dpi=1000, device="png")
  ## save high res for print
  #ggsave(file.path(plotdir, "p3_3_recovery_per_template_boxplot.pdf"), p3.3_h2om , unit="cm", width=19, height = 14, dpi=1000, device="pdf")
  #ggsave(file.path(plotdir, "p3_3_recovery_per_template_boxplot_full.pdf"), p3.3_full , unit="cm", width=19, height = 14, dpi=1000, device="pdf")
  
  
## ---- p3.4 --> RR - Boxplot of all recovery ratio and losses grouped by source ----- -----
  
## Preparation for Plots: Melt dataframe and factor
  
  dt_source_absolute3 <- props[, c("source", "recovery_ratio_phosphor_mean", "lost_phosphor_air.loss_mean", "lost_phosphor_water.loss_mean", "lost_phosphor_soil.loss_mean", 
                                   "recovery_ratio_nitrogen_mean", "lost_nitrogen_air.loss_mean", "lost_nitrogen_water.loss_mean", "lost_nitrogen_soil.loss_mean",
                                   "recovery_ratio_totalsolids_mean", "lost_totalsolids_air.loss_mean", "lost_totalsolids_water.loss_mean", "lost_totalsolids_soil.loss_mean",
                                   "recovered_water_mean", "lost_water_air.loss_mean", "lost_water_water.loss_mean", "lost_water_soil.loss_mean")]
  dt_source_absolute3$recovered_water_mean <- dt_source_absolute3$recovered_water_mean
  dt_source_absolute3$lost_water_air.loss_mean <- dt_source_absolute3$lost_water_air.loss_mean
  dt_source_absolute3$lost_water_water.loss_mean <- dt_source_absolute3$lost_water_water.loss_mean
  dt_source_absolute3$lost_water_soil.loss_mean <- dt_source_absolute3$lost_water_soil.loss_mean
  
  dtmelt_source_absolute3 <- melt(dt_source_absolute3, id=c("source"))
  dtmelt_source_absolute3$variable <- factor(dtmelt_source_absolute3$variable , labels = c("TP~recovered~('ratio')","TP~lost~to~air~('ratio')","TP~lost~to~water~('ratio')","TP~lost~to~soil~('ratio')",
                                                                                           "TN~recovered~('ratio')","TN~lost~to~air~('ratio')","TN~lost~to~water~('ratio')","TN~lost~to~soil~('ratio')",
                                                                                           "TS~recovered~('ratio')","TS~lost~to~air~('ratio')","TS~lost~to~water~('ratio')","TS~lost~to~soil~('ratio')",
                                                                                           "H2O~recovered~( m3/y/pers)","H2O~lost~to~air~( m3/y/pers)","H2O~lost~to~water~( m3/y/pers)","H2O~lost~to~soil~( m3/y/pers)"))
  ## Preparation: Calculate Median
  
  dataMedian <- summarise(group_by(dtmelt_source_absolute3, source, variable), MD = round(median(value),2))
  
  ## Preparation: Add General Labels
  
  xtxt3.4 <- expression(paste(""))
  labstxt3.4 <- expression(paste("Source Technology"))
  
  p3.4 <- ggplot(data=dtmelt_source_absolute3, aes(x= source, y= value))+
    geom_point(aes(colour= factor(source)), alpha=0.5, size=0.5, position = position_jitter())+
    scale_colour_manual(values = source_cols) +
    geom_boxplot(varwidth= FALSE, alpha=0.5, lwd=0.25, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
    geom_text(data = dataMedian, aes(source, MD, label = MD),size = 2, position = position_dodge(width = 0.8), vjust = -0.75)+
    facet_wrap(~variable, labeller = label_parsed, scale="free_y")+
    labs(color=labstxt3.4, x= xtxt3.4, y ="substance recoverd/lost")+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme_minimal()+
    theme(#text=element_text(family="Arial"),
      panel.grid =   element_line(colour = "#ECECEC", size=0.25),
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
      axis.text.x = element_text(size=7, colour="#B1B1B1", angle = 90), axis.text.y = element_text(size=7, colour="#B1B1B1"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=7, colour="#B1B1B1"), legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))
  
  ## save for screen view and ppt
  ggsave(file.path(plotdir, "p3_4_sourceboxplot_ratio_h2om.png"), p3.4, unit="cm", width=19, height = 16, dpi=1000, device="png")
  ## save high res for print
  #ggsave(file.path(plotdir, "p3_4_sourceboxplot_ratio_h2om.pdf"), p3.4, unit="cm", width=19, height = 16, dpi=1000, device="pdf")
  
 
## ---- p3.5 --> RR - Recovery versus length of systems for all substance, grouped by system templates --------

##  Labels
  xtxt3.5 <- expression(paste("length (number of technologies within system)"))
  labstxt3.5 <- str_wrap("System Template", 10)


  p3.5p <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_phosphor_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt3.5)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt3.5, y= "recovery ratio [-]") +
    ggtitle("TP Recovery")+
    guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
      axis.title.y=element_text(size=7, colour = "#6F6F6E"),
      axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
      axis.text.y = element_text(size=7, colour = "#6F6F6E"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=5, colour="#B1B1B1"), 
      legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), 
      legend.key.size = unit(1,"line"))
  
  
  p3.5n <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_nitrogen_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt3.5)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt3.5, y= "recovery ratio [-]") +
    ggtitle("TN Recovery") +
    guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
      axis.title.y=element_text(size=7, colour = "#6F6F6E"),
      axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
      axis.text.y = element_text(size=7, colour = "#6F6F6E"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=5, colour="#B1B1B1"), 
      legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), 
      legend.key.size = unit(1,"line"))
  
  
  p3.5ts<- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_totalsolids_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt3.5)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt3.5, y= "recovery ratio [-]") +
    ggtitle("TS Recovery") +
    guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
      axis.title.y=element_text(size=7, colour = "#6F6F6E"),
      axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
      axis.text.y = element_text(size=7, colour = "#6F6F6E"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=5, colour="#B1B1B1"), 
      legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), 
      legend.key.size = unit(1,"line"))
  
  
  p3.5h2o <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_water_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt3.5)+
    ylim(0,1)+
    labs(x=xtxt3.5, y= "recovery ratio [-]") +
    ggtitle("H2O Recovery") + 
    guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
      axis.title.y=element_text(size=7, colour = "#6F6F6E"),
      axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
      axis.text.y = element_text(size=7, colour = "#6F6F6E"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=5, colour="#B1B1B1"), 
      legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), 
      legend.key.size = unit(1,"line"))
  
  
  
  p3.5h2om <- ggplot(data=props, aes(x=ntechs, y=recovered_water_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt3.5)+
    labs(x=xtxt3.5, y= "recovered water [m3/year*person]") +
    ggtitle("H2O Recovery") +
    guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
      axis.title.y=element_text(size=7, colour = "#6F6F6E"),
      axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
      axis.text.y = element_text(size=7, colour = "#6F6F6E"),
      strip.text = element_text(size=8, face="bold"),
      legend.text=element_text(size=5, colour="#B1B1B1"), 
      legend.position= "bottom",       
      legend.title = element_text(size=9, face = "bold"), 
      legend.key.size = unit(1,"line"))
  

##  Create one plot with shared legend from multiple Plots 
  
    p3.5 <- grid_arrange_shared_legend(p3.5p, p3.5n,  p3.5ts, p3.5h2o, nrow=2, ncol=2)
    p3.5_h2om <- grid_arrange_shared_legend(p3.5p, p3.5n,  p3.5ts, p3.5h2om, nrow=2, ncol=2)
  
    ## save for screen view and ppt
    ggsave(file.path(plotdir, "p3_5_ntech_templates_substance.png"), p3.5, unit="cm", width=19, height = 14, dpi=1000, device="png")
    ggsave(file.path(plotdir, "p3_5_ntech_templates_substance_h20m.png"), p3.5_h2om, unit="cm", width=19, height = 14, dpi=1000, device="png")
    
    ## save high res for print
    #ggsave(file.path(plotdir, "p3_5_ntech_templates_substance.pdf"), p3.5, unit="cm", width=19, height = 19, dpi=1000, device="pdf")
    #ggsave(file.path(plotdir, "p3_5_ntech_templates_substance_h20m.pdf"), p3.5_h2om, unit="cm", width=19, height = 19, dpi=1000, device="pdf")
  

## ---- p3.6 --> RR - Accumulated revoery versus length of systems, coloured by system template ----

##  Labels
    xtxt3.6 <- expression(paste("length (number of technologies within system)"))
    labstxt3.6 <- str_wrap("System Template", 10)
    
    
    p3.6 <- ggplot(data=props, aes(x=factor(ntechs), y=recovery_ratio_accumulated_mean))+
      geom_point(aes(color=template), alpha=0.5, size=1.2, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = template_names, labstxt3.6)+
      geom_boxplot(aes(group=ntechs), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
      labs(x=xtxt3.6, y= str_wrap("accumulated Recovery Ratio (sum of ratio for all four substances) [-]",25)) +
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
        axis.title.y=element_text(size=7, colour = "#6F6F6E"),
        axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
        axis.text.y = element_text(size=7, colour = "#6F6F6E"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=5, colour="#B1B1B1"), 
        legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), 
        legend.key.size = unit(1,"line"))
    p3.6
    
    ## save for screen view and ppt
    ggsave(file.path(plotdir, "p3_6_accumultedrecovery_ntech.png"), p3.6, unit="cm", width=19, height = 14, dpi=1000, device="png")
    ## save high res for print
    #ggsave(file.path(plotdir, "p3_6_accumultedrecovery_ntech.pdf"), p3.6, unit="cm", width=19, height = 10, dpi=1000, device="pdf")

    
    
## ---- p3.7 --> RR - Standard deviation of recovery against recovery, coloured by system template ----
    labstxt3.7 <- str_wrap("System Template", 10)
    ## Select upper limit of y-axis by maximum value (+0.01) of standart deviation:
    ylim3.7 <- 0.01 + max(props$recovery_ratio_nitrogen_sd, 
                   props$recovery_ratio_phosphor_sd, 
                   props$recovery_ratio_totalsolids_sd)
    

    p3.7p <- ggplot(data=props, aes(x=recovery_ratio_phosphor_mean, y=recovery_ratio_phosphor_sd))+
      geom_point(aes(colour=template), alpha=0.5, size=0.6, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = template_names, labstxt3.7)+
      labs(x="Ratio [-]",y="Standard Deviation [-]")+
      xlim(0,1)+
      ylim(0,ylim3.7)+
      ggtitle("Phosphorus Recovery")+
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
        axis.title.y=element_text(size=7, colour = "#6F6F6E"),
        axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
        axis.text.y = element_text(size=7, colour = "#6F6F6E"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=5, colour="#B1B1B1"), 
        legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), 
        legend.key.size = unit(1,"line"))
    
    
    p3.7n <- ggplot(data=props, aes(x=recovery_ratio_nitrogen_mean, y=recovery_ratio_nitrogen_sd))+
      geom_point(aes(colour=template), alpha=0.5, size=0.6, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = template_names, labstxt3.7)+
      labs(x="Ratio [-]",y="Standard Deviation [-]")+
      xlim(0,1)+
      ylim(0,ylim3.7)+
      ggtitle("Nitrogen Recovery")+
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
        axis.title.y=element_text(size=7, colour = "#6F6F6E"),
        axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
        axis.text.y = element_text(size=7, colour = "#6F6F6E"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=5, colour="#B1B1B1"), 
        legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), 
        legend.key.size = unit(1,"line"))
    
    p3.7ts <- ggplot(data=props, aes(x=recovery_ratio_totalsolids_mean, y=recovery_ratio_totalsolids_sd))+
      geom_point(aes(colour=template), alpha=0.5, size=0.6, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = template_names, labstxt3.7)+
      labs(x="Ratio [-]",y="Standard Deviation [-]")+
      xlim(0,1)+
      ylim(0,ylim3.7)+
      ggtitle("Total Solids Recovery")+
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
        axis.title.y=element_text(size=7, colour = "#6F6F6E"),
        axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
        axis.text.y = element_text(size=7, colour = "#6F6F6E"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=5, colour="#B1B1B1"), 
        legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), 
        legend.key.size = unit(1,"line"))
    
    p3.7h2om <- ggplot(data=props, aes(x=recovered_water_mean, y=recovered_water_sd))+
      geom_point(aes(colour=template), alpha=0.5, size=0.6, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = template_names, labstxt3.7)+
      labs(x="kg/person*year",y="Standard Deviation [m3/person*year]")+
      ggtitle("Water Recovery Volume")+
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1), nrow=4))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
        axis.title.y=element_text(size=7, colour = "#6F6F6E"),
        axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
        axis.text.y = element_text(size=7, colour = "#6F6F6E"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=5, colour="#B1B1B1"), 
        legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), 
        legend.key.size = unit(1,"line"))
    
    ## Create one Plot with shared legends from all plots 
    p3.7 <- grid_arrange_shared_legend(p3.7p, p3.7n, p3.7ts, p3.7h2om, nrow=2, ncol=2)
  
    ## save for screen view and ppt
    ggsave(file.path(plotdir, "p3_7_SD_ratio.png"), p3.7, unit="cm", width=19, height = 14, dpi=1000, device="png")
    ## save high res for print
    #ggsave(file.path(plotdir, "p3_7_SD_ratio.pdf"), p3.7, unit="cm", width=19, height = 14, dpi=1000, device="pdf")


    
    
    
