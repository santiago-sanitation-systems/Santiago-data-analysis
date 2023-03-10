## clear memory
rm(list=ls()) 

## Set the working directory to the current directory
sourcedir=dirname(rstudioapi::getSourceEditorContext()$path)
setwd=sourcedir
## Insert the runname as it appears in your output folder where you have all the .Rdata files
runname = "runname_analysis_per_zone"

### define the path to your output folder from this run
rundir<-file.path("../Santiago-runfolder/output", runname)
## Run the helpers script. It defines important variables and also loads the prepared data from the _Prep_ script.
source("Santiago-Data-Helpers-Zones.R")


##########################################################################
### ---- 1 - Plotting Technology Appropriateness Scores (TAS) ----

## ---- p1.2 --> TAS - Overview on all TAS per technology ####

## factor fgs for setting order
tas_components_df$FG <- factor(tas_components_df$FG, levels=c('U','S','C','T','D'))

p1.2 <- ggplot(tas_components_df, aes(x=tech,y=TAS) )+
   geom_point(size=0.5, aes(colour=zone), alpha=0.8, stroke=0.5) +
  scale_shape_manual(values=c(2, 0, 1))+#,fill=fgcol) +
  theme_minimal() +
  guides(colour = guide_legend(title="Type of zone", order=1, ncol=5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
    axis.text.x = element_text(size=8.5, colour = "#6F6F6E",  angle = 80, hjust = 1),
    axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
    strip.text = element_text(size=8.5, face="bold"),
    legend.position= "bottom",       
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

xtxt1.3 <- expression(paste("Attribute Appropriateness Scores followed by Technology Appropriateness Score ", italic("TAS")))
ytxt1.3 <- expression(paste("score"))

p1.3 <- ggplot(tas_components_df_long, aes(x=variable, y=value)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap(~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=3, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=7, colour = "#6F6F6E"),
    axis.title.y= element_text(size=7, colour = "#6F6F6E"),
    axis.text.x = element_text(size=6, colour = "#6F6F6E", angle = 80, vjust = 1, hjust=1),
    axis.text.y = element_text(size=7, colour = "#6F6F6E"),
    strip.text = element_text(size=6, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 7, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  xlab(xtxt1.3) +
  ylab(ytxt1.3) +
  theme(panel.border=element_rect(colour="grey", fill=NA)) 
p1.3
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_allTechAppProfiles.png"), p1.3, unit="cm", width=25, height = 40, dpi=1000, device="png")

## Functional Group User Interface (FG U)
p1.3U <- ggplot(subset(tas_components_df_long, FG == "U"), aes(x=variable, y=value, fill=tech)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap(~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies from FG User's interface")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=3, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=7, colour = "#6F6F6E"),
    axis.title.y= element_text(size=7, colour = "#6F6F6E"),
    axis.text.x = element_text(size=6, colour = "#6F6F6E", angle = 80, vjust = 1, hjust=1),
    axis.text.y = element_text(size=7, colour = "#6F6F6E"),
    strip.text = element_text(size=6, face="bold"),
    legend.position= "bottom",       
    legend.title = element_text(size = 7, colour = "#1D1D1B", face = "bold"),
    legend.text=element_text(size=8.5, colour="#6F6F6E"),
    legend.key.size = unit(1,"line"))+
  xlab(xtxt1.3) +
  ylab(ytxt1.3) +
  theme(panel.border=element_rect(colour="grey", fill=NA)) 
p1.3U
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_U.png"), p1.3U, unit="cm", width=12, height = 8, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_U.png"), p1.3U, unit="cm", width=12, height = 8, dpi=1000, device="pdf")


#Techs Appropriate Profile of FG Storage and Containment
p1.3S <- ggplot(subset(tas_components_df_long, FG == "S"), aes(x=variable, y=value, fill=tech)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap( ~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies from FG Storage and Containment")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=4, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y= element_text(size=8.5, colour = "#6F6F6E"),
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
p1.3S
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_S.png"), p1.3S, unit="cm", width=23, height = 15, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_S.png"), p1.3S, unit="cm", width=23, height = 15, dpi=1000, device="pdf")

#Techs Appropriate Profile of FG Storage and Containment
p1.3C <- ggplot(subset(tas_components_df_long, FG == "C"), aes(x=variable, y=value, fill=tech)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap( ~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies from FG Conveyance")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=4, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y= element_text(size=8.5, colour = "#6F6F6E"),
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
p1.3C
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_C.png"), p1.3C, unit="cm", width=23, height = 10, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_C.png"), p1.3C, unit="cm", width=23, height = 10, dpi=1000, device="pdf")

#Techs Appropriate Profile of FG Treatment
p1.3T <- ggplot(subset(tas_components_df_long, FG == "T"), aes(x=variable, y=value, fill=tech)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap( ~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies from FG Treatment")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=4, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y= element_text(size=8.5, colour = "#6F6F6E"),
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
p1.3T
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_T.png"), p1.3T, unit="cm", width=23, height = 23, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_T.png"), p1.3T, unit="cm", width=23, height = 23, dpi=1000, device="pdf")

#Techs Appropriate Profile of FG Disposal and Reuse
p1.3D <- ggplot(subset(tas_components_df_long, FG == "D"), aes(x=variable, y=value, fill=tech)) +
  geom_point(size=1.25, aes(colour=zone), alpha=0.8, stroke=0.5) +
  #scale_color_manual(values=c("#e9a198", "#ffb266", "#ff9933", "#85c680", "#5cb356", "#4c93c3", "#fb61d7"))+
  facet_wrap( ~ tech, ncol=4) +
  theme_minimal() + 
  ggtitle("Technologies from FG Disposal & Reuse")+
  guides(colour = guide_legend("Type of zone", override.aes = list(size=4, alpha= 1), nrow = 1), fill = FALSE) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.5))+
  theme(#text=element_text(family="Arial"),
    panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
    plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
    axis.title.x= element_text(size=8.5, colour = "#6F6F6E"),
    axis.title.y= element_text(size=8.5, colour = "#6F6F6E"),
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
p1.3D
## save for screen view and ppt
ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_D.png"), p1.3D, unit="cm", width=23, height = 18, dpi=300, device="png")
## save high res for print
#ggsave(file.path(plotdir, "p1_3_TechAppProfiles_FG_D.png"), p1.3D, unit="cm", width=23, height = 18, dpi=1000, device="pdf")
