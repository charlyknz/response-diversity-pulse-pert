#### import data ####

library(tidyverse)
library(here)
library(cowplot)

pack <- 'pack1'
### read data ###
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
speciesData <- readRDS(here("data", pack, "species_measures.RDS"))
communityData <- readRDS(here("data", pack, "community_measures.RDS"))


str(speciesData)
str(communityData)

### species contributions ###
sample(speciesData$community_id, 10)
speciesData %>%
  #filter(community_id %in% c("Comm-5630" , "Comm-7528",  "Comm-2058" , "Comm-4940" , "Comm-13927" ,"Comm-12409" ,"Comm-5936" ,  "Comm-13770" ,"Comm-10428", "Comm-4303" ))%>%
  ggplot(., aes ( x = species_RR_AUC, y = species_delta_pi_AUC, alpha = alpha_ij_sd))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  geom_point()+
  labs(x = 'Absolute Contribution', y = 'Relative Contribution')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+ # scale_y_continuous(limits = c(-0.00002,0.00004), breaks = c(-0.00002,-0.00001,0,0.00001, 0.00002,0.00004))+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))
#ggsave(plot = last_plot(), file = here('output/ModelContributions_001.png'), width = 15, height = 15)

str(communityData)

#### Fundamental response traits and communtiy instability ####

p2<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = mean_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Mean Fundamental Response', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p3<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_diss_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Fundamental Response Dissimilarity', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p4<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_div_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Fundamental Response Divergence', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_a<-get_legend(p4)
cowplot:: plot_grid(p2, p3,p4+theme(legend.position = 'none'),labels = c(' ', ' ', ' ', 'd)'), ncol = 3)
ggsave(plot = last_plot(), file = here('output/Figure2_IGR_Instab.tiff'), width = 10.5, height = 8.5)


#### Realised response traits and community stability ####

p5<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = mean_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Mean Realised Response', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p6<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_diss_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Realised Response Dissimilarity', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p7<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_div_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Realised Response Divergence', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

cowplot:: plot_grid(p5,p6,p7, labels = c(' ', ' ', ' '), rel_heights = c(2,2), ncol = 3)
ggsave(plot = last_plot(), file = here('output/Figure3_Realised_Traits_Instab.tiff'), width = 10, height = 8)



#### Supplementary Figures ####
p2<-communityData %>%
  #filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Mean Fundamental Response', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p3<-communityData %>%
  # filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_diss_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Fundamental Response Dissimilarity', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p4<-communityData %>%
  #filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_div_igr_effect, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Fundamental Response Divergence', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_a<-get_legend(p4)
cowplot:: plot_grid(p2, p3,p4+theme(legend.position = 'none'),labels = c('(a)', '(b)', '(c)', '(d)'), ncol = 2)
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_IGR_Instab.png'), width = 15, height = 18)



p5<-communityData %>%
  # filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Mean Realised Response', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p6<-communityData %>%
  #filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_diss_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Realised Response Dissimilarity', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p7<-communityData %>%
  # filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_div_species_RR_AUC, y = comm_RR_AUC))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(x = 'Realised Response Divergence', y = 'OEV')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

cowplot:: plot_grid(p5,p6,p7, labels = c('(a)', '(b)', '(c)'), rel_heights = c(2,2), ncol = 2)
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_Realised_Instab.png'), width = 15, height = 18)


#### Response traits correlations ####

## fundamental ##
p8<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_igr_effect, y = RD_diss_igr_effect ))+
  geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Fundamental Response Dissimilarity', x = 'Mean Fundamental Response')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p9<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_igr_effect, y = RD_div_igr_effect ))+
  geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Fundamental Response Divergence', x = 'Mean Fundamental Response')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_p4 <- get_legend(p4)
cowplot:: plot_grid(p9+theme(legend.position = 'none'),p8,ncol = 2, labels = c('(a)', '(b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_IGR_Traits.png'), width = 15, height = 8)

## realised ##
p8a<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_species_RR_AUC, y = RD_diss_species_RR_AUC ))+
  geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Realised Response Dissimilarity', x = 'Mean Realised Response')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p9a<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = mean_species_RR_AUC, y = RD_div_species_RR_AUC ))+
  geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Realised Response Divergence', x = 'Mean Realised Response')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_p9 <- get_legend(p9a)
cowplot:: plot_grid(p9a+theme(legend.position = 'none'),p8a,ncol = 2, labels = c('(a)', '(b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_Realised_Traits.png'), width = 15, height = 8)


cowplot:: plot_grid(p9+theme(legend.position = 'none'),p8,p8a,p9a+theme(legend.position = 'none'),nrow = 2,ncol = 2, labels = c('(a)', '(b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_IGR_Real_Traits.png'), width = 15, height = 8)


p10<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_diss_species_RR_AUC, y = RD_div_species_RR_AUC ))+
  # geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Realised Response Divergence', x = 'Realised Response Dissimilarity')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p10a<-communityData %>%
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes ( x = RD_diss_igr_effect, y = RD_div_igr_effect ))+
  #geom_vline(xintercept = 0)+
  geom_point(alpha = 0.5)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  labs(y = 'Fundamental Response Divergence', x = 'Fundamental Response Dissimilarity')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

cowplot:: plot_grid(p10,p10a,ncol = 2, labels = c('(a)', '(b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_RD.png'), width = 10, height = 4)

communityData %>% 
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggpubr::ggscatter(., x = "RD_diss_igr_effect", y= "RD_div_igr_effect", cor.method = "spearman", cor.coef=T)

communityData %>% 
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggpubr::ggscatter(., x = "RD_diss_species_RR_AUC", y= "RD_div_species_RR_AUC", cor.method = "spearman", cor.coef=T)


#### absolute OEV ####
#### Fundamental responses and abs communtiy instability ####

p11<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = mean_igr_effect, y = OEV))+
  #geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Mean Fundamental Response', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p12<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_diss_igr_effect, y = OEV))+
  #  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Fundamental Response Dissimilarity', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p13<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_div_igr_effect, y = OEV))+
  geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Fundamental Response Divergence', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_a<-get_legend(p13)
cowplot:: plot_grid(p11, p12,p13+theme(legend.position = 'none'),labels = c('(a)', '(b)', '(c)', 'd)'), ncol = 3)
ggsave(plot = last_plot(), file = here('output/Appendix_IGR_absOEV.png'), width = 10.5, height = 8.5)


#### Realised responses and abs community stability ####

p14<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = mean_species_RR_AUC, y = OEV))+
  # geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#F8766D')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Mean Realised Response', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p15<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_diss_species_RR_AUC, y = OEV))+
  #geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#00BA38')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Realised Response Dissimilarity', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p16<-communityData %>%
  filter(alpha_ij_sd %in% c(0, 0.25,0.5))%>%
  ggplot(., aes ( x = RD_div_species_RR_AUC, y = OEV))+
  #geom_hline(yintercept=0)+
  geom_point(alpha = 0.5, color = '#619CFF')+
  facet_wrap(~alpha_ij_sd, ncol = 1)+
  labs(x = 'Realised Response Divergence', y = 'abs(OEV)')+
  theme_bw()+
  theme(axis.title.y=element_text(size=13, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=13,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

cowplot:: plot_grid(p14,p15,p16, labels = c('(a)', '(b)', '(c)'), rel_heights = c(2,2), ncol = 3)
ggsave(plot = last_plot(), file = here('output/Appendix_Realised_absOEV.png'), width = 10, height = 8)

#### Exploring low divergence #### 
str(communityData)
div<- communityData %>% 
  filter(RD_div_species_RR_AUC == 0 & OEV>20) %>%
  left_join(., speciesData)

Comm<-filter(div, case_id == 'Comm-10003-rep-4')


#### Appendix: Stability metrics ####
library(RSQLite)
#For stability calculation, we need the time series of community dynamics

## sub-sample rate
keep_every_t <- 1

pack<-'pack1'
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
other_pars <- readRDS(here("data", pack, "other_pars.RDS"))
conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))

dynamics <- tbl(conn_dynamics, "dynamics")

tot_biomass <- dynamics |>
  #collect()
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id,alpha_ij_sd, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  collect()

## RR measures
## First for each time point in each case
comm_time_stab <- tot_biomass |>
  ungroup() %>% 
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_RR = (Perturbed - Control) ) 

# RESISTANCE
Resistance <- comm_time_stab %>% 
  filter(Time == 502)%>% 
  rename(Resistance = comm_RR) %>% 
  select(case_id, alpha_ij_sd, replicate_id, Resistance)

# FINAL RECOVERY
Recovery <- comm_time_stab %>% 
  filter(Time == 700) %>% 
  rename(Recovery = comm_RR) %>% 
  select(case_id, alpha_ij_sd, replicate_id, Recovery)

# TEMPORAL VARIABILITY
CV <- comm_time_stab %>%
  group_by(case_id, replicate_id, alpha_ij_sd) %>% 
  reframe(mean = mean(comm_RR,na.rm = T),
          sd = sd(comm_RR,na.rm = T),
          CV = mean/sd)  

# RESILIENCE
resil <- comm_time_stab %>% 
  filter(Time > 502) 

#create an empty data frame
slope.RD<-tibble()
caseID <- unique(resil$case_id)

# the following loop cycles through all unique cases
for(i in 1:length(caseID)){
  temp<-resil[resil$case_id==caseID[i], ]#creates a temporary data frame for each case
  if(dim(temp)[1]>2){#does the next step only if at least 3 data points are present
    lm1<-lm(comm_RR~log(Time+1), temp)#makes a linear regreassion
    intcp.lm.RD <- coef(summary(lm1))[1, 1]#selects the intercept
    se.intcp.lm.RD<- coef(summary(lm1))[1, 2]#selects its standard error
    resil.lm.RD <- coef(summary(lm1))[2, 1]#selects the slope
    se.slp.lm.RD<- coef(summary(lm1))[2, 2]#selects its standard error
    sd.res.lm.RD<- sd(resid(lm1)) #selects the standard deviation of the residuals
    temp.stab.lm.RD<-1/sd.res.lm.RD
    p.lm.RD<-anova(lm1)$'Pr(>F)'[1]#gives the p-value
    slope.RD<-rbind(slope.RD,data.frame(temp[1,"case_id"],intcp.lm.RD, se.intcp.lm.RD, resil.lm.RD, se.slp.lm.RD, sd.res.lm.RD,temp.stab.lm.RD,p.lm.RD))
    rm(temp)
  }
}

names(slope.RD)
summary(slope.RD)

# combine all
com.stab.MA.all <- Recovery %>%
  left_join(., Resistance) %>%
  right_join(., CV)  %>% 
  left_join(., slope.RD) %>% 
  select(-c(se.intcp.lm.RD, se.slp.lm.RD,sd.res.lm.RD, p.lm.RD, intcp.lm.RD))

# join with RD data
str(communityData )

plot1<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_species_RR_AUC, y = Recovery))+
  labs(x = 'Realised Mean Response')+
  geom_point(color = '#F8766D')+theme_bw()

plot2<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_species_RR_AUC, y = Recovery))+
  labs(x = 'Realised Response Dissimilarity')+
  geom_point(color = '#00BA38')+theme_bw()

plot3<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_species_RR_AUC, y = Recovery))+
  labs(x = 'Realised Response Divergence')+
  geom_point(color = '#619CFF')+theme_bw()

plot4<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_species_RR_AUC, y = Resistance))+
  labs(x = 'Realised Mean Response')+
  geom_point(color = '#F8766D')+theme_bw()

plot5<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_species_RR_AUC, y = Resistance))+
  labs(x = 'Realised Response Dissimilarity')+
  geom_point(color = '#00BA38')+theme_bw()

plot6<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_species_RR_AUC, y = Resistance))+
  labs(x = 'Realised Response Divergence')+
  geom_point(color = '#619CFF')+theme_bw()

plot7<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_species_RR_AUC, y = CV))+
  labs(x = 'Realised Mean Response',y = "Temporal Stability")+
  geom_point(color = '#F8766D')+theme_bw()

plot8<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_species_RR_AUC, y = CV))+
  labs(x = 'Realised Response Dissimilarity',y = "Temporal Stability")+
  geom_point(color = '#00BA38')+theme_bw()

plot9<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_species_RR_AUC, y = CV))+
  labs(x = 'Realised Response Divergence',y = "Temporal Stability")+
  geom_point(color = '#619CFF')+theme_bw()


plot10<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_species_RR_AUC, y = resil.lm.RD))+
  labs(y="Resilience", x = 'Realised Mean Response')+
  geom_point(color = '#F8766D')+theme_bw()

plot11<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_species_RR_AUC, y = resil.lm.RD))+
  labs(y="Resilience",x = "Realised Response Dissimilarity")+
  geom_point(color = '#00BA38')+theme_bw()

plot12<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_species_RR_AUC, y = resil.lm.RD))+
  labs(y="Resilience",x = "Realised Response Divergence")+
  geom_point(color = '#619CFF')+theme_bw()

cowplot::plot_grid(plot4,plot5,plot6,plot10,plot11,plot12, plot7,plot8,plot9,plot1,plot2,plot3,
                   labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)", "j)", "k)", "l)"),
                   ncol = 3)
ggsave(plot = last_plot(), file = here("output/Stability_RealisedRD_alpha.png"), width = 15, height = 17)



#### fundamental response ###
plot1<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_igr_effect, y = Recovery))+
  labs(x = "Fundamental Mean Response")+
  geom_point(color = '#F8766D')+theme_bw()

plot2<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_igr_effect, y = Recovery))+
  labs(x = "Fundamental Response Dissimilarity")+
  geom_point(color = '#00BA38')+theme_bw()

plot3<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_igr_effect, y = Recovery))+
  labs(x = "Fundamental Response Divergence")+
  geom_point(color = '#619CFF')+theme_bw()

plot4<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_igr_effect, y = Resistance))+
  labs(x = "Fundamental Mean Response")+
  geom_point(color = '#F8766D')+theme_bw()

plot5<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_igr_effect, y = Resistance))+
  labs(x = "Fundamental Response Dissimilarity")+
  geom_point(color = '#00BA38')+theme_bw()

plot6<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_igr_effect, y = Resistance))+
  labs(x = "Fundamental Response Divergence")+
  geom_point(color = '#619CFF')+theme_bw()

plot7<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_igr_effect, y = CV))+
  labs(x = "Fundamental Mean Response", y = "Temporal Stability")+
  geom_point(color = '#F8766D')+theme_bw()

plot8<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>%  
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_igr_effect, y = CV))+
  labs(x = "Fundamental Response Dissimilarity", y = "Temporal Stability")+
  geom_point(color = '#00BA38')+theme_bw()

plot9<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_igr_effect, y = CV))+
  labs(x = "Fundamental Response Divergence", y = "Temporal Stability")+
  geom_point(color = '#619CFF')+theme_bw()


plot10<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = mean_igr_effect, y = resil.lm.RD))+
  labs(y="Resilience", x = "Fundamental Mean Response")+
  geom_point(color = '#F8766D')+theme_bw()

plot11<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_igr_effect, y = resil.lm.RD))+
  labs(y="Resilience", x = "Fundamental Response Dissimilarity")+
  geom_point(color = '#00BA38')+theme_bw()

plot12<-com.stab.MA.all %>% 
  left_join(.,communityData ) %>% 
  filter(alpha_ij_sd %in%c(0.25,0.5)) %>% 
  ggplot(., aes( x = RD_div_igr_effect, y = resil.lm.RD))+
  labs(y="Resilience",x = "Fundamental Response Divergence")+
  geom_point(color = '#619CFF')+
  theme_bw()

cowplot::plot_grid(plot4,plot5,plot6,plot10,plot11,plot12, plot7,plot8,plot9,plot1,plot2,plot3,
                   labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)", "j)", "k)", "l)"),
                   ncol = 3)
ggsave(plot = last_plot(), file = here("output/Stability_FundamentalRD_alpha.png"), width = 15, height = 17)

