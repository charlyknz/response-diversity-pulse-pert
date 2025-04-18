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
