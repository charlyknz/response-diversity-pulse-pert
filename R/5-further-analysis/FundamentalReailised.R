## Code in this file produces:
## Supplementary figures in Appendix S1 Box 1 
## calculating fundamental and realised responses for communities without interactions and correlating these

## source any required user defined functions
source(here("R/0-functions/my_auc.R"))
source(here("R/0-functions/Ross_et_al_functions.R"))

## sub-sample rate
keep_every_t <- 1

pack<-'pack1'
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
other_pars <- readRDS(here("data", pack, "other_pars.RDS"))

conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
#dbListTables(conn_dynamics)
dynamics <- tbl(conn_dynamics, "dynamics")

tot_biomass <- dynamics |>
  #collect()
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id,alpha_ij_sd, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  collect()


########



## read data
#temp <- readRDS(here("data", pack, "sim_results.RDS"))
#community_pars <- temp$community_pars
#dynamics <- temp$dynamics_long
species_igr_pert_effect <- readRDS(here("data", pack, "species_igr_pert_effect.RDS"))



## Community level stability ----

## Calculate and visualise the community level stability
## measures
## First for each time point in each case
comm_time_stab <- tot_biomass |>
  ungroup() %>% 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  #group_by(case_id, community_id, replicate_id, Time, Treatment) %>%
  #summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_RR = (Perturbed - Control) ) 


###########
## calculate stabilities (absolute abundance)

threshold <- other_pars$spp_RR_calc_threshold ## needed for database to have access
species_time_stab1 <- dynamics |>
  filter((Time %% keep_every_t) == 0) |> 
  pivot_wider(names_from = Treatment, values_from = Abundance) |> 
  ## threshold for calculating spp_RR ****IMPORTANT
  mutate(spp_RR = ifelse((Perturbed + Control) > threshold,
                         (Perturbed - Control)/(Perturbed + Control) ,
                         NA),
         LRR = ifelse((Perturbed + Control) > threshold,
                log(Perturbed/Control) ,
                NA),
         RR = ifelse((Perturbed + Control) > threshold,
                     (Perturbed - Control),
                     NA),
         total = (Perturbed+Control)
         ) |> 
 # select(-Control, -Perturbed) %>%
  collect() 


## calculate stabilities (relative abundance)
temp123 <- dynamics |>
  #filter(case_id %in% comms_without_nas) |> 
  filter((Time %% keep_every_t) == 0) |> 
  collect()

species_time_stab2 <- temp123 %>%
  full_join(tot_comm_ab) |> 
  mutate(pi = Abundance / tot_ab) |> 
  #filter(Time > 9999 & Time < 10051) |> 
  select(-Abundance, -tot_ab) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = pi) |> 
  # group_by(case_id, community_id) |> 
  #mutate(con.tot = sum(Control),
  #        treat.tot = sum(Perturbed))
  mutate(delta_pi = Perturbed - Control) |> 
  select(-Control, -Perturbed) |> 
  collect()


species_time_stab <- full_join(species_time_stab1, species_time_stab2)



species_stab <- species_time_stab |>
  #filter(!(case_id %in% temp$case_id & Species_ID %in% temp$Species_ID)) |> 
  #filter((Time %% keep_every_t) == 0) |> 
  drop_na(spp_RR) %>% 
 # filter(alpha_ij_sd == 0) %>% 
  mutate(USI = paste(case_id, replicate_id, Species_ID, alpha_ij_sd, sep ="_"))# |> 



## create USI to run loop
USIc <- unique(species_stab$USI)
names(species_stab)

#empty df
com.stab <- tibble()

for(i in 1:length(USIc)){
  temp<-species_stab[species_stab$USI==USIc[i], ]#creates a temporary data frame for each case
  if(dim(temp)[1]>3){#does the next step only if at least 3 data points are present
    OEV<-MESS::auc(x = temp$Time, temp$spp_RR, from = min(temp$Time, na.rm = TRUE), to = max(temp$Time, na.rm = TRUE),
             type = c("linear"),absolutearea = TRUE)
    species_RR_AUC  <-MESS::auc(x = temp$Time, temp$spp_RR, from = min(temp$Time, na.rm = TRUE), to = max(temp$Time, na.rm = TRUE),
                           type = c("linear"),absolutearea = FALSE)
    AUC.rr = MESS::auc(x = temp$Time, temp$RR, from = min(temp$Time, na.rm = TRUE), to = max(temp$Time, na.rm = TRUE),
                       type = c("linear"),absolutearea = FALSE)
    sum <- sum(temp$spp_RR)
    sum_diff = sum(temp$RR)
    com.stab<-rbind(com.stab,
                    data.frame(temp[1,c(1,2,4,7)],sum_diff,AUC.rr,
                               OEV,sum,species_RR_AUC))
    rm(temp)
  }
}

## Calculate response diversity from species dynamics
comm_indicies <- com.stab |> 
  mutate(abs.RR = abs(species_RR_AUC))|>
  group_by(replicate_id, case_id) |> 
  summarise(mean_species_RR_AUC = mean(species_RR_AUC, na.rm = TRUE),
            var_species_RR_AUC = var(species_RR_AUC, na.rm = TRUE),
            RD_diss_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = FALSE, na.rm = TRUE),
            RD_div_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = TRUE, na.rm = TRUE),
            mean_species_abs_RR = mean(AUC.rr),
            RD_diss_species_RR_abs = resp_div(AUC.rr, sign_sens = FALSE, na.rm = TRUE),
            RD_div_species_RR_abs = resp_div(AUC.rr, sign_sens = TRUE, na.rm = TRUE))

#### merge species responses in isolation (fundamental) and in community context (realised)

dummydata <- com.stab %>% 
  select(case_id, Species_ID, sum, sum_diff,species_RR_AUC,AUC.rr,alpha_ij_sd) %>% 
  mutate(species_id = str_replace(Species_ID, "Spp", "Spp-")) %>% 
  left_join(.,species_igr_pert_effect) 


# plot species realised responses against their fundamental responses
p1<-dummydata %>% 
  filter(alpha_ij_sd == 0) %>% 
ggplot(., aes(x = species_RR_AUC,y = igr_pert_effect))+
  geom_point()+theme_bw()

#in the next plot we can also look at the sum of species responses (p2), 
# difference in realised responses(p4), and their OEV (p3)
p2<-dummydata %>% 
  filter(alpha_ij_sd == 0) %>% 
  ggplot(., aes(x = sum,y = igr_pert_effect))+
  geom_point()+theme_bw()

p3<-dummydata %>% 
  filter(alpha_ij_sd == 0) %>% 
  ggplot(., aes(x = OEV,y = igr_pert_effect))+
  geom_point()+theme_bw()

p4<-dummydata %>% 
  filter(alpha_ij_sd == 0) %>% 
  ggplot(., aes(x = sum_diff,y = igr_pert_effect))+
  geom_point()+theme_bw()


cowplot::plot_grid(p1,p2,p3,p4,ncol = 2)
ggsave(plot = last_plot() , file = here("output/igr_realised.png"), width = 8, height = 8)


rm <- dummydata %>% 
  filter(  species_RR_AUC <155.44 & species_RR_AUC > 155.180) %>% 
  filter(Species_ID == "Spp3") %>% 
  left_join(., species_time_stab1) %>% 
  group_by(case_id) 


###check topt for spp
source(here("R/5-further-analysis/Spp_alpha_Bopt.R"))
data_bopt <-   AUC_info %>% 
  select(species_id, case_id, b_opt_i, replicate_id) %>% 
  rename(Species_ID = species_id)


rm <- rm %>% 
  left_join(.,data_bopt) #%>% 
#  left_join(., species_stab)


## Look at time series of species responses to get a grasp 
p1 <- rm %>% 
  mutate(igr = paste("IGReffect =", igr_pert_effect )) %>% 
  rename(difference_Pert_Ctrl = RR) %>% 
  pivot_longer(c(Perturbed,Control), names_to = "response", values_to="value") %>% 
  unique() %>% 
  ggplot(., aes(Time, y = value, color = as.factor(species_RR_AUC), linetype = as.factor(response), group = response))+
  geom_line(size = 0.7)+
  # geom_line(aes(x = Time, y = total), size = 0.6)+
  labs(col = "AUC" , y ="Biomass",linetype = "Response" )+
  facet_grid(~ igr)+
  theme_bw()
p1
#ggsave(plot = last_plot(), file = here("output/TimeSeriesRR.png"), width = 8, height = 4)



p2<-rm %>% 
  mutate(igr = paste("IGReffect =", igr_pert_effect )) %>% 
  rename(difference_Pert_Ctrl = RR) %>% 
  pivot_longer(c(total,difference_Pert_Ctrl), names_to = "response", values_to="value") %>% 
  unique() %>% 
  ggplot(., aes(Time, y = value, color = as.factor(species_RR_AUC), linetype = as.factor(response), group = response))+
  geom_line(size = 0.7)+
  # geom_line(aes(x = Time, y = total), size = 0.6)+
  labs(col = "AUC" , y ="Difference in biomass",linetype = "Response" )+
  facet_grid(~ igr)+
  theme_bw()
p2

p3<-rm %>% 
  mutate(igr = paste("IGReffect =", igr_pert_effect )) %>% 
  rename(difference_Pert_Ctrl = RR) %>% 
  #pivot_longer(c(spp_RR,difference_Pert_Ctrl), names_to = "response", values_to="value") %>% 
  unique() %>% 
  ggplot(., aes(Time, y = spp_RR, color = as.factor(species_RR_AUC), fill = as.factor(species_RR_AUC)))+
  geom_line(size = 0.7)+
  geom_area()+
  # geom_line(aes(x = Time, y = total), size = 0.6)+
  labs(col = "AUC" , fill  = "AUC" , y ="Area under the RR curve")+
  facet_grid(~ igr)+
  theme_bw()#+
 # theme(legend.position = "none")
p3
#ggsave(plot = last_plot(), file = here("output/TimeSeriesRR.png"), width = 8, height = 4)

cowplot::plot_grid(p1,p2,p3, ncol = 1)
ggsave(plot = last_plot(), file = here("output/AppendixFig_speciesTimeSeries.png"), width = 7, height = 8)

rm %>% 
  mutate(igr = paste("IGReffect =", igr_pert_effect )) %>% 
  rename(difference_Pert_Ctrl = RR) %>% 
  pivot_longer(c(total,difference_Pert_Ctrl), names_to = "response", values_to="value") %>% 
  unique() %>% 
ggplot(., aes(Time, y = value, color = as.factor(species_RR_AUC), linetype = as.factor(response), group = response))+
  geom_line(size = 0.7)+
 # geom_line(aes(x = Time, y = total), size = 0.6)+
  labs(col = "AUC" , y ="Difference in b",linetype = "Response" )+
  facet_grid(~ igr)+
  theme_bw()
ggsave(plot = last_plot(), file = here("output/TimeSeriesRR.png"), width = 8, height = 4)

ggplot(rm, aes(x = Time, y = spp_RR,color = as.factor(species_RR_AUC)))+
  geom_line()+
  labs(col = "AUC")+
  facet_grid(~ igr_pert_effect)

ggplot(rm, aes(x = Time, y = RR, color = as.factor(species_RR_AUC)))+
  geom_line()+
  labs(col = "AUC")+
  facet_grid(~ igr_pert_effect)

ggsave(plot = last_plot(), file = here("output/ExploreRR.png"), width = 8, height = 4)

###,
## Calculate response diversity from response curve traits
igr_respdiv <- species_igr_pert_effect |> 
  group_by(case_id) |> 
  summarise(mean_igr_effect = mean(igr_pert_effect),
            var_igr_effect = var(igr_pert_effect),
            RD_diss_igr_effect = resp_div(igr_pert_effect, sign_sens = FALSE),
            RD_div_igr_effect = resp_div(igr_pert_effect, sign_sens = TRUE))

## merge with comm stability measures 
comm_all <- full_join(comm_stab, comm_indicies) |> 
  full_join(igr_respdiv) |> 
  full_join(expt)

#saveRDS(comm_all, here("data", pack, "community_measures.RDS"))

comm_all %>% 
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_species_RR_AUC, y = RD_div_species_RR_AUC))+
  facet_grid(~alpha_ij_sd)+
  geom_point()+theme_bw()

comm_all %>% 
  filter(alpha_ij_sd %in%c(0,0.25,0.5)) %>% 
  ggplot(., aes( x = RD_diss_igr_effect, y = RD_div_igr_effect))+
  geom_point()+theme_bw()+
  facet_grid(~alpha_ij_sd)+
  geom_smooth()
## merge and save species level measures
species_igr_pert_effect <-
  species_igr_pert_effect |> 
  mutate(species_id = str_replace(species_id, "-", ""))
species_stab <- species_stab |> 
  rename(species_id = Species_ID)
species_all <- full_join(species_igr_pert_effect, species_stab) |> 
  full_join(expt)


# To create figures in Box1 in Appendix 1 run 
# Run manually to explore the effect of Temperature optima and competitiveness on species responses
source(here('R/5-further-analysis/Spp_alpha_Bopt.R'))

 