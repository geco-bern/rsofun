library(tidyverse)

dat = read.csv("/home/jjoshi/Downloads/try_hydraulics_tidy.csv") %>% select(-X)
categ = read.csv("/home/jjoshi/Downloads/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.csv", na.strings = c("", "NA")) %>% select(-X)

a = dat %>% left_join(categ)

b = a %>% 
  rename(P50Xylem = `Xylem.water.potential.at.which.50..of.conductivity.is.lost..P50...MPa...2099.`) %>% 
  select(PlantGrowthForm, LeafType, LeafPhenology, PhotosyntheticPathway, AccSpeciesID, AccSpeciesName, P50Xylem) %>% 
  filter(!is.na(P50Xylem))

b %>% drop_na %>% filter(PhotosyntheticPathway == "C4") %>% View()

b %>% 
  drop_na %>% 
  group_by(PlantGrowthForm, LeafType, LeafPhenology, PhotosyntheticPathway) %>% 
  summarize(P50_mean = mean(P50Xylem), 
            P50_sd = sd(P50Xylem),
            P50_count = length(P50Xylem)) %>% 
  filter(LeafType != "scale-shaped" &
         LeafPhenology != "deciduous/evergreen" &
         PhotosyntheticPathway != "C3/CAM" & 
         P50_count > 2
         ) %>% 
  write.csv("~/codes/rsofun/vignettes/ancillary_data/P50X2.csv", row.names = F)
