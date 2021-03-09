library(sf)
library(ggplot2)
link="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df_states <- as.data.frame(read.csv(file = url(link)))
nigeria_df_states <- nigeria_df_states[,c("wt_baseline", "state", "hhid")]

link="https://github.com/adam-porton/PubPol543/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_df_foodsec <- as.data.frame(read.csv(file = url(link)))
nigeria_df_foodsec$food_insecure <- as.integer(nigeria_df_foodsec$s8q8 == "1. YES")
nigeria_df_foodsec <- aggregate(food_insecure ~ hhid, nigeria_df_foodsec, max)
nigeria_df_foodsec <- merge(nigeria_df_states, nigeria_df_foodsec, by="hhid")
remove(nigeria_df_states)
nigeria_df_foodsec$food_insecure <- nigeria_df_foodsec$food_insecure*nigeria_df_foodsec$wt_baseline
nigeria_df_foodsec <- aggregate(cbind(food_insecure, wt_baseline)~state, nigeria_df_foodsec, sum)
nigeria_df_foodsec$food_insecure <- 100*(nigeria_df_foodsec$food_insecure/nigeria_df_foodsec$wt_baseline)
nigeria_df_foodsec <- nigeria_df_foodsec[,c("state", "food_insecure")]

nigeria_df_foodsec$state <- as.character(nigeria_df_foodsec$state)

nigeria_df_foodsec$state[(substring(nigeria_df_foodsec$state,2,2) == ".")] <-    substr(nigeria_df_foodsec$state[(substring(nigeria_df_foodsec$state,2,2) == ".")] , 4, length(nigeria_df_foodsec$state))

nigeria_df_foodsec$state[(substring(nigeria_df_foodsec$state,3,3) == ".")] <- substr(nigeria_df_foodsec$state[(substring(nigeria_df_foodsec$state,3,3) == ".")] , 5, length(nigeria_df_foodsec$state))

nigeria_df_foodsec$state[(nigeria_df_foodsec$state == "FCT")] <- "Fct, Abuja"

#Set Nasarawa to NA since very few respondents
nigeria_df_foodsec$food_insecure[(nigeria_df_foodsec$state=="Nasarawa")] <- NA

linkMap="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/nigeria_geojson.geojson" 

map_ng=read_sf(linkMap)

map_ng_vars=merge(map_ng, nigeria_df_foodsec, by='state') 

titletext <- "Pecentage of Nigerians who reported being food insecure, by state"
sourceText='Source: LSMS-Supported High-Frequency Phone Surveys on COVID-19; Grey = No data'

map=ggplot(data=map_ng) + geom_sf(fill='grey90',color=NA) + 
  theme_classic() +
  geom_sf(data=map_ng_vars, aes(fill=food_insecure), color=NA) + 
  scale_fill_gradient(low = 'blue', high= 'red',na.value = "grey50") +
  guides(fill=guide_legend(title="Percentage of people")) +
  ggtitle(titletext) +
  labs( caption = sourceText)
map

#Save out
ggsave("map_adam.png")
