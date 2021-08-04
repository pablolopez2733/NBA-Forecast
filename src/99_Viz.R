###############################################################################
############## SCRIPT FOR GENERATING DATA VISUALIZATIONS ######################
###############################################################################


# ESTIMATED POINTS VS ACTUAL POINTS FOR ELASTIC NET REGRESSION for CLE=================
df <- as.data.frame(cbind(y_train,predictions_train))

cle_en <- ggplot(df, aes(x = y_train, y = predictions_train))+
  geom_point(aes(x=y_train,y=predictions_train),color = "yellow") +
  stat_smooth(geom='line', alpha=0.8, se=FALSE, method='lm',colour="#b36bff")+
  dark_theme_gray() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, family = "Calibri",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 11, family = "Calibri",color = "white",hjust = 0),
    axis.title = element_text(size = 9, family = "Calibri",color = "#adadad"),
    plot.caption = element_text(size=8, family = "Serif",color = "#adadad",hjust = 0),
    axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
  labs(x='Actual point differential', y='Predicted point differential',
       title = "Predicted Score Differential vs Actual Score Differential",
       subtitle = 'R^2 = 0.8814 | Team: Los Angeles Lakers | 2014-2015 Season',
       caption = "Model: elastic net regression | Data: In-sample predictions" )

cle_en

# Table of number of different lineups per team---------------------------------
names <- c("Team",
           "Lineups")
team_usage <- data.frame()
for (k in names) team_usage[[k]] <- as.character()
numrows <- function(t)
{
  x <- x_matrix(t)
  r <- ncol(x)
  res <- data.frame(Team = t,
                    Lineups = r)
  return(res)
}

for (t in teams) {
  team_usage <- rbind(team_usage,numrows(t))
  print(paste0("Progress: ",nrow(team_usage)," out of 30 teams"))
}
write.csv(team_usage,"outputs/different_lineups.csv")

# gtable of elastic net fits
df <- read.csv("outputs/elasticnet_seasonfits.csv")
df %>%
  select(TEAM,R2_pred,alpha,lambda) %>% 
  gt() %>% 
  cols_label(
    R2_pred = "R^2",
    TEAM = "Team",
    alpha = "Alpha",
    lambda = "Lambda"
  ) %>% 
  # cols_hide(
  #   columns = vars(
  #     pass_p, plays,assists,goals,takeaways)
  # ) %>% 
  tab_header( 
    title = "Results for in-sample predictions using an elastic net model", # ...with this title
    subtitle = "Season: 2014-2015") %>% 
  #nxt
  data_color( # Update cell colors...
    columns = vars(R2_pred), # ...for dose column 
    colors = scales::col_numeric( # <- bc it's numeric
      palette = c(
        "white","Orange"), # A color scheme (gradient)
      domain = c(0.5,1) # Column scale endpoints
    )) %>% 
  cols_align(
    align = "center",
    columns = vars(R2_pred,TEAM,alpha,lambda)
  ) %>% 
  cols_move_to_end(
    columns = vars(R2_pred)
  ) %>% 
  tab_source_note(
    source_note = md("Data: nbastatR")
  ) %>% 
  
  gtsave("elastic_insample.png",path = paste0(getwd(),"/plots"))

