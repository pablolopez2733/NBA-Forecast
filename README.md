# Forecasting the NBA through regularized regression analysis
### Codes for BSc Thesis: Forecasting the NBA through regularized linear regression  
### Author: Pablo Lopez Landeros   
### Advisor: Dr. Abdolnasser Sadeghkhani   

Basketball is a dynamic sport in which two teams of five players each try to score as much points as possible while trying to receive the least possible amount. Although, there are five players per team on the court, an NBA roster consists of thirteen active players which can be substituted in and out of the game. As such, Basketball can be modeled as an interaction between multiple 5-man units each with an associated  net point contribution called plus minus. Such quantity is computed by calculating how many points each 5-man unit outscores their opponent while on the court. 


Since each of the 30 NBA teams play 82 games each season, for each team i, we define Ni as the number of different 5-man units the team used in a season. Let X be the 82 x Ni matrix for each team that indicates the number of seconds each 5-person unit plays in each game. 

y the 82x1 vector giving the margin of victory or defeat for each game. The objective then is to find b such that Xb = y. Thus, b will contain a plus-minus rate for every 5-person unit. BecauseNi > 82 for all teams, regularized regression models were adjusted using 2014-2015 NBA season play by play data.

Furthermore, we use playoffs data for the Golden State Warriors and the Cleveland Cavaliers to assess the models' predictive value. Our best model correctly predicts 15 out of 21 playoff games (72% accuracy) for Golden State and 13 out of 20 (65% accuracy) in Cleveland's case. 

We also discuss some applications for the proposed models such as player workload management and its ability to predict game outcomes. Lastly, we evaluate our best model´s ability to aid in trade evaluation scenarios. 



