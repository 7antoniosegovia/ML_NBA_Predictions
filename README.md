# Predicting NBA results with Machine Learning models


The aim of this [app](https://7antoniosegovia.shinyapps.io/NBA_predictions/) is to provide a visual interface for the results obtained in my experiment. I am trying to predict the results of NBA games from March 31, 2021 onwards. For that, I will use two machine learning models, a logistic regression model and a support vectors machine with linear kernel.  

Until March 31, there had been 695 games played in the NBA. Due to the schedule changes in this year's schedule because of the pandemic, each team will only play 72 games, instead of 82, as in a normal season. So, there will be a total of 1080 games played in the Regular Season. The idea is to train the models with these 695 games (~ 65%) and do "real-time testing" with the remaining games, updating the predictions and results every day.

For training, I have used data from all NBA games until March 31. Thanks to the package `nbastatR`, I have been able to scrape boxscore data and other stats with ease. I transformed the data and implemented functions to calculate moving averages for the teams' statistics in the last 10 games and to compute the ELO Ratings (more on ELO Ratings [here](https://fivethirtyeight.com/features/how-we-calculate-nba-elo-ratings/) and [here](https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/)). Therefore, the training dataset consists in 695 observations of 48 columns, 24 corresponding to the Away team and 24 corresponding to the Home Team. The variable I am trying to predict is `H_Win` (Home win), that takes the value 0 if the Away team wins, 1 if otherwise. Further information on the steps followed is available in the code ([GitHub](https://github.com/7antoniosegovia/ML_NBA_Predictions)).  

**WARNING:**  

* Don't try to access the app while there are NBA games currently running, it may not work properly.  
* Predictions for farther dates than the current day will not be as accurate, as the model will only take into account average stats for games until the current date.

