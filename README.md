# MLB World Series Team Segmentation Using K-Means Clustering

### By: Joseph Annand, James Keegan, and Steven Martinez

## Project Details and Overview

Each year millions of people participate in sports betting with the number of  users of popular sports betting websites, like FanDuel, growing each year (Statista Research Department, 2024). Bettors are able to put money on the line when predicting which teams will make the playoffs and win championships. Baseball is one of the oldest sports that has been consistently played at the professional level. The World Series, the championship series for Major League Baseball, has been played since 1903. Since before then, statistics for player and team performance have been recorded (National Baseball Hall of Fame). In recent years, advanced metrics for baseball have become popular and are readily available with MLB Statcast.  Bettors and analysts can use this available data to inform various predictions ranging from the achievements of specific players to teams’ regular season win-loss records. 

In this project, the following question will be explored: Can specific characteristics of winning teams be identified to help predict if an MLB team will make the playoffs, win the league pennant, or win the World Series?

In order to predict the outcome for an MLB team in a given season, two classification models were created. The first model is more specific in its predictions by distinguishing between teams that miss the playoffs, make the playoffs but not the World Series, lose in the World Series, and win the World Series. The second model is excellent at predicting if a team will make the playoffs or not while also providing specific team statistics that are most important in deciding if a team will succeed.

## Background & Question

### Research Question
Can MLB team segments, derived from offensive and defensive statistics, help predict whether an MLB team will make the playoffs, win the league pennant, or win the World Series?

### Why is it worth exploring?
Leveraging this data in a meaningful yet simple way could be invaluable to sports bettors, analysts, teams, and fans. Bettors can use this model to guide their betting decisions. Analysts can use it to inform their predictions on team success. Team organizations can use the information to learn which team attributes are most influential in successful MLB teams and find talent that positively impacts those attributes. For these reasons, in this project, segmentation analysis and predictive modeling are explored to create a method for predicting MLB team success in a given season.

### Hypothesis:
Specific offensive and defensive statistics as well as additional information regarding payroll management and home game ticket sales will contribute to accurate classification of teams by their success because statistics are indicators of overall team ability and team morale.

### Prediction:
Certain offensive and defensive statistics, like On-base percentage (OBP) and Walks and hits per innings pitched (WHIP), that represent a team’s ability to create scoring opportunities or prevent the opposing team from creating scoring opportunities will be most influential in predicting a team’s outcome for the season. Other statistics, like batting average and home runs, that reflect individual player ability will be less influential in the model.

## Data and Methods

### Dataset:
The data that was used was taken from a very accurate baseball statistics website called [Baseball Reference](https://www.baseball-reference.com). There were nine different datasets that were taken from this website, and these datasets were: standard batting data, standard pitching data, standard fielding data, player value data for batting and pitching, advanced batting and pitching statistics, sabermetrics data, and miscellaneous data. To extract this data from Baseball Reference, a custom function was created. The function can be found in the R `script scraping_baseball_data.R`.

### Data Dictionary:
Click on the following link to access the [Data Dictionary](https://docs.google.com/spreadsheets/d/1CIF_PlHCG6iH3jl4aIAVXccJDVC2CTumdDulyaqLHo0/edit?usp=sharing) and learn about each variable.

### Response/Outcome Variable:
A multi-class `team success` variable was created that represented how each team performed each year, and the different classes included missing the playoffs (1), making the playoffs and not going to the World Series (2), losing in the World Series (3), and winning the World Series (4).

### Predictor Variables:

## Analysis Plan

### Data Cleaning:

### Predictive Modeling:

## What will indicate if the question is answered and the hypothesis supported?

## Instructions for running the code

### Custom Function

### Troubleshooting and Recommendations 

## Team Duties and Work on Project
All group members were involved in writing code and writing the final report. Individuals wrote much of the code separately but came together to review the code weekly where constructive feedback was given and ensured the code was running properly after all commits on all group members computers. All files were tested by all group members and ran without issue. The group members initials are present next to each section of code to indicate who wrote each part. These initials indicate the delegated section assignments.

## References
Ramy Elitzur,
Data analytics effects in major league baseball,
Omega,
Volume 90,
2020,
102001,
ISSN 0305-0483,
https://doi.org/10.1016/j.omega.2018.11.010.
(https://www.sciencedirect.com/science/article/pii/S0305048318300215)

Abstract: The use of data analytics has enjoyed resurgence over the last two decades in professional sports, businesses, and the government. This resurgence is attributable to Moneyball, which exposed readers to the use of advanced baseball analytics by the Oakland Athletics, and how it has resulted in improved player selection and game management. Moreover, it changed managerial vocabulary, as the term “Moneyballing” now commonly describes organizations that use data analytics. The first research question that this study examines is whether the organizational knowledge related to baseball data analytics has provided any advantage in the competitive Major League Baseball (MLB) marketplace. The second research question is whether this strategic advantage can be sustained once this proprietary organizational knowledge becomes public. First, I identify “Moneyball” teams and executives, i.e., those who rely on baseball data analytics, and track their pay/performance over time. Next, using econometric models, I analyze whether these “Moneyball” teams and GMs, have enjoyed a pay-performance advantage over the rest of MLB, and whether this advantage persists after the information becomes public.
Keywords: Data analytics; Moneyball; OR in sports; Empirical analysis; Information; Major league baseball








