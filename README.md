# MLB World Series Team Segmentation Using K-Means Clustering

### By: Joseph Annand, James Keegan, and Steven Martinez

Full research paper can be found [here](https://docs.google.com/document/d/1KPKI_hL4eQsLtSYC39585JdPl7DB0yIPlYTsfp5u_3g/edit?tab=t.0).

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
The data that was used was taken from a very accurate baseball statistics website called [Baseball Reference](https://www.baseball-reference.com). There were nine different datasets that were taken from this website, and these datasets were: standard batting data, standard pitching data, standard fielding data, player value data for batting and pitching, advanced batting and pitching statistics, sabermetrics data, and miscellaneous data. To extract this data from Baseball Reference, a custom function was created. The function can be found in the R script `scraping_baseball_data.R`.

### Data Dictionary:
Click on the following link to access the [Data Dictionary](https://docs.google.com/spreadsheets/d/1CIF_PlHCG6iH3jl4aIAVXccJDVC2CTumdDulyaqLHo0/edit?usp=sharing) and learn about each variable.

### Response/Outcome Variable:
A multi-class `team success` variable was created that represented how each team performed each year, and the different classes included missing the playoffs (1), making the playoffs and not going to the World Series (2), losing in the World Series (3), and winning the World Series (4).

### Predictor Variables:
After joining all the datasets, there were 196 predictor variables. All of the variables were explored using PCA and LDA. The final random forest model consisted of 77 predictor variables after many of the variables were removed because of collinearity. The dataset for the random forest model can be found in the `final_data` folder, the dataset is called `rf_data.csv`. The final knn model with LDA used the dataset `final_dataset.csv`, which can also be found in the `final_data` folder.

## Analysis Plan

### Data Cleaning:
The data cleaning process began by merging multiple datasets and renaming variables with identical names to avoid confusion. For instance, both batters and pitchers have a "batting average" (BA) statistic, but its meaning differs depending on context. To clearly distinguish between variables and their sources, we added standardized prefixes:

* **sb.** = standard batting
* **sp.** = standard pitching
* **tf.** = team fielding
* **ab.** = advanced batting
* **ap.** = advanced pitching
* **pvb.** = player value batting
* **pvp.** = player value pitching
* **sbm.** = sabermetrics
* **md.** = miscellaneous

These prefixes allowed us to easily identify each variable’s origin and purpose throughout the analysis.

Next, we addressed a major class imbalance in the original `Team.Success` variable, which categorized teams into multiple outcomes (e.g., missed playoffs, playoff team, World Series loser, World Series winner). To mitigate this, we created a binary version of the variable: teams that missed the playoffs were labeled as 0, while all others (playoff teams and World Series participants) were labeled as 1. This resulted in the creation of a second, binary-labeled dataset. Both versions of the dataset—multi-class and binary—were used to compare model performance.

Lastly, for the final Random Forest model, we further refined the binary dataset by reducing the number of predictor variables from 196 to 77 to improve model efficiency and interpretability.

### Predictive Modeling:
To evaluate team success, we built and compared multiple classification models using both a **four-class** and **binary** version of the target variable (team success). Our modeling pipeline included extensive pre-processing steps: removing collinear and incomplete variables, applying transformations (e.g., Box-Cox), and using **Principal Component Analysis (PCA)** and **Linear Discriminant Analysis (LDA)** for dimensionality reduction.

We trained four **k-Nearest Neighbor (kNN)** models using either PCA or LDA as input and tested them on both the multiclass and binary response variables. The **kNN model using LDA** performed best for the **multiclass** setting, achieving better balance in correctly predicting all four team success classes. As a result, this model was selected as our final model for multiclass classification.

For the **binary classification task** (predicting whether a team made the playoffs), we trained a **Random Forest model**. It outperformed all other models, achieving **94.94% test accuracy**, an AUC of **0.957**, and a Kappa of **0.885**. The Random Forest model was particularly strong at distinguishing playoff vs. non-playoff teams, and its variable importance rankings provided valuable insights. However, in the multiclass setting, it struggled with underrepresented classes and heavily favored the first two categories due to class imbalance.

In summary, the **kNN with LDA** model is our final choice for **multiclass team success prediction**, while the **Random Forest model** is our final model for the **binary playoff prediction task** due to its superior performance and interpretability.

## Instructions for running the code
All essential scripts are written in R. Set the working directory for your session to the repository. The code for the final k-Nearest Neighbor and Random Forest models featured in the report is found in `code/kNN_lda_final.R` and `code/random_forest_final.R`, respectively. No data cleaning scripts need to be run prior to running these scripts, as the CSV files with cleaned and merged data have already been created. The scripts for data scraping, cleaning, and merging can be found in `code/Data Scraping and Cleaning`. The R Source Files in this folder are numbered based on the order they should be run to produce the same cleaned data used for this project. The code for exploratory data analysis (EDA) is distributed among a variety of R Source Files, R Markdown files, and Tableau files. These files can be found in `code/EDA` and do not need to be run in any particular order. Initial modeling prior to cross-validation and hyperparameter tuning can be found in `code/knn_kmeans_binary.Rmd` and `code/models_with_pca.R`. Like the scripts for the final models, these can be run without any data cleaning scripts run prior.

### Troubleshooting and Recommendations
During the analysis, users may encounter missing package errors or file path issues. To resolve these, ensure all required R packages are installed and that dataset files are placed in the correct directories. For additional support, refer to the R manual for troubleshooting R-specific issues.

It is recommended that the next steps for this project include the following actions. Initially, an exploratory analysis of the current models’ incorrect predictions across time should be performed to identify bias related to time. If that is observed, a time-aware cross-validation should be employed during training to correct the bias. Additionally, a more objective approach to feature selection of collinear variables should be performed to identify if using different subsets of predictors yields significantly different model results. The kNN model using LDA had the disadvantage of having less interpretability than the Random Forest. The team should explore ways to extract feature importance from LDA results. Ultimately, the models should be tested using 2025 season data, then the models can be evaluated by comparing its predictions for this season to the outcome in October of this year.

## Team Duties and Work on Project
All group members were involved in writing code and writing the final report. Individuals wrote much of the code separately but came together to review the code weekly where constructive feedback was given and ensured the code was running properly after all commits on all group members computers. All files were tested by all group members and ran without issue. The group members initials are present next to each section of code to indicate who wrote each part. These initials indicate the delegated section assignments.

## References
National Baseball Hall of Fame. (n.d.). Henry Chadwick. National Baseball Hall of Fame. Retrieved March 2025, 24, from https://baseballhall.org/hall-of-famers/chadwick-henry.

Statista Research Department. (2024, July 3). Sports betting worldwide - statistics & facts. Statista. Retrieved March 24, 2025, from 
https://www.statista.com/topics/1740/sports-betting/#topicOverview
https://onlinegrad.syracuse.edu/blog/sabermetrics-baseball-analytics-the-science-of-winning-accessible/#:~:text=Though%20the%20term%20%E2%80%9Csabermetrics%E2%80%9D%20has,dissect%20the%20science%20of%20winning.








