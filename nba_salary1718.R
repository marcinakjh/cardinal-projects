## title: "NBA Players' Salary Prediction using multiple regression model"
## author: "Koki Ando"
## Preparation
## Required Packages
library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
## Data Preparation
salary.table <- 
  read.csv("C:/Users/marci/Downloads/NBA_season1718_salary.csv")
ss <- read.csv("C:/Users/marci/AppData/Local/Temp/8e18db83-64ce-435f-9e9b-e321ab940443_Seasons_Stats.csv.zip.443/Seasons_Stats.csv")
## Data Components
str(salary.table)
str(ss)
## Data Cleaning
stats17 <- 
  ss %>% filter(Year >= 2017) %>% 
  select(Year:G, MP, PER, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) 
## Merging Data
stats_salary <- merge(stats17, salary.table, by.x = "Player", by.y = "Player")
names(stats_salary)[40] <- "salary17_18"
stats_salary <- stats_salary[-39]
## Correlation Checks
corrplot(cor(stats_salary %>% 
               select(salary17_18, MPG:SPG, 
                      Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")
stats_salary_cor <- 
  stats_salary %>% 
  select(salary17_18, PPG, MPG, TOPG, RPG, PER, SPG, APG)
ggpairs(stats_salary_cor)
cor(stats_salary_cor)[,"salary17_18"]
## Data Visualization
## Interactive Plot
names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary17_18, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary17_18, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Point per Game")
  )
## Simple Linear Regression Model
## Scatter Plot with Regression Line
stats_salary %>% 
  ggplot(aes(x = salary17_18, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
## Regression Analysis
stats_salary_regression <- 
  stats_salary %>% select(salary17_18, MPG:SPG)
lm(salary17_18~., data=stats_salary_regression)
## Is the Player Trusted by the Coach? How Many Turnovers do they Make?
avg.minutes <- mean(stats_salary_regression$MPG)
avg.turnover <- mean(stats_salary_regression$TOPG)
stats_salary_regression$Trusted <- as.factor(ifelse(stats_salary_regression$MPG >= avg.minutes, "Yes", "No"))
stats_salary_regression$Agressiveness <- as.factor(ifelse(stats_salary_regression$TOPG >= avg.turnover, "Yes", "No"))
head(stats_salary_regression)
## Parallel Slope Model
## Scatter Plot Colored by how Many Turnovers are Made
stats_salary_regression %>% 
  ggplot(aes(x = salary17_18, y = PPG, colour = Agressiveness)) + 
  geom_point() + 
  geom_smooth(method="lm")
lm(formula = salary17_18 ~ Trusted * Agressiveness, data=stats_salary_regression)
## Modeling and Conclusion
## Prediction Function
## Analysis Conclusion
salary_prediction <- function(m, point, minutes, turn_over){
  pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}
model <- lm(formula = salary17_18 ~ PPG + MPG + TOPG, data = stats_salary_regression)
salary_prediction(model, 16.7, 31.2, 1.5)