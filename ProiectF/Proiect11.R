library(tidyverse) #pentru a face datamaining 
library(modelr) #regresii
library(caret) #clasificari si regresii
library(rsample) #pentru a imparti setul de date in 2
library(corrplot) #afisarea grafica a unei matrici
library(ggplot2) #sistem pentru crearea declarativa de grafice
library(rpart) #este necesar pentru CART DECISSION TREE
library(rpart.plot) #aceasta func??ie ofera etichete pentru ramurile unui copac rpart
library(AmesHousing) 
library(dplyr)
library(magrittr)
library(GGally)

WorldHappiness <- read.csv("E:/PROIECT BIG/WorldHappiness.csv") #pune in variabila datele (TIBBLE)

#stergem coloane
WorldHappiness[5]<- list(NULL)
WorldHappiness[5]<- list(NULL)
WorldHappiness[11]<- list(NULL)
WorldHappiness[17]<- list(NULL)

#afisare grafic dupa generozitate(generosity)
hist(WorldHappiness$Generosity)
#afisare grafic dupa perceptia despre coruptie(Perception of corruption)
hist(WorldHappiness$Perceptions.of.corruption)
#afisare grafic in functie de suportul social
hist(WorldHappiness$Social.support)
#afisare grafic in functie de speranta la viata sanatoasa
hist(WorldHappiness$Healthy.life.expectancy)
#afisare grafic in functie de libertatea alegerii(freedom to make life choices)
hist(WorldHappiness$Freedom.to.make.life.choices)
#afisare grafic dupa generozitate explicat(Explained generosity)
hist(WorldHappiness$Explained.by..Generosity)
#afisare grafic in funcite de perceptia despre coruptie explicat(Explained Perception of Coruption)
hist(WorldHappiness$Explained.by..Perceptions.of.corruption)
#afisare grafic in functie de suportul social explicat(Explained by Social Support)
hist(WorldHappiness$Explained.by..Social.support)
#afisare grafic in functie de speranta la viata sanatoasa explicat(Explained Healthy life expectancy)
hist(WorldHappiness$Explained.by..Healthy.life.expectancy)
#afisare grafic in functie de libertatea alegerii explicat(Explained freedom to make life choices)
hist(WorldHappiness$Explained.by..Freedom.to.make.life.choices)
#afisare grafic in functie de ladder score si standard error of ladder score
hist(WorldHappiness$Standard.error.of.ladder.score)

#realizare pie chart in functie de suportul social
values <- c(86,60)
labels <- c("Social.support", "Generosity")
colors <- c("#75EED5", "#E5566A")
piepercent<- round(100*values/sum(values), 1)
pie(values,labels = piepercent, main = "chart", col = colors)
legend("topright", c("Social.support", "Generosity"), cex = 0.8, fill = colors)

#realizare pie chart in functie de logged
values <- c(86,60)
labels <- c("Ladder.score", "Logged.GDP.per.capita")
colors <- c("blue", "red")
piepercent<- round(100*values/sum(values), 1)
pie(values,labels = piepercent, main = "pie chart", col = colors)
legend("topright", c("Ladder.score", "Logged.GDP.per.capita"), cex = 0.8, fill = colors)

#valorile fara camp explicat
values <- c(86,60,40,20,30,10)
labels <- c("Social.support", "Healthy.life.expectancy", "Logged.GDP.per.capita", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")
colors <- c("blue", "red", "pink", "green", "yellow", "purple")
piepercent<- round(100*values/sum(values), 1)
pie(values,labels = piepercent, main = "pie chart", col = colors)
legend("topright", c("Social.support", "Healthy.life.expectancy", "Logged.GDP.per.capita", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption"), cex = 0.8, fill = colors)

#cu camp explicat 

values <- c(86,60,40,20,30,10)
labels <- c("Explained.by..Social.support", "Explained.by..Healthy.life.expectancy", "Explained.by..Logged.GDP.per.capita", "Explained.by..Freedom.to.make.life.choices", "Explained.by..Generosity", "Explained.by..Perceptions.of.corruption")
colors <- c("blue", "red", "pink", "green", "yellow", "purple")
piepercent<- round(100*values/sum(values), 1)
pie(values,labels = piepercent, main = "pie chart", col = colors)
legend("topright", c("Explained.by..Social.support", "Explained.by..Healthy.life.expectancy", "Explained.by..Logged.GDP.per.capita", "Explained.by..Freedom.to.make.life.choices", "Explained.by..Generosity", "Explained.by..Perceptions.of.corruption"), cex = 0.8, fill = colors)

#verificam tipul variabilei Ladder.score
class(Ladder.score)

#REGRESIE LINIARA

WorldHappiness %>%
  ggplot(aes(Ladder.score, Perceptions.of.corruption)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Perceptions.of.corruption )
summary(all_Ladder.score)

#concluzie: cu cat gradul de rasism este mai mare cu atat perceptiile asupra coruptiei sunt mai ridicate

WorldHappiness  %>%
  ggplot(aes(Ladder.score, Generosity)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Generosity )
summary(all_Ladder.score)

#concluzie:cu cat gradul de generozitate este mai mare cu atat oamenii sunt mai fericiti

WorldHappiness %>%
  ggplot(aes(Ladder.score, Healthy.life.expectancy)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Healthy.life.expectancy )
summary(all_Ladder.score)

#concluzie: cu cat duc o viata mai sanatoasa cu atat sunt mai fericiti

WorldHappiness %>%
  ggplot(aes(Ladder.score, Freedom.to.make.life.choices)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Freedom.to.make.life.choices)
summary(all_Ladder.score)

#concluzie: cu cat libertatea alegerii este mai ridicata cu atat poporul este mai fericit

WorldHappiness %>%
  ggplot(aes(Ladder.score, Social.support)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Social.support)
summary(all_Ladder.score)

#concluzie: cu cat oamenii primesc un suport social mai mare cu atat gradul de fericire creste mai mult

WorldHappiness %>%
  ggplot(aes(Ladder.score, Logged.GDP.per.capita)) + geom_point() + geom_smooth()

all_Ladder.score <- lm(data = WorldHappiness, Ladder.score ~ Logged.GDP.per.capita)
summary(all_Ladder.score)

#concluzie: cu cat veniturile unei tari sunt mai ridicate cu atat oamenii sunt mai fericiti

#regresie liniara simpla

WorldHappiness <- WorldHappiness[c("Ladder.score","Generosity")]
plot(WorldHappiness)


#functie ce gaseste cp optim
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}

#functia care gaseste cp-ul cel mai mic
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

#arbore in functie de variabilele independente numerice


set.seed(123)
ames_split <-initial_split(Ladder.score::make_ames(), prop = 0.7)
ames_train <-training(ames_split)
ames_test <-testing(ames_split)
m1 <-rpart(
  formula = Ladder.score ~.,
  data = ames_train,
  method = "anova"
)
hyper_grid <-expand.grid(
  minsplit = seq(7, 15, 1),
  maxdepth = seq(10, 20, 1)
)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = Ladder.score ~. ,
    data = ames_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
optimal_tree <-rpart(
  formula = Ladder.score ~.,
  data = ames_train,
  method = "anova",
  control = list(minsplit = 7, maxdepth = 10, cp = 0.01)
)
pred <- predict(optimal_tree, newdata = ames_test)
RMSE(pred = pred, obs = ames_test$WorldHappiness)


#bagging cu CARET

fitControl <- trainControl(
  method = "cv", #se face un cross validation
  number = 55 #setul se imparte in 55 de bucati, facand de 55x bagging
)

#optimizarea procedurii bagging
ntree <- 10:50
rmse <- vector(mode = "numeric", length = length(ntree)) #la fiecare bagging se salveaza rmse

for ( i in seq_along(ntree)) { #se utilizeaza pt a genera o scventa de aceiasi lungime ca si arg trecut
  set.seed(135) #pentru a controla rezultatele
  model <- bagging(
    formula = all_Ladder.score ~.,
    data = fitControl,
    coob = TRUE, 
    nbag = ntree[i] 
  )
  rmse[i] = model$err #eroare a modelului
}
plot(ntree, rmse, type ="l", lwd=4) #pe axa x ar trebui sa apara nr de noduri iar pe y eroarea medie

abline(v=25, col ="green", lty="dashed")
#----asta ultima e diagrama 

#arbore de decizie pt predictia numerica
set.seed(123)
ames_split <-initial_split(WorldHappiness::make_ames(), prop = 0.7)
ames_train <-training(ames_split)
ames_test <-testing(ames_split)
m1 <-rpart(
  formula = Ladder.score ~.,
  data = ames_train,
  method = "anova"
)
hyper_grid <-expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = Ladder.score ~. ,
    data = ames_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
optimal_tree <-rpart(
  formula = Ladder.score ~.,
  data = ames_train,
  method = "anova",
  control = list(minsplit = 5, maxdepth = 8, cp = 0.01)
)
pred <- predict(optimal_tree, newdata = ames_test)
RMSE(pred = pred, obs = ames_test$WorldHappiness)



#arbore de decizie

tree <- rpart(Ladder.score ~ Generosity + Social.support, WorldHappiness)
arbore <- data.frame(Generosity=-0.098, Social.support=0.954)
result <-predict(tree, arbore)
print(result)
tree<- rpart(Generosity ~ Social.support + Ladder.score, WorldHappiness)
arbore <- data.frame(Generosity=0.16, Ladder.score=7.554)
result<- predict(tree, arbore)
print(result)
rpart.plot(tree)

  