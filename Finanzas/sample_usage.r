#limpio la memoria
rm(list = ls())
gc()

library(tictoc)
library(data.table)
library(rpart)
library(ROCR)
library(xgboost)


source("C:/Users/Luxor/Documents/DM Finanzas/Suboscripts/loader.r")

# get df training df
full.df <- get.full.ds()


#################### TRAIN PART ####################### 

dgeneracion <- prepare.training.matrix.default( full.df  %>% filter(foto_mes == '201903') )


#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed(102191) #mi querida random seed, para que las corridas sean reproducibles

tic()
modelo = xgb.train(
    data = dgeneracion,
    objective = "binary:logistic",
    tree_method = "hist",
    max_bin = 31,
    base_score = mean(getinfo(dgeneracion, "label")),
    eta = 0.04,
    nrounds = 300,
    colsample_bytree = 0.6,
    verbose = T
)
toc()



#################### TEST PART ####################### 


list[ test, target ] <- prepare.predict.matrix.default( full.df %>% filter(foto_mes == '201904') )

prediction <- predict(modelo, test)



gain <- get.gain.vs.prob(prediction, target)

plot.gain.vs.prob( gain )


list[auc, roc.curve] <- get.roc.data(prediction, target)

ggplot( roc.curve, aes(x=x, y=y)) + geom_line()


