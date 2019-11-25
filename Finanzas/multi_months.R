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

mein.df <- data.table(
    case.name=c("apr - simple - 1m","apr - joint - 1m", "apr - simple - 3m","apr - joint - 3m", 
                "mar - simple - 1m","mar - joint - 1m", "mar - simple - 3m","mar - joint - 3m"),
    month.index=c(1,1,1,1,2,2,2,2),
    target.func=c(target.func.baja2, target.func.baja12) %>% rep(4),
    months.back=c(1,1,3,3,1,1,3,3)
    )




process <- function(month.index, target.func, months.back)
{
    print("Processing...")
    print(month.index)
    print(months.back)
    
    month.range <- month.index:(month.index + months.back - 1)
    
    dgeneracion <- full.df %>% 
        pick.months( month.range ) %>%
        prepare.training.matrix.default( target.func )
    
    
    
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
    list[ test, target ] <- prepare.predict.matrix.default( full.df %>% filter(foto_mes == get.month.at( month.index-1 ) ) )
    prediction <- predict(modelo, test)
    gain <- get.gain.vs.prob(prediction, target)
    list[auc, roc.curve] <- get.roc.data(prediction, target)
    return(data.table(auc=auc, gain.df=list(data=gain)))
}



mein.df1 <- mein.df %>%
    mutate( result=pmap( list(month.index, target.func, months.back), process ) ) %>% unnest(result)

plot.stuff <- mein.df1 %>%
    select(case.name, gain.df) %>%
    unnest(gain.df) %>%
    select(-target)


ggplot( plot.stuff, aes(x=prob, y=gain, color=case.name )) +
    geom_line() +
    xlim(0,0.1) +
    ylim(7000000,10000000)

