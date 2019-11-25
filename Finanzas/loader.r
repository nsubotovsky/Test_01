library(fst)
library(tidyverse)
library(gsubfn)
library(data.table)



################## metrics ################

get.roc.data <- function( predict, target )
{
    # calculo metricas
    roc_pred <- ROCR::prediction(predict, target, label.ordering = c(0, 1))
    
    # extraigo AUC
    auc <- unlist(ROCR::performance(roc_pred, "auc")@y.values)
    
    # extraigo ROC
    curve <- ROCR::performance(roc_pred,"tpr","fpr")
    curve.as.df <- data.table(x=curve@x.values[[1]], y=curve@y.values[[1]])
    colnames(curve.as.df) = c( 'x', 'y' )
    
    return(list( auc=auc, curve=curve.as.df))
}


get.gain.vs.prob <- function( prediction, target )
{
    prob.table <- data.table(prob=prediction, target=target) %>%
        arrange(-prob) %>%
        mutate(single.gain=ifelse(target==1, 19500, -500)) %>%
        mutate(gain=cumsum(single.gain)) %>%
        select(-single.gain)
    
    return(prob.table)
}


plot.gain.vs.prob <- function(df)
{
    ggplot( df, aes(x=prob, y=gain)) + geom_line() + xlim(0,0.1) +ylim(5000000,10000000)
}



func.gain <- function(probabilidades, clase, punto_corte = 0.025)
{
    return(sum(
        (probabilidades >= punto_corte) * ifelse(clase == "1", 19500, -500))
    )
}




################ Prepare training df #################

pick.months <- function(df, month.list)
{
    return( df %>% filter(foto_mes %in% ( month.list %>% sapply( get.month.at ) ) ) )
}


target.func.baja2 <- function(df)
{
    return( as.integer(ifelse(df$clase_ternaria == "BAJA+2", 1, 0)) )
}


target.func.baja12 <- function(df)
{
    return( as.integer(ifelse(df$clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0)) )
}


split.prepare.target <- function(df, target.func=target.func.baja2)
{
    df$clase_ternaria <- target.func(df)

    # split traing and target
    df.train <- df %>% select(-c("numero_de_cliente", "clase_ternaria"))
    df.target <- df$clase_ternaria
    
    return(list(train=df.train, target=df.target))
}


################ Prepare training df - xgb specific #################


prepare.training.matrix.default <- function(df, target.func=target.func.baja2)
{
    list[ df.train, df.target ] <- split.prepare.target(df,target.func)
    xgb.train.matrix <- xgb.DMatrix( data = data.matrix( df.train ), label = df.target )
    return(xgb.train.matrix)
}


prepare.predict.matrix.default <- function(df)
{
    list[df.train, df.target]  <- split.prepare.target(df)
    xgb.predict.matrix <- xgb.DMatrix( data = data.matrix( df.train ) )
    return(list(predict=xgb.predict.matrix, target=df.target))
}


############# Setup stuff ####################

init.wd <- function()
{
    setwd("C:/Users/Luxor/Documents/DM Finanzas")
}



########### Loading and saving ###################

fst.read <- function(path)
{
    return( fst::read.fst(path, as.data.table=T) )
}

fst.write <- function(df, path)
{
    return( fst::write.fst(df, path, compress=100 ) )
}


get.full.ds <- function()
{
    init.wd()
    return( fst.read("./datasetsOri/paquete_premium.rds") )
}


############ (sub)Sampling ###########################

sample.df <- function( df, fraction, ... )
{
    # This! : https://dplyr.tidyverse.org/articles/programming.html
    group_var <- enquos(...)
    return( df %>% group_by( !!!group_var ) %>% sample_frac( fraction ) %>% ungroup() )
}

summary.group <- function( df, ... )
{
    group_var <- enquos(...)
    return ( df %>% 
        group_by( !!!group_var ) %>%
        summarize(n=n()) %>%
        mutate(freq=n/sum(n)) )
}


################## References #####################

get.all.months <- function() c(
    '201607','201608','201609','201610','201611','201612','201701','201702','201703','201704',
    '201705','201706','201707','201708','201709','201710','201711','201712','201801','201802',
    '201803','201804','201805','201806','201807','201808','201809','201810','201811','201812',
    '201901','201902','201903','201904','201905','201906')

get.month.at <- function( month.index )
{
    all.months <- get.all.months()
    return( all.months[length( all.months )-month.index-2] )
}

