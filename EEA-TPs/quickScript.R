print("hello world")

# suppressPackageStartupMessages( library(  ) ) ## <=- use this to avoid error messages
# 



# ## Redundancias
# Abasto -> Balvanera
# Congreso -> Balbanera
# Barrio Norte -> Recoleta
# Catalinas -> Boca
# Centro / Microcentro -> Retiro
# Las CaÃ±itas -> Palermo
# Once -> Balbanera
# Parque Centenario -> Caballito
# Tribunales -> San Nicolas

barrios_portenos <- c("Agronomía", "Almagro", "Balvanera", "Barracas", "Belgrano", "Boedo", "Caballito", "Chacarita", "Coghlan", "Colegiales", "Constitución", "Flores", "Floresta", "Boca", "Paternal", "Liniers", "Mataderos", "Monserrat", "Monte Castro", "Pompeya", "Nuñez", "Palermo", "Parque Avellaneda", "Parque Chacabuco", "Parque Chas", "Parque Patricios", "Puerto Madero", "Recoleta", "Retiro", "Saavedra", "San Cristobal", "San Nicolás", "San Telmo", "Velez Sarsfield", "Versalles", "Villa Crespo", "Villa del Parque", "Villa Devoto", "Villa General Mitre", "Villa Lugano", "Villa Luro", "Villa Ortuzar", "Villa Pueyrredón", "Villa Real", "Villa Riachuelo", "Villa Santa Rita", "Villa Soldati", "Villa Urquiza")
sort(setdiff(unique(ar_properties$l3), barrios_portenos))


# for (i in sort(unique(ar_properties[,c('l3')])))
# {
#   print(i)
# }
# 


setwd("C:/Users/Luxor/Documents/GitHub/Test_01/EEA-TPs")

ar_properties <- read.csv(unz('ar_properties.zip', 'ar_properties.csv'), row.names = NULL, stringsAsFactors = TRUE, encoding = "UTF-8")

# No usamos esto, ya que filter() descarta las filas a proposito
# length(unique(ar_properties$id))
# ar_properties <- ar_properties %>% column_to_rownames( var='id')

head(ar_properties)
nrow(ar_properties)




ar_properties <- ar_properties %>% filter(
    l1             ==   'Argentina' &
    l2             ==   'Capital Federal' &
    currency       ==   'USD' &
    operation_type ==   'Venta' &
    property_type  %in% c('Casa', 'Departamento', 'PH'))


ar_properties <- ar_properties %>% select(
  id,
  l3,
  rooms,
  bedrooms,
  bathrooms,
  surface_total,
  surface_covered,
  price,
  property_type )

# drop empty factors
ar_properties <- droplevels(ar_properties)


nrow(ar_properties)

# Analisis exploratorios (I)

#Obtener la cantidad de valores unicos y de valores faltantes (NAs) para cada una de estas variables
uniques <- as.data.frame(apply(ar_properties, 2, n_distinct))
missing <- as.data.frame(colSums(is.na(ar_properties)))


# Obtener la matriz de correlacion para las variables numericas. Pista: usen ‘complete.obs’ para poder omitir los valores faltantes.
numeric_ar_properties <- ar_properties[,unlist(lapply(ar_properties, is.numeric))]
cor(numeric_ar_properties, use="complete.obs", method="pearson")

#Preparacion de los datos (II)

# En el punto 2 deberian haber encontrado que la variable bedrooms presenta una alta proporción de valores faltantes y que presenta una fuerte correlacion con la variable rooms. Por lo tanto, vamos a eliminarla.
ar_properties <- ar_properties %>% select( -c('bedrooms') )

# Eliminar todos los registros que presentan valores faltantes
ar_properties <- ar_properties[complete.cases(ar_properties),]


#Analisis exploratorios (II)

# Obtener estadisticas descriptivas para la variable precio (cuartiles, promedio, minimo y maximo) y realizar un histograma de la variable

summary.with.mean <- function(data)
{
    summary <- summary(data)
    summary['mean'] = mean(data)
    return( summary )
}


summary.with.mean(ar_properties$price)

hist(ar_properties$price)
hist(log(ar_properties$price))

ggplot(ar_properties, aes(x = price)) + geom_histogram() + scale_x_log10()

ar_properties %>% 
  group_by(property_type) %>% 
  summarize(mean = mean(price),
            )


tapply(ar_properties$price, ar_properties$property_type, summary.with.mean)


ggplot(ar_properties, aes(x=property_type, y=price, fill=property_type)) + geom_boxplot()
ggplot(ar_properties, aes(x=property_type, y=log(price), fill=property_type)) + geom_boxplot()

suppressPackageStartupMessages(library(GGally))
ggcorr(ar_properties, method = c("everything", "pearson")) 

#ggpairs(ar_properties %>% select( -c("id", "l3") ), ggplot2::aes(colour=property_type) )


# Tips on detecting outliers:
# https://towardsdatascience.com/a-brief-overview-of-outlier-detection-techniques-1e0b2c19e561

suppressPackageStartupMessages(library("solitude"))
iso <- solitude::isolationForest$new()

iso$fit(ar_properties %>% select( c("rooms", "price", "surface_total") ) )



ar_properties$anomalyScore = iso$scores$anomaly_score

print(iso$scores)

quantile(iso$scores$anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)

b<- ar_properties[iso$scores$anomaly_score > 0.7,]


# funcion para obtener las columnas especificadas, o las numericas en caso de no especificarse
get.numeric.columns <- function(df, columns.to.get=NULL) {
    if (is.null(columns.to.get))
        columns.to.get <- colnames(df)[unlist(lapply(df, is.numeric))]

    return(columns.to.get)
}



# funcion para escalar las columnas especificadas (o todas las numericas si no se especifica)
scale.numeric.cols <- function(df, columns.to.scale = NULL) {

    # Elegir como predeterminado a las numericas en caso de que no esten especificadas
    columns.to.scale <- get.numeric.columns(df, columns.to.scale)

    # Aplicar el escalamiento a las columnas elegidas, y agregarles el prefijo 'scaled.'
    scaled.df <- as.data.frame(scale(ar_properties[, columns.to.scale]))
    colnames(scaled.df) <- paste0('scaled.', columns.to.scale)
    return(cbind(df,scaled.df))
}



ar_properties <- scale.numeric.cols(ar_properties, c('price'))


linear.model.surface <- lm(price ~ surface_total, data = ar_properties)
linear.model.rooms <- lm(price ~ rooms, data = ar_properties)

summary(linear.model.surface)

summary(linear.model.rooms)


quantile(ar_properties$price
         , probs = seq(0, 0.01, length.out = 11)
)

quantile(ar_properties$price
         , probs = seq(.99, 1, length.out = 11)
)






quantile(ar_properties$anomalyScore
         , probs = seq(.99, 1, length.out = 11)
)


only.outliers = ar_properties %>% filter(anomalyScore > 0.60)


no.outliers = ar_properties %>% filter(anomalyScore <= 0.60)

ggplot(no.outliers, aes(x = rooms, y=price)) + geom_point(aes(colour = anomalyScore)) + scale_colour_viridis_c()

ggplot(ar_properties, aes(x = property_type, y=log(surface_total), fill = property_type)) + geom_boxplot()

#for (columnName in colnames(ar_properties))
#{
#  print(paste0(str_pad(columnName, 20, side='right'), '=> ', length(unique(ar_properties[[columnName]]))))
#}
#
#unique(ar_properties)


#n_distinct( ar_properties$l3, rm.na=T )

#ar_properties %>%
#  summarise(Unique_Elements = n_distinct)


#sort(unique(as.character(ar_properties$l3)))

