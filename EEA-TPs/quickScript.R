print("hello world")


ar_properties <- read.csv(unz('ar_properties.zip', 'ar_properties.csv'), row.names=NULL, stringsAsFactors=TRUE)
head(ar_properties)
nrow(ar_properties)

library(tidyverse)
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
summary(ar_properties$price)
mean(ar_properties$price)

hist(ar_properties$price)
hist(log(ar_properties$price))

ggplot(ar_properties, aes(x = price)) + geom_histogram() + scale_x_log10()

ar_properties %>% 
  group_by(property_type) %>% 
  summarize(mean = mean(price),
            )


tapply(ar_properties$price, ar_properties$property_type, summary)


ggplot(ar_properties, aes(x=property_type, y=price, fill=property_type)) + geom_boxplot()
ggplot(ar_properties, aes(x=property_type, y=log(price), fill=property_type)) + geom_boxplot()

library(GGally)
ggcorr(ar_properties, method = c("everything", "pearson")) 


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
