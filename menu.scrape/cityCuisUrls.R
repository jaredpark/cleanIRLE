require(XML)
source('parallelFuncs.R')
cuisUrls = get.city.cuis.urls('nm', 'albuquerque')
write.table(cuisUrls, file = 'albuquerqueCuisUrls.txt', row.names = F, col.names = F)