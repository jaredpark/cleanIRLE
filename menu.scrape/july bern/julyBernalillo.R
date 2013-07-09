load('./outputs.v2/NM_12_24_2012.Rout')
nmNoAbq = subset(nm, nm$city != 'Albuquerque')
bernalilloRest = unique(nmNoAbq$restaurant)

nm.cities = c('rio-rancho', 'santa-fe', 'las-cruces')

restUrls = c()

for (city in nm.cities){
  cuisUrls = c(cuisUrls, get.city.cuis.urls('nm', city))
}

write.table(cuisUrls, file = 'bernalilloCuisines.txt', row.names = F, col.names = F)
save(cuisUrls, file = 'bernalilloCuisines.Rout')

load('./july Bernalillo/allBernalilloUrls.Rout') #loads allUrls, a vector with each restaurant url from each cuisine for each of the three nm cities
allUrls = c(allUrls, 'http://www.allmenus.com/nm/albuquerque/30792-noko-sushi/menu/')

rests = unique(nm$restaurant[nm$city != 'Albuquerque'])
modRests = gsub(' ', '-', rests)
modRests = gsub('\\.', '', modRests)
length(modRests)

needed = c()
for (rest in modRests){
  index = which(grepl(tolower(rest), allUrls))
  if (length(index)==0){print(rest)} # print the name of december rest that has no corresponding url in july pull
  needed = c(needed, index)
}
needed = c(needed, which(grepl('eddies-bar--grill', allUrls)))
needed = c(needed, which(grepl('l--l-steak-co', allUrls)))
needed = c(needed, which(grepl('st-clair-winery--bistro', allUrls)))
needed = c(needed, which(grepl('andele-restaurant', allUrls)))
needed = c(needed, which(grepl('lorenzos-pan-am', allUrls)))
needed = c(needed, which(grepl('chocolate-maven-bakery--cafe', allUrls)))
needed = c(needed, which(grepl('ore-house-on-the-plaza', allUrls)))
needed = c(needed, which(grepl('san-francisco-street-bar--grill', allUrls)))
# the pink adobe does not have a menu on allmenus at this time
needed = c(needed, which(grepl('las-fuentas-restaurant--grill', allUrls)))
needed = c(needed, which(grepl('nile-cafe--catering-company', allUrls)))

bernalilloNeeded  = allUrls[needed]
bernalilloNeeded = unique(bernalilloNeeded)
length(bernalilloNeeded)
write.table(bernalilloNeeded, file = 'bernalilloRestUrls.txt', row.names = F, col.names = F)
save(bernalilloNeeded, file = 'bernalilloRestUrls.Rout')

menuUrlsLists = list.files('./july Bernalillo/menuUrlLists')

bernalilloUrls = c()
for (restFile in menuUrlsLists){
  load(paste('./july Bernalillo/menuUrlLists/', restFile, sep = ''))
  for (menu in 1:length(restUrls[[1]])){
    bernalilloUrls = c(bernalilloUrls, restUrls[[1]][[menu]])
  }
}

bernalilloUrls = unique(bernalilloUrls)
bernalilloUrls = bernalilloUrls[-which(grepl('javascript', bernalilloUrls))]
length(bernalilloUrls)

save(bernalilloUrls, file = './july Bernalillo/bernalilloUrls.Rout')
write.table(bernalilloUrls, file = './july Bernalillo/bernalilloUrls.txt', row.names = F, col.names = F)


load('./july Bernalillo/allBernalilloRestHtmlList.Rout')
source('~/GitHub/IRLE/menu.scrape/parallelFuncs.R')

city.rest.list = htmlList
city.df.out = NULL
num.rest.no.prices.in.city = 0
for (rest in 1:length(city.rest.list)){ 
  print(rest)
  restaurant.matrix = menu.to.matrix(city.rest.list[[rest]])
  restaurant.df = as.data.frame(restaurant.matrix, stringsAsFactors = F)
  has.prices = sum(na.exclude(as.numeric(restaurant.df$price))) != 0
  
  if (has.prices){
    city.df.out = rbind(city.df.out, restaurant.df)
  }
}

bernalillo = city.df.out[-which(duplicated(city.df.out)),]

cleanBernalillo = data.frame(apply(bernalillo, 2, specialToNothing), stringsAsFactors = F)

save(cleanBernalillo, file = '~/GitHub/IRLE/menu.scrape/july Bernalillo/bernalilloNoAbq.Rout')
write.table(cleanBernalillo, file = '~/GitHub/IRLE/menu.scrape/july Bernalillo/bernalilloNoAbq.txt', row.names = F, col.names = T, sep = ' ', na = 'NA')

