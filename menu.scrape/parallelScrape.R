# 12/24/2012 - first data pull
# 03/08/2013 - second, test pull
# 06/0x/2013 - abq second data pull

d = seq(0, 30*60, .05)
cost = ceiling((150+75*d)/3600)*.4
plot(d, cost, xlab = 'delay time (seconds)', ylab = 'cost (dollars)', type = 'l', ylim = c(0,max(cost)))

date = '06_02_2013'
# root = '/home/jared'
# fileDir = paste(root, '/objects', sep = '')
root = '~/GitHub/IRLE/menu.scrape'
fileDir = 'outputs.v2'
numRestPerScript = 10
city = 'albuquerque'

if (!require('XML', character.only = T)){
  install.packages('XML', dep = T)
}

source(paste(root, '/parallelFuncs.R', sep = ''))

# 1. cuisUrls = get.city.cuis.urls(state, city) --> outputs a vector containing city cuisine urls
##### cost = 1 search
# 2. loop over elements of cuisUrls
#    a. counter = 1
#    b. restUrls = get.cuis.rest.urls(cuisUrls[element]) --> outputs a vector containing cuisine restaurant urls
##### cost = (# of cuisines) searches
# continued in urlsToMenus

cuisUrls = get.city.cuis.urls('nm', city)
restUrls = NULL
for (cuisine in 1:length(cuisUrls)){
  restUrls = c(restUrls, get.cuis.rest.urls(cuisUrls[[cuisine]]))
}

numScripts = ceiling(length(restUrls)/numRestPerScript)
for (scriptNum in 1:numScripts){
  index = (1:10) + (scriptNum-1)*numRestPerScript
  scriptUrls = restUrls[index]
  scriptUrls = scriptUrls[!is.na(scriptUrls)]
  write.table(c(scriptUrls), file = paste(fileDir, '/script.', scriptNum, '.urls.txt', sep = ''), col.names = F, row.names = F)
}

nm = state.to.df('nm', file.location)
ca = state.to.df('ca', file.location)

################
##  writing data frames to text files
################

save(nm, file = paste(file.location, 'NM_', date, '.Rout', sep = ''))
save(ca, file = paste(file.location, 'CA_', date, '.Rout', sep = ''))

write.table(nm, file = paste(file.location, 'nm.data.v2.txt', sep = ''), row.names = F, col.names = T, sep = ' ', na = 'NA')
write.table(ca, file = paste(file.location, 'ca.data.v2.txt', sep = ''), row.names = F, col.names = T, sep = ' ', na = 'NA')