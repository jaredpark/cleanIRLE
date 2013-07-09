which(apply(apply(decAbq[, 10:12], 2, as.numeric), 1, sum, na = T) == 0)

june = read.table('~/GitHub/IRLE/menu.scrape/june abq/abqData_6_15_2013.txt', header = T, string = F)
dec = read.table('~/GitHub/IRLE/menu.scrape/outputs.v2/nm.data.v2.txt', header = T, string = F)

june = subset(june, june$city == 'Albuquerque')
dec = subset(dec, dec$city == 'Albuquerque')

dRest = unique(dec$restaurant)
jRest = unique(june$restaurant)
hRest = unique(hope$restaurant)

sum(is.element(dec$restaurant, 'Pei Wei'))
dec$street[dec$restaurant == 'Pei Wei']

noJdata = dRest[!is.element(dRest, jRest)]
# noJData[c(4,5,6,7,8,9,10,)]



allRest = read.table('~/GitHub/IRLE/menu.scrape/allAbqRestUrls.txt', strings = F)
load('~/GitHub/IRLE/menu.scrape/outputs/albuquerque.rest.list.Rout')

urlTable = NULL
for (cuisine in 1:length(rest.list.no.closed)){
  for (restaurant in 1:length(rest.list.no.closed[[cuisine]])){
    name = rest.list.no.closed[[cuisine]][[restaurant]][[1]]
    url = rest.list.no.closed[[cuisine]][[restaurant]][[4]]
    urlTable = rbind(urlTable, c(name, url))
  }
}
urlTable[,1] = gsub("\\'", '', urlTable[,1])

urlsNeeded = NULL
restNames = NULL
for (rest in noJdata){
  index = which(urlTable[,1] == rest)
  urls = urlTable[index,2]
  urlsNeeded = c(urlsNeeded, urls)
  restNames = c(restNames, rep(rest, length(urls)))
}

c = 1
for (rest in restNames){
  menuTypes = unique(dec[dec$restaurant == rest, 'menu'])
  if (length(menuTypes) > 1){
    newUrls = paste(urlsNeeded[c], menuTypes[-1], '/', sep = '')
    urlsNeeded = c(urlsNeeded, newUrls)
  }
  c = c + 1
}

write.table(urlsNeeded, file = '~/GitHub/IRLE/menu.scrape/june abq/urlsRemaining.txt', row.names = F, col.names = F)
a = read.table('~/GitHub/IRLE/menu.scrape/june abq/urlsRemaining.txt', strings = F)
nrow(a)

nums = as.numeric(gsub('finalMenu([[:digit:]]+)html.Rout', '\\1', list.files('~/GitHub/IRLE/menu.scrape/june abq/lastmenus/', 'finalMenu')))
out = NULL
c = 1
for (num in nums){
  load(paste('~/GitHub/IRLE/menu.scrape/june abq/lastmenus/finalMenu', num, 'html.Rout', sep = ''))
  temp = menu.to.matrix(restMenuDoc)
  out = rbind(out, temp)
  print(c)
  c = c + 1
}
lastHalf = out
load('~/GitHub/IRLE/menu.scrape/june abq/abqData.Rout')
firstHalf = out
last = data.frame(lastHalf)
head(rownames(last))

hope = rbind(firstHalf, last)
hope = data.frame(hope)

write.table(hope, file = paste('~/GitHub/IRLE/menu.scrape/june abq/', 'abqData_6_25.txt', sep = ''), row.names = F, col.names = T, sep = ' ', na = 'NA')