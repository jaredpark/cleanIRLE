load('./outputs.v2/NM_12_24_2012.Rout')
abqDat1 = nm[which(nm$city == 'Albuquerque'),]
rm(nm)
rownames(abqDat1) = 1:nrow(abqDat1)
abqDat2 = read.table('./june abq/abqData_6_25_2013.txt', header = T, strings = F)

row = 350
angstrom = strsplit(abqDat2[row, 'item'], '')[[1]][[48]]

specialToNothing = function(vec){
  out = gsub('[^[:alnum:] -\\(\\)]', '', vec)
  out = gsub(angstrom, '', out)
  return(out)
}

abqDat1$menu = specialToNothing(abqDat1$menu)
abqDat1$category = specialToNothing(abqDat1$category)
abqDat1$item = specialToNothing(abqDat1$item)
abqDat2$menu = specialToNothing(abqDat2$menu)
abqDat2$category = specialToNothing(abqDat2$category)
abqDat2$item = specialToNothing(abqDat2$item)

naToString = function(vec){
  out = ifelse(is.na(vec), 'NA', vec)
  return(out)
}

abqDat1 = abqDat1[!duplicated(abqDat1),]
abqDat2 = abqDat2[!duplicated(abqDat2),]

abqMerge = NULL
c = 1
rows = 1:nrow(abqDat1)
for (row in rows){
  decRow = abqDat1[row, ]
  sameStreet = abqDat2$street == decRow$street
  sameMenu = abqDat2$menu == decRow$menu
  sameCategory = abqDat2$category == decRow$category
  sameItem = abqDat2$item == decRow$item
  exactJuneMatch = which(sameStreet & sameMenu & sameCategory & sameItem)
  if (length(exactJuneMatch) == 1){
    junePrice = abqDat2[exactJuneMatch, c('price', 'low', 'high')]
    names(junePrice) = paste(names(junePrice), 2, sep = '') #c('price2', 'low2', 'high2')
    itemOut = cbind(decRow, junePrice)
  } else if (length(exactJuneMatch) == 0){
    itemOut = unlist(c(decRow, 'price2' = 'none', 'low2' = 'x', 'high2' = 'x'))
  } else {
    itemOut = unlist(c(decRow, 'price2' = 'multiple', 'low2' = 'x', 'high2' = 'x'))
  }
  abqMerge = rbind(abqMerge, itemOut)
  if (c %% 100 == 0){print(c)}
  c = c + 1
}

sum(is.na(abqDat1$price)) + sum(is.na(abqDat1$low))
sum(is.na(abqMerge$price)) + sum(is.na(abqMerge$low))

decNAprice = is.na(abqMerge$price)
sum(decNAprice)
sum(is.na(abqMerge$price2[decNAprice])) + sum(abqMerge$price2[decNAprice] == 'none', na.rm = T) + sum(abqMerge$price2[decNAprice] == 'multiple', na.rm = T)
sum(grepl('[[:digit:]]+', abqMerge$price2[decNAprice]))
# three were NA, now have prices

new = abqMerge
multRows = which(abqMerge$price2 == 'multiple')
for (row in multRows){
  decRow = abqDat1[row,]
  sameStreet1 = abqDat1$street == decRow$street
  sameMenu1 = abqDat1$menu == decRow$menu
  sameCategory1 = abqDat1$category == decRow$category
  sameItem1 = abqDat1$item == decRow$item
  sameStreet2 = abqDat2$street == decRow$street
  sameMenu2 = abqDat2$menu == decRow$menu
  sameCategory2 = abqDat2$category == decRow$category
  sameItem2 = abqDat2$item == decRow$item
  exactDecMatch = which(sameStreet1 & sameMenu1 & sameCategory1 & sameItem1)
  exactJuneMatch = which(sameStreet2 & sameMenu2 & sameCategory2 & sameItem2)
  if (length(exactDecMatch) == length(exactJuneMatch)){
    decPrices = abqDat1[exactDecMatch, c('price', 'low', 'high')]
    junePrices = abqDat2[exactJuneMatch, c('price', 'low', 'high')]
    decPrices = apply(decPrices, 2, naToString)
    junePrices = apply(junePrices, 2, naToString)
    if (mean(decPrices == junePrices) == 1){
      new[exactDecMatch, c('price2', 'low2', 'high2')] = abqDat1[exactDecMatch, c('price', 'low', 'high')]
    } else {
      print('change in prices')
    }
  } else{
    print('different number of same named item')
  }
}
which(new$price2 == 'multiple')
# cool

new2 = new
noneRows = which(abqMerge$price2 == 'none')
for (row in noneRows){
subset(abqDat1, street == abqDat1$street[row])[,-c(1:6)]
subset(abqDat2, street == abqDat1$street[row])[,-c(1:6)]
  decRow = abqDat1[row,]
  sameStreet1 = abqDat1$street == decRow$street
  sameMenu1 = abqDat1$menu == decRow$menu
  sameCategory1 = abqDat1$category == decRow$category
  sameItem1 = abqDat1$item == decRow$item
  sameStreet2 = abqDat2$street == decRow$street
  sameMenu2 = abqDat2$menu == decRow$menu
  sameCategory2 = abqDat2$category == decRow$category
  sameItem2 = abqDat2$item == decRow$item
  exactDecMatch = which(sameStreet1 & sameMenu1 & sameCategory1 & sameItem1)
  exactJuneMatch = which(sameStreet2 & sameMenu2 & sameCategory2 & sameItem2)
}

noneStreet = unique(abqMerge$street[noneRows])
for (addr in noneStreet){
  decDat = subset(abqDat1, street == addr)
  juneDat = subset(abqDat2, street == addr)
  noneDat = subset(abqDat1[noneRows,], street == addr)
  noneItems = noneDat$item
  noneItems[is.element(noneItems, juneDat$item)]
  decDat[is.element(noneItems, juneDat$item), c('menu', 'category')]
  juneDat[is.element(noneItems, juneDat$item), c('menu', 'category')]
  noneItems
  juneDat$item
  cbind(decDat$item, juneDat$item)
}

abqMerge[which(duplicated(abqDat1)),]

abqMerge[abqMerge$street == '1690 Rio Rancho Dr SE',]






aqbMerge1 = abqMerge
missing = subset(abqMerge1, abqMerge1$price2 == 'not found')
locWithMissing = unique(missing$street)
for (loc in locWithMissing){
  if (!is.element(loc, abqDat2$street)){
    print(paste(loc, 'not in june data'))
  } else {
    locDecDat = abqDat1[abqDat1$street == loc, ]
    decMenus = unique(locDecDat$menu)
    locJuneDat = abqDat1[abqDat2$street == loc, ]
    juneMenus = unique(locJuneDat$menu)
    if (length(decMenus) != length(juneMenus)){
      sharedMenus = decMenus[is.element(decMenus, juneMenus)]
    }
  }
}

notFound[1:100,]
sum()

dec = abqDat1[is.element(abqDat1$restaurant, 'Christy Maes'),]
june = abqDat2[is.element(abqDat2$restaurant, 'Christy Maes'),]
location = '1400 San Pedro Dr NE'

