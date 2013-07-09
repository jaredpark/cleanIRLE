load('allAbqRestHtmlList.Rout') #loads object "htmlList.Rout from restHtmlMerge.R script
source('parallelFuncs.R')

out = city.to.df(htmlList)

#out = NULL
#for (file in city.menu.filenames){
#  load(paste(file.location, file, sep = ''))
#  out = rbind(out, city.to.df(docs))
#}

save(out, file = 'abqData.Rout')
write.table(out, file = 'abqData.txt', row.names = F, col.names = T, sep = ' ', na = 'NA')
