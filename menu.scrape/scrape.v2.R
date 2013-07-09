# 12/24/2012 - first data pull
# 03/08/2012 - second, test pull

date = '06_02_2013'
file.location = '~/projects/Reich/menu.scrape/outputs.pull3/'
require(XML)

################
##   going from air to html menus in a list of lists
################

ca.cities = c('fremont', 'union-city', 'hayward', 'san-leandro', 'oakland', 'berkeley',
              tolower(c('Campbell', 'Cupertino', 'Gilroy', 'Los-Altos', 'Los-Altos-Hills', 
                        'Los-Gatos', 'Milpitas', 'Monte-Sereno', 'Morgan-Hill', 'Mountain-View',
                        'Palo-Alto', 'Santa-Clara', 'Saratoga', 'Sunnyvale', 'San-Jose')))
nm.cities = c('albuquerque')#, 'rio-rancho', 'santa-fe', 'las-cruces')

source('scrape.funcs.v3.R')

for (nm.city in 1:length(nm.cities)){
  print(nm.cities[nm.city])
  get.menus('nm', nm.cities[nm.city], file.location)
}

for (ca.city in 1:length(ca.cities)){
  print(ca.cities[ca.city])
  get.menus('ca', ca.cities[ca.city], file.location)
}

################
##   going from html menus in a list of lists to R data frames
################

nm = state.to.df('nm', file.location)
ca = state.to.df('ca', file.location)

################
##  writing data frames to text files
################

save(nm, file = paste(file.location, 'NM_', date, '.Rout', sep = ''))
save(ca, file = paste(file.location, 'CA_', date, '.Rout', sep = ''))

write.table(nm, file = paste(file.location, 'nm.data.v2.txt', sep = ''), row.names = F, col.names = T, sep = ' ', na = 'NA')
write.table(ca, file = paste(file.location, 'ca.data.v2.txt', sep = ''), row.names = F, col.names = T, sep = ' ', na = 'NA')