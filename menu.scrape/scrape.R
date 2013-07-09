require('XML')

location = '~/projects/Reich/menu.scrape/outputs/'
states = c('ca', 'nm')
new.cities = c('fremont', 'union-city', 'hayward', 'san-leandro', 'oakland', 'berkeley')

for (which.state in 1:length(states)){
  if (states[which.state] == 'ca'){
    city = tolower(c('Campbell', 'Cupertino', 'Gilroy', 'Los-Altos', 
                     'Los-Altos-Hills', 'Los-Gatos', 'Milpitas', 'Monte-Sereno', 
                     'Morgan-Hill', 'Mountain-View', 'Palo-Alto', 'Santa-Clara',
                     'Saratoga', 'Sunnyvale', 'San-Jose'))
    city = c(city, new.cities)
  } else if (states[which.state] == 'nm'){
    city = c('albuquerque', #'south-valley', 'los-ranchos-de-alburquerque', 'corrales', 
             'rio-rancho', 'santa-fe', 'las-cruces')
  }
  state = states[which.state]
  
  for (i in 1:length(city)){
    theurl <- paste("http://www.allmenus.com/", state, "/", city[i], "/", sep = '')
    
    doc = htmlTreeParse(theurl,isURL=T)
    top = xmlRoot(doc)
    children = xmlChildren(top)$body
    
    path = c('content', 'guide', 'cuisines', 'all_cuisines')
    num.path = rep(NA, length(path))
    num.path[1] = which(as.character(xmlApply(children, xmlAttrs)) == path[1])
    if (!is.element(path[2], as.character(xmlApply(children[[num.path[1]]], xmlAttrs)))){
      cuisine.strings = 'all.cuis'
      cuisine.hrefs = ''
      index = 1
    } else{
      num.path[2] = which(as.character(xmlApply(children[[num.path[1]]], xmlAttrs)) == path[2])
      num.path[3] = which(as.character(xmlApply(children[[num.path[1]]][[num.path[2]]], xmlAttrs)) == path[3])
      num.path[4] = which(as.character(xmlApply(children[[num.path[1]]][[num.path[2]]][[num.path[3]]], xmlAttrs)) == path[4])
      
      all.cuisines = children[[num.path[1]]][[num.path[2]]][[num.path[3]]][[num.path[4]]]
      cuisine.strings = as.character(unlist(xmlApply(all.cuisines, xmlValue)))
      
      cuisine.hrefs = rep(NA, length(cuisine.strings))
      index = which(as.logical(sapply(xmlApply(all.cuisines, xmlValue), length)))
      
      counter = 1
      for (cuisine in index){
        cuisine.hrefs[counter] = as.character(xmlApply(all.cuisines[[cuisine]], xmlGetAttr, name = 'href'))
        counter = counter + 1
      }
    }
    
    cuis.urls = paste(theurl, cuisine.hrefs, sep = '')
    
    rest.list = list()
    for (cuis in 1:length(cuis.urls)){
      doc = htmlTreeParse(cuis.urls[cuis], isURL=T)
      top = xmlRoot(doc)
      children = xmlChildren(top)$body[[1]]
      path = which(as.character(xmlApply(children, xmlAttrs)) == 'restaurant_list')
      path = c(path, which(as.character(names(children[[path[1]]])) == 'ol'))
      cuis.rest.nodes = children[[path[1]]][[path[2]]]
      
      rest.list[[cuisine.strings[cuis]]] = list()
      for (rest in 1:length(cuis.rest.nodes)){
        rest.node = cuis.rest.nodes[[rest]]
        rest.path = which(as.character(xmlApply(rest.node, xmlAttrs)) == 'basics')
        rest.name.p = which(as.character(xmlApply(rest.node[[rest.path]], xmlAttrs)) == 'restaurant_name')
        rest.cuisines.p = which(as.character(xmlApply(rest.node[[rest.path]], xmlAttrs)) == 'restaurant_cuisines')
        rest.addr.p = which(as.character(xmlApply(rest.node[[rest.path]], xmlAttrs)) == 'restaurant_address')
        rest.href = as.character(xmlApply(rest.node[[rest.path]][[rest.name.p]], xmlGetAttr, name = 'href'))
        rest.name = as.character(xmlValue(rest.node[[rest.path]][[rest.name.p]]))
        rest.cuisines = as.character(xmlValue(rest.node[[rest.path]][[rest.cuisines.p]]))
        rest.addr = as.character(xmlValue(rest.node[[rest.path]][[rest.addr.p]]))
        rest.url = paste('http://www.allmenus.com', rest.href, sep = '')
        rest.list[[cuisine.strings[cuis]]][[rest]] = list(name = rest.name, address = rest.addr, cuisine = rest.cuisines, url = rest.url)
      }
    }
    
    rest.list.no.closed = list()
    for (cuis in 1:length(rest.list)){
      rest.list.no.closed[[cuisine.strings[cuis]]] = list()
      counter = 1
      for (rest in 1:length(rest.list[[cuis]])){
        temp = as.numeric(regexpr('.* - CLOSED', rest.list[[cuis]][[rest]][1]))
        is.closed = temp != -1
        if (!is.closed){
          rest.list.no.closed[[cuis]][[counter]] = rest.list[[cuis]][[rest]]
          counter = counter + 1
        }
      }
    }

    cuis.index = as.numeric(which(sapply(rest.list.no.closed, length)>0))
    for (cuis in cuis.index){
      for (rest in 1:length(rest.list.no.closed[[cuis]])){
        rest.url = rest.list.no.closed[[cuis]][[rest]]$url
        doc = htmlTreeParse(rest.url, isURL=T)
        top = xmlRoot(doc)
        children = xmlChildren(top)$body
        path = which(as.character(xmlApply(children, xmlAttrs)) == 'content')
        path = c(path, 1)
        if (is.element('alternative_menus', as.character(xmlApply(children[[path[1]]][[path[2]]], xmlAttrs)))){
          path = c(path, which(as.character(xmlApply(children[[path[1]]][[path[2]]], xmlAttrs)) == 'alternative_menus'))
          path = c(path, 1, 3, 1)
          alt.url = paste('http://www.allmenus.com',xmlGetAttr(children[[path[1]]][[path[2]]][[path[3]]][[path[4]]][[path[5]]][[path[6]]], 'href'), sep = '')
          rest.list.no.closed[[cuis]][[rest]][['alt.url']] = alt.url
        } else {
          rest.list.no.closed[[cuis]][[rest]][['alt.url']] = NA
        }
      }
    }
    
    menu.docs = list()
    for (cuis in cuis.index){
      menu.docs[[cuis]] = list()
      for (rest in 1:length(rest.list.no.closed[[cuis]])){
        menu.docs[[cuis]][[rest]] = list()
        rest.url = rest.list.no.closed[[cuis]][[rest]]$url
        doc = htmlTreeParse(rest.url, isURL=T)
        menu.docs[[cuis]][[rest]][['menu']] = doc
        if (!is.na(rest.list.no.closed[[cuis]][[rest]][['alt.url']])){
          alt.rest.url = rest.list.no.closed[[cuis]][[rest]]$alt.url
          doc = htmlTreeParse(alt.rest.url, isURL=T)
          menu.docs[[cuis]][[rest]][['alt.menu']] = doc
        } else {
          menu.docs[[cuis]][[rest]][['alt.menu']] = NA
        }
        if (rest%%20 == 0){print(rest)}
      }
    }
    
    save(rest.list.no.closed, file = paste(location, city[i], '.rest.list.Rout', sep = ''))
    save(menu.docs, file = paste(location, city[i], '.menus.Rout', sep = ''))
    
    # making file with names of restaurants included in the list
    rest.names = c()
    rest.addr = c()
    for (cuis in cuis.index){
      for (rest in 1:length(rest.list.no.closed[[cuis]])){
        rest.names = c(rest.names, rest.list.no.closed[[cuis]][[rest]]$name)
        rest.addr = c(rest.addr, rest.list.no.closed[[cuis]][[rest]]$address)
      }
    }
    rest.info = data.frame(name = rest.names, addr = rest.addr)
    write.table(rest.info, file = paste(location, city[i], '.allmenus_restaurant_names&addr.txt', sep = ''), row.names = F)
    print(i)
  }
}

################
##   going from menus to data
################

setwd('~/projects/Reich/menu.scrape')
city.menu.filenames = list.files(path = './outputs', pattern = '\\.menus\\.')
cities = gsub('(.*)\\.menus.Rout', '\\1', all.menu.list.names)

city.counter = 1
for (city in all.menu.list.names){
  load(paste('./outputs/', city.menu.filenames[1], sep = ''))
  
  
  city.counter = city.counter + 1
}

