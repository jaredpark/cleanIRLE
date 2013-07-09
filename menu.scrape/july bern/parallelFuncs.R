# parallelFuncs is edit to scrape.funcs.v3; run on ec2 - grubhub shuts me out after ~500 requests

# for one city/state:
# 1. cuisUrls = get.city.cuis.urls(state, city) --> outputs a vector containing city cuisine urls
##### cost = 1 search
# 2. loop over elements of cuisUrls
#    a. counter = 1
#    b. restUrls = get.cuis.rest.urls(cuisUrls[element]) --> outputs a vector containing cuisine restaurant urls
##### cost = (# of cuisines) searches
#    c. loop over every N restaurants, sections, in restUrls vector - batch new job
#       i. restUrlList = alt.menu.urls(restUrls[section]) --> outputs a list; one element for each of N restaurants, each with number of menu types elements, each with menu url
##### cost = N searches
#       ii. menuList = get.menu.docs(restUrlList) --> outputs a list; one element for each restaurant of N restaurants, each with number menus elements, each with menu html
##### cost = (# of menus between N restaurants) searches
#       iii. save menuList as "state.city.menuList.(counter).(date).Rout"
#       iv. counter = counter + 1
# 3. loop over i in 1:counter
#   a. load "state.city.menuList.(i).(date).Rout" --> object called 'menuList'
#   b. cityMenuList = append(cityMenuList, menuList)
# 4. run state.to.df(state, fileLocation)

get.one.menu = function(url){
  doc = htmlTreeParse(url, isURL=T) # one request per restaurant menu
  return(doc)
}

get.city.cuis.urls = function(state, city){  
  
  require('XML')
  
  rootUrl <- paste("http://www.allmenus.com/", state, "/", city, "/", sep = '')
  
  doc = htmlTreeParse(rootUrl,isURL=T) # 1 request per city/state
  top = xmlRoot(doc)
  children = xmlChildren(top)$body
  contentPath = which(as.character(xmlApply(children, xmlAttrs)) == 'content')
  children = children[[contentPath]]
  cuisinePath = grep('cuisines', as.character(xmlApply(children, xmlAttrs)))
  children = children[[cuisinePath]]
  urlPath = which(as.character(xmlApply(children, xmlAttrs)) == 'all_cuisines')
  all.cuisines = children[[urlPath]]
  cuisine.strings = as.character(unlist(xmlApply(all.cuisines, xmlValue)))
  
  cuisine.hrefs = rep(NA, length(cuisine.strings))
  index = which(as.logical(sapply(xmlApply(all.cuisines, xmlValue), length)))
  
  counter = 1
  for (cuisine in index){
    cuisine.hrefs[counter] = as.character(xmlApply(all.cuisines[[cuisine]], xmlGetAttr, name = 'href'))
    counter = counter + 1
  }
  
  cuis.urls = paste(rootUrl, cuisine.hrefs, sep = '')
  return(cuis.urls)
}

get.cuis.rest.urls = function(cuis.url){  
  url = paste(cuis.url, '&filters=none', sep = '')
  doc = htmlTreeParse(url, isURL=T) # one request per cuisine in city/state
  top = xmlRoot(doc)
  children = xmlChildren(top)$body[[1]]
  path = which(as.character(xmlApply(children, xmlAttrs)) == 'restaurant_list')
  path = c(path, which(as.character(names(children[[path[1]]])) == 'ol'))
  cuis.rest.nodes = children[[path[1]]][[path[2]]]
  cuis.rest.urls = c()
  rest.names = c()
  rest.addrs = c()
  for (rest in 1:length(cuis.rest.nodes)){
    rest.node = cuis.rest.nodes[[rest]]
    rest.path = which(as.character(xmlApply(rest.node, xmlAttrs)) == 'basics')      
    rest.name.p = which(as.character(xmlApply(rest.node[[rest.path]], xmlAttrs)) == 'restaurant_name')
    rest.href = as.character(xmlApply(rest.node[[rest.path]][[rest.name.p]], xmlGetAttr, name = 'href'))
    rest.name = as.character(xmlValue(rest.node[[rest.path]][[rest.name.p]]))
    rest.addr.p = which(as.character(xmlApply(rest.node[[rest.path]], xmlAttrs)) == 'restaurant_address')
    rest.addr = as.character(xmlValue(rest.node[[rest.path]][[rest.addr.p]]))
    rest.url = paste('http://www.allmenus.com', rest.href, sep = '')
    cuis.rest.urls = c(cuis.rest.urls, rest.url)
    rest.names = c(rest.names, rest.name)
    rest.addrs = c(rest.addrs, rest.addr)
  }

  names.and.addrs = paste(rest.names, rest.addrs)
  names(cuis.rest.urls) = names.and.addrs
  cuis.rest.urls = cuis.rest.urls[!duplicated(names.and.addrs)]
  if (length(grep('CLOSED', names(cuis.rest.urls), ignore.case = T))){
    cuis.rest.urls = cuis.rest.urls[-c(0,grep('CLOSED', names(cuis.rest.urls), ignore.case = T))]
  }
  
  return(cuis.rest.urls)
}

alt.menu.urls = function(rest.urls){
  url.list.output = list()
  for (rest in 1:length(rest.urls)){
    url.list.output[[rest.urls[rest]]] = list('main' = rest.urls[rest])
    rest.url = rest.urls[rest]
    doc = htmlTreeParse(rest.url, isURL=T) # one request per restaurant menu
    top = xmlRoot(doc)
    children = xmlChildren(top)$body
    path = which(as.character(xmlApply(children, xmlAttrs)) == 'content')
    path = c(path, 1)
    if (is.element('alternative_menus', as.character(xmlApply(children[[path[1]]][[path[2]]], xmlAttrs)))){
      path = c(path, which(as.character(xmlApply(children[[path[1]]][[path[2]]], xmlAttrs)) == 'alternative_menus'))
      path = c(path, 1)
      num.alt.menus = length(xmlApply(children[[path[1]]][[path[2]]][[path[3]]][[path[4]]], xmlAttrs)) - 2
      for (alt in 1:num.alt.menus){
        alt.url = paste('http://www.allmenus.com',xmlGetAttr(children[[path[1]]][[path[2]]][[path[3]]][[path[4]]][[2+alt]][[1]], 'href'), sep = '')
        url.list.output[[rest.urls[rest]]][[alt+1]] = alt.url
      }
    }
  }
  return(url.list.output)
}
# b = alt.menu.urls(a)

get.menu.docs = function(url.list){
  menu.docs = list()
  for (rest in 1:length(url.list)){
    menu.docs[[url.list[[rest]][[1]]]] = list()
    for (menu in 1:length(url.list[[rest]])){
      rest.url = url.list[[rest]][[menu]]
      doc = htmlTreeParse(rest.url, isURL=T) # one request per restaurant menu
      menu.docs[[names(url.list[[rest]][[1]])]][[menu]] = doc
    }
  }
  return(menu.docs)
}

get.one.menu = function(url){
  doc = htmlTreeParse(url, isURL=T) # one request per restaurant menu
  return(doc)
}

# c = get.menu.docs(b)
# d = menu.to.matrix(c[[1]][[1]])

get.menus = function(state, city, file.location){
  main.urls = get.city.rest.urls(state, city)
  all.urls = alt.menu.urls(main.urls)
  docs = get.menu.docs(all.urls)
  save(docs, file = paste(file.location, state, '.', city, '.menuList.Rout', sep = ''))
  print(paste('finishing', city))
}

menu.to.matrix = function(document){
  require('XML')
  datum.names = c('restaurant', 'phone', 'street', 'city', 'state', 'zip',
                  'menu', 'category', 'item', 'price', 'low', 'high')
  data.out = NULL
  
  menu.type = sub('http:.*/(.*)/$', '\\1', document$file)
  
  top = xmlRoot(document)
  children = xmlChildren(top)$body
  path = which(as.logical(xmlApply(children, xmlAttrs) == 'content'))
  child = children[[path]]
  path = as.numeric(which(xmlApply(child, xmlGetAttr, 'id') == 'restaurant'))
  g.child = child[[path]]
  if (!is.element('menu', xmlApply(g.child, xmlAttrs))){
    return(NULL)
  } else{
    # getting basic restaurant info
    
    name = xmlValue(g.child[[as.numeric(which(xmlApply(g.child, xmlAttrs) == 'name'))]])
    
    path = as.numeric(which(xmlApply(g.child, xmlAttrs) == 'primary_info'))
    restaurant.info = g.child[[path]]
    
    raw.address = as.character(xmlApply(restaurant.info[[as.numeric(which(xmlApply(restaurant.info, xmlGetAttr, 'id') == 'address'))]], xmlValue))
    raw.phone = xmlValue(restaurant.info[[which(xmlApply(restaurant.info, xmlGetAttr, 'id') == 'phone_number')]])
    if (length(raw.phone) == 0){
      phone = 'NA'
    } else {
      phone = sub('\\(([[:digit:]]+)\\) (.*)', '\\1-\\2', raw.phone)
    }
    
    if (raw.address[2] == ','){
      
      street = raw.address[1]
      city = raw.address[3]
      state = raw.address[4]
      zip = raw.address[5]
      
    } else {(stop('document -> content -> restaurant -> primary_info -> address is not in anticipated format'))}
    
    # move to menu node
    path = which(as.logical(xmlApply(g.child, xmlAttrs) == 'menu'))
    this.menu = g.child[[path]]
    num.categories = length(xmlApply(this.menu, xmlAttrs))
    
    for (category in 1:num.categories){
      this.category = this.menu[[category]]
      
      category.string = xmlValue(this.category[[as.numeric(which(xmlApply(this.category, xmlAttrs) == 'category_head'))]][[1]])
      if (length(category.string) == 0){
        category.string = NA
      }
      
      category.child = this.category[[2]]
      num.items.in.category = length(xmlApply(category.child, xmlAttrs))
      
      for (item in 1:num.items.in.category){
        this.item = category.child[[item]]
        item.name = xmlValue(this.item[[which(xmlApply(this.item, xmlAttrs) == 'name')]])
        if (length(item.name) == 0){
        } else{
          if (is.element('price', xmlApply(this.item, xmlAttrs))){
            item.price.string = xmlValue(this.item[[which(xmlApply(this.item, xmlAttrs) == 'price')]])
            if (sub('(.*) - .*', '\\1', item.price.string) == item.price.string){
              item.price = as.numeric(sub('\\$([[:digit:]]+\\.[[:digit:]]+)', '\\1', item.price.string))
              item.low = NA
              item.high = NA
            } else{
              item.price = NA
              priceString1 = sub('(.*) - (.*)', '\\1', item.price.string)
              priceString2 = sub('(.*) - (.*)', '\\2', item.price.string)
              price1 = as.numeric(sub('\\$(.*)', '\\1', priceString1))
              price2 = as.numeric(sub('\\$(.*)', '\\1', priceString2))
              item.low = min(c(price1, price2))
              item.high = max(c(price1, price2))
            }
            
          } else {
            item.price = NA
            item.low = NA
            item.high = NA
          }
          #     item.description = xmlValue(this.item[[which(xmlApply(this.item, xmlAttrs) == 'description')]])
          if (sum(na.exclude(c(item.price, item.low, item.high))) != 0){
            
            newline = c(name, phone, street, city, state, zip,
                        menu.type, category.string, item.name, 
                        item.price, item.low, item.high)
            cleanNewline = gsub("'", "", newline)
            cleanNewline = gsub('"', '', cleanNewline)
            if (length(newline) != length(datum.names)){
              print(newline)
            }
            data.out = rbind(data.out, cleanNewline) #, item.description))
          } else {
          }
        }
      }
    }
    if (is.null(data.out)){
    } else {
      colnames(data.out) = datum.names
      return(data.out)
    }
  }
}

restaurant.to.matrix = function(rest.menu.list){
  rest.menu.out = NULL
  for (menu.number in 1:length(rest.menu.list)){
    rest.menu.out = rbind(rest.menu.out, menu.to.matrix(rest.menu.list[[menu.number]]))
  }
  return(rest.menu.out)
}

city.to.df = function(city.rest.list){
  city.df.out = NULL
  num.rest.no.prices.in.city = 0
  
  for (rest in 1:length(city.rest.list)){ 
    restaurant.matrix = restaurant.to.matrix(city.rest.list[[rest]])
    restaurant.df = as.data.frame(restaurant.matrix, stringsAsFactors = F)
    has.prices = sum(na.exclude(as.numeric(restaurant.df$price))) != 0
    
    if (has.prices){
      city.df.out = rbind(city.df.out, restaurant.df)
    }
  }
  return(city.df.out)
}

state.to.df = function(state, file.location){
  city.menu.filenames = list.files(path = file.location, pattern = paste('^', state, '\\..*\\.menuList\\.', sep = ''))
  state.data.out = NULL
  for (file in city.menu.filenames){
    print(paste('loading', file))
    load(paste(file.location, file, sep = ''))
    state.data.out = rbind(state.data.out, city.to.df(docs))
  }
  return(state.data.out)
}