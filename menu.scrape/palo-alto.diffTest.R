paloAltoMain = get.city.rest.urls('ca', 'palo-alto')
paloAltoAll = alt.menu.urls(paloAltoMain)
docs = get.menu.docs(paloAltoAll)
save(docs, file = paste(file.location, 'ca', '.', 'palo-alto', '.menuList.Rout', sep = ''))
