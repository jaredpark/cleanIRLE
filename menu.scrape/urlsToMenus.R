# 2. cont
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

date = '06_02_2013'
root = '/home/jared'
numRestPerScript = 10
fileDir = paste(root, '/objects', sep = '')
city = 'albuquerque'
require(XML)

scriptNum = 1
restUrls = read.table(paste(fileDir, '/script.', scriptNum, '.urls.txt', sep = ''))

restUrlList = alt.menu.urls(restUrls)
menuList = get.menu.docs(restUrlList)

save(menuList, file = paste(fileDir, '/', city, '.menuList.', scriptNum, '.Rout'))

