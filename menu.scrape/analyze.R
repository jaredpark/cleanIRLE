source('analyzeFuncs.R')
source('scrape.funcs.v3.R')

load('./outputs.v2/CA_12_24_2012.Rout')
load('./outputs.v2/NM_12_24_2012.Rout')
ca.0 = ca
nm.0 = nm

load('./outputs.pull2/CA_03_10_2013.Rout')
load('./outputs.pull2/NM_03_10_2013.Rout')
ca.1 = ca
nm.1 = nm

rm(ca, nm)

file.location = '~/projects/Reich/menu.scrape/outputs.pull2/'

ca.1.reformat = reformatLaterDataPull(ca.0, ca.1)
nm.1.reformat = reformatLaterDataPull(nm.0, nm.1)

rest.0 = unique(nm.0$restaurant)
diff = NULL
num = NULL
for (rest in rest.0){
  num0 = sum(nm.0$restaurant == rest)
  num1 = sum(nm.1$restaurant == rest)
  if (num0 != num1){
    diff = c(diff, rest)
    num = c(num, num0)
  }
}
diff[9]
nm.0[nm.0$restaurant == diff[9], 9]
nm.1[nm.1$restaurant == diff[9], ]
nm.1.reformat[nm.1.reformat$restaurant == diff[9], 9]

plot(early$price, a$price, ylim = c(0,20), xlim = c(0,20))
plot(early$low, a$low, ylim = c(0,20), xlim = c(0,20))
plot(early$high, a$high, ylim = c(0,20), xlim = c(0,20))

d = as.numeric(a$price) - as.numeric(early$price)
restNoDiff = unique(early$restaurant[d == 0])
restWithDiff = unique(early$restaurant[d != 0])
early[d != 0, ]
length(d)