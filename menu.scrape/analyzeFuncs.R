reformatLaterDataPull = function(t0, t1){
  newData = NULL
  for (row in 1:nrow(t0)){
    if (row%%5000 == 1){
      print(row)
    }
    rowData0 = t0[row, ]
    rest = rowData0$restaurant
    loc = rowData0$street
    item = rowData0$item
    locIndex = which(t1$street == loc & t1$restaurant == rest)
    if (length(locIndex) == 0){
      rowData1 = c('location closed or moved', rep(NA, length(rowData0)-1))
    } else {
      locData1 = t1[locIndex, ]
      sameItemIndex = which(locData1$item == item)
      if (length(sameItemIndex) == 0){
        rowData1 = c('item removed or changed names', rep(NA, length(rowData0)-1))
      } else {
        if (length(sameItemIndex) > 1){
          sameMenuIndex = locData1$menu[sameItemIndex] == rowData0$menu
          sameCategoryIndex = locData1$category[sameItemIndex] == rowData0$category
          sameItemIndex = sameItemIndex[sameMenuIndex & sameCategoryIndex]
          if (length(sameItemIndex) > 1){
            sameItemIndex = sameItemIndex[1]
          }
        }
        rowData1 = locData1[sameItemIndex, ]
      }
    }
    newData = rbind(newData, rowData1)
  }
  return(newData)
}

# newdata = NULL
# for each row r in t0:
#   i1 <- rows of t1 that are the same restaurant
#   i2 <- i1 & rows of t1 that are the same item name as item in row r
#   if length(i2) > 1:
#     menu0 = t0$menu[r]
#     sameMenu = which(t1$menu[i2] == menu0)
#   if length(i2) != 0:
#     sameMenu = True
#     price0 = t0$price[i2[sameMenu]]
#     low0 = t0$low[i2[sameMenu]]
#     ...
#     d = c(price1 - price0, low1 - low0, high1 - high0)
#     newrow = c(t0[r,], t1[i2, !(1:6)], d) with 1:6 holding restaurant info
#     newdata = rbind(newdata, newrow)
#   
#   
