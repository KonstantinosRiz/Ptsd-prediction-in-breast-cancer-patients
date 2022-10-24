inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}

i <- 1
for (col_name in colnames(dataPCL)) {
  col <- dataPCL[, col_name]
  type <- class(col)

  ##### For table column #1 uncomment this
  
  # cat(col_name)
  
  ##### End column #1

  
  ##### For table column #2 uncomment this
  
  # if (type == 'numeric') {
  #   cat(type, ' (', typeof(col), ')', sep='')
  # }
  # else if (type == 'integer') {
  #   cat ('numeric (integer)')
  # }
  # else {
  #   cat(type)
  # }
  
  ##### End column #2
  
  ##### For table column #3 uncomment this
  # cat (i, ' ', sep='')
  if (type == class(factor()) && col_name != 'mrn') {
    num_lev <- nlevels(col)
    cat(num_lev, ' levels', ' ', sep='')

    for (level_name in levels(col)) {
      cat(level_name, ':', ' ', sep='')
    }
  }
  else {
    cat(' ')
  }

  cat('\n')
  # inc(i)
}




# require(XLConnect)
# wb <- loadWorkbook("test.xlsx", create=TRUE)
# setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
# 
# Data <- data.frame(
#   a = 1:10,
#   b = letters[1:10]
# )
# 
# writeWorksheet(wb,Data,"aSheet",startRow=1,startCol=1,header=TRUE)
# 
# saveWorkbook(wb)
