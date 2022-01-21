

for ( k in which( !is.na( demog$grade.repeat) ) ) {
  figfn=paste0( figdir, demog$id[k] )
  print( demog[k,] )
  mygrade = get_grade( figfn )
  demog[k,'method']='keyboard'
  demog[k,'grade.repeatagain'] = as.character( mygrade )
  print( demog[k,] )
  write.csv( demog, demogout, row.names=FALSE)
  }
