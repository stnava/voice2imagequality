library( randomForest )
myqc = read.csv( "data/brain_qc.csv" )
myrbp = read.csv( "data/rb_ppmi_orb.csv")
myrbp$id = tools::file_path_sans_ext( basename( myrbp$id ))
myqc$id = tools::file_path_sans_ext( basename( myqc$id ))
mydf = merge( myrbp, myqc, by='id' )
mydf = mydf[ !is.na( mydf$grade), ]
mydf$numericGrade = NA
mydf$numericGrade[mydf$grade =='a']=5
mydf$numericGrade[mydf$grade =='b']=4
mydf$numericGrade[mydf$grade =='c']=2
mydf$numericGrade[mydf$grade =='f']=0
nms=names(mydf)
istrain = sample( 1:nrow(mydf), round(0.9*nrow(mydf)) )
mydftr=mydf[istrain,]
mydfte=mydf[-istrain,]
myform = as.formula( paste( "numericGrade ~ ", paste0( nms[c(12:15, 22:24)], collapse='+') ) )
myform = as.formula( paste( "numericGrade ~ ", paste0( nms[c( 22:24)], collapse='+') ) )
mdl = lm(  myform , data=mydftr )
pred = predict( mdl, newdata=mydfte )
rtemis::mplot3.xy( mydfte$numericGrade, pred, fit=T, se.fit=T )
print(summary(mdl))
print( cor.test( mydfte$numericGrade, pred, type='k' ))
