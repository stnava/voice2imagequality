library( audio )
library( googleLanguageR )

recordIt <- function( ) {
  sample_rate <- 16000
  seconds_to_record <- 3
  num_channels <- 1
  reporting_interval_seconds <- 1
  # allocate buffer to record into
  buffer <- rep(NA_real_, sample_rate * seconds_to_record)
  # start recording into the buffer
  # readline(paste("press enter to record \n ") )
  Sys.sleep(2)
  print("Begin")
  rec <- record(buffer, sample_rate, num_channels)
  for (reports in seq(reporting_interval_seconds, seconds_to_record, reporting_interval_seconds)){
    Sys.sleep(reporting_interval_seconds)
    analysis_samples <- (((reports - 1) * sample_rate) + 1) : (reports * sample_rate)
    abs_max <- max(abs(buffer[analysis_samples]), na.rm = TRUE)
    # print(sprintf('Max Value = %2.3f dB FS', 20*log10(abs_max) ))
  }
  # play back the buffer
  # play(buffer)
  wavfn= "/tmp/temp.wav"
  save.wave(buffer, wavfn )
  temp = gl_speech( wavfn, languageCode = "en-US" )
  myword = temp$transcript[1]
  myconf = temp$transcript[2]
  myout = myword
  print( temp$transcript )
  if ( myword %in% c("hey","a")) myout='a'
  if ( myword %in% c("bee","be","c")) myout='b'
  if ( myword %in% c("see","sea","c")) myout='c'
  if ( myword %in% c("f","eff","ff","FF","F","fail")) myout='f'
  return( unlist( c(myout, myword, myconf ) ) )
  }

get_grade <- function( x ) {
  system( paste( "open", x ) )
  ANSWER=NA
  pgrades = c( 1,2,3,4,5, "a","b","c","d","f" )
  pgrades = c( "a","b","c","f" )
  pgradeschar = paste0( pgrades, collapse='/' )
  while ( ! ANSWER %in% pgrades )
    ANSWER <- readline(paste("Enter grade for:", basename(x), "[", pgradeschar, "] \n ") )
  return( ANSWER )
}


get_grade_audio <- function( x ) {
  system( paste( "open", x ) )
  ANSWER=NA
  pgrades = c( 1,2,3,4,5, "a","b","c","d","f" )
  pgrades = c( "a","b","c","f" )
  pgradeschar = paste0( pgrades, collapse='/' )
  while ( ! ANSWER %in% pgrades ) {
    print( paste("Say grade for:", basename(x), "[", pgradeschar, "] \n ") )
    temp = recordIt()
    ANSWER=temp[1]
  }
  return( ANSWER )
}

figdir='/tmp/inspection/'
demogout = "data/pdbp_qc.csv"
demog = read.csv( demogout )
print( table( demog$grade ) )
hasqc=which( !is.na( demog$grade ) )
noqc=which( is.na( demog$grade ) )
print( paste("HacQC:", length(hasqc),"vs",length(noqc)))
if ( length(hasqc) > 0  & length( noqc ) > 0 )
  noqc = c( noqc, sample( hasqc, 20 ) ) else noqc = hasqc
for ( k in sample(noqc) ) {
  figfn=paste0( figdir, demog$id[k] )
  # mygrade = get_grade( figfn )
  mygrade = get_grade_audio( figfn )
  demog[k,'method']='voice'
  if ( is.na( demog$grade[k] ) ) {
    demog$grade[k] = as.character( mygrade )
    demog[k,'time']=Sys.time()
  } else {
    demog[k,'grade.repeat'] = as.character( mygrade )
    demog[k,'time.repeat']=Sys.time()
  }
  print( demog[k,] )
  write.csv( demog, demogout, row.names=FALSE)
  }
