get_delta <- function(dfile, xtaxa, cname, mean='grand_mean',se = 'grand_SE'){
  
  z = grep('taxa', colnames(dfile))
  if(length(z)==0){dfile$taxa = dfile$species}
  df1 = filter(dfile, taxa==xtaxa & year == data_year)# this year
  df2 = filter(dfile, taxa==xtaxa & year == data_year-1)# last year
  
  deltaSPP = df1[,mean] - df2[,mean] # change
  #deltaSPP
  if(deltaSPP > 0){dspp = " increased"}else{dspp = " decreased"}
  dtext = paste0("The abundance of ", cname, dspp," from ", 
                 round(df2[,mean],1)," (\u00B1 ",round(df2[,se],1),
                 " se) 120 m\u207B\u00B3 in ", data_year-1, " to ", 
                 round(df1[,mean],1)," (\u00B1 ",round(df1[,se],1),
                 " se) 120 m\u207B\u00B3 in ", data_year)
  return(dtext)
}