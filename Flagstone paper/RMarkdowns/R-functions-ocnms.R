
#### Summary Stats Function ####

Sum_Stats <- function( form1 , data.file ){
     # calculate a bunch of summary stats and put in one file
     # text in quotes, form1 looks like: "Y = site + year"
     # no spaces allowed in column titles
     
     library('stringr')
     data.file$xNx = 1
     
     mn = aggregate( as.formula( form1 ) , data = data.file , FUN = mean )
     s2 = aggregate( as.formula( form1 ) , data = data.file , FUN = var )
     
     x = stringr::str_split( form1 , "~", simplify = TRUE )[2]
     form2 = paste0( 'xNx ~ ', x )
     n  = aggregate( as.formula( form2 ) , data = data.file , FUN = sum )
     
     BY = stringr::str_split( x , "\\+" , simplify = TRUE ) 
     BY = stringr::str_remove_all(BY," ")
     BY = as.vector(BY , mode = 'character')
     
     a = merge( mn , s2 , by = BY, all = TRUE )
     z = merge( a , n , by = BY , all = TRUE )
     colnames( z )[(ncol(z)-2):ncol(z)] <- c( 'mean' , 'var' , 'n' ) 
     
     z$sd = sqrt(z$var)
     z$se = z$sd/sqrt((z$n-1))
     
     return( z )
     
}


### Add various information to files ####

# Add plotting info to files
# Colnames specific to these files

Add_Info <- function(data.file, cnames){ #specific to this file
     df = data.file
     df_info = df[, cnames]
     nc = ncol(df_info)
     df_info$col <- site.col$col[match( df_info$site , site.col$site)]
     df_info$pch <- year.pch$pch[match( df_info$year , year.pch$year)]
     df_info$pch = as.numeric(df_info$pch)
     z = grep("zone", cnames)
     if( length(z)==1 ){ df_info$bgcol <- ifelse(df_info$zone==5, 'white', df_info$col) }else{df_info$bgcol = df_info$col}
     
     df1 = cbind(df_info , df[,c(nc+1):ncol(df)])
}


### adonis2 correction ####
# corrects fomatting differences between adonis and andons2 ouput

adonis2.correction <- function(data.file){
     x <- data.frame(data.file)
     colnames(x) = c('Df','SumsOfSqs','R2','F.Model','Pr(>F)')
     x$MeanSqs = x$SumsOfSqs / x$Df
     x = x[, c('Df','SumsOfSqs', 'MeanSqs', 'F.Model', 'R2','Pr(>F)')]
     r = grep('Residual',rownames(x))
     rownames(x)[r] <- 'Residuals'
     return(x)
}

#### ordination code ####

run.multivar.nt <- function(data.file, drop2015 = TRUE, spp, data.transform = NA, nperm = 999, 
                            outname = 'Results-' ){
     
     data.file1 = data.file
     
     if(drop2015==TRUE){data.file1 = data.file[data.file$year != 2015,]}
     
     df = data.file1[,spp]
     if(is.na(data.transform) == TRUE){df = data.file1[,spp]}
     if(is.na(data.transform) == FALSE){
          if(data.transform == "sqrt"){df = data.file1[,spp]^(1/2)}
          if(data.transform == "4th-root"){df = data.file1[,spp]^(1/4)}
          if(data.transform == "log"){df = log(data.file1[,spp]+1) }
     }
     
     ## capscale 
     CAPid = as.factor(paste(data.file1$site , data.file1$zone , data.file1$year, sep = "_") )
     CAPfile = data.frame(cbind(CAPid,data.file1))
     
     #library(ecole)
     df_matrix <- ecole::bray0(df)
     
     assign('df_matrix', get('df_matrix'), pos=1)
     
     cap = CAPdiscrim( df_matrix ~ as.factor(CAPid) , data=CAPfile , m=0 , permutations=nperm )
    
     cap <- add.spec.scores(cap , df)
     cap.scores = scores(cap)
     cap.df = data.frame(cbind( data.file1 , cap.scores ) )

     capture.output( cap, paste0(outname,"CAP_site_depth_year.txt") )
     saveRDS( cap, paste0(outname,"CAP_site_depth_year.rds") )
     saveRDS( cap.scores , paste( outname,  "cap.scores.rds") )
     write.csv( cap.df , paste0(outname,  "data_YSD.csv"), row.names = FALSE )
     
     ## permanova
     
     data.file1$year = as.factor(data.file1$year)
     
     pman = adonis2( df_matrix ~ zone + site + year +
                                 zone*site + zone*year + site*year, 
                                 data = data.file1, by="term", permutations = nperm )
     
     pm.tab <- adonis2.correction(pman)
     
     vexp = 1 - pm.tab["Residual" , "R2"]
     texp = round(pm.tab[1:7 , "R2"]/vexp, 3)
     
     trms = rownames(pm.tab)[1:7]
     (prop = data.frame(cbind(trms,texp)))
     
     saveRDS(pman, file = paste0(outname,'PerMANOVA_transect_all.rds'))
     capture.output(pman, file =  paste0(outname,'PerMANOVA_transect_all.txt'))
     
     ######### convert to random effects
     
     vcomp = varcompnt( x = pm.tab, y = data.file1)
     
     saveRDS(vcomp, file = paste0(outname, 'PerMANOVA_transect_all-var_comp.rds'))
     capture.output(vcomp, file =  paste0(outname, 'PerMANOVA_transect_all-var_comp.txt'))
     
     results <- list(cap.df , cap, pman, vcomp)
     
     names(results) = c('cap.df','cap','pman', 'vcomp')
     return(results)
}

### Plot capscale ordinations/summarize data ####

Plot_Ordination <- function( data.file , ord.file, Yform, Xform, method = "CAPdiscrim",
                             Xlim=NA, Ylim=NA, Xlim2 = NA, Ylim2=NA, Xlab = "Axis 1", Ylab = "Axis 2", 
                             min.score = 0.0, plot.species = TRUE, spp.separate = FALSE, 
                             fig.legend=NA, legend.pos='topleft', sppcol='red', bg.equals.col=TRUE){
     
     form1 = paste0(Yform, '1', Xform)
     form2 = paste0(Yform, '2', Xform)
     df_1 = Sum_Stats( form1 , data.file )
     df_2 = Sum_Stats( form2 , data.file )
     
     if(bg.equals.col == TRUE){
          df_1$bgcol = df_1$col
          df_2$bgcol = df_2$col
     }
     
     if(is.na(Xlim[1]) | is.na(Ylim[1])){
          plot( df_1$mean , df_2$mean, pch = as.numeric(df_1$pch) , col = df_1$col, bg = df_1$bgcol , xlab=Xlab, ylab=Ylab)}else{
               plot( df_1$mean , df_2$mean, pch = as.numeric(df_1$pch) , col =df_1$col, bg = df_1$bgcol, xlim = Xlim, ylim = Ylim , xlab=Xlab, ylab=Ylab )
          }
     # error bars
     arrows( df_1$mean+df_1$se , df_2$mean,  df_1$mean-df_1$se , df_2$mean, col = df_1$col, length = 0)
     arrows( df_1$mean, df_2$mean+df_2$se , df_1$mean , df_2$mean-df_2$se, col = df_1$col, length = 0)
     segments( par()$usr[1],0,par()$usr[2],0, lty = 'dotted', lwd = 0.5)
     segments( 0, par()$usr[3],0,par()$usr[4], lty = 'dotted', lwd = 0.5)
     # spp spp scores
     
     if(!is.na(fig.legend)){legend(legend.pos, legend = fig.legend, bty='n')}
     
     if(method == "CAPdiscrim"){spp_scores = ord.file$cproj}else{spp_scores = ord.file$CCA$biplot}
     
     if(plot.species == TRUE){ 
          spp_scores1 = spp_scores[ abs(spp_scores[,1]) > min.score | abs(spp_scores[,2]) > min.score, ]
          text(spp_scores1[,1] , spp_scores1[,2], rownames(spp_scores1), cex=0.8 , col=sppcol)
     }
     if(spp.separate == TRUE){
          spp_scores1 = spp_scores[ abs(spp_scores[,1]) > min.score | abs(spp_scores[,2]) > min.score, ]
          if(is.na(Xlim2)[1] | is.na(Ylim2)[1] ){
               plot(spp_scores1[,1] , spp_scores1[,2], pch="" , xlab=Xlab, ylab=Ylab)}else{
                    plot(spp_scores1[,1] , spp_scores1[,2], pch="" , xlim = Xlim2, ylim = Ylim2 , xlab=Xlab, ylab=Ylab)
               }
          
          text(spp_scores1[,1] , spp_scores1[,2], rownames(spp_scores1), cex=0.8 , col=sppcol)
          segments( par()$usr[1],0,par()$usr[2],0, lty = 'dotted', lwd = 0.5)
          segments( 0, par()$usr[3],0,par()$usr[4], lty = 'dotted', lwd = 0.5)
     }
     # zero guides
     segments( par()$usr[1],0,par()$usr[2],0, lty = 'dotted', lwd = 0.5)
     segments( 0, par()$usr[3],0,par()$usr[4], lty = 'dotted', lwd = 0.5)
}

#### variance component function ####

# function to calculate variance components model
# specific to this file and model structure

varcompnt <- function(x = anovtab, y = original.data){
     # Depth = Fixed; site & year, random
     # Assumes IV ~ zone + site + year
     # original data (wide format) to get n for var comps
     
     # re-calculate F tests
     
     vctab = x[ , c('Df','MeanSqs','F.Model')]
     colnames(vctab)[3] = 'F'
     
     adenom = x['zone:site','MeanSqs'] +  x['zone:year','MeanSqs'] - x['zone:site:year','MeanSqs']
     vctab['zone','F'] = x['site','MeanSqs'] / adenom
     vctab['site','F'] = x['site','MeanSqs'] / x['site:year','MeanSqs']
     vctab['year','F'] = x['year','MeanSqs'] / x['site:year','MeanSqs']
     vctab['zone:site','F'] = x['zone:site','MeanSqs'] / x['zone:site:year','MeanSqs']
     vctab['zone:year','F'] = x['zone:year','MeanSqs'] / x['zone:site:year','MeanSqs']
     vctab['site:year','F'] = x['site:year','MeanSqs'] / x['Residuals','MeanSqs']
     # vctab['zone:site:year','F'] = x['zone:site:year','MeanSqs'] / x['Residuals','MeanSqs']
     
     # get correct denominator DF; df2
     
     vctab$df2 = NA
     
     adf2 = adenom / 
          (
               ( x['zone:site','MeanSqs'] / x['zone:site','Df']) + 
                    ( x['zone:year','MeanSqs'] / x['zone:year','Df']) + 
                    ( x['zone:site:year','MeanSqs'] / x['zone:site:year','Df']) 
          )
     
     vctab['zone','df2'] = adf2
     vctab['site','df2'] = x['site:year','Df']
     vctab['year','df2'] = x['site:year','Df']
     vctab['zone:site','df2'] = x['zone:site:year','Df']
     vctab['zone:year','df2'] = x['zone:site:year','Df']
     vctab['site:year','df2'] = x['Residuals','Df']
     # vctab['zone:site:year','df2'] = x['Residuals','Df']
     
     vctab = vctab[,c('Df','df2', 'MeanSqs','F')]
     colnames(vctab) = c('df1','df2', 'MS','F')
     
     # get p-value from F-test
     vctab$P = NA
     for(i in 1:7){
          vctab$P[i] =  pf(vctab$F[i], vctab$df1[i],vctab$df2[i], lower.tail = FALSE)
     }
     
     # vc proportions
     # pqr from model
     p = vctab['zone','df1']+1
     q = vctab['site','df1']+1
     r = vctab['year','df1']+1
     
     # calculate harmonic mean for unbalanced designs
     
     y$n = 1
     ABC = aggregate( n ~ zone + site + year, data=y, FUN = sum)
     hmean = p*q*r/sum(1/ABC$n)
     n = hmean
     
     # add variance components
     vctab$vc = NA
     vctab['site','vc'] = (x['site','MeanSqs'] - x['site:year','MeanSqs'])/(n*p*r)
     vctab['year','vc'] = (x['year','MeanSqs'] - x['site:year','MeanSqs'])/(n*p*q)
     vctab['zone:site','vc'] = (x['zone:site','MeanSqs'] - x['zone:site:year','MeanSqs'])/(n*r)
     vctab['zone:year','vc'] = (x['zone:year','MeanSqs'] - x['zone:site:year','MeanSqs'])/(n*q)
     vctab['site:year','vc'] = (x['site:year','MeanSqs'] - x['Residuals','MeanSqs'])/(n*p)
     # vctab['zone:site:year','vc'] = (x['zone:site:year','MeanSqs'] - x['Residuals','MeanSqs']) /(n)
     vctab['Residuals','vc'] <- vctab['Residuals','MS']
     
     #convert to proportions
     tvc = sum(vctab$vc, na.rm = TRUE)
     vctab$prop = vctab$vc/tvc
     return(vctab)
     
} # end function     

#### to_wide #####

to_wide <- function(data.file, spp.gr = 'taxa'){
     data.file$taxa.gr = data.file[,spp.gr]
     form1 =  as.formula(paste0('Count ~ year + site + zone + transect + ',spp.gr))
     df1 = aggregate(Count ~ year + site + zone + transect + taxa.gr, data = data.file , FUN = sum )
     dfw = spread(df1, taxa.gr , Count)
     df = Add_Info(dfw, c('year','site','zone'))
     rownames(df) <- paste0('Tran_' , 1:nrow(df) )
     return(df)
}

######### species by year #####

species.plot <- function(data.file, 
                         group.var = 'species', 
                         years = 2015:2021, 
                         Xlab ="",
                         Ylim = NA,
                         Ylab = "Log(x+1)", 
                         errors = 1,
                         Colors = NA){
     
     if(is.na(Ylim)[1]==TRUE){Ylim = c( min(data.file$mean-data.file$se), max(data.file$mean+data.file$se) )}
     x = grep(group.var,colnames(data.file))
     data.file$gvar = data.file[,2]
     
     p1 = ggplot(data.file,  aes( x = year , y = mean, color = gvar, fill = gvar) ) + 
          geom_line( size = 1 ) + 
          geom_point(size = 2) +
          xlab(Xlab) +
          ylab(Ylab) +
          ylim(Ylim) +
          scale_x_continuous('', labels = as.character(years), breaks = years)+
          theme_bw()+
          theme(legend.title = element_blank())
     
     if(errors == FALSE){ p1 <- p1 }
     if(errors == 1){ p1 <- p1 + geom_errorbar( aes(ymin = se_lo, ymax = se_up), width=0 ) }
     if(errors == 2){ p1 <- p1 + geom_ribbon(   aes(ymin = se_lo, ymax = se_up), alpha = 0.5) }
     if(!is.na(Colors)[1]){ p1 <- p1 + scale_color_manual(values = Colors)}
     
     return(p1)
     
}


######### species by YxS #############

species.plot.SxY <- function(data.file , 
                             years = 2015:2021, 
                             Xlab ="",
                             Ylim = NA,
                             Ylab = "Mean"
){
     #data.file = fishx[fishx$taxa=="SEME",]
    
     data.file <- data.file[data.file$site %in% c('Neah Bay',
                                                  'Tatoosh Island',
                                                  'Cape Alava',
                                                  'Cape Johnson',
                                                  'Destruction Island'),]
    
    
    
     data.file$site <- factor(data.file$site, levels = c('Neah Bay',
                                                         'Tatoosh Island',
                                                         'Cape Alava',
                                                         'Cape Johnson',
                                                         'Destruction Island'))
     p1 = ggplot(data.file,  aes( x = year , y = mean, color=site) ) + 
          geom_line( size = 1 ) + 
          geom_point(size = 2) +
          xlab(Xlab) +
          ylab(Ylab) +
          ggtitle(data.file$taxa[1]) + 
          # ylim(Ylim) +
          scale_x_continuous('', labels = as.character(years), breaks = years)+
          scale_color_manual(values = col) +
          theme_bw()+
          theme(legend.title = element_blank())
     p1
     # if(errors == FALSE){ p1 <- p1 }
     # if(errors == 1){ p1 <- p1 + geom_errorbar( aes(ymin = se_lo, ymax = se_up), width=0 ) }
     # if(errors == 2){ p1 <- p1 + geom_ribbon(   aes(ymin = se_lo, ymax = se_up), alpha = 0.5) }
     
     return(p1)
     
}



##### species by YxSxD ##########



species.plot.SxYxD <- function(data.file , 
                               years = 2015:2021, 
                               Xlab ="",
                               #Ylim = NA,
                               Ylab = "Mean"
){
     
     data.file$site <- factor(data.file$site, levels = c('Neah Bay',
                                                         'Tatoosh Island',
                                                         'Cape Alava',
                                                         'Cape Johnson',
                                                         'Destruction Island') )
     data.file$cat = paste(data.file$site, data.file$zone) 
     
     p1 = ggplot(data.file,  aes( x = year , y = mean, color=cat) ) + 
          geom_line( size = 1) + 
          geom_point(size = 2) +
          xlab(Xlab) +
          ylab(Ylab) +
          # ylim(Ylim) +
          scale_x_continuous('', labels = as.character(years), breaks = years)+
          scale_color_brewer(palette = "Paired") +
          theme_bw()+
          theme( legend.title = element_blank() )
     
     p1
     # if(errors == FALSE){ p1 <- p1 }
     # if(errors == 1){ p1 <- p1 + geom_errorbar( aes(ymin = se_lo, ymax = se_up), width=0 ) }
     # if(errors == 2){ p1 <- p1 + geom_ribbon(   aes(ymin = se_lo, ymax = se_up), alpha = 0.5) }
     
     return(p1)
     
}


# plot1 = species.plot.SxY(data.file = fishx[fishx$taxa=="SEME",])
# plot1

#### matrix transform pre ecole::bray0 ####

# for transforming all RxC for subsetted data matrix prior to distance estimation

matrix.transform <- function(data.file, data.transform = 'none'){
     
     if(data.transform == 'none'){ df = data.file }
     if(is.na(data.transform) == TRUE){ df = data.file }
     if(data.transform == "sqrt"){df = data.file^(1/2)}
     if(data.transform == "4th-root"){df = data.file^(1/4)}
     if(data.transform == "log"){df = log(data.file+1) }
     return(df)
     
}




