
#' Title
#'
#' @param icwd_wide
#'
#' @return
#' @export
#'
#' @examples
pivot_longer_icwd <- function(icwd_wide){
  icwd_wide %>%
    gather(Transect, Cover, T1:T24) %>%
    filter(!is.na(Cover))%>%
    mutate(Transect = str_replace(Transect, "\\T",""),
           Transect = as.numeric(Transect))

# now that the data is in long format, filter out the NAs associated with 'trace species' and rows with zero hits which is a byproduct of having the data in wide format.
# zero transects prevent this from being adequate
}

# add attributes for simple output - annual data transfer to LADWP
#' Title
#'
#' @param long
#' @param cYear
#' @param species
#' @param entity
#'
#' @return
#' @export
#'
#' @examples
add_species_agency_plotid <- function(long,cYear,species,entity){
  long %>% left_join(species, by = "Code")%>%
    mutate(source = 'Joint Monitoring 2015-current year',
           source.abr = 'jm',
           Year = cYear,
           Entity = entity,
           plotid = paste(Parcel,Transect,source.abr,sep = '_'),
           plotid.full = paste(Parcel,Transect,source.abr,Entity,sep = '_'))%>%
    select(Parcel, Code, Transect, Cover, Year, Entity, plotid, Species, CommonName, Order, Family, Genus, Lifecycle, Lifeform, Veg_Type, source, source.abr, Phreatophyte, plotid.full) %>%
    arrange(Parcel, Transect, Code)
}

#
#' Title
#'
#' @param processed
#' @param cYear
#' @param entity
#'
#' @return
#' @export
#'
#' @examples
save_csv_and_return_path <- function(processed, cYear, entity) {
  processed %>% write_csv(paste0("output/",entity,"_lpt_",cYear,".csv"))
  return(paste0("output/",entity, "_lpt_",cYear,".csv"))
}

count_parcels_all_years <- function(lpt_updated_master){
  lpt_updated_master %>%
    group_by(Year) %>%
    summarise(n = n_distinct(Parcel))
}

count_parcels_cyear <- function(n_parcels_all_years,cYear){
  n_parcels_sampled_cYear <- n_parcels_all_years %>%
    filter(Year == cYear)
  n_parcels_sampled <- n_parcels_sampled_cYear$n

  return(n_parcels_sampled)
}

count_transects_cyear <- function(lpt_updated_master,cYear){
  cyr_transects <- lpt_updated_master %>% filter(Year == cYear)%>%
    summarise(n = n_distinct(plotid))

  n_transects <- cyr_transects$n
  return(n_transects)
}



# filter out invasive species LELA2 for transect summaries.
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
filt_lela <- function(data){
  data %>% filter(Species != "LELA2",Species !="LELA")#
}

# revert to single parcels name
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mult_to_single_parcel_name <- function(x){
  x$Parcel[x$Parcel=="TIN028_FSP022_FSP019"]<-"TIN028"
  x$Parcel[x$Parcel=="TIN028_FSP019_FSP022"]<-"TIN028"
  x$Parcel[x$Parcel=="MAN006_IND229"]<-"MAN006"
  x$Parcel[x$Parcel=="LAW137_PLC210"]<-"LAW137"
  x$Parcel[x$Parcel=="LAW108_FSL047"]<-"LAW108"
  x$Parcel[x$Parcel=="LAW109_FSL048"]<-"FSL048"
  x$Parcel[x$Parcel=="IND163_BEE017"]<-"IND163"
  x$Parcel[x$Parcel=="IND139_MAN005"]<-"IND139"
  x$Parcel[x$Parcel=="IND024_BLK103"]<-"IND024"
  x$Parcel[x$Parcel=="FSP004_BGP188"]<-"FSP004"
  x$Parcel[x$Parcel=="FSP006_BGP182"]<-"FSP006"
  x$Parcel[x$Parcel=="ABD012_BLK029"]<-"ABD012"
  x$Parcel[x$Parcel=="BLK002_TIN061"]<-"BLK002"
  x$Parcel[x$Parcel=="TIN061_BLK002"]<-"BLK002"
  x$Parcel[x$Parcel=="BIS055_FSL214"]<-"BIS055"
return(x)
}

# summarise cover types to transect, add wvcom values for some transects
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
summarise_to_transect <- function(x, y){
  tran.sums <- x %>% group_by(Parcel,Year,Transect,source.abr,plotid,Lifecycle,Lifeform)%>%
    summarise(Cover=sum(Cover))# summarise for each e.g. lifecycle/lifeform annual/perennial grass

  tran.tlc <- tran.sums %>%
    group_by(Parcel,Year,plotid) %>% # sum all cover across all lifecycle/lifeform
    summarise(tot.live.cover = sum(Cover))

  pft.wide<-tran.sums %>% spread(Lifeform,Cover)
  pft.wide[is.na(pft.wide)] <- 0

  # create total cover variable
  pft.wide <- pft.wide%>%
    mutate(Cover= Grass + Herb + Shrub + Herb_Shrub + Tree,
           Shrub = Shrub + Herb_Shrub + Tree)

  pft.wide.wtot.cov <- pft.wide %>% filter(Lifecycle=="Perennial")%>%
    left_join(tran.tlc, by = c("Parcel","Year","plotid"))

  bind_add_proportion <- bind_rows(pft.wide.wtot.cov, y) %>%
    mutate(pShrubTran = Shrub / Cover,
           pGrassTran = Grass / Cover,
           pHerbTran = Herb / Cover) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  return(bind_add_proportion)
}

# summarise from transects to parcels
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
summarise_to_parcel <- function(x){
  p <- x %>% group_by(Parcel,Year)%>% summarise(
    PerHits=sum(Cover),
    ShrubHits=sum(Shrub),
    HerbHits=sum(Herb),
    GrassHits=sum(Grass),
    Cover=mean(Cover),
    Shrub=mean(Shrub),
    Herb=mean(Herb),
    Grass=mean(Grass),
    TLC=mean(tot.live.cover),

    pShrub=mean(pShrubTran),
    pGrass=mean(pGrassTran),
    pHerb=mean(pHerbTran),
    n.transects = n()) %>%
    mutate(NominalYear = case_when(Year %in% c(1985, 1986, 1987) ~ 1986,
                                   !Year %in% c(1985, 1986, 1987) ~ Year))

return(p)
}


# add deltas baseline to each year
#' Title
#'
#' @param parcels
#'
#' @return
#' @export
#'
#' @examples
add_parcel_deltas <- function(parcels){

  p <- parcels
  for (PID in unique(p$Parcel)) {
  #defines baselineRow variable as parcels with Nominal year as 1986
  baselineRow <- p[p$Parcel==PID & p$NominalYear==1986, ]

  #dim() function gets or sets the dimension of a matrix, array or data frame.
  #so the dim function asks if the baselineRow for the current parcel in the loop
  #has been updated with the baseline year. If it hasn't
  if (dim(baselineRow)[1]==0) next


  otherYears <- (p$Parcel==PID & !(p$NominalYear==1986))

  # calculate deltas by subtracting baseline cover from each of
  # other rows, leaving negative values indicating declines from baseline
  p$Cover.Delta[otherYears] <-
    p$Cover[otherYears] - baselineRow$Cover

  p$Shrub.Delta[otherYears] <-
    p$Shrub[otherYears] - baselineRow$Shrub

  p$Herb.Delta[otherYears] <-
    p$Herb[otherYears] - baselineRow$Herb

  p$Grass.Delta[otherYears] <-
    p$Grass[otherYears] - baselineRow$Grass
  }
  return(p)
}

#' Title
#'
#' @param parcels_deltas
#' @param attributes
#'
#' @return
#' @export
#'
#' @examples
wellfield_control_means <- function(parcels_deltas, attributes){

  Parcels <- attributes %>% select(Parcel, Type) %>% left_join(parcels_deltas, by = "Parcel")

  Parcels %>% group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count=n(),
      Cover=mean(Cover),
      Grass=mean(Grass),
      Herb=mean(Herb),
      Shrub=mean(Shrub)
      )
}

wellfield_control_means_rarefied <- function(parcels_deltas, attributes){

  Parcels <- attributes %>% select(Parcel, Type) %>% left_join(parcels_deltas, by = "Parcel")

  Parcels %>% filter(Parcel %in% c(
    'BGP031',
    'BLK115',
    'FSL187',
    'IND096',
    'IND163',
    'LNP018',
    'MAN060',
    'PLC024',
    'PLC106',
    'PLC121',
    'PLC223',
    'UNW029',
    'UNW039',
    'BGP154',
    'BGP162',
    'BLK009',
    'BLK016',
    'BLK024',
    'BLK033',
    'BLK039',
    'BLK044',
    'BLK069',
    'BLK074',
    'BLK075',
    'BLK094',
    'BLK099',
    'IND011',
    'IND035',
    'IND106',
    'IND111',
    'IND132',
    'IND139',
    'IND231',
    'LAW063',
    'LAW065',
    'LAW085',
    'LAW107',
    'LAW120',
    'LAW122',
    'MAN006',
    'MAN007',
    'MAN037',
    'TIN028',
    'TIN068')) %>% group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count=n(),
      Cover=mean(Cover),
      Grass=mean(Grass),
      Herb=mean(Herb),
      Shrub=mean(Shrub)
    )
}


plot_wellfield_control <- function(wellcont_means_rarefied){
  plot <-
    wellcont_means_rarefied %>%
    select(-Herb) %>%
    pivot_longer(Cover:Shrub, names_to = "type", values_to = "cover") %>%
    ggplot(aes(x = NominalYear, y = cover, color = Type))+
    geom_point()+
    geom_line()+
    facet_wrap(~type, ncol = 2)

 # ggsave("wellfield_control_rarefied.png", plot, width = 7, height = 7)
  # return("wellfield_control_rarefied.png")
  return(plot)
}

#' Title
#'
#' @param dtw
#'
#' @return
#' @export
#'
#' @examples
#' # this functino per parcel -  needs to be embedded into loop

plot.dtw <- function(PID) {

  # Extract the parcel of interest.

  pcldtw <- dtw %>% filter(Parcel == PID) %>% arrange(Year)
  pcldtw <- pcldtw %>% mutate(phreatic.zone = MIN - 3)
  n <- dim(pcldtw)[1]

  # Find the maximum value that DTW attained over the time span to
  # be plotted.  Allow for missing data.  Set up the plot limits.
  # specifying the max first, then min in ylim sets up the reverse scale we want for DTW plotting where 0 is at top or soil surface representation.

  if(is.na(pcldtw$DTW))
  {ylim<-c(9,0)
  }else{
    this.max.DTW <- max(pcldtw$DTW, na.rm=TRUE) + 1
    # this.min.DTW <- 0)
    ylim <- c(this.max.DTW,0)
  }


  plot(xlim, ylim, xlab='', ylab='DTW [ft]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)  # Draw solid lines at important depth points.

  # could control ylim in global chunk but if so, need to change above plot function and below axis setup
  # add the axes and a frame.
  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  abline(h=6, lwd=1, col="green")
  #text(rmarg, 2, 'Grass root zone', adj=0, xpd=NA)

  abline(h=12, lwd=1, col="brown")
  #text(rmarg, 4, 'Shrub root zone', adj=0, xpd=NA)

  #abline(h=Parcel$DTW[1], lwd=1, col='blue')
  #text(rmarg, Parcel$DTW[1], '1985 DTW', adj=0, xpd=NA)


  # Assess reliability and present data only in cases where
  # the data are reliable or relative recovery reliable.
  # if (as.character(Attribute$DTW.Reliability) %in% c('Reliable', 'Relative Recovery Reliable' , 'Baseline Not Reliable', 'Current DTW Not Reliable','Current DTW Reliable','Need hydrograph Evaluation','NoData','Not Reliable','Baseline Reliable','Validation Needed)) {

  # Draw in a lines and points graph.
  lines(pcldtw$Year, pcldtw$DTW, type='b', pch=16, col="purple")
  #lines(Parcel$Year, Parcel$DTWcap1, type='l', pch=16, col="lightblue")
  # lines(pcldtw$Year, pcldtw$MIN, type='b', pch=16, col="darkblue")
  # lines(pcldtw$Year, pcldtw$MAX, type='b', pch=16, col="grey")
  # lines(pcldtw$Year, pcldtw$phreatic.zone, type='b', pch=16, col="blue")
  # } else {
  #   # Draw nothing.
  # }
  # Indicate the DTW reliability.
  # text(
  #   xlim[1] + (xlim[2] - xlim[1])*dtw.xy[1],
  #   dtw.ylim[1] * dtw.xy[2],
  #   Attribute$DTW.Reliability, adj=1, family='sans', font=4, cex=1.25
  # )
}


#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ndvi <- function(PID) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  rspcl <- rs %>% filter(Parcel == PID)
  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(rspcl)[1]

  # YLIM
  # Check the y-axis limits.  Extend them if necessary.
  ym <- max(rspcl$NDVI_SUR) + .01
  ymin <- min(rspcl$NDVI_SUR) - .01
  # ymin <- 0
  # ylim <- c(0, .6)

  # ylim <- c(ymin, ym)
  ylim <- c(ymin,ym)

  # if (max(Parcel$NDVI_SUR, na.rm=TRUE) > -1.5) { ylim <- c(ymin, ym) }
  #
  # # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='NDVI [dimensionless]', xlim=xlim, ylim=ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(rspcl$Year[i]-bar.space, 2), rep(rspcl$Year[i]+bar.space, 2))
    bar.y <- c(0, rspcl$NDVI_SUR[i], rspcl$NDVI_SUR[i], 0)

    polygon(bar.x, bar.y, col='green')
  }

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.


  # abline(h=Parcel$NDVI_SUR[1], lwd=1, col ='blue')

  #text(rmarg, Parcel$NDVI_SUR[1], 'Baseline', adj=0, xpd=NA)


}



#' Title
#'
#' @param PID
#'
#' @return
#' @export
#'
#' @examples
plot.ppt <- function(PID) {

  # Extract the parcel of interest.
  #Parcel    <- subset(Covariates, Parcel==PID)
  ppt <- rs %>% filter(Parcel == PID)
  # Parcel    <- subset(Covariates2, Parcel==PID)
  # Attribute <- subset(Attributes, Parcel==PID)

  # We are only plotting one number per year here.  Get the number of
  # years to plot.

  n <- dim(ppt)[1]

  # HARD-CODED NUMBER:
  # Check the y-axis limits.  Extend them if necessary.

  # ylims are defined in setup chunk
  ylim <- c(0,400)


  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='PPT [mm]', xlim=xlim, ylim= ylim, type='n', yaxs='i', axes=F)

  # Jazz it up with pretty axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(ppt$Year[i]-bar.space, 2), rep(ppt$Year[i]+bar.space, 2))
    bar.y <- c(0, ppt$PPT[i], ppt$PPT[i], 0)

    polygon(bar.x, bar.y, col='lightblue')
  }
  # abline(h=Parcel$PPT[1], lwd=1,col='blue')

  #text(rmarg, Parcel$PPT[1], 'Baseline', adj=0, xpd=NA)
}


# plot.perennial.cover()
#
# Function to create the perennial cover plot.

#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.cover <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }

  # Extract the parcel of interest.

  Transect <- subset(transects, Parcel==PID)
  # Attribute3 <- subset(Attributes3, Parcel==PID)

  # Calculate mean and standard error of the mean for each year.

  # Cover.mean <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=mean)
  # colnames(Cover.mean) <- c('Year', 'Mean')

  Cover.mean <- Transect %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Cover))

  # Cover.stderr <- aggregate(Transect$Cover, list(Year=Transect$Year), FUN=stderr)
  # colnames(Cover.stderr) <- c('Year', 'SEM')

  Cover.stderr <- Transect  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Cover))

  stats <- Cover.mean%>%left_join(Cover.stderr, by="Year") %>% arrange(Year)

  # stats <- merge(Cover.mean, Cover.stderr, by="Year") %>% arrange(Year)


  n <- dim(stats)[1]

  # Check the y-axis limits.  Extend them if necessary.
  this.max.cover <- max(stats$Mean, na.rm=TRUE)+15
  this.min.cover <- min(stats$Mean, na.rm=TRUE)

  ylim <- c(0, this.max.cover)
  # ylim <- c(0, 60)
  #ylim <- c(0, 104)

  #if (max(stats$Mean + stats$SEM + asterisk.offset, na.rm=TRUE) > 60) { ylim <- c(0, 104) }

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Perennial Cover [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.
  # Only do this if the data will be analyzed using Dunnett's method.

  abline(h=stats$Mean[1], lwd=1,col='blue')

  # text(rmarg, stats$Mean[1], 'Baseline', adj=0, xpd=NA)


  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='brown')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 95% CI or 1.96 * standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+1.96*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))

  }

  # Now, rather than performing the one-way analysis of variance (weighted) followed
  # by Dunnett's method, we will simply perform a two-sample t-test with unequal
  # variances.  Note that the weighted analysis of variance is essentially
  # a generalization of this approach.

  ## remove this analyzable dependency to force in one sample tests
  #if (analyzable) {

  # We want to pick off the first year.  For safety's sake, we
  # sort the data to make sure they are in order, although
  # that seems to be a side effect of the code above.

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  # Just roll through the other years one by one.  This could probably
  # be done more compactly.  On the other hand, it should be clear
  # what is happening here.

  #  TO DO:  What *would* be better would be cleaning
  # up this clunky method of getting the years.  It would be best
  # to have a list (vector) containing the years to be analyzed.

  # periodically the error
  # Error in t.test.default(data2, mu = mu.d1) : 'mu' must be a single number


  for (i in 2:n) {

    # data1 <- Transect$Cover[Transect$Year==stats$Year[1]]#baseline
    # data1 <- Transect$Cover[Transect$Year==1986]#baseline
    data1 <- Transect$Cover[Transect$Year < 1989]
    # data1 <- Transect$Cover[Transect$Year %in% c(1984,1985,1986,1987,1988)]

    mu.d1<-mean(data1)

    data2 <- Transect$Cover[Transect$Year==stats$Year[i]]#all other years in time series

    # per the greenbook update in 2017, I use the two sample t-test for parcels with greater than four transects and non-zero variance; and the one sample t-test with mu = mean of small sample or the live cover value reduced to perennial only from the wvcom variable.

    ifelse (length(data1) <= 4,#if parcel has less than five transects or was assigned a single wvcomm live cover variable as the baseline cover, conduct a one sample t-test on reduced live cover based on proportion of perennial grass and perennial total cover assigned to the parcel.

            fit <- t.test(data2, mu = mu.d1),
            fit <- t.test(data1,data2)# otherwise conduct the two sample t-test, defaulting to welch-satherwaite method to account for unequal variance.
    )

    pvalues[i] <- fit$p.value
  }

  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      text(stats$Year[i], stats$Mean[i]+2.0*(stats$SEM[i])+ asterisk.offset, '*', adj=.6)
    }
  }

}



# plot.perennial.cover()
## Function to create the perennial cover plot.
#PID<-"BLK044"
#' Title
#'
#' @param PID
#' @param transects
#'
#' @return
#' @export
#'
#' @examples
plot.perennial.grass <- function(PID, transects) {

  stderr <- function(x) { return(sqrt(var(x)/length(x))) }
  # Extract the parcel of interest.

  grass.pcl <- subset(transects, Parcel==PID)
  # Attribute3 <- subset(Attributes3, Parcel==PID)

  # Calculate mean and standard error of the mean for each year.

  # rewrite old code using dplyr pipes. reads more logically

  #old way
  #Grass.mean <- aggregate(Transect$Grass, list(Year=Transect$Year), FUN=mean)
  #colnames(Grass.mean) <- c('Year', 'Mean')

  #new
  Grass.mean <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(Mean=mean(Grass))


  # Grass.stderr <- aggregate(Transect$Grass, list(Year=Transect$Year), FUN=stderr)
  # colnames(Grass.stderr) <- c('Year', 'SEM')

  Grass.stderr <- grass.pcl  %>% group_by(Year) %>% dplyr::summarise(SEM=stderr(Grass))


  stats <- Grass.mean%>%left_join(Grass.stderr, by="Year") %>% arrange(Year)


  # Get the number of years to plot.
  n <- dim(stats)[1]

  # this.max.cover <- max(stats$Mean, na.rm=TRUE)
  # this.min.cover <- min(stats$Mean, na.rm=TRUE)

  this.max.grass <- max(stats$Mean, na.rm=TRUE)+15
  this.min.grass <- min(stats$Mean, na.rm=TRUE)

  #ylim is finite

  # ylim <- c(0, 60)
  ylim <- c(0, this.max.grass)

  # Check the y-axis limits.  Extend them if necessary.

  # if (max(stats$Mean + stats$SEM + asterisk.offset, na.rm=TRUE) > 60) { ylim <- c(0, 104) }

  # Set up a blank plot first.

  plot(xlim, ylim, xlab='', ylab='Perennial Grass [%]', xlim=xlim, ylim=ylim, yaxs='i', type='n', axes=F)

  # add axes and a frame.

  axis(side=1, at=seq(xlim[1], xlim[2]), labels=FALSE, tcl=-0.2)
  axis(side=1, at=pretty(xlim))
  axis(side=2, at=pretty(ylim), las=2)
  abline(h=pretty(ylim), col="lightgray")
  box()

  # If there is a baseline, add a horizontal line for the baseline.
  # This will allow the subsequent plotting to overlay the line.



  abline(h=stats$Mean[1], lwd=1, col='blue')
  # text(rmarg, stats$Mean[1], 'Baseline', adj=0, xpd=NA)


  # Draw in the bars for each row (i.e., each year).

  for (i in 1:n) {

    # Draw in the bar for the year.  We need an x and a y vector to give the
    # corners of the bar that will be filled in with color.

    bar.x <- c(rep(stats$Year[i]-bar.space, 2), rep(stats$Year[i]+bar.space, 2))
    bar.y <- c(0, stats$Mean[i], stats$Mean[i], 0)

    polygon(bar.x, bar.y, col='lightgreen')
  }

  # Draw in the standard error bars for each row.

  for (i in 1:n) {

    # Draw in the 2 x standard error of the mean using the usual graphics.

    lines(c(stats$Year[i], stats$Year[i]), c(stats$Mean[i]-1.96*(stats$SEM[i]), stats$Mean[i]+ 1.96*(stats$SEM[i])))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]-1.96*(stats$SEM[i]),2))
    lines(c(stats$Year[i]-bar.space, stats$Year[i]+bar.space), rep(stats$Mean[i]+1.96*(stats$SEM[i]),2))

  }

  # Now, rather than performing the one-way analysis of variance (weighted) followed
  # by Dunnett's method, we will simply perform a two-sample t-test with unequal
  # variances.  Note that the weighted analysis of variance is essentially
  # a generalization of this approach.

  # if (analyzable) {

  # We want to pick off the first year.  For safety's sake, we
  # sort the data to make sure they are in order, although
  # that seems to be a side effect of the code above.

  stats <- arrange(stats, Year)

  # Set up a container for the p-values.

  pvalues <- rep(NA, n)

  # Just roll through the other years one by one.  This could probably
  # be done more compactly.  On the other hand, it should be clear
  # what is happening here.

  # TO DO:  What *would* be better would be cleaning
  # up this clunky method of getting the years.  It would be best
  # to have a list (vector) containing the years to be analyzed.

  # blyears<-c(1984,1985,1986,1987,1988)
  for (i in 2:n) {#start at 2 because 1 is the baseline year and this for loop is only for each comparison to baseline
    #Transect$Grass[Transect$Year==stats$Year[1]]
    #str(stats)
    #length(data1)
    # data1 <- Transect$Grass[Transect$Year==stats$Year[1]]#baseline
    data1 <- grass.pcl$Grass[grass.pcl$Year < 1989]
    # data1 <- Transect$Grass[Transect$Year %in% c(1984,1985,1986,1987,1988)]#baseline

    mu.d1<-mean(data1)



    # could have a covariate indicating  baseline year and flags for comparisons, ordinal perhaps
    data2 <- grass.pcl$Grass[grass.pcl$Year==stats$Year[i]]
    #mu <- data1
    # str(mu)
    #ifelse(length(data1) == 0), mu= 0,

    ifelse (length(data1) <= 4,#if parcel has single transect or was assigned the wvcomm variable as the baseline cover, conduct a one sample t-test. this should include the value of zero for grass where no grass detected during baseline.

            fit <- t.test(data2, mu = mu.d1),

            fit <- t.test(data1,data2)# otherwise conduct the two sample t-test, defaulting to welch-satherwaite method to account for unequal variance.
    )


    # fit <- t.test(data1, data2)

    pvalues[i] <- fit$p.value
  }
  # kable(pvalues)
  # Add an asterisk according to whether the comparison with
  # baseline was significant.

  for (i in 1:n) {

    if (!is.na(pvalues[i]) & pvalues[i] < 0.05) {
      text(stats$Year[i], stats$Mean[i]+2*(stats$SEM[i])+ asterisk.offset, '*', adj=.5)
    }
  }
  # }
}




# # five_row_timeseries <- function(attributes_pfix, transects, dtw_pfix, rs_pfix, cYear){
#
#   stderr <- function(x) { return(sqrt(var(x)/length(x))) }
#
#
#   # Set global configurations.
#
#   r.squared.digits <- 2  ### Round the r-squared values to this many digits.
#   p.value.digits   <- 4  ### Round the p-values to this many digits
#   b.digits         <- 4  ### Round the estimates to this many digits
#
#
#
#   # Configure global plot tuning parameters.
#
#   bar.space <- 0.25        ### The space between each bar on the graphs.
#
#   # can define year here for x-axis if doing one off pdf export
#   xlim <- c(1985, 2020)   ### The x-axis limits for the graphing
#   #xlim <- c(1985, cYear)   ### The x-axis limits for the graphing effort.
#   ylim <- c(0, 62.4)       ### The y-axis limits for percentage cover.
#   ylim.ndvi<-c(.1,.8)
#   dtw.max <- 5             ### Maximum y-axis limits for the DTW measurements.
#   dtw.split <- 6           ### Where to switch the y-axis for the DTW measurements.
#   dtw.xy <- c(.95, .90)    ### DTW plot annotation location.
#   asterisk.offset <- 4     ### How much to space the asterisks.
#   rmarg <- cYear + 1.5     ### Tune stuff in the right margin.
#   show.caption <- TRUE     ### Print a figure caption or not?
#
# AttributesPID <- attributes_pfix %>% filter(reinv == "r", Parcel %in% c("BLK094","BLK099"))
#
#   # filter(!Parcel %in% c('BGP013','BLK006',"BGP204", "BGP205", "BLK008", "FSL179", "IND086", "LAW076", "LAW110","MAN038", "PLC113", "PLC220", "TIN006", "UNW074"))
#
# PIDs <- unique(AttributesPID$Parcel)
# # all.pid <- PIDs
#
# # layout = "l-page",
# # FILEpdf <- paste0(dir, '/output/TimeSeries2018.all.pdf')
# FILEpdf <- "output/time_series_all_f2020e.pdf"
# # library(plyr)
# fig.num <- 1
#
# pdf(FILEpdf)
# # png(FILEpng)
# par(mfrow=c(5,1), oma=c(5,0,5,0), plt=c(.1,.85,.1,.85))
#
# for (PID in PIDs) {
#   # call functions
#   plot.perennial.cover(PID, transects)
#   plot.perennial.grass(PID, transects)
#   plot.dtw(PID, dtw_pfix)
#   plot.ndvi(PID, rs_pfix)
#   plot.ppt(PID, rs_pfix)
#
#   # caption variables
#   # Parcel <- parcels %>% filter(Parcel==PID)
#
#   Attribute3 <- attributes_pfix %>% filter(Parcel==PID)
#   Transect.n <- transects %>% filter(Parcel==PID,Year==cYear)
#   n.tran <-  Transect.n %>% group_by(Parcel) %>% dplyr::summarise(count = n())
#
#   Descriptor <- paste(PID,'(W/C): ',Attribute3$Type,'| Type: ', Attribute3$GB_TYPE,'|', Attribute3$Holland, sep=' ')
#   ESD <- paste(Attribute3$taxorder, Attribute3$compname,  '| ESD: ',Attribute3$Ecologic_3, sep=' ')
#   geom <- paste('Geomorphic:',Attribute3$geomdesc)
#   # last.year <- max(Parcel$Year, na.rm=TRUE)
#   test.type <- Attribute3$test_type
#   Caption <- paste('Figure ', fig.num, ': ', test.type,': Baseline (',Attribute3$bl.origin,') vs. reinventory (* p < 0.05).\n Baseline sample size (n = ',Attribute3$n86,'). Current year sample size (n = ',n.tran$count,').', ' Error bars = 95% CI.', sep='')
#   mtext(side=3, outer=TRUE, line=0, geom)
#   mtext(side=3, outer=TRUE, line=3, Descriptor)
#   mtext(side=3, outer=TRUE, line=1.5, ESD)
#   mtext(side=1, outer=TRUE, line=3, adj=0.15, family='serif', Caption)
#   # update fig.num for next plot
#   fig.num <- fig.num + 1
#   }
# dev.off()
# }


#' Title
#'
#' @param transects
#' @param attributes_reinv
#'
#' @return
#' @export
#'
#' @examples
nest_transects <- function(transects, attributes_reinv){

  byparyear <- transects %>% filter(Parcel %in% attributes_reinv$Parcel) %>%
    group_by(Parcel,Year) %>%
    nest() %>%
    mutate(n = map_dbl(data,nrow))

  gb <- byparyear %>% filter(Year > 1990)
  bl <- byparyear %>% filter(Year < 1990)
  parcel_year_nested_transects <- gb %>% left_join(bl, by = 'Parcel')

  return(parcel_year_nested_transects)
}

#' Title
#'
#' @param data.x
#' @param data.y
#'
#' @return
#' @export
#'
#' @examples
t2samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Cover,data.y$Cover, conf.level = .95)
}

#' Title
#'
#' @param data.x
#' @param data.y
#'
#' @return
#' @export
#'
#' @examples
t1samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  failproof.t(data.x$Cover,mu = mean(data.y$Cover), conf.level = .95)

}

#' Title
#'
#' @param parcel_year_meta_2samp
#'
#' @return
#' @export
#'
#' @examples
two_sample_ttest <- function(parcel_year_meta_2samp){
parcel_year_meta_2samp %>% mutate(model = map2(data.x, data.y, t2samp)) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>% mutate(significance = ifelse(p.value < 0.05, ifelse(statistic > 0  ,'ns','significant'),'ns'))
}

#' Title
#'
#' @param parcel_year_meta_1samp
#'
#' @return
#' @export
#'
#' @examples
one_sample_ttest <- function(parcel_year_meta_1samp){
  parcel_year_meta_1samp %>% mutate(model = map2(data.x, data.y, t1samp)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    # hoist(model,"null.value") %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0 ,'ns','significant'),
                                 'ns')
           )
}

#' Title
#'
#' @param data
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
plot_1samptest_timeseries <- function(data,cYear,parcel.select){
  data <- data %>% filter(Parcel %in% parcel.select)
  plot <- data %>% ggplot(aes(x=Year.x))+
    ggtitle(paste("Perennial Cover (1991-",cYear,") Compared to Baseline (1984-1987).
 One-Sample t-test (mu = mean of baseline transects for n < 5, ns p > 0.05, significant p < 0.05"))+
  # ns (not significant, p > 0.05), significant (p < 0.05)")
  # +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=significance,width=0.2))+
  geom_line(aes(y=estimate))+
  # geom_line(aes(y=baseline, color='Baseline'))+#baseline year
  scale_color_manual(values = c("blue", "red"))+
  ylim(0,100)+
  ylab("Perennial Cover (%)")+
  facet_wrap(~Parcel, ncol = 1)+
    theme(legend.position="top")
  # ggsave("plot_1samp.png", plot, width = 7, height = 7)
  # return("plot_1samp.png")
  return(plot)

}


#' Title
#'
#' @param data
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
plot_2samptest_timeseries <- function(data,cYear,parcel.select){
  data <- data %>% filter(Parcel %in% parcel.select)
  plot <- data %>% ggplot(aes(x=Year.x))+
  ggtitle(paste("Perennial Cover Time Series (1991-",cYear,") Relative to Baseline (1984-1987).
  Welch Two Sample t-test (ns p > 0.05, significant p < 0.05)"))+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=significance,width=0.2))+
  geom_line(aes(y=estimate1))+#reinventory year
  scale_color_manual(values = c("green","orange", "blue", "red"))+
    # scale_color_npg()+
  geom_line(aes(y=estimate, color='Deviation from Baseline\n (relative to zero)'))+#baseline year
  geom_line(aes(y=estimate2, color='Baseline'))+#baseline year
  # ylim(0,100)+
  ylab("Perennial Cover (%)")+
  facet_wrap(~Parcel,ncol = 1)+
    theme(legend.position="top")
  # ggsave("plot_2samp.png", plot, width = 7, height = 7)
  # return("plot_2samp.png")
  return(plot)

}

#' join_summaries
#'
#' @param parcels_deltas
#' @param attributes_pfix
#' @param parcel_year_meta_combined_results
#' @param cYear
#'
#' @return
#' @export
#'
#' @examples
join_summaries <- function(parcels_deltas,attributes_pfix, parcel_year_meta_combined_results, cYear){

deltas <- parcels_deltas %>%
  filter(Year == cYear) %>%
  select(Parcel, TLC, Cover, Cover.Delta, Grass, pGrass, Grass.Delta, Shrub, pShrub, Shrub.Delta)%>%
  mutate(across(Cover:Shrub.Delta, round, 1))

test <- parcel_year_meta_combined_results %>%
  filter(Year.x == cYear)

par.delta.test.att <- deltas %>%
  left_join(test, by = 'Parcel') %>%
  left_join(attributes_pfix, by = 'Parcel')

# datatable(par.delta.test.att)
return(par.delta.test.att)

}

parcel_data_table <- function(deltas_ttest_att,cYear){
  table <- deltas_ttest_att %>% mutate(p.value = round(p.value,3)) %>%
    filter(!Parcel %in% c("FSL044")) %>%
    select(Parcel,GB_TYPE,Holland,wellfield,Type, Cover.Delta,p.value,
           significance,method,Grass.Delta, Shrub.Delta,
           pGrass,pShrub,Cover,Grass,Shrub) %>%
    datatable(filter = 'top',
              options = list(
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2, rightColumns = 0),
                pageLength = 5,
                autoWidth = TRUE,
                colReorder = TRUE)
              # ,
              # caption = paste("Baseline vs",cYear,"statistical significance summary."

              )


  # %>% htmlwidgets::saveWidget('parcel-summary.html')

  # return("parcel-summary.html")
  return(table)
}


panel_map<- function(cYear,parcels_shp_ttest, wf, or,streams,canals,laa,lakes, monit.sites){

  # write function to handle custom plot with input as string e.g. 'Laws'
  limit <- parcels_shp_ttest %>% filter(grepl(wf,wellfield))

  tpc.below <- limit %>% filter(significance == 'significant')
  # the perennial grass currently doesn't have the sig tests,
  # so we specify this manually on the attribute table for now.
  pgr.below <- limit %>% filter(pgr20 == 1)

  tmap_mode("plot")
  # tm_basemap(leaflet::providers$Esri.WorldStreetMap, group = "Esri World StreetMap") +
  # tm_basemap(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") +


  # , breaks = c(-Inf,-.4,-.3,-.2,-.1,-.05,.05,.1,.2,.3,.4,Inf),
  #-----------------------------------------
  tmgrass <-

    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Grass"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf), palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(pgr.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 2)+
    tm_text("PCL", size = .5,  col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')
  #

  # tm_layout(legend.outside = TRUE, legend.outside.position = 'top', compass.type = 'arrow')


  # auto.placement
  #-----------------------------------------
  tmgrassd <- tm_shape(limit, group = 'Wellfield - Parcels') +
    # tm_text("PCL_merged",  col = "gray30", root = 3,remove.overlap=TRUE,group = 'Labels') +
    tm_polygons(col =c("Grass.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(pgr.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 1)+
    #
    # tm_shape(prodwell, group = 'Production Wells') +
    # tm_text("STAID",  col = "white", size=.4,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'red') +
    # tm_symbols(col = "purple", scale = .05,title.col = "STAID",  id = "STAID",popup.vars = c("PURPOSE","CONTROL","WF"),group = 'Production Wells')+
    #


    tm_shape(canals, group = 'Canals') +
    # tm_text("NAME",  col = "blue",size= .5, remove.overlap=TRUE)+
    tm_lines(col = "blue", scale = .6, group = 'Canals')+


    tm_shape(streams, group = 'Streams') +
    # tm_text("NAME",  col = "blue", size= .5,remove.overlap=TRUE,along.lines = TRUE,overwrite.lines = TRUE) +
    tm_lines(col = "blue", scale = 1, group = 'Streams')+

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",popup.vars = c("SITE","TYPE"),group = 'On/Off Monitoring Sites')+

    tm_shape(or, group = 'River') +
    # tm_text("NAME",  col = "blue", size = .7,remove.overlap=TRUE,group = 'River',along.lines=TRUE,overwrite.lines = TRUE) +
    tm_lines(col = "blue", scale = 1, group = 'River')+

    tm_shape(laa, group = 'LAA') +
    # tm_text("NAME",  col = "blue", size = .7,remove.overlap=TRUE,group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA')+

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')
  # +
  # tm_text("NAME",  col = "black", size = .7,remove.overlap=TRUE,group = 'Lakes')

  #-----------------------------------------
  #-----------------------------------------
  #-----------------------------------------
  tmcov <- tm_shape(limit, group = 'Wellfield - Parcels') +
    # tm_text("PCL_merged",  col = "gray30", root = 3,remove.overlap=TRUE,group = 'Labels',shadow = TRUE) +
    tm_polygons(col =c("Cover"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+
    # tm_text("PCL",  size = .5,col = "black",remove.overlap=FALSE,group = 'Labels',shadow = TRUE) +
    #   tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    # tm_borders(col = 'red')

    tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 2)+
    tm_text("PCL",  size = .5,col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')
  #-----------------------------------------
  tmcovd <- tm_shape(limit, group = 'Wellfield - Parcels') +
    # tm_text("PCL_merged",  col = "gray30", root = 3,remove.overlap=TRUE,group = 'Labels',shadow = TRUE) +
    tm_polygons(col =c("Cover.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red')+

    # tm_shape(prodwell, group = 'Production Wells') +
    # tm_text("STAID",  col = "black", size=.7,remove.overlap=TRUE,group = 'Labels',shadow=TRUE) +
    # tm_symbols(col = "purple", scale = .1,title.col = "STAID",  id = "STAID",popup.vars = c("PURPOSE","CONTROL","WF"),group = 'Production Wells')+

    tm_shape(canals, group = 'Canals') +
    # tm_text("NAME",  col = "blue", root = 3,remove.overlap=TRUE) +
    tm_lines(col = "blue", scale = .6, group = 'Canals')+

    tm_shape(streams, group = 'Streams') +
    # tm_text("NAME",  col = "blue", size= .5,remove.overlap=TRUE,along.lines = TRUE) +
    tm_lines(col = "blue", scale = .7, group = 'Streams')+

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",popup.vars = c("SITE","TYPE"),group = 'On/Off Monitoring Sites')+


    tm_shape(or, group = 'River') +
    # tm_text("SITE",  col = "black", size = .7,remove.overlap=TRUE,group = 'River',along.lines=TRUE) +
    tm_lines(col = "blue", scale = 1, group = 'River')+

    tm_shape(laa, group = 'LAA') +
    # tm_text("SITE",  col = "black", size = .7,remove.overlap=TRUE,group = 'River',shadow = TRUE) +
    tm_lines(col = "blue", scale = 1, group = 'LAA')+

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')
  #   tm_text("NAME",  col = "black", size = .7,remove.overlap=TRUE,group = 'Lakes')
  #
map <-tmap_arrange(tmgrass, tmgrassd,tmcov,tmcovd,ncol=2)

tmap_save(map,paste0('docs/assets/',cYear,'_',wf,'map.png'))
# save images in docs/assets so that workflowr website will be
# able to access the images. This might all be do to the tmap_arrange
# not producing an object compatible with target that can be called with
#tar_read()
  return(paste0('assets/',cYear,'_',wf,'map.png'))
# return the path to the images with assets as the root for website in docs
# directory.
  # tmap_arrange()
  # tmap_arrange(tmcov,tmcovd,tmgrass,tmgrassd)
  # tmap_arrange(tmgrass,tmgrassd)

}

