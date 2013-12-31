<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Central Park Temperatures}
-->

# Monthly Temperatures in Central Park
http://www.erh.noaa.gov/okx/climate/records/monthannualtemp.html








```r
inputPath <- "./CentralPark/Data/Derived/CentralParkTemp.csv"
changeMonths <- c(as.Date("1900-01-15"), as.Date("1950-01-15"), as.Date("2000-01-15"))
changeLabels <- c("1900", "1950", "2000")

vpLayout <- function(x, y) { grid::viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores ) { 
  return( base::range(scores) ) #A new function isn't necessary.  It's defined in order to be consistent.
}
hSpread <- function( scores ) { 
  return( stats::quantile(x=scores, probs=c(.25, .75)) ) 
}
seSpread <- function( scores ) { 
  return( base::mean(scores) + base::c(-1, 1) * stats::sd(scores) / base::sqrt(base::length(scores)) ) 
}
bootSpread <- function( scores, conf=.66 ) {
  plugin <- function( d, i ) { base::mean(d[i]) }

  dist <- boot::boot(data=scores, plugin, R=99) #999 for the publication
  ci <- boot::boot.ci(dist, type = c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}

darkTheme <- ggplot2::theme(
  axis.title          = ggplot2::element_text(color="gray30", size=9),
  axis.text.x         = ggplot2::element_text(color="gray30", hjust=0),
  axis.text.y         = ggplot2::element_text(color="gray30"),
  axis.ticks.length   = grid::unit(0, "cm"),
  axis.ticks.margin   = grid::unit(.00001, "cm"),
#   panel.grid.minor.y  = element_line(color="gray95", size=.1),
#   panel.grid.major    = element_line(color="gray90", size=.1),
  panel.margin        = grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin         = grid::unit(c(0, 0, 0, 0), "cm")
)
lightTheme <- darkTheme + ggplot2::theme(
  axis.title          = ggplot2::element_text(color="gray80", size=9),
  axis.text.x         = ggplot2::element_text(color="gray80", hjust=0),
  axis.text.y         = ggplot2::element_text(color="gray80"),
  panel.grid.minor.y  = ggplot2::element_line(color="gray99", size=.1),
  panel.grid.major    = ggplot2::element_line(color="gray95", size=.1)
)
dateSequence <- base::as.Date(c("1870-01-01","1880-01-01","1890-01-01",
                              "1900-01-01","1910-01-01","1920-01-01","1930-01-01","1940-01-01",
                              "1950-01-01","1960-01-01","1970-01-01","1980-01-01","1990-01-01",
                              "2000-01-01","2010-01-01"))
xScale       <- ggplot2::scale_x_date(breaks=dateSequence, labels=scales::date_format("%Y"))
xScaleBlank  <- ggplot2::scale_x_date(breaks=dateSequence, labels=NULL) #This keeps things proportional down the three frames.

paletteDark <- RColorBrewer::brewer.pal(n=10L, name="Paired")[c(2L,4L,6L,8L)][c(1, 2, 4, 3)]
paletteLight <- RColorBrewer::brewer.pal(n=10L, name="Paired")[c(1L,3L,5L,7L)][c(1, 2, 4, 3)]
    
```




```r
dsLinear <- utils::read.csv(inputPath, stringsAsFactors=FALSE)
dsLinear <- dsLinear[dsLinear$Year <= 2012L, ] #The 2013 year is missing
dsLinear$StageID <- dsLinear$CenturyHalfID
dsLinear$Date <- base::as.Date(dsLinear$Date)
# sapply(dsLinear, class)
```

## Section 1: Cartesian Rolling Plot
This section corresponds to the MBR manuscript, but with the modified dataset.

Smoothed monthly  birth rates (General Fertility Rates;  GFR's) for Oklahoma County, 1990-1999, plotted in a linear plot.  The top plot shows the connected raw data with a February smoother; the middle plot shows smoothing with a 12-month moving average, blue/green line, superimposed on a February smoother, red tan line); the bottom plot shows the smoothers and confidence bands, which are H-spreads (ie, the distance between the .25 and .75 quantiles) defined using the distribution of GFR's for the given month and 11 previous months.


```r

dsLinear <- Wats::AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")

portfolioCartesian <- Wats::AnnotateData(
  dsLinear = dsLinear, 
  dvName = "Temp", 
  centerFunction = stats::median, 
  spreadFunction = seSpread)
#   spreadFunction = hSpread)


topPanel <- Wats::CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date",
  yName = "Temp",
  stageIDName = "StageID",
  changePoints = changeMonths,
  changePointLabels = changeLabels,
  yTitle = "Mean Monthly Temp",
  drawRollingBand = FALSE,
  drawRollingLine = FALSE,
  drawSparseLineAndPoints = FALSE,
  paletteDark = paletteDark,
  paletteLight = paletteLight
)

middlePanel <- CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "Temp", 
  stageIDName = "StageID",
  changePoints = changeMonths, 
  changePointLabels = changeLabels,
  yTitle = "Mean Monthly Temp",
  drawRollingBand = FALSE, 
  drawJaggedLine = FALSE,
  drawSparseLineAndPoints = FALSE,
  paletteDark = paletteDark,
  paletteLight = paletteLight
)

bottomPanel <- Wats::CartesianRolling(
  dsLinear = portfolioCartesian$dsLinear, 
  xName = "Date", 
  yName = "Temp", 
  stageIDName = "StageID", 
  changePoints = changeMonths, 
  yTitle = "Mean Monthly Temp", 
  changePointLabels = changeLabels, 
  drawJaggedLine = FALSE,
  drawSparseLineAndPoints = FALSE,
  paletteDark = paletteDark,
  paletteLight = paletteLight
)

topPanel <- topPanel + xScale + darkTheme
middlePanel <- middlePanel + xScale + darkTheme
bottomPanel <- bottomPanel + xScaleBlank + darkTheme

grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(3,1)))
print(topPanel, vp=vpLayout(1, 1))
print(middlePanel, vp=vpLayout(2, 1))
```

```
Warning: Removed 11 rows containing missing values (geom_path).
```

```r
print(bottomPanel, vp=vpLayout(3, 1))
```

```
Warning: Removed 11 rows containing missing values (geom_path).
```

```r
grid::popViewport()
```

<img src="figure_rmd/CartesianRolling.png" title="plot of chunk CartesianRolling" alt="plot of chunk CartesianRolling" width="1800px" />


## Section 2: Carteisan Periodic 
This section corresponds to the MBR manuscript, but with the modified dataset.

Carteisan plot of the GFR time series data in Oklahoma County, with H-spread Bands superimposed.


```r
cartesianPeriodic <- Wats::CartesianPeriodic(
  portfolioCartesian$dsLinear, 
  portfolioCartesian$dsPeriodic, 
  xName = "Date", 
  yName = "Temp",
  stageIDName = "StageID", 
  changePoints = changeMonths, 
  changePointLabels = changeLabels,
  yTitle = "Mean Monthly Temp",
  paletteDark = paletteDark,
  paletteLight = paletteLight,
  drawPeriodicBand = TRUE #The only difference from the simple linear graph above
)
cartesianPeriodic <- cartesianPeriodic + xScale + darkTheme 
print(cartesianPeriodic)
```

<img src="figure_rmd/CartesianPeriodic.png" title="plot of chunk CartesianPeriodic" alt="plot of chunk CartesianPeriodic" width="1800px" />


## Section 3: Polar Periodic
This section corresponds to the MBR manuscript, but with the modified dataset.

Wrap Around Time Series (WATS Plot) of the Oklahoma City GFR data, 1990-1999







