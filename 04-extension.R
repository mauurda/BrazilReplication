
# Load necessary packages
library(dplyr) # data manipulation
library(Synth) # models

#Measure Function
measure_effect <- function(dataprep.obj,synth.obj){
  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }
  treat_period <- outersect(unlist(dataprep.obj[["tag"]]["time.plot"]),unlist(dataprep.obj[["tag"]]["time.predictors.prior"]))
  treat_start_ind <- which(unlist(dataprep.obj[["tag"]]["time.plot"])== unlist(treat_period[1]))
  i = treat_start_ind
  auc=0
  while(i<= length(unlist(dataprep.obj[["tag"]]["time.plot"]))){
    y0s <- dataprep.obj$Y0plot[i,]
    cweights <- synth.obj$solution.w
    vweights <- synth.obj$solution.v
    effect<-dataprep.obj$Y1plot[i,1]-sum(unlist(y0s)*unlist(cweights))
    auc= auc+effect
    i=i+1
  }
  return(auc)
  
}


# Load data
df <- read.csv("df.csv", header = TRUE)

# Prepare dataset
df$state <- as.character(df$state) # required by dataprep()

# Plot: Homicide rates for Sao Paulo and Brazil (average)
df1 <- df %>%
  mutate(homicide.sp = ifelse(homicide.rates & state == "São Paulo", homicide.rates, NA)) %>%
  select(year, homicide.sp)

df2 <- df %>%
  mutate(homicide.rates1 = ifelse(homicide.rates & state != "São Paulo", homicide.rates, NA)) %>%
  group_by(year) %>%
  summarise(homicide.br = mean(homicide.rates1, na.rm = TRUE))


# Prepare data for synth
dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(14,32,33,52,53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo with 5 units"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Main model: gaps plot

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)



########################
### Robustness Tests ###
########################

# Please set your working directory to the data/ folder
#library(reshape2)
#library(Synth)

# Clear the workspace

# Load data
df <- read.csv("df.csv", header = TRUE)

# Prepare dataset
df$state <- as.character(df$state) # required by dataprep()

## Leave-one-out

# Loop over leave one outs
storegaps <- matrix(NA, length(1990:2009), 26)


co <- unique(df$code)
co <- co[-which(co==35)]
colnames(storegaps) <- co # codes

loocv_effects = list()

for(k in 1:26){
  
  # Data prep for training model
  omit <- co[k]
  
  # Prepare data for synth
  dataprep.out2 <-
    dataprep(df,
             predictors = c("state.gdp.capita",
                            "state.gdp.growth.percent",
                            "population.projection.ln",
                            "years.schooling.imp"
             ),
             special.predictors = list(
               list("homicide.rates", 1990:1998, "mean"),
               list("proportion.extreme.poverty", 1990:1998, "mean"),
               list("gini.imp", 1990:1998, "mean")
             ),
             predictors.op = "mean",
             dependent     = "homicide.rates",
             unit.variable = "code",
             time.variable = "year",
             unit.names.variable   = "state",
             treatment.identifier  = 35,
             controls.identifier   = co[-which(co==omit)],
             time.predictors.prior = c(1990:1998),
             time.optimize.ssr     = c(1990:1998),
             time.plot             = c(1990:2009)
    )
  
  # Run synth
  synth.out2 <- synth(dataprep.out2)
  loocv_effects[as.character(k)] <- measure_effect(dataprep.out2,synth.out2)
  
  storegaps[,k] <- (dataprep.out2$Y0%*%synth.out2$solution.w)
} # Close loop over leave one outs

# Leave-one-out: graph
#setEPS()
#postscript(file    = "leave-one-out.eps",
#horiz   = FALSE,
#onefile = FALSE,
#width   = 7,
#height  = 5.25)

path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)

for(i in 1:4){
  lines(1990:2009,
        storegaps[,i],
        col = "darkgrey",
        lty = "solid")
}

lines(1990:2009,
      dataprep.out$Y0plot %*% synth.out$solution.w,
      col = "black",
      lty = "dashed",
      lwd = 2)

legend(x = "bottomleft",
       legend = c("São Paulo",
                  "Synthetic São Paulo",
                  "Synthetic São Paulo (leave-one-out)"
       ),
       lty    = c("solid", "dashed", "solid"),
       col    = c("black", "black", "darkgrey"),
       cex    = .8,
       bg     = "white",
       lwdc(2, 2, 1)
)

#invisible(dev.off())

#Synth for without RR - 14
dataprep.out_RR <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(33, 42, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_RR <- synth(dataprep.out_RR)
measure_effect(dataprep.out_RR,synth.out_RR)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_RR,
  synth.res    = synth.out_RR)
)

# Plot: Without RR
path.plot(synth.res    = synth.out_RR,
          dataprep.res = dataprep.out_RR,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without Roraima"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Without RR
gaps.plot(synth.res    = synth.out_RR,
          dataprep.res = dataprep.out_RR,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)


#Synth for without 33 - RJ
dataprep.out_RJ <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(14 ,42, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_RJ <- synth(dataprep.out_RJ)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_RJ,
  synth.res    = synth.out_RJ)
)

# Plot: Without RJ
path.plot(synth.res    = synth.out_RJ,
          dataprep.res = dataprep.out_RJ,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without Rio"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# gaps plot - without RJ

gaps.plot(synth.res    = synth.out_RJ,
          dataprep.res = dataprep.out_RJ,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

#Synth for without 42 - SC
dataprep.out_SC <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(14 ,33, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_SC <- synth(dataprep.out_SC)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_SC,
  synth.res    = synth.out_SC)
)

# Plot: Without RJ
path.plot(synth.res    = synth.out_SC,
          dataprep.res = dataprep.out_SC,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without SC"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# gaps plot - without SC

gaps.plot(synth.res    = synth.out_SC,
          dataprep.res = dataprep.out_SC,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

#Synth for without 53 - DF
dataprep.out_DF <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(14, 33,42),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_DF <- synth(dataprep.out_DF)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_DF,
  synth.res    = synth.out_DF)
)

# Plot: Without DF
path.plot(synth.res    = synth.out_DF,
          dataprep.res = dataprep.out_DF,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without DF"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# gaps plot - without DF

gaps.plot(synth.res    = synth.out_DF,
          dataprep.res = dataprep.out_DF,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

#################################################
############### Sensitivity Test ################
#################################################
donnors_main <- c()
# Prepare data for synth

#Without Roramia
dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(32,33,42,53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

wo_roraima <- measure_effect(dataprep.out,synth.out)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo with 4 units"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Main model: gaps plot

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

#Without Roramima & Rio
co_ext <- 
dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(32,42,53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo with 3 units"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Main model: gaps plot

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)



#Without Roramima & Rio & Districto Fe
dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(32,42),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo with 2 units"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Main model: gaps plot

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)



#Without Roramima & Rio & Districto Fe & ES
dataprep.out <-
  dataprep(df,
           predictors = c("state.gdp.capita",
                          "state.gdp.growth.percent",
                          "population.projection.ln",
                          "years.schooling.imp"
           ),
           special.predictors = list(
             list("homicide.rates", 1990:1998, "mean"),
             list("proportion.extreme.poverty", 1990:1998, "mean"),
             list("gini.imp", 1990:1998, "mean")
           ),
           predictors.op = "mean",
           dependent     = "homicide.rates",
           unit.variable = "code",
           time.variable = "year",
           unit.names.variable   = "state",
           treatment.identifier  = 35,
           controls.identifier   = c(42),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out <- synth(dataprep.out)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

# Plot: Main model
path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo with 1 unit"),
          Legend.position = c("bottomleft")
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 50, 1999, 50,
       col    = "black",
       length = .1)

text(1995, 50,
     "Policy Change",
     cex = .8)


# Main model: gaps plot

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap in Homicide Rates"),
          Xlab         = c("Year"),
          Ylim         = c(-30, 30),
          Main         = ""
)

abline(v   = 1999,
       lty = 2)

arrows(1997, 20, 1999, 20,
       col    = "black",
       length = .1)

text(1995, 20,
     "Policy Change",
     cex = .8)

sensitivity_exclusion <- c(
  
  -47.21143,
  -15.71276,
  -38.87922,
  -84.86245,
  -113.4188
  
)
loocv_effects
loo_original_model <- c(
  -18.19069,
  -69.6683,
  -71.19024,
  -12.21043,
  -14.75528,
  -15.71276
)
boxplot(sensitivity_exclusion)
boxplot(unlist(loocv_effects))


#plotting uncertainty

plot(NA, xlim=c(-130,10), ylim=c(0,5), axes=F, ylab="",
     xlab="Distributions of Effects (area under the curve) for different synthetic control models.")

boxplot(sensitivity_exclusion, horizontal=T,add=T,axes=F,at=1,outline=F)
boxplot(unlist(loocv_effects),horizontal=T,add=T,axes=F,at=2,outline=F)
boxplot(loo_original_model, horizontal=T,add=T,axes=F,at=3,outline=F)

abline(v=0,lty=2,lwd=2)
text(y = seq(1, 3, by=1), x=par("usr")[1], par("usr")[3] - 0.2,
     labels = (c("Exc ST","Loo ST","Loo ST(Orig)")),
     cex=1,srt = 45, pos = 2, xpd = TRUE)

axis(1)

