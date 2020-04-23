# Load necessary packages
library(dplyr) # data manipulation
library(Synth) # models
library(ggplot2)

#Our function for measuring Treatment Effect
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

#All states
co <- unique(df$code)
co <- co[-which(co==35)]

########################
###   Main Analysis  ###
########################

#(simplified)
{
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
           controls.identifier   = c(14,26,32,33,53,42),
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

}
measure_effect(dataprep.out,synth.out)
########################
###     Extension    ###
########################

## RT 1: Leave one out for all states in donnor pool

# store results
storegaps <- matrix(NA, length(1990:2009), 26)
colnames(storegaps) <- co # codes
rt1_results = list()

# Loop over leave one outs
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
  rt1_results[as.character(k)] <- measure_effect(dataprep.out2,synth.out2)
  
  storegaps[,k] <- (dataprep.out2$Y0%*%synth.out2$solution.w)
} # Close loop over leave one outs

#Replicate Graph from paper
{
# Leave-one-out: graph
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
}

#Effect measurements
rt1_results

#RT2: Leave One out of the original Model
cont <- c(26,14,33,53,42,32)
rt2_results <- list()

for (i in 1:6){
  cont_2 <- cont[-i]
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
             controls.identifier   = cont_2,
             time.predictors.prior = c(1990:1998),
             time.optimize.ssr     = c(1990:1998),
             time.plot             = c(1990:2009)
    )
  
  # Run synth
  synth.out <- synth(dataprep.out)
  rt2_results<- c(unlist(rt2_results),unlist(measure_effect(dataprep.out, synth.out)))
  
}


#Manual RT2. We used this and Manual RT2 to create synth plots and weight tables.
#We added the curly braces so that you can minimize them in your R Studio.
#Feel free to remove the curly brackets and run them, but you can also just
#use the loop above to get the range of treatment effects. 
{
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
           controls.identifier   = c(26, 32, 33, 42, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_RR <- synth(dataprep.out_RR)

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

measure_effect(dataprep.out_RR, synth.out_RR)


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
           controls.identifier   = c(14, 26, 32, 42, 53),
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

measure_effect(dataprep.out_RJ, synth.out_RJ)

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
           controls.identifier   = c(14, 26, 32 ,33, 53),
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

# Plot: Without SC
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

measure_effect(dataprep.out_SC, synth.out_SC)

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
           controls.identifier   = c(14, 26,32, 33,42),
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

measure_effect(dataprep.out_DF, synth.out_DF)


#Synth for without 32 - ES
dataprep.out_ES <-
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
           controls.identifier   = c(14, 26,33,42, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_ES <- synth(dataprep.out_ES)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_ES,
  synth.res    = synth.out_ES)
)

# Plot: Without DF
path.plot(synth.res    = synth.out_ES,
          dataprep.res = dataprep.out_ES,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without ES"),
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


# gaps plot - without ES

gaps.plot(synth.res    = synth.out_ES,
          dataprep.res = dataprep.out_ES,
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

measure_effect(dataprep.out_ES, synth.out_ES)


#Synth for without 26 - Pernambuco
dataprep.out_P <-
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
           controls.identifier   = c(14, 32,33,42, 53),
           time.predictors.prior = c(1990:1998),
           time.optimize.ssr     = c(1990:1998),
           time.plot             = c(1990:2009)
  )

# Run synth
synth.out_P <- synth(dataprep.out_P)

# Get result tables
print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out_P,
  synth.res    = synth.out_P)
)

# Plot: Without P
path.plot(synth.res    = synth.out_P,
          dataprep.res = dataprep.out_P,
          Ylab         = c("Homicide Rates"),
          Xlab         = c("Year"),
          Legend       = c("São Paulo","Synthetic São Paulo without Pernambuco"),
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


# gaps plot - without P

gaps.plot(synth.res    = synth.out_P,
          dataprep.res = dataprep.out_P,
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
measure_effect(dataprep.out_P, synth.out_P)
}

#Stores Results for RT2
rt2_results


#RT3: Exclude Worst Fitted, decrease donnor Pool Size
rt3_results <- list()

#Ordered after running the process manually
cont <- c(26,14,33,53,42,32)

#looped RT3
for (i in 1:5){
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
             controls.identifier   = cont,
             time.predictors.prior = c(1990:1998),
             time.optimize.ssr     = c(1990:1998),
             time.plot             = c(1990:2009)
    )
  
  # Run synth
  synth.out <- synth(dataprep.out)
  
  # Save results
  rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))
  
  cont <- cont[-1]
}

#Manual RT3. 

{
#With all 6
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
           controls.identifier   = c(14,26,32,33,42,53),
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

rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))

#Without Pernambuco
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

rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))

#Without Roramia & Pernambuco
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

rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))

#Without Roramima & Rio & Pernambuco
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

rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))

#Without Roramima & Rio & Districto Fe & Pernambuco
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

rt3_results<- c(unlist(rt3_results),unlist(measure_effect(dataprep.out, synth.out)))
}

#Creates Figure 3

plot(NA, xlim=c(-130,10), ylim=c(0,4), axes=F, ylab="",
     xlab="Distributions of Effects (area under the curve) for different synthetic control models.")

boxplot(rt3_results, horizontal=T,add=T,axes=F,at=3,outline=F)
boxplot(unlist(rt1_results),horizontal=T,add=T,axes=F,at=1,outline=F)
boxplot(rt2_results, horizontal=T,add=T,axes=F,at=2,outline=F)

abline(v=0,lty=2,lwd=2)
text(y = seq(1, 3, by=1), x=par("usr")[1], par("usr")[3] - 0.2,
     labels = (c("RT 1","RT 2","RT 3")),
     cex=1,srt = 45, pos = 2, xpd = TRUE)

axis(1)

#Creates Appendix A.1
boxplot(unlist(rt1_results),ylab= "Treatment effect Size", xlab="RT1")

#Creates Appendix A.2

#setting up dataset
{
cumu_rrsc_indexes <- c(which(df$code==14),which(df$code==33),which(df$code==42),which(df$code==35))
cumu_rrsc_df<-df[cumu_rrsc_indexes,]
cumu_rrsc_neg_df<-df[-cumu_rrsc_indexes,]

cumu_brazil <- cumu_rrsc_neg_df %>%
  group_by(year)%>%
  summarise(homicide.rates = mean(homicide.rates))
cumu_brazil<- as_tibble(cumu_brazil)
cumu_brazil$state<- "Brazil (exc. RR,RJ,SC,SP)"
cumu_rrsc <- select(cumu_rrsc_df,year,homicide.rates,state)
cumu_data <- rbind(cumu_brazil,cumu_rrsc)
#write.csv(cumu_data,"cumu_data.csv",sep = ",")
}
#I exported and re-imported this from excel because it was faster
#to add the last two columns: change and cumulative there. I used the
#homicide rate for eacxh state (and brazil without them) in 1990 as a
#baseline (= 1) and then added the change from each year multiplied by
#the cumulative change from last year to construct the graph.You don't
#need to run the past chunk; just make sure the dataset imported underneath
#is in your working directory.

#Read Data
cumu_data_processed <- read.csv("cumu_data_processed.csv")

theme_set(theme_classic())
ggplot(cumu_data_processed,aes(x=year)) + 
  geom_line(aes(y=cumulative,color=state)) + 
  labs(x="Year",y="Cumulative change",
       title = "Homicide trends in Brazil for selected states.") + 
  scale_colour_manual(values = c("#D16103", "#C3D7A4", "#52854C", "#4E84C4","#293352"))


