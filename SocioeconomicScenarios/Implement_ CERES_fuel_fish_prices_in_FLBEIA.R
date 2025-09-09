
# Implementing CERES project fuel price and fish price percentage changes in FLBEIA 

rm(list=ls())
library(FLCore) 
library(FLBEIA) 

# load the FLBEIA input lists (biols, fleets, BD, SRs, etc.)
data(one)  # this is the FLBEIA dummy data set 

first.yr          <- 1990
proj.yr           <- 2010 
last.yr           <- 2025  
yrs <- c(first.yr=first.yr, proj.yr=proj.yr, last.yr=last.yr)

i1  <- yrs['proj.yr'] - yrs['first.yr'] + 1  # position of first projection yr in objects
i2  <- yrs['last.yr'] - yrs['first.yr'] + 1  # position of last yr in objects

# ------- Define Tables with annual change rates in fish and fuel prices ---------------------

##### the following Tables (fish_price, fuel_price) apply to all case studies - do not modify ### 

# ----- annual increase rate of Fish prices
fish_price <- matrix(NA, nrow=1, ncol=4)
fish_price <- c(1.57, 1.67, 1.33, 1.64)         # annual increase rates per scenario
names(fish_price) <- c('WM', 'NE', 'GS', 'LS')
fish_price        <- fish_price/100             # divide by 100 as the are % rates
fish_price

# ----- annual increase rate of fuel prices
fuel_price <- matrix(NA, nrow=3, ncol=4)
fuel_price <- c(2.59, 2.80, 2.59, 2.61)         # annual increase rates per scenario
names(fuel_price) <- c('WM', 'NE', 'GS', 'LS')         
fuel_price           <- fuel_price/100          # divide by 100 as the are % rates
fuel_price

##### the following Table (fuelcost_fr) should be filled in by each case study 
#     with own values calculated per metier                    

# ----Fraction of Variable costs corresp. to fuel costs
# the change in fuel costs affects Variable costs. Hence we need the fraction of 
# Variable costs corresponding to fuel costs (or energy costs) by fleet-metier combination. 
# !!!!  to be estimated per case study!!!!!!

fuelcost_fr        <- matrix(NA, nrow=1, ncol=1)
fuelcost_fr        <- c(0.5)       # fraction of variable costs that is due to fuel costs per metier
names(fuelcost_fr) <- c('met1')    # name of metier      
fuelcost_fr

## In this example there is only 1 fleet and one metier. Modify accordingly to cover all metiers
# fuelcost_fr        <- matrix(NA, nrow=1, ncol=3) # ncol equals to the number of metiers
# fuelcost_fr        <- c(0.5, 0.3, 0.4)       # fraction of variable costs that is due to fuel costs per metier
# names(fuelcost_fr) <- c('met1', 'met2', 'met3')    # name of metier      
# fuelcost_fr

#------ Modify fish prices and vcosts in the fleets object before running FLBEIA   -------

# In the following example, fish and fuel prices are changed according to 'WM' scenario before running FLBEIA

# ----- modify fish prices according to WM (SSP5)
x          <- seq(1, i2 - i1 + 1, 1)   # sequence of years to modify
rate       <- fish_price['WM']         # select scenario
price_mult <- (1 + rate)^x             # these are the fish price multipliers

# for stock1
for (i in 1 : 12){  # repeat for all age classes
  oneFl[["fl1"]]@metiers[["met1"]]@catches[["stk1"]]@price[i, i1:i2, , , ] <- oneFl[["fl1"]]@metiers[["met1"]]@catches[["stk1"]]@price[i, i1:i2, , , ]*price_mult 
  # ... add all fleets and metier for the given stock
}

# for stock 2
#...

# for stock 3
#...


#------ set fuel costs according to WM (SSP5) --------

# Energy or fuel costs form a given fraction of variable costs for each fleet-metier combination
# Fuel costs increase with fuel prices. The annual multiplier for fuel costs is mult = (1 + fuel_price),
# where fuel_price is annual rates of change (defined above). The annual change rate in Variable costs 
# due to fuel costs will be (mult - 1) * fuelcost_fr, where fuelcost_fr is the fraction 
# of Variable costs corresponding to energy costs 

fuelcost_mult     <- (1 + fuel_price['WM']) 
vcost_mult        <- (1 + (fuelcost_mult - 1) * fuelcost_fr)
vcost_m           <- matrix(NA, nrow=(last.yr - proj.yr + 1), ncol=length(vcost_mult))
vcost_m[,1]       <- vcost_mult[1]^x
# In this example there is 1 metier. Fill in vcost columns for each of your metiers 
#  vcost_m[,2]       <- vcost_mult[2]^x
#  vcost_m[,3]       <- vcost_mult[3]^x
# ...  

colnames(vcost_m) <- c('met1')  # name vcost with the names of metiers (as in the FLBEIA objects)
vcost_m

# apply it to all fleets and metiers
oneFl[["fl1"]]@metiers[["met1"]]@vcost[, i1:i2, , , ] <- oneFl[["fl1"]]@metiers[["met1"]]@vcost[, i1:i2, , , ]*vcost_m[,'met1'] 
# oneFl[["fl1"]]@metiers[["met2"]]@vcost[, i1:i2, , , ] <- oneFl[["fl1"]]@metiers[["met2"]]@vcost[, i1:i2, , , ]*vcost_m[,'met2'] 
# ...

# -------- Run FLBEIA for WM scenario --------------- 

WM <- FLBEIA(biols = oneBio,
              SRs = oneSR,      
              BDs = NULL,        
              fleets = oneFl,   
              covars = oneCv,   
              indices = NULL,
              advice = oneAdv,
              main.ctrl = oneMainC,
              biols.ctrl = oneBioC,
              fleets.ctrl = oneFlC,
              covars.ctrl = oneCvC,
              obs.ctrl = oneObsC,
              assess.ctrl = oneAssC, 
              advice.ctrl = oneAdvC) 
