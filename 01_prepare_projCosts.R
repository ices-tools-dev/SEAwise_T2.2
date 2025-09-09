library(reshape2)
library(FLBEIA)

load(file = file.path("output", "fleets.Rdata"))

yrMax <- 2100
yrRef <- 2018

### calculate relative changes in costs and prices for projection years

growthRate <- read.csv("./growthRate.csv", check.names = F, stringsAsFactors = F)
names(growthRate)[1] <- "country"
head(growthRate)
growthRate <- melt(growthRate, id = "country")
names(growthRate)[2] <- "year"
growthRate$year <- as.numeric(as.character(growthRate$year))
head(growthRate)

lut <- structure(list(countryCode = c("BE", "DK", "EN", "FR", "GE", 
  "NL", "SC", "SW"), country = c("Belgium", "Denmark", "United Kingdom", 
  "France", "Germany", "Netherlands", "United Kingdom", "Sweden"
  )), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L), class = "data.frame")

growthRate$countryCode <- lut$countryCode[match(growthRate$country, table = lut$country)]
growthRate <- subset(growthRate, !is.na(countryCode))
head(growthRate)


# fuel and fish prices
priceRate <- read.csv("./priceRate.csv", check.names = F, stringsAsFactors = F)
names(priceRate)[1] <- "scen"

# produce full look-up results for relative costs by fleet/metier and prices --------
# the routine uses gdp estimates for future inflation rates by country
# future relative fuel and fish prices for under different climate change scenarios
# for European countries (aggregate)
#
# fixed costs - By each fleet's country using inflation rates
# variable costs - By each fleet/metier, considering the country's inflation rate for the non fuel portion,
#     and fuel price change for the fuel portion of variable costs
# fish price - same for all - simply the relative change across all fleet/metiers

res <- vector("list", nrow(lut))
names(res) <- lut$countryCode
for(i in seq(res)){
  gdp.i <- subset(growthRate, country == lut$country[i])
  gdp.i$countryCode <- lut$countryCode[i]

  # expand years if required
  yrAvail <- range(gdp.i$year)
  gdp.i<- merge(gdp.i,
    data.frame(year = yrAvail[1]:yrMax, country = gdp.i$country[1], countryCode = gdp.i$countryCode[1]),
    all = TRUE)
  # hit <- which(gdp.i$year < yrAvail[1])
  # if(length(hit) > 0){gdp.i$value[hit] <- subset(gdp.i, year == yrAvail[1])$value}
  hit <- which(gdp.i$year > yrAvail[2])
  if(length(hit) > 0){gdp.i$value[hit] <- subset(gdp.i, year == yrAvail[2])$value}

  # calculate cumulative cost factor
  gdp.i$relVal <- 1
  gdp.i$var <- "inflation"
  yrRange <- range(gdp.i$year)
  yrIncl <- yrRange[1]:yrRange[2]
  for(yr in yrIncl[-1]){
    idx <- which(gdp.i$year == yr)
    gdp.i$relVal[idx] <- gdp.i$relVal[idx-1] * (1+(gdp.i$value[idx-1]/100))
  }
  
  # calculate cumulative fuel cost and fish price factors
  price.i <- merge(priceRate, data.frame(year = yrIncl), all = TRUE)
  price.i$relVal <- (1+price.i$rate/100)^(price.i$year - yrRange[1])
  # price.i$relVal[which(price.i$year<=yrRef)] <- 1
  price.i$countryCode <- names(res)[i]
  
  tmp <- merge(gdp.i[,c("countryCode", "year", "var", "relVal")], 
    price.i[,c("countryCode", "year", "scen", "var", "relVal")], 
    all = TRUE)
  head(tmp)
  
  res[[i]] <- tmp
}

res <- do.call("rbind", res)
head(res)

# cost multiplier query function ------------------------------------------

# This function does the actualy step of weighting the variable costs by each 
# metiers proportions of costs (fuel-based, vs other)

load(file = file.path("./output", "frac_fcost.2060.Rdata"))
load(file = file.path("./output", "frac_vcost.2060.Rdata"))


relCostQuery <- function(fl = "BE_Beam_lthan_24", mt = "BT2_dot_4", scen = "WM_RCP8.5_SSP5", 
  relValue = res, frac_fcost = frac_fcost, frac_Vcost = frac_vcost, 
  yrRelPrice = NULL, yrRelCost = NULL){
  
  cc <- strsplit(fl, split = "_")[[1]][1]
  sc <- scen
  dfsub <- subset(x = relValue, subset = countryCode == cc & scen %in% c(NA, sc))
  
  if(nrow(dfsub)>0){
    
    tmp1 <- subset(dfsub, var == "inflation")[,c("year", "var", "relVal")]
    tmp2 <- subset(dfsub, var %in% c("fuel_price", "fish_price"))[,c("year", "var", "relVal")]
    tmp <- merge(tmp1, tmp2, all = T)
    out <- dcast(tmp, formula =  year ~ var, value.var = "relVal")
    
    frac_vcost_sub <- subset(frac_vcost, fleet == fl & metier == mt)
    frac_fcost_sub <- subset(frac_fcost, fleet == fl)
    
    out <- merge(out, frac_vcost_sub[,-which(names(frac_vcost_sub) %in% c("fleet", "metier"))], all = TRUE)
    out <- merge(out, frac_fcost_sub[,-which(names(frac_fcost_sub) %in% c("fleet"))], all = TRUE)
    
    out$relVcost <- 
      out$inflation * (out$Value_of_unpaid_labour_euro + out$`Repair_&_maintenance_costs_euro` + out$Other_variable_costs_euro) + 
      out$fuel_price * out$Energy_costs_euro
    out$relFcost <- out$inflation
    out$relPrice <- out$fish_price
    
    out <- out[,c("year", "relVcost", "relFcost", "relPrice")]
    
    if(!is.null(yrRelPrice)){
      idx <- which(out$year == yrRelPrice)
      out[,c("relPrice")] <- out[,c("relPrice")] / out[idx,c("relPrice")]
    }
    if(!is.null(yrRelCost)){
      idx <- which(out$year == yrRelCost)
      out[,c("relVcost", "relFcost")] <- 
        apply(out[,c("relVcost", "relFcost")], 2, FUN = function(x){x / x[idx]})
    }
    
    out$fleet <- fl
    out$metier <- mt
    out$scen <- scen
    
  }else{
    out <- NULL
  }

  return(out)
}


# apply data extraction ---------------------------------------------------

SCEN <- unique(res$scen)[1:4]

relEcon <- vector("list", length(SCEN))
names(relEcon) <- SCEN

for(h in seq(SCEN)){
  relEconFl <- vector("list", length(fleets))
  for(i in seq(fleets)){
    fl <- names(fleets)[i]
    relEconMt <- vector("list", length(fleets[[fl]]@metiers))
    for(j in seq(fleets[[fl]]@metiers)){
      mt <- names(fleets[[fl]]@metiers)[j]
      relEconMt[[j]] <- relCostQuery(fl = fl, mt = mt, yrRelPrice = 2018, yrRelCost = 2018,
        relValue = res, frac_fcost = frac_fcost, frac_Vcost = frac_vcost, scen = names(relEcon[h]))
    }
    relEconFl[[i]] <- do.call("rbind", relEconMt)
  }
  relEcon[[h]] <- do.call("rbind", relEconFl)
}
relEcon <- do.call("rbind", relEcon)

relEcon <- relEcon[,c("fleet", "metier", "scen", "year", "relVcost", "relFcost", "relPrice")]
rownames(relEcon) <- seq(nrow(relEcon))
dim(relEcon)
head(relEcon, 10)
tail(relEcon)

save(relEcon, file = "output/relEcon.Rdata")

