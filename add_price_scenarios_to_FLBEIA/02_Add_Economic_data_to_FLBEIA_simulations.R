# ============================================================== # 
# Script to add economic info to the 
# simulations:
# - based on script 04: addEcon = TRUE to add fCosts and vCosts
# - update economic data for the projection period
# ============================================================== #

# Initialise system --------------------------------------------------------------
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()

# Required packages -------------------------------------------------------

library(FLCore) 
library(FLash) 
library(FLAssess)
library(FLFleet)
library(FLXSA)
library(FLBEIA) 
library(beepr)
library(pals)
library(pracma)
library(stringr)

# Global settings
first.yr          <- 2014
proj.yr           <- 2019 
last.yr           <- 2060
hist.yrs  <- ac(first.yr:(proj.yr-1))
proj.yrs  <- ac(proj.yr:last.yr)
ni <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$iter})) # number of iterations
ns <- 1 # max(sapply(stocks, FUN = function(x){dims(x)$season}))  # number of seasons
# Year extent in objects
yrs = unlist(list("first.yr" = first.yr, "proj.yr" = proj.yr, "last.yr" = last.yr))

# helper function to update the price for each stock
update.price = function(price.array,rel.price.per.yr,update.yrs) {
  Ak <- aperm(price.array[,update.yrs], perm = c(2,1,3:6))
  Ak <- Ak * rel.price.per.yr
  Ak <- aperm(Ak, perm = c(2,1,3:6))
  return(Ak)
}

# load economic data table ------------------------------------------------

load("./output/relEcon.Rdata")
# change the LS_RCP6.0_SSP2 with LS_RCP4.5_SSP2
relEcon$scen = gsub("RCP6.0","RCP4.5",relEcon$scen)
econ.scenarios = unique(relEcon$scen)
print(econ.scenarios)

# load results ------------------------------------------------------------

clim.scenario =  c("RCP4.5","RCP8.5") 
econ.scen = econ.scenarios[grep(pattern = paste(clim.scenario,collapse = "|"),econ.scenarios)]

# for different HCRs
HCR.scenario  = c("FmsyH") 

# create new directory for multi-runs
# for the EMSRR output
dir.create(file.path("output", "multi_econ","EMSRR","econ_projections"),recursive = T,showWarnings = F)

# specify number of parallel cores to loop over several iterations
parallel.cores = 2 # use two here (laptop), for a higher performing cluster use something around 15 maybe....

for(scen in econ.scen){ # for each economic scenario
  for(hcr in HCR.scenario){ # for each HCR
    iter.results = list.files("./output/multi_econ/EMSRR/",pattern = hcr)
    
    # get clim scenario matching the economic climate scenario
    clim.scen = tolower(as.character(na.omit(str_match(scen, clim.scenario))))
    iter.results = iter.results[grep(clim.scen,iter.results)]
    if(isempty(iter.results)){
      next()
    }
    
    for(iter in iter.results){ # for each multi-iteration
      # remove everything beforehand to make sure no previous simulation is loaded!!!
      rm(list = ls()[grep("SR.ensembles",ls())])
      # load
      load(file = file.path("./output/multi_econ/EMSRR/", iter))
      res = get(ls()[grep("SR.ensembles",ls())])
      
      fleets = res$fleets
      
      # adjust costs and revenue ------------------------------------------------
      yrAdj <- proj.yrs
      
      start.t = Sys.time()
      
      # parallelize outer loop
      library(foreach)
      library(parallel)
      library(doParallel)
      n.cores = parallel.cores # parallel cores
      cl = makeCluster(n.cores)
      registerDoParallel(cl)
      fleets.update.list = foreach(i = seq(fleets),.combine = list,.multicombine = TRUE) %dopar% {
        fl <- names(fleets)[i]
        sc <- scen
        df_fl <- subset(relEcon, scen == sc & year %in% as.numeric(yrAdj) & fleet == fl)
        for(j in seq(fleets[[fl]]@metiers)){
          mt <- names(fleets[[fl]]@metiers)[j]
          df_flmt <- subset(df_fl, metier == mt)
          mat <- match(as.numeric(yrAdj), df_flmt$year)
          # adjust fixed costs
          if(j == 1){
            fleets[[fl]]@fcost[,yrAdj] <- fleets[[fl]]@fcost[,yrAdj] * df_flmt$relFcost[mat]
          }
          # adjust variable costs
          fleets[[fl]]@metiers[[mt]]@vcost[,yrAdj] <- fleets[[fl]]@metiers[[mt]]@vcost[,yrAdj] * df_flmt$relVcost[mat]
          # adjust fish price
          tmp = fleets[[fl]]@metiers[[mt]]@catches
          price.tmp = lapply(tmp, function(x) update.price(price.array = x@price,
                                                           rel.price.per.yr = df_flmt$relPrice[mat],
                                                           update.yrs = yrAdj))
          tmp = Map(function(x, y) { x@price[,yrAdj] <- y; x }, fleets[[fl]]@metiers[[mt]]@catches, price.tmp)
          # update the data slot of the catches obj.
          slot(fleets[[fl]]@metiers[[mt]]@catches,".Data") = tmp
          
          # this is the wrong way to do it (cos it will get rid of some slots characteristic of the S4-class):
          #class(tmp) = class(fleets[[fl]]@metiers[[mt]]@catches)
          #fleets[[fl]]@metiers[[mt]]@catches = tmp
        }
        out = fleets[[fl]]
      }
      stopCluster(cl)
      # bring to old form (is this step required or is it sufficient to change the class to "FLFleetsExt"?)
      slot(fleets,".Data") = fleets.update.list # update the data slot of fleets
      # for(i in seq(fleets)){
      #   fleets[[i]] = fleets.update.list[[i]]
      # }
      print(Sys.time()-start.t)
      # add to original simulation
      res$fleets = fleets
      cat(paste(gsub(pattern = ".RData","",iter),"||",hcr,"\n"))
      assign(x = gsub(pattern = ".RData","",iter),value = res)
      save(list = gsub(pattern = ".RData","",iter),
           file = file.path("./output/multi_econ/EMSRR/econ_projections",
                            paste(paste(gsub(pattern = ".RData","",iter),"econ_projection",scen,sep = "_"),"RData",sep = ".")))
    }
  }
}
# ------------------------------------------------------------------------------------------------------------ #



