### Function to convert VWC into SWP via a lookup table
# Lookup table encodes the Gardner equation with parameters inv.b and log.a
# Inputs are VWC, param, and stat
# VWC should be a scalar or vector of volumetric soil water content expressed as cm^3 cm^-3
# param indicates which Gardner parameters should be used: "site", "depth10cm", or "depth25cm"
# stat indicates which SWP should be output: "median", "lower", or "upper"
# Note: "lower" and "upper" denote the central 50th percentile
# Note: function will return "VWC out of range" message and NA as output if VWC < 0.015 or SWC >= 0.45

# Load lookup table
if(file.exists("source/lookup.Rdata")) {
  load("source/lookup.Rdata")
} else {
  load("../../source/lookup.Rdata")
}


vwc2swp <- function(vwc, param = "site", stat = "median") {
  if(min(vwc) < 0.015 | max(vwc) >= 0.45){
    warning("SWC out of range")
  } 
  
  if(param == "site") {
    SWP <- lookup[get_inds(vwc), 3:5]
  } else if (param == "depth10cm") {
    SWP <- lookup[get_inds(vwc), 6:8]
  } else if (param == "depth25cm") {
    SWP <- lookup[get_inds(vwc), 9:11]
  }
  
  if(stat == "median") {
    return(SWP[,1])
  } else if (stat == "lower") {
    return(SWP[,2])
  } else if (stat == "upper") {
    return(SWP[,3])
  }
}

get_ind <- function(vwc) {
  which.min(abs(lookup$vwc - vwc))
}
get_inds <- Vectorize(get_ind)
