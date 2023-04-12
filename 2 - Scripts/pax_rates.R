setwd("/Users/sdong217/Desktop/COVID_NH/NH_CMS_data/raw")
data = read.csv("faclevel_2022.csv")
data_dec22 = data[which(substring(data["Week.Ending"][,],0,2)=="12"),]
state_abbrevs = unique(data_dec22["Provider.State"][,])

pax_rates = function(state = state_abbrevs, data = data_dec22){
  state_pax = c()
  for(state_ind in 1:length(state_abbrevs)){
    state = data[which(data["Provider.State"]==state_abbrevs[state_ind]),]
    pax = sum(state["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + sum(state["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
    cases = sum(state["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
    state_pax[length(state_pax)+1] = pax/cases
  }
  return(state_pax)
}
