setwd("/Users/sdong217/Desktop/COVID_NH/NH_CMS_data/raw")
data_2023 = read.csv("faclevel_2023.csv")
state_abbrevs = unique(data_2023["Provider.State"][,])

pax_mol_rates = function(state = state_abbrevs, data = data_2023){
  state_pax = c()
  for(state_ind in 1:length(state_abbrevs)){
    state = data[which(data["Provider.State"]==state_abbrevs[state_ind]),]
    pax = sum(state["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + 
      sum(state["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T) +
      sum(state["Therapeutic.Molnupiravir..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) +
      sum(state["Therapeutic.Molnupiravir..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
    cases = sum(state["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
    state_pax[length(state_pax)+1] = pax/cases
  }
  return(state_pax)
}
