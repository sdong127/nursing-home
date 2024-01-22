setwd("/Users/sdong217/Desktop/COVID_NH/NH_CMS_data/raw")
data = read.csv("faclevel_2023.csv")

## mortality rate over 2023
infs_data = data[which(data["Week.Ending"][,]!="04/02/23" & data["Week.Ending"][,]!="04/09/23"),]
deaths_data = data[which(data["Week.Ending"][,]!="01/01/23" & data["Week.Ending"][,]!="01/08/23"),]

infs = sum(infs_data["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
deaths = sum(deaths_data["Residents.Weekly.COVID.19.Deaths"],na.rm=T)

deaths/infs


## see how mortality rate changes over 2022
# first 4 months
start_2022 = data[which(substring(data["Week.Ending"][,],0,2)=="01" | substring(data["Week.Ending"][,],0,2)=="02" | substring(data["Week.Ending"][,],0,2)=="03" | substring(data["Week.Ending"][,],0,2)=="04"),]
mid_2022 = data[which(substring(data["Week.Ending"][,],0,2)=="05" | substring(data["Week.Ending"][,],0,2)=="06" | substring(data["Week.Ending"][,],0,2)=="07" | substring(data["Week.Ending"][,],0,2)=="08"),]
end_2022 = data[which(substring(data["Week.Ending"][,],0,2)=="09" | substring(data["Week.Ending"][,],0,2)=="10" | substring(data["Week.Ending"][,],0,2)=="11" | substring(data["Week.Ending"][,],0,2)=="12"),]

start_infs_data = start_2022[which(start_2022["Week.Ending"][,]!="04/17/22" & start_2022["Week.Ending"][,]!="04/24/22"),]
start_deaths_data = start_2022[which(start_2022["Week.Ending"][,]!="01/02/22" & start_2022["Week.Ending"][,]!="01/09/22"),]

start_infs = sum(start_infs_data["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
start_deaths = sum(start_deaths_data["Residents.Weekly.COVID.19.Deaths"],na.rm=T)

start_deaths/start_infs

# middle 4 months
mid_infs_data = mid_2022[which(mid_2022["Week.Ending"][,]!="08/21/22" & mid_2022["Week.Ending"][,]!="08/28/22"),]
mid_deaths_data = mid_2022[which(mid_2022["Week.Ending"][,]!="05/01/22" & mid_2022["Week.Ending"][,]!="05/08/22"),]

mid_infs = sum(mid_infs_data["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
mid_deaths = sum(mid_deaths_data["Residents.Weekly.COVID.19.Deaths"],na.rm=T)

mid_deaths/mid_infs

# last 4 months
end_infs_data = end_2022[which(end_2022["Week.Ending"][,]!="12/18/22" & end_2022["Week.Ending"][,]!="12/25/22"),]
end_deaths_data = end_2022[which(end_2022["Week.Ending"][,]!="09/04/22" & end_2022["Week.Ending"][,]!="09/11/22"),]

end_infs = sum(end_infs_data["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
end_deaths = sum(end_deaths_data["Residents.Weekly.COVID.19.Deaths"],na.rm=T)

end_deaths/end_infs


## paxlovid/molnupiravir
# pax over 2023
pax = sum(infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + 
  sum(infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T) 
pax/infs

# mol over 2023
mol = sum(infs_data["Therapeutic.Molnupiravir..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + 
  sum(infs_data["Therapeutic.Molnupiravir..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
mol/infs

# first 4 months
pax_start = sum(start_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + sum(start_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
pax_start/start_infs

# middle 4 months
pax_mid = sum(mid_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + sum(mid_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
pax_mid/mid_infs

# last 4 months
pax_end = sum(end_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.This.Facility"],na.rm=T) + sum(end_infs_data["Therapeutic.Paxlovid..Number.of.Residents.Treated.from.Stock.Stored.at.Another.Facility"],na.rm=T)
pax_end/end_infs
