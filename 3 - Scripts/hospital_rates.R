setwd("/Users/sdong217/Desktop/COVID_NH/NH_CMS_data/raw")
data = read.csv("faclevel_2023_full.csv")

## hospitalization rate over 2023
infs = sum(data["Residents.Weekly.Confirmed.COVID.19"],na.rm=T)
hospitals = sum(data["Residents.Hospitalizations.with.Confirmed.COVID.19"],na.rm=T)

hospitals/infs
