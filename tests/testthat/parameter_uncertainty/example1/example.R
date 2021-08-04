library(dplyr)
library(campsis)

model <- getNONMEMModelTemplate(3,4)
model <- model %>% add(InfusionDuration(1, rhs="5"))

dataset <- Dataset(20)
dataset <- dataset %>% add(Infusion(0, 100, 1))
dataset <- dataset %>% add(Infusion(24, 100, 1))
dataset <- dataset %>% add(Infusion(48, 100, 1))
dataset <- dataset %>% add(Infusion(72, 100, 1))
sampling <- c(1,2,4,8,10,12,16,20,24)
dataset <- dataset %>% add(Observations(times=c(sampling, sampling + 72)))

results <- model %>% simulate(dataset, dest="mrgsolve", seed=1)
spaghettiPlot(results, "Y")

etas <- (model@parameters %>% select("omega"))@list %>% purrr::map_chr(~paste0("ETA_", .x@name))
nonmemDataset <- dataset %>% export(dest="RxODE", model=model) %>% select(-all_of(etas))
nonmemDataset <- nonmemDataset %>% left_join(results %>% transmute(ID=id, TIME=time, EVID=0, DV=Y)) %>% campsisqual::standardiseDataset()
write.csv(nonmemDataset, file="dataset.csv", row.names = FALSE, quote = FALSE)

