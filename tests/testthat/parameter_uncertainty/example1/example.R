library(dplyr)
library(campsis)

model <- getNONMEMModelTemplate(3,4)
model <- model %>% add(CompartmentInfusionDuration(1, rhs="5"))

dataset <- Dataset(20)
dataset <- dataset %>% add(Infusion(0, 100, 1))
dataset <- dataset %>% add(Infusion(24, 100, 1))
dataset <- dataset %>% add(Infusion(48, 100, 1))
dataset <- dataset %>% add(Infusion(72, 100, 1))
sampling <- c(1,2,4,8,10,12,16,20,24)
dataset <- dataset %>% add(Observations(times=c(sampling, sampling + 72)))

results <- model %>% simulate(dataset, dest="mrgsolve", seed=1)
spaguettiPlot(results, "Y")

nonmemDataset <- dataset %>% export(dest="RxODE", model=model) %>% select(-ETA_1, -ETA_2, -ETA_3, -ETA_4)
nonmemDataset <- nonmemDataset %>% left_join(results %>% transmute(ID=id, TIME=time, EVID=0, DV=Y)) %>% pmxqual::standardiseDataset()
write.csv(nonmemDataset, file="dataset.csv", row.names = FALSE, quote = FALSE)

