require(here)
ginkgo = data.frame(read.csv(here("data", "ginkgo_data_2022.csv")))
seed_site = subset(ginkgo, select= c('seeds_present','site_id'))
trees = length(unique(seed_site$site_id))

seed = sum(ginkgo$seeds_present)/10
seed

boxplot(max_depth ~ seeds_present, data = ginkgo, main = 'Doubly-Conditional Boxplot Species', names = c('no seeds', 'seeds'))                       
                       