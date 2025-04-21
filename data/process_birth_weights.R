# Read original file, write weights only.
df <- read.csv('birth_weights.csv')
birth_weights <- subset(df, select='birth_weight')
write.csv(birth_weights, 'nc_birth_weights.csv', row.names=FALSE)

# Make sample and write.
set.seed(1939)
n <- 50
sample <-  birth_weights[sample(nrow(birth_weights), n),]
write.csv(sample, 'nc_birth_weights_sample.csv', row.names=FALSE)
