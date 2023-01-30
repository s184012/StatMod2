df <- read.table("w1e2_Data.txt", header = T)
df

mdl <- lm(y ~x, df)
mdl

