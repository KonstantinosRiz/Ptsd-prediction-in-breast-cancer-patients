df1 <- data.frame(a = c(1, 2, 5, 10, 18, 36),
                  b = c(-7, 8, 0, -14, -40, 69),
                  c = c(1, 7, -17, 69, 76, -3))

df2 <- df1
df3 <- df1

df2[4, "b"] <- NA
df3[4, "b"] <- NA

df2[6, "c"] <- NA
df3[2, "a"] <- NA

# set.seed(1)
imp1 <- mice(df2, m=1, method="rf", seed=1)
imp_df2 <- complete(imp1, 1)

# set.seed(1)
imp2 <- mice(df2, m=1, method="rf", seed=1)
imp_df2_2 <-  complete(imp2, 1)

# set.seed(1)
imp3 <- mice(df3, m=1, method="rf", seed=1)
imp_df3 <- complete(imp3, 1)

# df2[4, "c"] <- 0
# cor <- cor(df2, use="pairwise.complete.obs")
# cor

imp4 <- mice(df4, m=1, method="rf", seed=1)
