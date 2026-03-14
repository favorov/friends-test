## ----'setup'------------------------------------------------------------------
data(friends.test.cogaps.example)

## ----'examine input'----------------------------------------------------------
featureLoadings <- friends.test.cogaps.example$loadings
head(featureLoadings)

## ----'friends are genes'------------------------------------------------------
friends_genes <- friends.test(t(featureLoadings))

## ----'friends summary'--------------------------------------------------------
lapply(friends_genes, length)

## ----'inspect pattern 4 friends'----------------------------------------------
loadings <- featureLoadings[, "Pattern_4"]
df_ecdf <- ecdf(loadings)
df <- data.frame(value = loadings,
    friend = names(loadings) %in% 
        names(friends_genes$Pattern_4),
    loading_ecdf = df_ecdf(loadings)
)
aggregate(friend ~ round(loading_ecdf, 1), data = df, 
    FUN = function(x) c(share = mean(x), n = sum(x))
)

