###############################################################
#' Script for replicating experiments
#' results are located on the REPORTS and GRAPHS folders
###############################################################

set.seed(33)


# Ridge season fits============================================================
names <- c("TEAM","LAMBDA","R2","SST","SSE","RMSE")
ridge_res <- data.frame()
for (k in names) ridge_res[[k]] <- as.character()
for (t in teams) {
  ridge_res <- rbind(ridge_res,fit_ridge(t))
  print(paste0("Progress: ",nrow(res_ridge)," of ", "30"))
}
opath <- paste0(getwd(),"/tests/ridge/")
write.csv(ridge_res,paste0(opath,"ridge_fits.csv"))

# Lasso season fits============================================================
names <- c("TEAM","LAMBDA","R2","SST","SSE","RMSE")
lasso_res <- data.frame()
for (k in names) lasso_res[[k]] <- as.character()
for (t in teams) {
  lasso_res <- rbind(lasso_res,fit_lasso(t))
  print(paste0("Progress: ",nrow(lasso_res)," of ", "30"))
}
opath <- paste0(getwd(),"/tests/lasso/")
write.csv(lasso_res,paste0(opath,"lasso_fits.csv"))

# Elastic Net fits whole season ==============================================
names <- c("TEAM","R2","SST","SSE","ALPHA","LAMBDA","RMSE",
           "R_SQUARED","MAE","RMSESD","RsquaredSD","MAESD")
elnet_res <- data.frame()
for (k in names) elnet_res[[k]] <- as.character()
for (t in teams) {
  elnet_res <- rbind(elnet_res,fit_elastic(t))
  print(paste0("Progress: ",nrow(elnet_res)," of ", "30"))
}
opath <- paste0(getwd(),"/tests/elastic_net/")
write.csv(elnet_res,paste0(opath,"elastic_fits.csv"))

# Elastic Net for playoff prediction ===========================================
names <- c("TEAM","ALPHA","LAMBDA","R2","SST","SSE","RMSE",
           "R2_os","correct","juegos_pred","accuracy")
elastic_playoffs <- data.frame()
for (k in names) elastic_playoffs[[k]] <- as.character()

# Adjust and predict for playoffs
playoff_teams <- c("CLE","GSW","HOU","ATL")
for (t in playoff_teams) {
  elastic_playoffs <- rbind(elastic_playoff_pred(t))
  print(paste0("Progress: ",
               nrow(elastic_playoffs),
               " of ",
               length(playoff_teams)))
}
opath <- paste0(getwd(),"/tests/elastic_net/")
write.csv(elastic_playoffs,paste0(opath,"elastic_playoff_pred.csv"))

# Elastic net in-season predictions ============================================
names <- c("TEAM","ALPHA","LAMBDA","R2","SST","SSE","RMSE",
           "R2_os","correct","juegos_pred","accuracy")
season_preds <- data.frame()
for (k in names) season_preds[[k]] <- as.character()
for (t in teams) {
  season_preds <- rbind(season_preds,elastic_season_pred(t))
  print(paste0("Progress: ",nrow(season_preds)," of ", "30"))
}
opath <- paste0(getwd(),"/tests/elastic_net/")
write.csv(season_preds,paste0(opath,"elastic_season_pred.csv"))



# Ridge for playoff predictions ================================================
# define team:
t <- "CLE"
ridge_prediction <- ridge_playoffs(t)



# Player exclusion experiments =================================================
# Ersan Ilyasova
ily <- fit_elastic("MIL")
no_ily <- fit_elastic_exc("MIL","Ilyasova")

# Tiago Splitter
splitter <- fit_elastic("SAS")
no_splitter <- fit_elastic_exc("SAS","Splitter")

