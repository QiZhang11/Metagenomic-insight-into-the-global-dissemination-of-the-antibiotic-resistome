library(caret)
library(car)
df <- read.csv("COREARG.csv",header=TRUE,sep=",")

# Define Formula
form1<- as.formula(Core~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16)

# Define predictors
impVars <- df[,c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16")]
# Variable Inflation Factor
all_vifs_test <- try(vif(lm(form1, data=df)), silent=TRUE)
if (class(all_vifs_test) == "try-error"){
  lm_alias <- alias(lm(form1, data=df))
  broken <- data.frame(lm_alias$Complete)
  broken_var <- row.names(broken)
  nam_var <- names(impVars) %in% broken_var
  impVars <- impVars[!nam_var]
  form_all_new <- as.formula(paste("Core ~ ", 
                                   paste(names(impVars), collapse=" + "),
                                   sep=""))
  all_vifs <- vif(lm(form_all_new, data=df))
} else {
  all_vifs <- all_vifs_test
}

if(any(all_vifs>5)){
  all_vifs <- as.data.frame(all_vifs)  
  while((nrow(all_vifs) > 2)& (max(all_vifs[, 1]) > 5)  &  
        (class(all_vifs) != "try-error")) {
    remove_var <- rownames(all_vifs)[which(all_vifs[, 1] == max(all_vifs[, 1]))]  
    impVars <- impVars[!names(impVars) %in% remove_var]  
    fullForm <- paste ("Core ~ ", paste (names(impVars), collapse=" + "), sep="")  
    fullMod <- lm(as.formula(fullForm), data=df)  
    all_vifs <- try(as.data.frame(vif(fullMod)), silent=TRUE) 
  }
  vif_filtered_variables <- names(fullMod$model)[!names(fullMod$model) %in% 
                                                   "MBC"]
} else {
  all_vifs <- as.data.frame(all_vifs)
  vif_filtered_variables <- rownames(all_vifs)[!names(all_vifs) %in% "MBC"]
}
# Filtered Formula
form_vif <- as.formula(paste("Core~",paste(vif_filtered_variables, collapse='+')))
write.csv(all_vifs,file="all_vifs1.csv")
  
