################################################################################
##########################   INTRODUZIONE AL DATASET   #########################
################################################################################

# Importo le librerie necessarie
library(readr)
library(car)
library(GGally)
library(dplyr)
library(corrplot)
library(MASS)
library(ggplot2)
library(tidyr)
library(cluster)
library(factoextra)
library(boot)
library(RColorBrewer)

# Carico il dataset
data <- read_csv("dataset.csv")

# Pulisco il dataset
data <- na.omit(data)
data <- data[ , !(names(data) %in% c("No", "US"))]
data$ShelveLoc <- factor(data$ShelveLoc)
data$Urban <- factor(data$Urban)
str(data)


################################################################################
######################   ANALISI ESPLORATIVA DEL DATASET   #####################
################################################################################


# Osserviamo ora la correlazione tra le caratteristiche del brano
cor_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         order = "hclust", addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, diag = FALSE)

# Osservo il ggpairs plot
ggpairs(data[,c("Age","Advertising","Income","CompPrice","Price","Sales")],
        aes(col=as.factor(data$ShelveLoc)))


################################################################################
##################   CREAZIONE E ANALISI DEL MODELLO LINEARE   #################
################################################################################


# Creo il modello lineare
model <- lm(Price ~ . - Urban - ShelveLoc, data)
summary(model)

# Controllo le ipotesi di omoschedasticità e normalità dei residui
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# Shapiro-Wilk test
shapiro.test(model$res)

# AIC
AIC(model)


################################################################################
########################   ANALISI DEI PUNTI INFLUENTI   #######################
################################################################################


# Vediamo se ci sono dei punti influenti
influencePlot( model, id.method = "identify", main = "influential Plot",
               sub = "Circle size is proportial to Cook's Distance" )

# Calcoliamo i parametri per trovare i punti influenti
cooksd <- cooks.distance(model)
studentized_residuals <- rstudent(model)
standardized_residuals <- rstandard(model)
leverages <- hatvalues(model)

# Disegno tutti i punti in funzione della loro influenza

par(mfrow = c(2, 2)) # Metto 4 grafici sulla stessa figura

# Cook's Distance plot
plot(cooksd, pch="*", cex=1.5, main="Cook's Distance", ylab="Cook's Distance")
abline(h = 4/nrow(data), col="red")

# Studentized Residuals plot
plot(studentized_residuals, pch="*", cex=1.5, main="Studentized Residuals", ylab="Studentized Residuals")
abline(h = c(-2, 2), col="red")

# Standardized Residuals plot
plot(standardized_residuals, pch="*", cex=1.5, main="Standardized Residuals", ylab="Standardized Residuals")
abline(h = c(-2, 2), col="red")

# Leverage plot
plot(leverages, pch="*", cex=1.5, main="Leverages", ylab="Leverage")
abline(h = 2*mean(leverages), col="red")

par(mfrow = c(1, 1)) # Reimposto il default di un grafico per figura

# Identifico ora tutti i punti influenti
influential_points <- which(cooksd > (4/nrow(data)) | 
                                abs(studentized_residuals) > 2 | 
                                abs(standardized_residuals) > 2 | 
                                leverages > 2*mean(leverages))

# Stampo i dettagli dei punti influenti
if (length(influential_points) > 0) {
    cat("Punti influenti identificati: ", length(influential_points),"\n")
    for (i in influential_points) {
        cat("\nPunto:", i, "\n")
        cat("Cook's Distance:", cooksd[i], "\n")
        cat("Studentized Residual:", studentized_residuals[i], "\n")
        cat("Standardized Residual:", standardized_residuals[i], "\n")
        cat("Leverage:", leverages[i], "\n")
        cat("Valori delle variabili:\n")
        print(data[i, ])
    }
} else {
    cat("Non ci sono punti influenti.\n")
}


################################################################################
#############   TEST ANOVA PER LA VARIABILE CATEGORICA ShelveLoc   #############
################################################################################


# BoxPlot della variabile Sales nelle varie categorie
my_colors = brewer.pal(length(levels(data$ShelveLoc)), "Set2")
boxplot(data$Sales ~ data$ShelveLoc, col=my_colors)
abline(h = mean(data$Sales))

# Test ANOVA per ShelveLoc
anova_model <- aov(Sales ~ ShelveLoc, data = data)
summary(anova_model)

# Controlliamo la normalità dei gruppi
Ps = c(shapiro.test(data$Sales[data$ShelveLoc == "Bad"])$p,
       shapiro.test(data$Sales[data$ShelveLoc == "Medium"])$p,
       shapiro.test(data$Sales[data$ShelveLoc == "Good"])$p)
print(Ps)

# Controlliamo l'omoschedasticità tra i gruppi
Varianze = c(var(data$Sales[data$ShelveLoc == "Bad"]),
             var(data$Sales[data$ShelveLoc == "Medium"]),
             var(data$Sales[data$ShelveLoc == "Good"]))
print(Varianze)
bartlett.test(data$Sales, data$ShelveLoc)
leveneTest(data$Sales, data$ShelveLoc)

# Aggiungo la variabile categorica al modello
model <- update(model, Price ~ . + ShelveLoc, data)
summary(model)

# AIC
AIC(model)


################################################################################
###############   TEST ANOVA PER LA VARIABILE CATEGORICA Urban   ###############
################################################################################


# BoxPlot della variabile Sales nelle varie categorie
my_colors = brewer.pal(length(levels(data$Urban)), "Set2")
boxplot(data$Price ~ data$Urban, col=my_colors)
abline(h = mean(data$Price))

# Test ANOVA per Urban
anova_model <- aov(Sales ~ ShelveLoc, data = data)
summary(anova_model)

# Non aggiungiamo la variabile Urban al modello


################################################################################
#################   COLLINEARITÀ E SELEZIONE DELLE COVARIATE   #################
################################################################################


# Calcolo del VIF del modello
vif(model)

# Effettuo la variable selection
model <- step(model, direction = "both")
summary(model)

# Controllo le ipotesi di omoschedasticità e normalità dei residui
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# Shapiro-Wilk 
shapiro.test(model$res)

# AIC e MSE
AIC(model)
mean((model$residuals)^2)


################################################################################
################################   PREDIZIONE   ################################
################################################################################


# Divido il dataset in due
train = sample(nrow(data), floor(nrow(data)/2))
data_train = data[train,]
data_test = data[-train,]

# Creo un modello lineare solo su data_train
predict_model = update(model, . ~ ., data = data_train)
summary(predict_model)

# MSE sul test set
mean( (data_test$Price - predict(predict_model, data_test))^2 )


################################################################################
#############################   CROSS-VALIDATION   #############################
################################################################################


# Calcolo l'MSE del modello su tutto il dataset
MSE_dataset = mean((data$Price-predict(model, data))^2)

# Trasformo il modello in GLM
glm.fit = glm(Price ~ Sales + CompPrice + Income + Advertising + ShelveLoc + Age, data = data)
summary(glm.fit)

# Calcolo l'errore di cross-validation
cv.error = cv.glm(data,glm.fit,K=5)$delta[1]
print(cv.error)

