
# -------------------
# C h a p i t r e 8 – E s t i m a t i o n
# -------------------

# un sondage de 580 personnes revele que 40 sont defavorable au candidat A.En utilisant la loi normale, donne la marge d'erreur de l'intervalle de confiance de p (proportion) au seuil de alpha 2%
# choix possible : a = 0.0145, b = 0.0245, c = 0.0345, d = 0.0445, e = 0.0545, f = 0.0645, g = 0.0745 et h = 0.0845

x <- c(94, 95, 96, 98, 100, 102, 104, 105, 106, 99, 101, 103)
n <- 12
sd <- sd(x)
mean <- mean(x)
alpha <- 0.05

# Fonction pour calculer l'intervalle de confiance pour la moyenne
calculate_ci_mean <- function(mean, sd, n, alpha) {
  margin_error <- qt(1 - alpha / 2, df = n - 1) * sd / sqrt(n)
  margin_error
  ci_lower <- mean - margin_error
  ci_upper <- mean + margin_error
  return(c(ci_lower, ci_upper))
}

# Fonction pour calculer l'intervalle de confiance pour la proportion
calculate_ci_proportion <- function(p_hat, n, alpha) {
  margin_error <- qnorm(1 - alpha / 2) * sqrt((p_hat * (1 - p_hat)) / n)
  ci_lower <- p_hat - margin_error
  ci_upper <- p_hat + margin_error
  return(c(ci_lower, ci_upper))
}

# Fonction pour calculer l'intervalle de confiance pour la différence entre les moyennes
calculate_ci_diff_mean <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y, alpha) {
  sp <- sqrt(((n_x - 1) * sd_x^2 + (n_y - 1) * sd_y^2) / (n_x + n_y - 2))
  margin_error <- qt(1 - alpha / 2, df = n_x + n_y - 2) * sp * sqrt(1 / n_x + 1 / n_y)
  ci_lower <- mean_x - mean_y - margin_error
  ci_upper <- mean_x - mean_y + margin_error
  return(c(ci_lower, ci_upper))
}

# Fonction pour calculer l'intervalle de confiance pour la différence entre les proportions
calculate_ci_diff_proportion <- function(p_hat_x, n_x, p_hat_y, n_y, alpha) {
  margin_error <- qnorm(1 - alpha / 2) * sqrt((p_hat_x * (1 - p_hat_x) / n_x) + (p_hat_y * (1 - p_hat_y) / n_y))
  ci_lower <- p_hat_x - p_hat_y - margin_error
  ci_upper <- p_hat_x - p_hat_y + margin_error
  return(c(ci_lower, ci_upper))
}

# Exemple d'utilisation
# Remplacez les valeurs suivantes par celles de votre problème spécifique
mean_x <- 30
sd_x <- 5
n_x <- 580

mean_y <- 28
sd_y <- 4
n_y <- 30

#------ ou ---

# Exemple de données pour l'échantillon X
echantillon_X <- c(22, 25, 28, 32, 29, 30, 27, 26, 31, 33, 28, 29, 30, 31, 33, 27, 29, 31, 30, 28, 26, 32, 34, 30, 28)

# Calcul de la moyenne, de l'écart-type et de la taille de l'échantillon pour X
mean_x <- mean(echantillon_X)
sd_x <- sd(echantillon_X)
n_x <- length(echantillon_X)

# Affichage des résultats
print(paste("Moyenne de l'échantillon X:", round(mean_x, 2)))
print(paste("Écart-type de l'échantillon X:", round(sd_x, 2)))
print(paste("Taille de l'échantillon X:", n_x))

#----------- suite 

alpha <- 0.02

# Calcul de l'intervalle de confiance pour la différence entre les moyennes
ci_diff_mean <- calculate_ci_diff_mean(mean_x, sd_x, n_x, mean_y, sd_y, n_y, alpha)
print(paste("Intervalle de confiance pour la différence entre les moyennes: [", round(ci_diff_mean[1], 2), ",", round(ci_diff_mean[2], 2), "]"))



# -------------------
# C h a p i t r e 9 – T e s t s d ’ h y p o t h è s e s
# -------------------

# Variables modifiables
alpha <- 0.05

# Exemple de données pour le test sur la moyenne
x <- c(23, 45, 67, 12, 34, 56, 78, 90, 45, 67)
mu_0 <- 50 # Valeur que vous choisissez comme hypothèse nulle

# Exemple de données pour le test sur la proportion
p_hat <- mean(x)
p_0 <- 0.5 # Valeur que vous choisissez comme hypothèse nulle
n <- length(x)

# Exemple de données pour le test sur l'égalité de deux moyennes
x1 <- c(23, 45, 67, 12, 34)
x2 <- c(56, 78, 90, 45, 67)

# Exemple de données pour le test sur l'égalité de deux proportions
p1_hat <- mean(x1)
n1 <- length(x1)
p2_hat <- mean(x2)
n2 <- length(x2)

# Fonction pour les tests d'hypothèses sur la moyenne
test_hypothese_moyenne <- function(x, mu_0, alpha, alternative = "two.sided") {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  t_stat <- (mean_x - mu_0) / (sd_x / sqrt(n))
  
  if (alternative == "two.sided") {
    p_value <- 2 * pt(-abs(t_stat), df = n - 1)
  } else if (alternative == "greater") {
    p_value <- pt(t_stat, df = n - 1)
  } else if (alternative == "less") {
    p_value <- pt(-t_stat, df = n - 1)
  }
  
  result <- list(
    test_statistic = t_stat,
    p_value = p_value,
    reject_null = p_value <= alpha
  )
  return(result)
}

#------------------
# autre exemple baser sur la fonction ci-haut
#------------------

# Données de la question
mean_x <- 60  # Résultat moyen en 2010
mean_y <- 58  # Résultat moyen en 2020
n_y <- 51     # Nombre d'étudiants en 2020
sd_y <- 4.75  # Écart-type en 2020

# Niveau de signification
alpha <- 0.05

# Calcul de la statistique de test (t_stat)
t_stat <- (mean_y - mean_x) / (sd_y / sqrt(n_y))

# Degrés de liberté
df <- n_y - 1

# Calcul de la valeur critique
critical_value <- qt(1 - alpha, df)

# Affichage des résultats
cat("Statistique du test (t_stat):", t_stat, "\n")
cat("Degrés de liberté (df):", df, "\n")
cat("Valeur critique (alpha =", alpha, "):", critical_value, "\n")

# Test d'hypothèse
if (t_stat > critical_value) {
  cat("On peut conclure avec confiance que le résultat moyen des étudiants en 2020 est supérieur au résultat moyen de 2010 (C1).\n")
} else {
  cat("On ne peut pas conclure avec confiance que le résultat moyen des étudiants en 2020 est supérieur au résultat moyen de 2010 (C2).\n")
}

#-----------------

# Fonction pour les tests d'hypothèses sur la proportion
test_hypothese_proportion <- function(p_hat, p_0, n, alpha, alternative = "two.sided") {
  z_stat <- (p_hat - p_0) / sqrt((p_0 * (1 - p_0)) / n)
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "greater") {
    p_value <- pnorm(z_stat)
  } else if (alternative == "less") {
    p_value <- pnorm(-z_stat)
  }
  
  result <- list(
    test_statistic = z_stat,
    p_value = p_value,
    reject_null = p_value <= alpha
  )
  return(result)
}

# Fonction pour les tests d'hypothèses sur l'égalité de deux moyennes
test_hypothese_diff_moyennes <- function(x1, x2, alpha, alternative = "two.sided") {
  n1 <- length(x1)
  n2 <- length(x2)
  mean_diff <- mean(x1) - mean(x2)
  sd_pooled <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
  t_stat <- mean_diff / (sd_pooled * sqrt(1/n1 + 1/n2))
  
  if (alternative == "two.sided") {
    p_value <- 2 * pt(-abs(t_stat), df = n1 + n2 - 2)
  } else if (alternative == "greater") {
    p_value <- pt(t_stat, df = n1 + n2 - 2)
  } else if (alternative == "less") {
    p_value <- pt(-t_stat, df = n1 + n2 - 2)
  }
  
  result <- list(
    test_statistic = t_stat,
    p_value = p_value,
    reject_null = p_value <= alpha
  )
  return(result)
}

# Fonction pour les tests d'hypothèses sur l'égalité de deux proportions
test_hypothese_diff_proportions <- function(p1_hat, n1, p2_hat, n2, alpha, alternative = "two.sided") {
  p_diff <- p1_hat - p2_hat
  p_pooled <- (p1_hat * n1 + p2_hat * n2) / (n1 + n2)
  z_stat <- p_diff / sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "greater") {
    p_value <- pnorm(z_stat)
  } else if (alternative == "less") {
    p_value <- pnorm(-z_stat)
  }
  
  result <- list(
    test_statistic = z_stat,
    p_value = p_value,
    reject_null = p_value <= alpha
  )
  return(result)
}

# Choisissez la fonction à appeler
# résultat_test <- test_hypothese_moyenne(x, mu_0, alpha, alternative = "two.sided")
# résultat_test <- test_hypothese_proportion(p_hat, p_0, n, alpha, alternative = "two.sided")
# résultat_test <- test_hypothese_diff_moyennes(x1, x2, alpha, alternative = "two.sided")
# résultat_test <- test_hypothese_diff_proportions(p1_hat, n1, p2_hat, n2, alpha, alternative = "two.sided")

# Affichage des résultats
cat("Résultat du test :", "\n")
cat("Statistique du test :", résultat_test$test_statistic, "\n")
cat("P-valeur :", résultat_test$p_value, "\n")
cat("Rejeter l'hypothèse nulle :", résultat_test$reject_null, "\n") # FALSE, cela signifie que l'on ne rejette pas l'hypothèse nulle.
    

# -------------------
# C h a p i t r e 1 0 – T e c h n i q u e s d e s o n d a g e s
# -------------------

# Déclaration des variables
y <- c(23, 45, 67, 12, 34, 56, 78, 90, 45, 67)
N <- 1000  # Taille totale de la population
s <- sd(y) # Écart type de l'échantillon 
n <- length(y) # taille echantillon
f <- 0.8  # Taux de réponse de 80% du sondage y

# Fonction pour calculer l'estimateur de la moyenne "moyenne"
estimateur_moyenne <- function(y) {
  return(mean(y))
}

# Fonction pour calculer l'estimateur du total "total"
estimateur_total <- function(y, N) {
  return(N * mean(y))
}

# Fonction pour calculer l'estimateur de l'écart type de l'estimateur de la moyenne "ecart_type_moyenne"
estimateur_ecart_type_moyenne <- function(s, n, f) {
  return(sqrt((1 - f) * (s^2 / n)))
}

# Fonction pour calculer l'estimateur de l'écart type de l'estimateur du total  "ecart_type_total"
estimateur_ecart_type_total <- function(estimateur_ecart_type_moyenne, N) {
  return(N * estimateur_ecart_type_moyenne)
}

# Choix de la fonction à utiliser
choix <- "total"  # Remplacez par "moyenne", "total", "ecart_type_moyenne", ou "ecart_type_total"

if (choix == "moyenne") {
  resultat <- estimateur_moyenne(y)
  cat("Estimateur de la moyenne (y̅):", resultat, "\n")
} else if (choix == "total") {
  resultat <- estimateur_total(y, N)
  cat("Estimateur du total (𝜏̂):", resultat, "\n")
} else if (choix == "ecart_type_moyenne") {
  resultat <- estimateur_ecart_type_moyenne(s, n, f)
  cat("Estimateur de l'écart type de l'estimateur de la moyenne (σ̂_y̅):", resultat, "\n")
} else if (choix == "ecart_type_total") {
  estimateur_ecart_type_moyenne_y <- estimateur_ecart_type_moyenne(s, n, f)
  resultat <- estimateur_ecart_type_total(estimateur_ecart_type_moyenne_y, N)
  cat("Estimateur de l'écart type de l'estimateur du total (σ̂_𝜏̂):", resultat, "\n")
} else {
  cat("Choix invalide. Veuillez choisir parmi 'moyenne', 'total', 'ecart_type_moyenne', ou 'ecart_type_total'.\n")
}

# ----------------------------------------------------------------
# une estimation stratifiée
# Paramètres
population_mean <- 500  # Moyenne de la population
total_population <- 1000  # Taille totale de la population
strata_sizes <- c(200, 300, 500)  # Tailles des strates
strata_means <- c(480, 520, 490)  # Moyennes des strates
strata_variances <- c(100, 120, 80)  # Variances des strates

# Calcul de l'estimateur de la moyenne
weight <- strata_sizes / total_population
mean_estimate <- sum(weight * strata_means)

# Calcul de l'estimateur de l'écart-type de l'estimateur de la moyenne
variance_estimate <- sum(weight^2 * strata_variances)
se_mean_estimate <- sqrt(variance_estimate)

# Calcul de l'estimateur de la variance de 𝑦̅_ℎ pour chaque strate
variance_ybar_h <- (1 - (strata_sizes / total_population)) * (strata_variances / strata_sizes)

# Affichage des résultats
cat("Estimateur de la moyenne (𝜇̂):", mean_estimate, "\n")
cat("Estimateur de l'écart-type de l'estimateur de la moyenne (𝜎̂_𝜇̂):", se_mean_estimate, "\n")

# Affichage de l'estimateur de la variance de 𝑦̅_ℎ pour chaque strate
for (h in 1:length(strata_sizes)) {
  cat("Estimateur de la variance de 𝑦̅_ℎ pour la strate", h, ":", variance_ybar_h[h], "\n")
}

#--------------------------------------------------------------------------
# Si vous avez uniquement les données brutes sans les informations sur les strates alors estimation simple 

# Exemple de données brutes (remplacez cela par vos données réelles)
donnees_brutes <- c(45, 50, 55, 48, 52, 60, 58, 53, 51, 49)

# Taille totale de la population
taille_totale_population <- 1000

# Calcul de la moyenne
moyenne <- mean(donnees_brutes)

# Calcul de l'écart type
ecart_type <- sd(donnees_brutes)

# Taille de l'échantillon
taille_echantillon <- length(donnees_brutes)

# Calcul de l'estimateur de l'écart-type de l'estimateur de la moyenne (en supposant une population infinie)
se_mean_estimate <- ecart_type / sqrt(taille_echantillon)

# Affichage des résultats
cat("Moyenne:", moyenne, "\n")
cat("Écart type:", ecart_type, "\n")
cat("Taille de l'échantillon:", taille_echantillon, "\n")
cat("Estimateur de l'écart-type de l'estimateur de la moyenne (𝜎̂_𝜇̂):", se_mean_estimate, "\n")


# -------------------
# C h a p i t r e 3 et Section 1.4 – Tests du Khi-deux
# -------------------

# -------------------
# sans alpha
# -------------------

# ajustement 
# Définir les données observées et théoriques pour le test d'ajustement
observed_ajustement <- c(25, 35, 40)
expected_ajustement <- c(20, 30, 40)  # Remplacez par vos propres valeurs théoriques

# Fonction pour le test d'ajustement du khi-deux
test_ajustement <- function(observed, expected) {
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1  # Degrés de liberté
  p_value <- 1 - pchisq(chi_squared, df)  # Calcul de la valeur p
  return(list(chi_squared = chi_squared, df = df, p_value = p_value))
}

# Exemple d'utilisation du test d'ajustement
result_ajustement <- test_ajustement(observed_ajustement, expected_ajustement)
cat("Statistique du test d'ajustement:", result_ajustement$chi_squared, "\n")
cat("Degrés de liberté:", result_ajustement$df, "\n")
cat("Valeur p:", result_ajustement$p_value, "\n")

# indépendance
#------------------------------------------------------------------------
# Définir les données observées et théoriques pour le test d'indépendance
observed_independance <- matrix(c(20, 30, 10, 40, 50, 20), nrow = 2)
expected_independance <- matrix(c(15, 25, 15, 45, 55, 25), nrow = 2)  # Remplacez par vos propres valeurs théoriques

# Fonction pour le test d'indépendance du khi-deux
test_independance <- function(observed, expected) {
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- prod(dim(observed) - 1)  # Degrés de liberté
  p_value <- 1 - pchisq(chi_squared, df)  # Calcul de la valeur p
  return(list(chi_squared = chi_squared, df = df, p_value = p_value))
}

# Exemple d'utilisation du test d'indépendance
result_independance <- test_independance(observed_independance, expected_independance)
cat("Statistique du test d'indépendance:", result_independance$chi_squared, "\n")
cat("Degrés de liberté:", result_independance$df, "\n")
cat("Valeur p:", result_independance$p_value, "\n")

#-----------------------
# avec alpha
#-----------------------

# Définir le niveau de signification
alpha <- 0.05

# Variables pour le test d'ajustement
observed_ajustement <- c(25, 35, 40)
expected_ajustement <- c(20, 30, 40)  # Remplacez par vos propres valeurs théoriques

# Variables pour le test d'indépendance
observed_independance <- matrix(c(20, 30, 10, 40, 50, 20), nrow = 2)
expected_independance <- matrix(c(15, 25, 15, 45, 55, 25), nrow = 2)  # Remplacez par vos propres valeurs théoriques

# Fonction pour le test d'ajustement du khi-deux
test_ajustement <- function(observed, expected, alpha) {
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- length(observed) - 1  # Degrés de liberté
  p_value <- 1 - pchisq(chi_squared, df)  # Calcul de la valeur p
  
  # Points critiques pour le test du Khi-deux
  critical_value <- qchisq(1 - alpha, df)
  
  # Afficher les résultats
  cat("Statistique du test d'ajustement:", chi_squared, "\n")
  cat("Degrés de liberté:", df, "\n")
  cat("Valeur p:", p_value, "\n")
  cat("Points critiques (alpha =", alpha, "):", critical_value, "\n")
  
  # Tester l'hypothèse nulle
  if (chi_squared > critical_value) {
    cat("Hypothèse nulle rejetée. La distribution observée ne correspond pas à la distribution théorique.\n")
  } else {
    cat("Hypothèse nulle non rejetée. La distribution observée correspond à la distribution théorique.\n")
  }
}

# Exemple d'utilisation du test d'ajustement
test_ajustement(observed_ajustement, expected_ajustement, alpha)

# Fonction pour le test d'indépendance du khi-deux
test_independance <- function(observed, expected, alpha) {
  chi_squared <- sum((observed - expected)^2 / expected)
  df <- prod(dim(observed) - 1)  # Degrés de liberté
  p_value <- 1 - pchisq(chi_squared, df)  # Calcul de la valeur p
  
  # Points critiques pour le test du Khi-deux
  critical_value <- qchisq(1 - alpha, df)
  
  # Afficher les résultats
  cat("Statistique du test d'indépendance:", chi_squared, "\n")
  cat("Degrés de liberté:", df, "\n")
  cat("Valeur p:", p_value, "\n")
  cat("Points critiques (alpha =", alpha, "):", critical_value, "\n")
  
  # Tester l'hypothèse nulle
  if (chi_squared > critical_value) {
    cat("Hypothèse nulle rejetée. Les variables ne sont pas indépendantes.\n")
  } else {
    cat("Hypothèse nulle non rejetée. Les variables sont indépendantes.\n")
  }
}

# Exemple d'utilisation du test d'indépendance
test_independance(observed_independance, expected_independance, alpha)


# -------------------
# C h a p i t r e 4 – Droite des moindres carrés et corrélation
# -------------------

# Données
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 4, 3, 5)

# Calcul des moyennes
x_bar <- mean(x)
y_bar <- mean(y)

# Calcul des variances
var_y <- sum((y - y_bar)^2) / (length(y) - 1)
var_x <- sum((x - x_bar)^2) / (length(x) - 1)

# Calcul de la covariance
cov_xy <- sum((x - x_bar) * (y - y_bar)) / (length(x) - 1)

# Calcul du coefficient de corrélation
correlation <- cov_xy / sqrt(var_x * var_y)

# Calcul du coefficient de la droite de régression (b)
b <- cov_xy / var_x

# Calcul du coefficient a dans l'équation y = a + bx
a <- y_bar - b * x_bar

# Calcul de l'écart-type de b
std_err_b <- sqrt(var_y) / sqrt(var_x) * sqrt((1 - correlation^2) / (length(x) - 2))

# Calcul de la statistique T pour tester l'indépendance
T_statistic <- b / std_err_b

# Niveau de signification
alpha <- 0.05

# Valeur critique pour un test bilatéral
critical_value <- qt(1 - alpha/2, df = length(x) - 2)

# Affichage des résultats
cat("Coefficient de corrélation (r):", correlation, "\n")
cat("Coefficient de la droite de régression (b):", b, "\n")
cat("Coefficient a dans l'équation y = a + bx:", a, "\n")
cat("Écart-type de b:", std_err_b, "\n")
cat("Statistique T pour tester l'indépendance:", T_statistic, "\n")

# Test d'indépendance
if (abs(T_statistic) > critical_value) {
  cat("On rejette H0: r = 0 au niveau de signification", alpha, "\n")
} else {
  cat("On ne rejette pas H0: r = 0 au niveau de signification", alpha, "\n")
}
