# -------------------------------------------------------------------------- ###
# Soru 1a https://github.com/metinh/butunleme.odev.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ----   
library(dplyr)

titanic %>%
  group_by(sex) %>%
  summarise(mean_fare = mean(fare))

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ----   
library(ggplot2)

ggplot(titanic, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Histogram of Age")

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ----

library(ggplot2)

# Histogramı çizdirmek için veriyi kullanalım
ggplot(titanic, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, color = "pink", alpha = 0.7, aes(y = ..density..)) +
  geom_density(alpha = 0.3) +
  labs(x = "Age", y = "Density", title = "Histogram of Age") +
  
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a--- 
   
[1] 10 13

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b 
 library(dplyr)

 dat3 <- inner_join(dat1, dat2)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----  graphic <- ggplot(dat, aes(x = fare, y = age)) +
geom_point()
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ----   mylist <- list(1:3, c(3:5, NA))
myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ----   zarAtisi <- function() 
zar1 <- sample(1:6, 1, replace = TRUE)  # Zar 1'in atış sonucunu rastgele seçer
zar2 <- sample(1:6, 1, replace = TRUE)  # Zar 2'nin atış sonucunu rastgele seçer
cat("Zar 1: ", zar1, "\n")
cat("Zar 2: ", zar2, "\n")


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----    zarAtisi <- function() 
zar1 <- sample(1:6, 1, replace = TRUE)  # Zar 1'in atış sonucunu rastgele seçer
zar2 <- sample(1:6, 1, replace = TRUE)  # Zar 2'nin atış sonucunu rastgele seçer
cat("Zar 1: ", zar1, "\n")
cat("Zar 2: ", zar2, "\n")


# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----   
titanic <- read.csv("titanic.csv") ornek olarak yaziyorum


age_survived <- titanic$age[titanic$survived == 1]
age_not_survived <- titanic$age[titanic$survived == 0]


mean_age_survived <- mean(age_survived, na.rm = TRUE)
mean_age_not_survived <- mean(age_not_survived, na.rm = TRUE)


t_test <- t.test(age_survived, age_not_survived, var.equal = TRUE)

print(paste("Kurtulan yolcuların yaş ortalaması:", mean_age_survived))
print(paste("Kurtulamayan yolcuların yaş ortalaması:", mean_age_not_survived))
print("Bağımsız İki Örneklem t-testi sonuçları:")
print(t_test)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----  
library(tidyr)

dat2 <- dat %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "gdp") %>%
  mutate(year = as.integer(year))

print(dat2)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 5a ----   library(ggplot2)

diamonds <- tibble(
  price = c(326, 326, 327, 334, 335),
  cut = c("Ideal", "Premium", "Good", "Premium", "Good"),
  depth = c(61.5, 59.8, 56.9, 62.4, 63.3),
)

ggplot(data = diamonds, aes(x = cut, y = price, fill = color)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cut", y = "Price", fill = "Color") +
  theme_minimal()

# -------------------------------------------------------------------------- ###