## TTS models

# NUREG/CR3391 vol.2 by Guthrie (1983)
CR3391CF <- function(form, Cu, Ni, fluence) {
  ifelse(form %in% c("P", "F", "B"), -38.39 + 555.6 * Cu + 480.1 * Cu * tanh(0.353 * Ni / Cu),
  ifelse(form %in% c("W"), 624 * Cu - 333.1 * sqrt(Cu * Ni) + 251.2 * Ni, 
         NA))
}

CR3391FF <- function(fluence) {
  fl <- fluence / 1e19
  fl ^ (0.2661 - 0.0449 * log(fl))   
}

CR3391 <- function(form, Cu, Ni, fluence) {
  CR3391CF(form, Cu, Ni) * CR3391FF(fluence)
}


# EPRI NP-3319 TTS model by Odette (1984)
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

NP3319CF <- function(form, Cu, Ni) {
  ifelse(form %in% c("P", "F", "B"), Cu * 216 * (1 + 0.33 * (erf(0.77 * Ni / Cu - 1) + 1)),
  ifelse(form %in% c("W"), Cu * 200 * (1 + 1.38 * (erf(0.30 * Ni / Cu - 1) + 1)),
         NA))
}

NP3319FF <- function(form, fluence) {
  fl <- fluence / 1e19
  ifelse(form %in% c("P", "F", "B"), fl ^ 0.28,
  ifelse(form %in% c("W"), (1 - exp(-fl / 0.11)) ^ 1.36 * fl ^ 0.18,
         NA))
}

NP3319 <- function(form, Cu, Ni, fluence) {
  NP3319CF(form, Cu, Ni) * NP3319FF(form, fluence)
}


# Regulatory Guide 1.99 Revision 2, RG1.99/2 model (1988)
CF.base = matrix(c(20, 20, 20, 20, 22, 25, 28, 31, 34, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 78, 82, 86, 91, 95, 100, 104, 109, 114, 119, 124, 129, 134, 139, 144, 149, 153, 158, 162, 166, 171, 175, 20, 20, 20, 20, 26, 31, 37, 43, 48, 53, 58, 62, 67, 71, 75, 80, 84, 88, 92, 97, 102, 107, 112, 117, 121, 126, 130, 134, 138, 142, 146, 151, 155, 160, 164, 168, 173, 177, 182, 185, 189, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 65, 72, 79, 85, 91, 99, 104, 110, 115, 120, 125, 129, 134, 138, 143, 148, 151, 155, 160, 164, 167, 172, 175, 180, 184, 187, 191, 196, 200, 203, 207, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 65, 76, 83, 91, 100, 110, 118, 127, 134, 142, 149, 155, 161, 167, 172, 176, 180, 184, 187, 191, 194, 198, 202, 205, 209, 212, 216, 220, 223, 227, 231, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 67, 77, 86, 96, 105, 115, 123, 132, 141, 150, 159, 167, 176, 184, 191, 199, 205, 211, 216, 221, 225, 228, 231, 234, 238, 241, 245, 248, 250, 254, 257, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 67, 77, 86, 96, 106, 117, 125, 135, 144, 154, 164, 172, 181, 190, 199, 208, 216, 225, 233, 241, 249, 255, 260, 264, 268, 272, 275, 278, 281, 285, 288, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 67, 77, 86, 96, 106, 117, 125, 135, 144, 154, 165, 176, 184, 194, 204, 214, 221, 230, 239, 248, 257, 266, 274, 282, 290, 298, 303, 308, 313, 317, 320), nrow=41)
CF.base <- rbind(CF.base, CF.base[41,])  # for 2D interpolation
CF.base <- cbind(CF.base, CF.base[,7])  # for 2D interpolation
CF.weld = matrix(c(20, 20, 21, 22, 24, 26, 29, 32, 36, 40, 44, 49, 52, 58, 61, 66, 70, 75, 79, 83, 88, 92, 97, 101, 105, 110, 113, 119, 122, 128, 131, 136, 140, 144, 149, 153, 158, 162, 166, 171, 175, 20, 20, 26, 35, 43, 49, 52, 55, 58, 61, 65, 68, 72, 76, 79, 84, 88, 92, 95, 100, 104, 108, 112, 117, 121, 126, 130, 134, 138, 142, 146, 151, 155, 160, 164, 168, 172, 177, 182, 185, 189, 20, 20, 27, 41, 54, 67, 77, 85, 90, 94, 97, 101, 103, 106, 109, 112, 115, 119, 122, 126, 129, 133, 137, 140, 144, 148, 151, 155, 160, 164, 167, 172, 175, 180, 184, 187, 191, 196, 200, 203, 207, 20, 20, 27, 41, 54, 68, 82, 95, 106, 115, 122, 130, 135, 139, 142, 146, 149, 151, 154, 157, 160, 164, 167, 169, 173, 176, 180, 184, 187, 191, 194, 198, 202, 205, 209, 212, 216, 220, 223, 227, 231, 20, 20, 27, 41, 54, 68, 82, 95, 108, 122, 133, 144, 153, 162, 168, 175, 178, 184, 187, 191, 194, 197, 200, 203, 206, 209, 212, 216, 218, 222, 225, 228, 231, 234, 238, 241, 245, 248, 250, 254, 257, 20, 20, 27, 41, 54, 68, 82, 94, 108, 122, 135, 148, 161, 172, 182, 191, 199, 207, 214, 220, 223, 229, 232, 236, 239, 243, 246, 249, 251, 254, 257, 260, 263, 266, 269, 272, 275, 278, 281, 285, 288, 20, 20, 27, 41, 54, 68, 82, 95, 108, 122, 135, 148, 161, 176, 188, 200, 211, 221, 230, 238, 245, 252, 257, 263, 268, 272, 276, 280, 284, 287, 290, 293, 296, 299, 302, 305, 308, 311, 314, 317, 320), nrow=41)
CF.weld <- rbind(CF.weld, CF.weld[41,])  # for 2D interpolation
CF.weld <- cbind(CF.weld, CF.weld[,7])  # for 2D interpolation

RG199R2CF <- function(form, Cu, Ni) {
  # limit the composition
  Cu[Cu<=0] <- 0
  Cu[Cu>=0.40] <- 0.40
  Ni[Ni<=0] <- 0
  Ni[Ni>=1.20] <- 1.20
  dx <- 0.01  # Cu step=0.01
  dy <- 0.20  # Ni step=0.20
  i <- floor(Cu * 100) + 1  # find row
  j <- floor(Ni * 5) + 1  # find column
  x1 <- (i - 1) * dx
  x2 <- x1 + dx
  y1 <- (j - 1) * dy
  y2 <- y1 + dy
  # calc base value under given composition
  b11 <- CF.base[i,j]
  b12 <- CF.base[i,j+1]
  b21 <- CF.base[i+1,j]
  b22 <- CF.base[i+1,j+1]
  b <- b11 / dx / dy * (x2 - Cu) * (y2 - Ni) +  
       b21 / dx / dy * (Cu - x1) * (y2 - Ni) +  
       b12 / dx / dy * (x2 - Cu) * (Ni - y1) +  
       b22 / dx / dy * (Cu - x1) * (Ni - y1)
  # calc weld value under given composition
  w11 <- CF.weld[i,j]
  w12 <- CF.weld[i,j+1]
  w21 <- CF.weld[i+1,j]
  w22 <- CF.weld[i+1,j+1]
  w <- w11 / dx / dy * (x2 - Cu) * (y2 - Ni) +  
       w21 / dx / dy * (Cu - x1) * (y2 - Ni) +  
       w12 / dx / dy * (x2 - Cu) * (Ni - y1) +  
       w22 / dx / dy * (Cu - x1) * (Ni - y1)
  ifelse(form %in% c("P", "F", "B"), b,
  ifelse(form %in% c("W"), w,
         NA))
}

RG199R2FF <- function(fluence) {
  fl <- fluence / 1e19
  fl ^ (0.28 - 0.10 * log10(fl))
}

RG199R2 <- function(form, Cu, Ni, fluence) {
  RG199R2CF(form, Cu, Ni) * RG199R2FF(fluence)
}


# NUREG/CR6551 TTS model
CR6551SMD <- function(form, P, Tc, fluence) {
  fl = fluence / 1e19
  a <- ifelse(form %in% c("P", "SRM"), 1.24e-7,
       ifelse(form %in% c("F"), 8.98e-8,
       ifelse(form %in% c("W"), 1.10e-7,
              NA)))
  fp <- (fl) ^ (0.4449 + 0.0597 * log10(fl))
  a * exp(1.906e4 / (Tc + 460)) * (1 + 57.7 * P) * fp
}

CR6551CRP <- function(form, Cu, Ni, fluence, ti) {
  b <- ifelse(form %in% c("P", "SRM"), 172,
       ifelse(form %in% c("F"), 135,
       ifelse(form %in% c("W"), 209,
              NA)))
  g = 0.5 + 0.5 * tanh((log10(fluence + 5.48e12 * ti) - 18.290) / 0.6)
  h <-  ifelse(Cu <= 0.072, 0.0,
        ifelse(Cu >= 0.300, 0.367, 
                            (Cu - 0.072) ^ 0.678))
  b * (1 + 2.56 * Ni ^ 1.358) * h * g
}

CR6551 <- function(form, Cu, Ni, P, Tc, fluence, ti) {
  CR6551SMD(form, P, Tc, fluence) + CR6551CRP(form, Cu, Ni, fluence, ti)
}


# test models
CR3391("W", Cu=0.1, Ni=0.1, fluence=1e19) # 54.21
NP3319("P", Cu=0.17, Ni=0.64, fluence=0.073e19) # 29.2, book=29.4
NP3319("P", Cu=0.12, Ni=0.53, fluence=0.200e19) # 27.4, book=29.2
NP3319("W", Cu=0.19, Ni=0.59, fluence=0.073e19) # 20.2, book=20.5
NP3319("W", Cu=0.28, Ni=0.55, fluence=0.200e19) # 58.5, book=59.2
RG199R2("P", Cu=0.1, Ni=0.1, fluence=1e19) # 49.5
CR6551(form="P", Cu=0.2, Ni=0.5, P=0.01, Tc=550, fluence=2.5e18, ti=10000) # 68.3
CR6551(form="F", Cu=0.04, Ni=0.7, P=0.01, Tc=530, fluence=1.5e19, ti=75000) # 39.1
CR6551(form="W", Cu=0.3, Ni=0.6, P=0.015, Tc=550, fluence=1.5e19, ti=75000) # 205.2


