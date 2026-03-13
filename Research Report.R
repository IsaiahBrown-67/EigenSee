#Danny DeVito Photo
# Step 1: Load the grayscale photograph
setwd("/Users/ibsqu/ibrown/LinAlg/data") # Replace with your photo path
library(imager)
photo <- load.image("danny.jpg") # Insert photo name
dim(photo) #Last dimension should be 3
#[1] 300 409 1 3
photo = grayscale(photo)
dim(photo) # Last dimension should be 1 now
#[1] 300 409 1 1
# Step 2: Perform SVD on the image
svd_photo <- svd(photo)
U<- svd_photo$u
D<- diag(svd_photo$d) # Convert singular values into a diagonal matrix
D[1:5,1:5]
#[1,] 203.7385  0.00000  0.00000  0.00000  0.00000
#[2,]   0.0000 36.33936  0.00000  0.00000  0.00000
#[3,]   0.0000  0.00000 22.35128  0.00000  0.00000
#[4,]   0.0000  0.00000  0.00000 17.15884  0.00000
#[5,]   0.0000  0.00000  0.00000  0.00000 15.82589
plot(diag(D[1:5,1:5]), type = 'b', pch = 19, ylab = "Singular values",
     main = "Top Singular Values 1-5")
K=length(svd_photo$d) # find total K values
K#[1] 300
V<- svd_photo$v
# Step 3a: Adjust the number of singular values to retain
k1 <- 75 #divided 300 by 4
U_k1 <- U[, 1:k1]
D_k1 <- D[1:k1, 1:k1]
V_k1 <- V[, 1:k1]
k2 <- 38 #divided 75 by 1/2
U_k2 <- U[, 1:k2]
D_k2 <- D[1:k2, 1:k2]
V_k2 <- V[, 1:k2]
k3 <- 19 #divided 38 by 1/2
U_k3 <- U[, 1:k3]
D_k3 <- D[1:k3, 1:k3]
V_k3 <- V[, 1:k3]
k4 <- 10 #divided 19 by 1/2
U_k4 <- U[, 1:k4]
D_k4 <- D[1:k4, 1:k4]
V_k4 <- V[, 1:k4]
16
par(mar = c(2,2, 2.5, 2.5), mfrow = c(2, 3))
plot(photo,
     xlim = c(0, dim(photo)[1]), ylim = c(0, dim(photo)[2]),
     axes = F,
     main = "Grayscale Photo")
# noised photos
photo_noised1 <- as.cimg(U_k1 %*% D_k1 %*% t(V_k1))
plot(photo_noised1, axes = F, main = "75 Modes")
photo_noised2 <- as.cimg(U_k2 %*% D_k2 %*% t(V_k2))
plot(photo_noised2, axes = F, main = "38 Modes")
photo_noised3 <- as.cimg(U_k3 %*% D_k3 %*% t(V_k3))
plot(photo_noised3, axes = F, main = "19 Modes")
photo_noised4 <- as.cimg(U_k4 %*% D_k4 %*% t(V_k4))
plot(photo_noised4, axes = F, main = "10 Modes")
# Step 3b: Adjust the number of singular values to retain (Starting at a different K values)
## Recall total K = 300
k1 <- 2:300
U_k1 <- U[, k1]
D_k1 <- D[k1, k1]
V_k1 <- V[, k1]
k2 = 3:300 # start of k1 + 1
U_k2 <- U[, k2]
D_k2 <- D[k2, k2]
V_k2 <- V[, k2]
#Lowest k to start is 2
k3 = 2:100 # K/3
U_k3 <- U[, k3]
D_k3 <- D[k3, k3]
V_k3 <- V[, k3]
k4 = 2:25 # K/4
U_k4 <- U[, k4]
D_k4 <- D[k4, k4]
V_k4 <- V[, k4]
k5 = 2:12 # K/2
U_k5 <- U[, k5]
D_k5 <- D[k5, k5]
V_k5 <- V[, k5]
# Step 4: Reconstruct the photograph with different k values
par(mar = c(2,2, 2.5, 2.5), mfrow = c(2, 3))
photo_noised1 <- as.cimg(U_k1 %*% D_k1 %*% t(V_k1))
photo_noised2 = as.cimg(U_k2 %*% D_k2 %*% t(V_k2))
photo_noised3 = as.cimg(U_k3 %*% D_k3 %*% t(V_k3))
photo_noised4 = as.cimg(U_k4 %*% D_k4 %*% t(V_k4))
17
photo_noised5 = as.cimg(U_k5 %*% D_k5 %*% t(V_k5))
plot(photo,
     axes = FALSE, main = "Original Grayscale Photo")
# Plotting noised photos
plot(photo_noised1, axes = FALSE, main = "2:300 Modes")
plot(photo_noised2, axes = FALSE, main = "3:300 Modes")
plot(photo_noised3, axes = FALSE, main = "2:100 Modes")
plot(photo_noised4, axes = FALSE, main = "2:25 Modes")
plot(photo_noised5, axes = FALSE, main = "2:12 Modes")

#Jack Black Photo
# Step 1: Load the grayscale photograph
setwd("/Users/ibsqu/ibrown/LinAlg/data")
library(imager)
photo <- load.image("jack_black.jpg") # Replace with your photo path
photo = grayscale(photo)
dim(photo)
# [1] 2760 2760 1 1
# Step 2: Perform SVD on the image
svd_photo <- svd(photo)
U=svd_photo$u
D=diag(svd_photo$d) # Convert singular values into a diagonal matrix
V=svd_photo$v
ktotal = length(svd_photo$d) # find total K values
ktotal
#[1] 2760
# Step 3: Retain k singular values: divide previous kvalues by doubling the previous divisor
k1 = round(ktotal/2, digits =0) # first k value when we half ktotal
k1 # 1380
U_k1 <- U[, 1:k1]
D_k1 <- D[1:k1, 1:k1]
V_k1 <- V[, 1:k1]
k2 = round(k1/4, digits = 0) # second k value
k2 # 345
U_k2 <- U[, 1:k2]
D_k2 <- D[1:k2, 1:k2]
V_k2 <- V[, 1:k2]
k3 = round(k2/8, digits = 0) # thrid k value
k3 # 58
U_k3 <- U[, 1:k3]
D_k3 <- D[1:k3, 1:k3]
V_k3 <- V[, 1:k3]
k4 = round(k3/8,digits =0) # fourth k value using /8 because /16 gives us too small of a number that even we
cannot make out what the celebrity is.
18
k4 # 5
U_k4 <- U[, 1:k4]
D_k4 <- D[1:k4, 1:k4]
V_k4 <- V[, 1:k4]
# Step 4: Reconstruct the photograph with k=50 and plot
# original photo
par(mar = c(2,2, 2.5, 2.5), mfrow = c(2, 3))
plot(photo,
     xlim = c(0, dim(photo)[1]), ylim = c(0, dim(photo)[2]),
     axes = F,
     main = "Grayscale Photo")
# noised photos
photo_noised1 <- as.cimg(U_k1 %*% D_k1 %*% t(V_k1))
plot(photo_noised1, axes = F, main = "1380 Modes")
photo_noised2 <- as.cimg(U_k2 %*% D_k2 %*% t(V_k2))
plot(photo_noised2, axes = F, main = "345 Modes")
photo_noised3 <- as.cimg(U_k3 %*% D_k3 %*% t(V_k3))
plot(photo_noised3, axes = F, main = "58 Modes")
photo_noised4 <- as.cimg(U_k4 %*% D_k4 %*% t(V_k4))
plot(photo_noised4, axes = F, main = "5 Modes")

#Michelle Obama Photo

# Step 1: Load the grayscale photograph
setwd("/Users/ibsqu/ibrown/LinAlg/data") # Replace with your photo path
library(imager)
photo <- load.image("michelle.jpg") # Insert photo name 
dim(photo) #Last dimension should be 3 
#[1] 1800 1800   1   3 
photo = grayscale(photo)
dim(photo) # Last dimension should be 1 now 
#[1] 1800 1800   1   1  

# Step 2: Perform SVD on the image
svd_photo <- svd(photo)
U <- svd_photo$u
D <- diag(svd_photo$d)  # Convert singular values into a diagonal matrix
K = length(svd_photo$d) # find total K values
K 
#[1] 1800
V <- svd_photo$v

# Step 3: Adjust the number of singular values to retain (Starting at a different K values)
## Recall total K = 1800 
k1 <- 900    #/2 #y
U_k1 <- U[, 1:k1]
D_k1 <- D[1:k1, 1:k1]
V_k1 <- V[, 1:k1]

k2 <- 112    #/16 #y
U_k2 <- U[, 1:k2]
D_k2 <- D[1:k2, 1:k2]
V_k2 <- V[, 1:k2]

k3 <- 23     #/64 #y
U_k3 <- U[, 1:k3]
D_k3 <- D[1:k3, 1:k3]
V_k3 <- V[, 1:k3]

k4 <- 5      #/256 #y
U_k4 <- U[, 1:k4]
D_k4 <- D[1:k4, 1:k4]
V_k4 <- V[, 1:k4]


# Step 4: Reconstruct the photograph with different k values
par(mar = c(2,2, 2.5, 2.5), mfrow = c(2, 3)) 
photo_noised1 = as.cimg(U_k1 %*% D_k1 %*% t(V_k1))
photo_noised2 = as.cimg(U_k2 %*% D_k2 %*% t(V_k2))
photo_noised3 = as.cimg(U_k3 %*% D_k3 %*% t(V_k3))
photo_noised4 = as.cimg(U_k4 %*% D_k4 %*% t(V_k4))


plot(photo, 
     axes = FALSE, main = "Original Grayscale Photo")

# Plotting noised photos
plot(photo_noised1, axes = FALSE, main = "900 Modes")
plot(photo_noised2, axes = FALSE, main = "112 Modes")
plot(photo_noised3, axes = FALSE, main = "23 Modes")
plot(photo_noised4, axes = FALSE, main = "5 Modes")
