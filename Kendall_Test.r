#Sample Data
ex_1x <- c(4,6,3,5,1,2)
ex_1y <- c(3,5,4,6,2,1)
ex_2x <- c(1,2,3,4,5,6,7,8,9,10,11,12)
ex_2y <- c(1,5,2,6,7,3,4,10,11,8,9,12)
ex_3x <- c(69,72,64,60,84,90,89,77,78,95,75,86)
ex_3y <- c(68,68,73,73,77,80,84,85,88,92,92,95)
ex_4x <- c(16,17,18,19)
ex_4y <- c(2,3,1,4)
ex_4z <- c(1,3,2,4)

rank_kendall <- function(x,y,twoside = FALSE, onlytau= FALSE) {
  #Inisialisasi variabel :
  N <- length(x)
  S <- 0
  Si <- 0
  i <- 1
  C <- 0
  D <- 0
  
  #Perankingan Data :
  rankX <- rank(x, ties.method = "average")
  rankY <- rank(y, ties.method = "average")
  
  #Format menjadi matriks dan sortir :
  matriksRank <- matrix(c(rankX,rankY),nrow = N, ncol = 2)
  matriksRankSorted <- matriksRank[order(matriksRank[,1]),]
  rankX <- matriksRankSorted[,1]
  rankY <- matriksRankSorted[,2]
  
  #Looping untuk menghitung variabel S,C,D :
  while(i<n) {
    Si <- 0
    j <- i+1
    while(j<n+1) {
      if(matriksRankSorted[i,2] < matriksRankSorted[j,2]) {
        Si = Si+1
        C = C+1
      } else if(matriksRankSorted[i,2] > matriksRankSorted[j,2]) {
        Si = Si-1
        D = D+1
      }
      j <- j+1
    }
    i <- i +1
    S <- S + Si
  }
  
  #Mencari duplicate value pada vector :
  tx_array = rankX[duplicated(rankX)]
  ty_array = rankY[duplicated(rankY)]
  tx = 0;
  ty = 0;
  
  #Menghitung Tx :
  tx = calcT(tx_array,rankX)
  
  #Menghitung Ty :
  ty = calcT(ty_array,rankY)
  
  #Menghitung Tau :
  if(tx==0 && ty==0) {
    Tau = S / (0.5*N*(N-1))
  } else {
    Tau = S / (sqrt(0.5*N*(N-1))*sqrt(0.5*N*(N-1)))
  }
  
  #Menghitung Z :
  if(N>10) {
    Z = Tau / sqrt((2*(2*N+5))/(9*N*(N-1)))
    if(!twoside) {
      pval = pnorm(-abs(Z))
    } else {
      pval = 2*(pnorm(-abs(Z)))
    }
    if(!onlytau) {
      print(paste("T : ",Tau))
      print(paste("Z : ",Z))
      print(paste("pval : ",pval))
    }
  } else {
    if(!onlytau) {
    print(paste("τ : ",Tau))
    print(paste("N : ",N))
    print(paste("S : ",S))
    }
  }
    if(onlytau) {
      return(Tau)
    }
}

partial_kendall <- function(x,y,z) {
  Tau_xy = rank_kendall(x,y,onlytau = TRUE)
  Tau_xz = rank_kendall(x,z,onlytau = TRUE)
  Tau_yz = rank_kendall(y,z,onlytau = TRUE)
  Tau_xyz = (Tau_xy - (Tau_xz*Tau_yz))/(sqrt(1-(Tau_xz^2))*sqrt(1-(Tau_yz^2)))
  print(paste('τxy : ',Tau_xy))
  print(paste('τxz : ',Tau_xz))
  print(paste('τyz : ',Tau_yz))
  print(paste('τxy.z : ',Tau_xyz))
}

#Fungsi untuk menghitung T(y / x / z) dengan parameter :
  #rank_vector = vector ranking variabel X / Y / Z
  #t_array = vector duplicate value dari rank_vector
calcT <- function(t_array, rank_vector) {
  result = 0
  if(length(t_array) != 0) {
    for(i in 1:length(t_array)) {
      ti = sum(rank_vector==t_array[i])
      result_i = ti*(ti - 1)
      result = result + result_i
    }
    result = 0.5 * result
  }
  return(result)
}

#Fungsi untuk mencari duplicate value dari suatu vector
getDuplicate <- function(rank_vector)  {
  return(rank_vector[duplicated(rank_vector)])
}

rank_kendall(x,y)
partial_kendall(x,y,z)

