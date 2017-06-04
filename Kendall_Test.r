#Sample Data
x <- c(42,46,39,37,65,88,86,56,62,92,54,81)
y <- c(82,98,87,40,116,113,111,83,85,126,106,117)
a <- c(42,46,39,37,65,88,86,56,62,92,54,81)
z <- c(0,0,1,1,3,4,5,6,7,8,8,12)

rank_kendall <- function(x,y,twoside = FALSE) {
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
  calcT(tx_array,rankX)
  
  #Menghitung Ty :
  calcT(ty_array,rankY)
  
  #Menghitung Tau :
  if(tx==0 && ty==0) {
    Tau = S / (0.5*N*(N-1))
  } else {
    Tau = S / (sqrt(0.5*N*(N-1))*sqrt(0.5*N*(N-1)))
  }
  
  return(Tau)
  
  #Menghitung Z :
  if(N>10) {
    Z = Tau / sqrt((2*(2*N+5))/(9*N*(N-1)))
    if(!twoside) {
      pval = pnorm(-abs(Z))
    } else {
      pval = 2*(pnorm(-abs(Z)))
    }
  }
  
  print(paste('C : ',C,sep=""))
  print(paste('D : ',D,sep=""))
  print(paste('Ty : ',ty,sep=""))
  print(paste('Tx : ',tx,sep=""))
  print(paste('S : ',S,sep=""))
  print(paste('Tau : ',Tau,sep=""))
  print(paste('Z : ',Z,sep=""))
  print(paste('pval : ',pval,sep=""))
  
}

partial_kendall <- function(x,y,z) {
  Tau_xy = rank_kendall(x,y)
  Tau_xz = rank_kendall(x,z)
  Tau_yz = rank_kendall(y,z)
  Tau_xyz = (Tau_xy - (Tau_xz*Tau_yz))/(sqrt(1-(Tau_xz^2))*sqrt(1-(Tau_yz^2)))
  print(paste('Result : ',Tau_xyz,sep=""))
  print(paste('XY : ',Tau_xy,sep=""))
  print(paste('Xz : ',Tau_xz,sep=""))
  print(paste('YZ : ',Tau_yz,sep=""))
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
