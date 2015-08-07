3 + 3
val dim = 5
var a = Array.ofDim[Int](dim, dim)
for(i <- 0 until dim){
  for(j<- 0 until dim){
    a(i)(j) = 90
  }
}

for(i <- 0 until dim){
  a(i)(0) = i
  a(0)(i) = i
}


for(i <- 0 until dim){
  for(j<- 0 until dim) {
    print(a(i)(j) + " ")
  }
  println
  }


