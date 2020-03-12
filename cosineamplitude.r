
cosine_amplitude <- function(mat){
  mat_2 = matrix(rep(1,dim(mat)[1]^2),nrow = dim(mat)[1])
  for(i in 1:(dim(mat)[1]-1)){
    mat_2[i,i] = 1
    for(j in c((i+1):dim(mat)[1])){
      
      mat_2[i,j] = sum(mat[i,]*mat[j,])/sqrt(sum(mat[i,]^2)*sum(mat[j,]^2))
      mat_2[j,i] = mat_2[i,j]
    }
  }
  mat_2[dim(mat)[1],dim(mat)[1]] = 1
  return(mat_2)
}

is_reflexive <- function(mat){
  for(i in 1:dim(mat)[1]){
    if(mat[i,i] != 1)
      return(FALSE)
  }
  return(TRUE)
}

is_symmetric <- function(mat){
  for(i in 1:(dim(mat)[1]-1)){
    for(j in c((i+1):dim(mat)[1])){
      if(mat[i,j] != mat[j,i])
        return(FALSE)
    }
  }
  return(TRUE)
}


is_transitive <- function(mat){
  for(i in 1:((dim(mat)[1]))){
    for(j in 1:((dim(mat)[1]))){
      for(k in 1:((dim(mat)[1]))){
        if(mat[i,k] < min(mat[i,j],mat[j,k]))
          return(FALSE)
      }
    }
  }
  return(TRUE)
}


max_min_composition <- function(mat){
  mat_2 = matrix(rep(0,dim(mat)[1]^2),nrow=dim(mat)[1])
  for(i in seq(1,dim(mat)[1])){
    for(j in seq(1,dim(mat)[1])){
      min_vector =c()
      for(k in seq(1,dim(mat)[1])){
        min_vector[k] = min(mat[i,k],mat[k,j])
      }
      mat_2[i,j] = max(min_vector)
    }
  }
  return(mat_2)
}



lambda_cut <- function(mat,lambda){
  mat[mat>=lambda] = 1
  mat[mat != 1] = 0
  return(mat)
}


get_equivalence_classes <- function(mat){
  classes = list()
  r = seq(1,dim(mat)[1])
  #names(r) = r
  

  while(length(r)!=0){
    class1 = c()
    for(j in seq(1,length(r))){
      f=0
      for(k in seq(1,dim(mat)[1]))
      {
        if(mat[r[1],k]!=mat[r[j],k]){
          f=1
          break
        }
      }
      if(f == 0){
        class1[length(class1)+1] = r[j]
      }
    }
    
    
    classes[[length(classes)+1]] = class1
    if(length(r)<=length(class1))
      break
    
    r = r[-c(class1)]
  }
  
  
  return(classes)
    
}

mat = matrix(
      c(
        c(0.3,0.6,0.1),
         c(0.2,0.4,0.4),
         c(0.1,0.6,0.3),
         c(0.7,0.2,0.1),
         c(0.4,0.6,0)
      ),
      nrow = 5, byrow = TRUE
    )


mat = cosine_amplitude(mat)

f = 0
if( is_reflexive(mat) && is_symmetric(mat) ){
  count = 0
  while(!is_transitive(mat)){
    if(count <= dim(mat)[1]-1){
      mat = max_min_composition(mat)
      count = count + 1
    }
    else{
      print("not equivalence")
      f = 1
      break
    }
  }
  
  
}


if(f == 0){
  print("equivalence matrix")
  print(mat)
  mat = lambda_cut(mat, 0.80)
  print("matrix after lamda cut")
  print(mat)
  print("equivalence classes")
  print(get_equivalence_classes(mat))
}
