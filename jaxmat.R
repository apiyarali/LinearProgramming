#jaxmat.R
#August 28, 2020
#Run the JaxGuide app to see many examples.

#Because \ is special in R, you need to use \\ when TeX wants \
#Functions whose names end in I or D let you replace \ by a backtick `

#Inline -- use ` in place of \\
jaxI <- function(s){
  s <- gsub("`","\\\\",s)   #replace all instances
  #When combining several string arguments with no separator, use paste0  
  withMathJax(paste0("\\(",s,"\\)"))
}

#Display -- use ` in place of \\
jaxD <- function(s){
  s <- gsub("`","\\\\",s)
  withMathJax(paste0("\\[",s,"\\]"))
}

#Functions that start with "tex." return building blocks.
#Use them within jaxI and jaxD

#Display a letter in bold with an arrow over it
tex.vec <- function(name) {
  return(paste0("\\vec{\\mathbf{",name,"}}"))
}

jax.vecname <- function(name){
  withMathJax(paste0("\\(",tex.vec(name),"\\)"))
}

#Create a fraction
tex.frac <- function(num, denom){
  return(paste0("\\frac{",num,"}{",denom,"}"))
}

jax.fracI <- function(num, denom){
  withMathJax(gsub("`","\\\\",paste0("\\(",tex.frac(num,denom),"\\)")))
}

jax.fracD <- function(num, denom){
  withMathJax(gsub("`","\\\\",paste0("\\[",tex.frac(num,denom),"\\]")))
}

#Create an integral
tex.int <- function(integrand, lower = "", upper = ""){
  return(paste0("\\int_{",lower,"}^{",upper,"}",integrand))
}
tex.int("t^2dt", lower = "2", upper = "x")

jax.intI <- function(integrand, lower = "", upper = ""){
  withMathJax(gsub("`","\\\\",
                   paste0("\\(",tex.int(integrand,lower=lower,upper=upper),"\\)")))
}
jax.intI("t^2dt", lower = "2", upper = "x")

jax.intD <- function(num, denom){
  withMathJax(gsub("`","\\\\",
                   paste0("\\[",tex.int(integrand,lower=lower,upper=upper),"\\]")))
}

#Create a sum
tex.sum <- function(summand, lower = "", upper = ""){
  return(paste0("\\sum_{",lower,"}^{",upper,"}",summand))
}


jax.sumI <- function(summand, lower = "", upper = ""){
  withMathJax(gsub("`","\\\\",
          paste0("\\(",tex.sum(summand,lower=lower,upper=upper),"\\)")))
}


jax.sumD <- function(summand, lower = "", upper = ""){
  withMathJax(gsub("`","\\\\",
            paste0("\\[",tex.sum(summand,lower=lower,upper=upper),"\\]")))
}
  

#Utility functions for vectors and matrices
#These can be used to build more complex output expressions.
#Creates a column vector in TeX
tex.column <- function(v, as.point = FALSE){
  M <- length(v)
  #When combining components of a character vector, use collapse
  vec <- paste(v,collapse = "\\\\")
  #When combining several string arguments, use paste for " " separator
  if (as.point)
    return(paste("\\begin{pmatrix}",vec,"\\end{pmatrix}"))
  return(paste("\\begin{bmatrix}",vec,"\\end{bmatrix}"))
}
tex.column(c(0,2,3))

#Creates a row of a matrix in TeX
tex.row <- function(v){
  paste(v,collapse = " & ")
}
tex.row(c(0,2,3))

#Converts a matrix to TeX
tex.matrix <- function(m){
  M <- nrow(m)
  str <- "\\begin{bmatrix}"
  str <- paste(str,tex.row(m[1,]))
  if (M == 1)
    return(paste(str,"\\end{bmatrix}"))
  for (i in 2:M) {
    str <- paste(str,tex.row(m[i,]),sep= "\\\\")
  }
  return(paste(str,"\\end{bmatrix}"))
}
tex.matrix(rbind(c(1,2),c(3,4)))

#These functions can be called from R Shiny.
#If display is TRUE, the output is centered on a separate line.
#If display is FALSE, the output is inline.

#Display the columns of matrix m as a sequence of column vectors
jax.vecList <- function(m, sep = ",",display = TRUE){
  N <- ncol(m)
  str <- ifelse(display,"\\[","\\(")
  for (i in 1:N){
    str <- paste0(str,tex.column(m[,i]),sep="")
    if (i < N)
      str <- paste0(str,sep)
  }
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#m <- rbind(c(0,1,2,-2,-1),c(0,0,0,0,0))
#jax.vecList(m)

#Display a matrix acting on vector v1 to produce vector v2
jax.mTimesV <- function(mName, v1, v2 ,display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  str <- paste0(str,mName,sep="")
  str <- paste0(str,tex.column(v1),sep="=")
  str <- paste0(str,tex.column(v2),sep="")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#Display a function acting on point p1 to produce point p2
jax.fTimesP <- function(fName, p1, p2 ,display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  str <- paste0(str,fName)
  str <- paste0(str,tex.column(p1, as.point = TRUE))
  str <- paste0(str,"=",tex.column(p2, as.point = TRUE))
  str <- gsub("`","\\\\",str)
  withMathJax(paste0(str,ifelse(display,"\\]","\\)")))
}

#Display matrix m1 times matrix m2 to produce matrix m1m2
jax.mTimesM <- function(mName, m1, m2 ,display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  str <- paste0(str,mName,sep="=")
  str <- paste0(str,tex.matrix(m1),sep="")
  str <- paste0(str,tex.matrix(m2),sep="=")
  str <- paste0(str,tex.matrix(m1%*%m2),sep="")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#jax.mTimesV("A", c(2,3), c(4,4))

#Display a matrix, preceded by an optional name
jax.matrix <- function(m, name =character(0), display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  if (length(name) > 0){
    str <- paste0(str,name,"=")
  }
  str <- paste0(str,tex.matrix(m),collapse = "")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#Display a vector, preceded by an optional name
jax.vector <- function(v, name =character(0), display = TRUE, as.point = FALSE){
  str <- ifelse(display,"\\[","\\(")
  if (length(name) > 0){
    str <- paste0(str,ifelse(as.point,"\\mathbf{","\\vec{\\mathbf{"),name)
    str <- paste0(str,ifelse(as.point,"}","}}"),"=")
  }
  str <- paste0(str,tex.column(v, as.point),collapse = "")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}








