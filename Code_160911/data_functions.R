# Generic functions for data manipulation


### WRITING AGE FUNCTION TO RECODE DATA INTO 5 YEAR AGE GROUPS ###


age_5 <- function (x){
  if (x >= 0 & x <= 4) {
    return ("04")
  } else if (x >= 5 & x <= 9){
    return ("59")
  } else if (x >= 10 & x <= 14){
    return ("1014")
  } else if (x >= 15 & x <= 19){
    return ("1519")   
  } else if (x >= 20 & x <= 24){
    return ("2024")
  } else if (x >= 25 & x <= 29){
    return ("2529")
  } else if (x >= 30 & x <= 34){
    return ("3034")
  } else if (x >= 35 & x <= 39){
    return ("3539")
  } else if (x >= 40 & x <= 44){
    return ("4044")
  } else if (x >= 45 & x <= 49){
    return ("4549")
  } else if (x >= 50 & x <= 54){
    return ("5054")
  } else if (x >= 55 & x <= 59){
    return ("5559")
  } else if (x >= 60 & x <= 64){
    return ("6064")
  } else if (x >= 65 & x <= 69){
    return ("6569")
  } else if (x >= 70 & x <= 74){
    return ("7074")  
  } else if (x > 74){
    return ("7400")
  } else
    return ("NA")
}

### FUNCTION TO PUT CASES INTO BROADER AGE GROUPS ###

age_broad <- function (x){
  
  if (x > 0 & x <= 14){
    return ("014")
  } else if (x >= 15 & x <= 24){
    return ("1524")
  } else if (x >= 25 & x <= 44){
    return ("2544")
  } else if (x >= 45 & x <= 64){
    return ("4564")
  } else if (x >= 65 & x<= 74) {
    return ("6574")
  } else if (x > 74){
    return ("7400")
  } else 
    return ("NA")
}




### INDICATOR FUNCTION FOR 65-74 YEAR OLDS ###

retired <- function (x) {
  
  if (x >= 65 & x <= 74){
    return ("2")
  } else if (x > 75){
    return ("3")
  } else
    return ("1")
}


### FUNCTION TO PRINT WHO STANDARD PROPORTIONS ###

WHO_stand_fun <- function (x) {
  if (x >= 0 & x <= 4) {
    return ("0.0886")
  } else if (x >= 5 & x <= 9){
    return ("0.0869")
  } else if (x >= 10 & x <= 14){
    return ("0.0860")
  } else if (x >= 15 & x <= 19){
    return ("0.0847")   
  } else if (x >= 20 & x <= 24){
    return ("0.0822")
  } else if (x >= 25 & x <= 29){
    return ("0.0793")
  } else if (x >= 30 & x <= 34){
    return ("0.0761")
  } else if (x >= 35 & x <= 39){
    return ("0.0715")
  } else if (x >= 40 & x <= 44){
    return ("0.0659")
  } else if (x >= 45 & x <= 49){
    return ("0.0604")
  } else if (x >= 50 & x <= 54){
    return ("0.0537")
  } else if (x >= 55 & x <= 59){
    return ("0.0455")
  } else if (x >= 60 & x <= 64){
    return ("0.0372")
  } else if (x >= 65 & x <= 69){
    return ("0.0296")
  } else if (x >= 70 & x <= 74){
    return ("0.0221")  
  } else if (x > 74){
    return ("0.0306")
  } else
    return ("NA")
}

