mean(c(10,20,30,40))

custom_mean = function() {
  sum_vector = sum(c(10,20,30,40))
  mean_vector = sum_vector/length(c(10,20,30,40))
 return(mean_vector)
}
abc = custom_mean()


custom_mean_arg= function(my_vector) {
  sum_vector = sum (my_vector)
  mean_vector = sum_vector/length(my_vector)
  return(mean_vector)
}

custom_mean_arg(c(100,120,200,20))

custom_mean_for= function(my_vector) {
 sum_vector = 0
 for(element in my_vector){   
print(sum_vector)
sum_vector = sum_vector+element}

  mean_vector = sum_vector/length(my_vector)
  return(mean_vector)
}

custom_mean_for(c(100,120,200,'20'))


custom_mean_if= function(my_vector) {
  if(class(my_vector)=='character'){
  print('please pass a numeric vector!')
    }else{
  sum_vector = 0
  for(element in my_vector){   
    print(sum_vector)
    sum_vector = sum_vector+element}
  
  mean_vector = sum_vector/length(my_vector)
  return(mean_vector)
}}

custom_mean_if(c(100,120,200,'20'))


while_func = function(){
  i=0
  while(i<15){
  print('this is valid!')
 i=i+1
  }
  print('we have reached the end!')
}
  
while_func()

while_func = function(my_vector){
  i=1
  while(i<= length(my_vector)){
    print(my_vector[i])
    
    i=i+1
  }
  print('we have reached the end!')
}

while_func(c(10,20))






