n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
vec_1 == 3
vec_2 = vec_1 == 3

vec_1[vec_2]

length(vec_1)
sum(vec_1 == 3)
n = 10

vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))


for (i in 1:10)
{
  print((paste0("This is loop iteration:", i)))
}

#number 7 
n <- 8
for (i in 1:n)
{
  print(i)
}


#Q 8 

n=17 
vec_1 = sample(10,n,replace=TRUE)
for (i in 1:n)
{
  print(paste("The element of vec_1 at index",i, "is", vec_1[i]))
}


create_and_print_vec=function(n,min=1,max=10)
{
  vec_3=sample(min:max,n,replace=TRUE)
  for(i in 1:n)
  {
    print(paste("The element at index",i, "is", vec_3[i]))
  }
}
create_and_print_vec(10)



my_vec = rep(1:3, 5)
my_bool_vec = my_vec == 3
data=data.frame(my_vec, my_bool_vec)
data[my_bool_vec,1]



