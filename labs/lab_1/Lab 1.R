c(1, 2, 3)
[1] 1 2 3
> 
  > "c(1, 2, 3)"
[1] "c(1, 2, 3)"
> c_1 = c(1, 2, 3)
> c_2 = "c(1, 2, 3)"
> c_1
[1] 1 2 3
> c_2
[1] "c(1, 2, 3)"
> my_vec = c(1:6)
> mat_1 = matrix(my_vec, nrow = 3)
> mat_1
[,1] [,2]
[1,]    1    4
[2,]    2    5
[3,]    3    6
> mat_1$3
Error: unexpected numeric constant in "mat_1$3"
> mat_1[3,1]
[1] 3
> 
  > mat_2 - matrix(data=my_vec,nrow=2,ncol=3)
Error: object 'mat_2' not found
> mat_2 = matrix(data=my_vec,nrow=2,ncol=3)
> mat_2
[,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
> 
  > mat_3 = matrix(data=my_vec,nrow=3,ncol=2)
> mat_3 
[,1] [,2]
[1,]    1    4
[2,]    2    5
[3,]    3    6
> 
  > my_vec$1,4
Error: unexpected numeric constant in "my_vec$1"
> c(my_vec(1,2))
Error in my_vec(1, 2) : could not find function "my_vec"
> c(my_vec+1)
[1] 2 3 4 5 6 7
> c((my_vec)+1)
[1] 2 3 4 5 6 7
> c(my_vec-1)
[1] 0 1 2 3 4 5
> c(my_vec,1)
[1] 1 2 3 4 5 6 1
> 
  > 
  > mat_4 = matrix(data=c(my_vec,7),nrow=2,ncol=3)
Warning message:
  In matrix(data = c(my_vec, 7), nrow = 2, ncol = 3) :
  data length [7] is not a sub-multiple or multiple of the number of rows [2]
> mat_4 = matrix(data=c(my_vec,7,8),nrow=2,ncol=4)
> mat_4
[,1] [,2] [,3] [,4]
[1,]    1    3    5    7
[2,]    2    4    6    8
> mat_4 = matrix(data=c(my_vec,7),nrow=2,ncol=4)
Warning message:
  In matrix(data = c(my_vec, 7), nrow = 2, ncol = 4) :
  data length [7] is not a sub-multiple or multiple of the number of rows [2]
> 
  > mynum = 5.2
> mystring = "five point two"
> myvector = c(0:5)
> 
  > 
  > my_list_1 = list("two"=mynum,"one"=mystring,"three"=myvector)
> my_list_1
$two
[1] 5.2

$one
[1] "five point two"

$three
[1] 0 1 2 3 4 5

> my_list_1[[1]]
[1] 5.2
> my_list_1[[as.numeric("1")]]
[1] 5.2
> my_list_1[["1"]]
NULL
> my_list_1[["one"]]
[1] "five point two"
> my_list_1$one
[1] "five point two"
> my_list_1$"one"
[1] "five point two"
> my_list_1$1
Error: unexpected numeric constant in "my_list_1$1"
> my_list_1$"1"
NULL
> 
create_and_print_vec=function(n,min=1,max=10)
  {
    vec_3=sample(min:max,n,replace=TRUE)
    for(i in 1:n)
    {
      print(paste("The element at index",i, "is", vec_3[i]))
    }
  }
create_and_print_vec(10)
