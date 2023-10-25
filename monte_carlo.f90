! monte_carlo.f90
! i followed this tutorial: https://www.youtube.com/watch?v=Yv0ZDn4v0DA&list=PL7p7K-N4TmQsI7KgdyKyh19sXWTd8UC28&index=8

program mc_int
    implicit none
    real, external :: f
    real :: a, b, p, count, integrate, x
    integer :: n,i

    print *, "lower limit ="
    read *, a
    print *, "upper limit ="
    read *, b
    print *, "number of randum nums ="
    read *, n

    count=0
    do i=1,n
        p=rand()  ! psuedo-random
        x=a+p+p*(b-a-1)
        count = count+f(x)
    end do

    integrate = (b-a)*count/n
    print *, "integral =", integrate

end program mc_int

real function f(x)
    real :: x
    f = x**2
end function