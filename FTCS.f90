! FTCS.f90
! unsteady heat conduction (one dimensional)
! https://www.youtube.com/watch?v=JJp6OIBMC6Q
program FTCS
    implicit none
    integer :: m,n,i,j,a=1
    real, allocatable :: P(:,:)
    real :: r,l,t,delta_x,delta_t,g
    n=10
    r=0.25
    t=.03
    delta_t=0.005
    l=1
    3 format (T10,F15.8)  ! F is for real (float)

    delta_x = 1/(n-1)  ! n = points
    g = floor(t/delta_t)
    m = g  ! time

    allocate (P(m,n))
    P(1,:)=20  ! initial condition: indexing begins at 1

    do i=2,m    ! points
        P(i,1)=100 ! boundary condition (left)
        P(i,n)=0  ! boundary condition (right)
        do j=2,n-1  ! time
            P(i,j)=r*(P(i-1,j+1)+P(i-1,j-1))+(1-2*r)*(P(i-1,j))  ! T_i=r(T_i-1 + T_i+1) + (1-2r)T_i
        end do
    end do

    do i=1,m
        print *, (P(i,j),j=1,n)
    end do


end program FTCS




