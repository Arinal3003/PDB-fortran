program PDEO2
    implicit none
    real :: x0,y,u,u0,u1,u2,u3,f0,f1,f2,f3,h,y0
    integer :: i,n, io
    
open(newunit=io, file="Output.txt", status="new", action="write")
x0 = 0
y0 = 1
u0 = 0
write(*,*) 'masukan nilai h'
read(*,*) h
write(*,*) 'masukan nilai n'
read(*,*) n
do i = 1, n
    x0 = i*h
    f0 = fun(x0,y0,u0)
    u1 = u0 + (0.5)*h*f0
    f1 = fun(x0+h*0.5,y0+h*0.5*u0,u1)
    u2 = u0 + (0.5)*h*f1
    f2 = fun(x0+h*0.5,y0+h*0.5*u0,u2)
    u3 = u0 + (0.5)*h*f2
    f3 = fun(x0+h*0.5,y0+h*0.5*u0,u3)

    u = u0 + h*(f0+2*f1+2*f2+f3)/6
    y = y0 + h*(u0+2*u1+2*u2+u3)/6

    write(io,*) i*h, y
    

    u0 = u
    y0 = y    
end do
close(io)

contains
real function fun(x,y,u) result(f)
    real, intent(in):: x,u,y
    f = -(2/x)*u-y
end function fun
end program
