program zeta
implicit none
  ! define variables
  real :: x         ! input varialbe
  real :: zeta_var  ! result
  real :: zeta_fun  ! zeta function
  

  ! ask for user input
  print *,'Enter x:'
  read *,x

  ! call subroutine to get zeta
  call zeta_sub(x, zeta_var)

  ! print result of subroutine
  print *,'Sub::For x=',x,' , zeta is:',zeta_var

  ! print result of function
  print *,'Fun::For x=',x,' , zeta is:',zeta_fun(x)
  
  ! terminate program
  stop

end program zeta




subroutine zeta_sub(x,zeta_var)
implicit none
  ! formal variables
  real,intent(in)  :: x         ! input varialbe
  real,intent(out) :: zeta_var  ! result
  ! local variables
  real,parameter   :: pi=3.1415926  ! the constant pi

  ! compute zeta
  zeta_var=pi/x

end subroutine zeta_sub




function zeta_fun(x)
implicit none
  ! function name
  real :: zeta_fun
  ! formal variables
  real :: x         ! input varialbe
  ! local variables
  real,parameter   :: pi=3.1415926  ! the constant pi

  ! compute result
  zeta_fun=pi/x

end function zeta_fun
