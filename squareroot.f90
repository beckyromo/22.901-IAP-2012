program squareroot
implicit none
  ! declare variables
  real :: x     ! user input


  ! ask for x
  print *,'Enter x:'
  read *,x

  ! check to see if x is negative
  if (x < 0.0 ) then
    ! negate x
    x=-1.0*x
    ! take square root
    x=sqrt(x)
    ! print result to user
    print *,'The square root is: +/-',x,'i'
  else
    ! x is positive, take square root
    x=sqrt(x)
    ! print result
    print *,'The square root is: +/-',x
  end if

  ! terminate code
  stop

end program squareroot
