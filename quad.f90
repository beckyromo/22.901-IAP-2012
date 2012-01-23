program quad
implicit none
  ! declare variables
  real :: a  ! input for degree 2 coefficient
  real :: b  ! input for degree 1 coefficient
  real :: c  ! input for degree 0 coefficient
  real :: answer  ! result

  ! get coefficients from user
  print *,'Enter the three coefficients of the quadratic equation:'
  read *,a,b,c

  ! check radical to see if complex
  if ( (b**2 - 4.0*a*c) < 0 ) then
    print *,'The roots are:',-b/(2.0*a),'+',abs(b**2 - 4.0*a*c),'i and'&
    &,-b/(2.0*a),'-',abs(b**2 - 4.0*a*c),'i'
  ! check radical to see if zero
  else if ( (b**2 - 4.0*a*c) == 0 ) then
    print *,'Both roots are:',-b/(2.0*a)
  ! else the roots are real
  else
    print *,'The roots are:',(-b+sqrt(b**2 - 4.0*a*c))/(2.0*a),'and',(-b-sqrt(b**2 - 4.0*a*c))/(2.0*a)
  end if
  
  ! terminate code
  stop
end program quad
