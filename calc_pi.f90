program calc_pi
implicit none


  ! declare variables
  real(8)    :: npart  ! number of particles
  real(8)    :: x      ! length of square
  real(8)    :: xc     ! x coordinate
  real(8)    :: yc     ! y coordinate
  real(8)    :: pi     ! result
  real(8)    :: rn     ! random number seeder
  real(8)    :: nhits  ! hit counts
  integer(8) :: i      ! loop counter


  ! seed random number
  rn=rand(14)
  ! initialize hit counter
  nhits=0

  ! set bounds of square area and circle radius
  x=1.0

  ! ask user for number of particles
  write(*,*) 'Enter number of particles to simulate:'
  read(*,*) npart

  ! simulate particles
  do i=1,npart
    xc=x*rand(0)
    yc=x*rand(0)
    if (sqrt(xc**2+yc**2) < x) nhits=nhits+1
  end do

  ! calculate pi and display to user
  pi=4.0*nhits/npart
  write(*,*) pi
    
    
  ! terminate program
  stop

end program calc_pi
