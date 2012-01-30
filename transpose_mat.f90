program transpose_mat
implicit none
  ! declare variables
  integer :: n ! dimension length
  integer :: i ! row counter
  integer :: j ! col counter
  real    :: tmp ! temp var
  real,allocatable :: M(:,:) ! matrix

  ! get dimension
  write(*,*) 'Enter dimension of matrix:'
  read(*,*) n

  ! allocate matrix
  allocate(M(n,n))

  ! read matrix
  write(*,*) 'Enter matrix:'
  read(*,*) M

  ! perform transpose
  col: do j=1,n
    row: do i=1,j

      ! check for diagonals
      if (i==j) cycle
      ! swap values
      tmp = M(j,i)
      M(j,i)=M(i,j)
      M(i,j)=tmp
    end do row
  end do col

  ! print to user
  do i=1,n
    write(*,'(100(F9.4,1X))') (M(i,j),j=1,n) ! implied do loop
  end do
      
  ! deallocate
  deallocate(M)

  ! terminate the program
  stop

end program transpose_mat
