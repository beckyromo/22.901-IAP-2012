program matrixmultiply
implicit none

  ! declare variabes
  real,allocatable  :: A(:,:) ! input matrix A
  integer           :: n      ! number of rows in matrix A
  integer           :: m      ! number of columns in matrix A
  real,allocatable  :: B(:,:) ! inpute matrix B
  integer           :: p      ! number of rows in matrix B
  integer           :: q      ! number of columns in matrix B
  real,allocatable  :: C(:,:) ! result of matrix multiplication
  
  ! ask for matrix A dimensions
  print *,'Enter the dimensions of the first matrix:'
  read *,n,m
  ! allocate space for matrix A
  allocate(A(n,m))
  ! ask for the matrix A elements
  print *,'Enter the elements of the matrix by columns:'
  read *,A

  ! ask for matrix B dimensions
  print *,'Enter the dimensions of the second matrix:'
  read *,p,q
  
  ! check for matrix multiplication dimension agreement
  if (m/=p) then
    ! inform user that matrix multiplication is not allowed
    print *,'Matrix multiplication is not allowed.  Inter matrix dimensions must agree.'
    
  else ! continue program
    ! allocate space for matrix B
    allocate(B(p,q))
    ! ask for the matrix B elements
    print *,'Enter the elements of the matrix by columns:'
    read *,B
    ! allocate space for the result matrix C
    allocate(C(n,q))
  
    ! call subroutine to calculate matrix mupltiplication
    call matrixmultiplication_sub(A,B,C,n,m,p,q)   

    ! print results
    print *,'The result is:',C
  
  end if
  
  ! deallocate memory
  deallocate(A)
  deallocate(B)
  deallocate(C)

  ! terminate program
  stop
  
end program matrixmultiply


subroutine matrixmultiplication_sub(A,B,C,n,m,p,q)
implicit none
  ! declare arguments
  real,intent(in)     :: A(n,m) ! input matrix A
  integer,intent(in)  :: n      ! number of rows in matrix A
  integer,intent(in)  :: m      ! number of columns in matrix A
  real,intent(in)     :: B(p,q) ! inpute matrix B
  integer,intent(in)  :: p      ! number of rows in matrix B
  integer,intent(in)  :: q      ! number of columns in matrix B
  real,intent(inout)  :: C(n,q) ! result of matrix multiplication
  ! declare local variables
  integer :: i  ! iteration counter for number of columns in matrix C
  integer :: j  ! iteration counter for number of rows in matric C
  integer :: k  ! summation iteration counter


  ! compute matrix multiplication
  col: do j=1,q
    row: do i=1,n
      C(i,j)=sum(A(i,1:m)*B(1:m,j))
    end do row
  end do col

end subroutine matrixmultiplication_sub
