module geometry
  implicit none
  private
  public :: read_geometry

  ! declare variables
  type,public :: geometry_type
    integer   :: nsubslabs    ! number of sub slabs
    real(8)   :: len_slab     ! length of slab
    real(8)   :: len_subslab  ! lenght of the subslab  
  end type geometry_type

contains

!==============================================================================
! read_geometry
!==============================================================================
  subroutine read_geometry(this)
    ! declare arguments
    type(geometry_type) :: this

    ! ask user for length of slab
    write(*,*) 'Enter length of slab:'
    read(*,*) this%len_slab

    ! ask user for number of sub-slabs
    write(*,*) 'Enter number of sub-slabs:'
    read(*,*) this%nsubslabs

    ! calculates length of sub-slab
    this%len_subslab=this%len_slab/dble(this%nsubslabs)

  end subroutine read_geometry  

!==============================================================================
end module geometry
