module particle
  use pdfs
  implicit none
  private
  public :: particle_init
  
  ! declare variables
  type,public :: particle_type
    integer   :: slab   ! the slab id number
    real(8)   :: xloc   ! the x location of the particle
    real(8)   :: mu     ! the angle cosine of travel
    logical   :: alive  ! a logical to indicate if the particle is alive
  end type particle_type
  
contains

!==============================================================================
! particle_init
!==============================================================================
  subroutine particle_init(this,len_slab)
    ! declare arguments
    type(particle_type) :: this
    real(8)             :: len_slab
    
    ! call pdfs function get_particle_pos, send slab length, get x
    this%xloc=get_particle_pos(len_slab)
        
    ! call pdfs function get_particle_mu, returns mu from -1 to 1
    this%mu=get_particle_mu()
    
    ! make neutron alive
    this%alive=.true.
        
  end subroutine particle_init

!===============================================================================
end module particle
