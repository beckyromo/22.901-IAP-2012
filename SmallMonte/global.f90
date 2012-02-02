module global
  use geometry, only: geometry_type
  use material, only: material_type
  use particle, only: particle_type
  use tally, only: tally_type
  use pdfs
  
  implicit none
  save
  ! declare variables
  type(geometry_type)          :: geo       ! geometry
  type(material_type)          :: mat       ! material
  type(particle_type)          :: neutron   ! particle
  type(tally_type),allocatable :: tal(:)    ! tally
  integer(8)                   :: npart     ! number of particles

contains

!==============================================================================  
! allocate
!==============================================================================  
  subroutine allocate_problem()
    implicit none
    
    ! allocate tal for number of slabs
    allocate(tal(geo%nsubslabs))
    
  end subroutine allocate_problem
  
!==============================================================================  
! deallocate
!==============================================================================  
  subroutine free_memory()
    implicit none
    
    ! deallocate tal from memory
    deallocate(tal)
  
  end subroutine free_memory
  
!==============================================================================
end module global
