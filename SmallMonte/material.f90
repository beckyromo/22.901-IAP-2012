module material
  implicit none
  private
  public :: read_material

  ! declare variables
  type,public :: material_type
    real(8)   :: totalxs    ! total macroscopic cross-section
    real(8)   :: absxs      ! absorption macroscopic cross-section
    real(8)   :: scattxs    ! scattering macroscopic cross-section
  end type material_type

contains

!==============================================================================
! read_material
!==============================================================================
  subroutine read_material(this)
    ! declare arguments
    type(material_type)   :: this

    ! ask user for macroscopic absorption cross-section
    write(*,*) 'Enter macroscopic absorption cross-section:'
    read(*,*) this%absxs

    ! ask user for macroscopic scattering cross-section
    write(*,*) 'Enter macroscopic scattering cross-section:'
    read(*,*) this%scattxs

    ! ask user for total macroscopic cross-section
    write(*,*) 'Enter total macroscopic cross-section:'
    read(*,*) this%totalxs

  end subroutine read_material

!==============================================================================
end module material
