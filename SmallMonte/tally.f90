module tally
  implicit none
  
  ! declare variales
  type :: tally_type
    real(8) :: c1 = 0.0_8       ! collision accumulator
    real(8) :: c2 = 0.0_8       ! square of collision accumulator
    real(8) :: s1 = 0.0_8       ! path accumulator
    real(8) :: s2 = 0.0_8       ! square of path accumulator
    real(8) :: smean = 0.0_8    ! mean for tracklength est
    real(8) :: cmean = 0.0_8    ! mean for collision est
    real(8) :: svar = 0.0_8     ! variance for tracklength est
    real(8) :: cvar = 0.0_8     ! variance for collision est
    real(8) :: track = 0.0_8    ! temp. track var
    real(8) :: coll = 0.0_8     ! temp. collision var
  end type tally_type

contains

!==============================================================================
! tally_reset
!==============================================================================
  subroutine tally_reset(this)
    ! delcare arguments
    type(tally_type) :: this    ! tally type variable
    
    ! reset coll and track to zero (dble precision)
    this%coll=0.0_8
    this%track=0.0_8
    
  end subroutine tally_reset

!==============================================================================
! bank_tally
!==============================================================================
  subroutine bank_tally(this)
    ! declare arguments
    type(tally_type) :: this         ! tally type variable
    
    ! count tallies
    this%c1=this%c1+this%coll
    this%c2=this%c2+this%coll**2
    this%s1=this%s1+this%track
    this%s2=this%s2+this%track**2
    
  end subroutine bank_tally

!==============================================================================
! perform_statistics
!==============================================================================
  subroutine perform_statistics(this,npart,len_subslab)
    ! declare arguments
    type(tally_type)  :: this         ! tally type variable
    integer(8)        :: npart        ! number of particles
    real(8)           :: len_subslab  ! length of the subslab  
    
    ! compute mean and variance for track
    this%smean=this%s1/(len_subslab*dble(npart))
    this%svar=(1.0_8 / (dble(npart-1)*len_subslab) )*(this%s2/dble(npart) &
      &   - (this%s1/dble(npart))**2)
        
    ! compute mean and variance for coll
    this%cmean=this%c1/(len_subslab*dble(npart))
    this%cvar=(1.0_8 / (dble(npart-1)*len_subslab) )*(this%c2/dble(npart) &
      &   - (this%c1/dble(npart))**2)
            
  end subroutine perform_statistics

!==============================================================================
end module tally
