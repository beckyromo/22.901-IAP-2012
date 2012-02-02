module execute
  use global
  use pdfs
  use tally, only: bank_tally, perform_statistics, tally_reset
  use particle, only: particle_init
  use random_number_generator, only: initialize_rng
  
  implicit none
  private
  public :: run_problem
  public :: print_tallies
  

contains

!==============================================================================  
! run_problem
!==============================================================================  
  subroutine run_problem()
    ! local variables
    integer             :: i        ! loop counter
           
    ! initialize random number generator
    call initialize_rng()
    
    ! loop over the number of histories/particles
    do i=1,npart
    
      ! call particle_init, gives xloc and mu, and sets neutron to alive
      call particle_init(neutron,geo%len_slab)
      ! call reset_tallies
      call reset_tallies()
      ! get the slab id with function get_slab_id
      neutron%slab=get_slab_id()
      
      ! loops while neutron is alive
      do while (neutron%alive) 
        ! call transport
        call transport()
        ! call interaction if neutron is alive
        if (neutron%alive) call interaction()
      end do
      
      ! bank tallies
      call bank_tallies()
      
    end do
    
  end subroutine run_problem
  
!==============================================================================    
! transport
!==============================================================================  
  subroutine transport()
    ! declare local variables
    real(8)   :: s        ! free flight distance
    real(8)   :: newx     ! temporary new x location
    real(8)   :: neig     ! x location of the nearest neighbor in direction
    logical   :: resample ! reseample distance
    
    
    ! set resample to true
    resample=.true.
    
    ! loop while resample is true
    do while (resample)
      ! access get_collision_distance function, needs totalxs, returns s
      s=get_collision_distance(mat%totalxs)
      ! based on free-flight and neutron mu compute new x location
      newx=neutron%xloc + s*neutron%mu
            
      ! get nearest neighbor x in the mu direction
      if (neutron%mu > 0.0) then
        neig=dble(neutron%slab)*geo%len_subslab
      else
        neig=dble(neutron%slab-1)*(geo%len_subslab)
      end if
      
      ! check for surface crossing
      if ( (neutron%mu > 0.0_8 .AND. newx > neig) .OR. &
        & (neutron%mu < 0.0_8 .AND. newx < neig) ) then ! it crossed surface
      
        ! check if neutron leaked
        if ( (neutron%slab==1 .AND. neutron%mu < 0.0_8) .OR. &
          & (neutron%slab==geo%nsubslabs .AND. neutron%mu > 0.0_8) ) then
          ! kill neutron
          neutron%alive = .false.
          ! set resample to false
          resample = .false.
        end if
           
        ! record track length to tally (only free-flight to surface)
        tal(neutron%slab)%track = tal(neutron%slab)%track + abs(neig - &
          neutron%xloc)/abs(neutron%mu)
      
        ! move neutron to neighbor surface
        neutron%xloc = neig
      
        ! change slab id number
        if (neutron%mu < 0.0_8) then
          neutron%slab=neutron%slab-1
        else
          neutron%slab=neutron%slab+1
        end if
        
      else      ! neutron did not cross a surface, it collided
        ! record full free-flight distance
        tal(neutron%slab)%track = tal(neutron%slab)%track + s
        
        ! move neutron to new location
        neutron%xloc = newx
        
        ! set resample to false
        resample = .false.
      
      end if
            
    end do
    
  end subroutine transport
  
  
!==============================================================================  
! interaction
!==============================================================================  
  subroutine interaction()
    integer   :: colid   ! collision id
    
    ! add 1/totalxs to collision temp variable
    tal(neutron%slab)%coll=tal(neutron%slab)%coll + 1.0_8/mat%totalxs
    
    ! get reaction type by accessing the function get_collision_type
    ! send absoprtion, scatting, totalxs , returns collision id
    ! 1= absoprtion, 2 = scattering
    colid=get_collision_type(mat%absxs,mat%scattxs,mat%totalxs)
    if (colid==1) then
      ! kill particle because it was absorbed
      neutron%alive=.false.
    else
      ! particle scatters so sample a new angle
      neutron%mu=get_scatter_mu()
    end if
      
  end subroutine interaction
  
  
!==============================================================================  
! get_slab_id
!==============================================================================
  function get_slab_id()
    ! declare arguments and result
    integer   :: get_slab_id    ! slab id result
    
    ! find which slab the particle is in
    get_slab_id=ceiling(neutron%xloc/dble(geo%len_subslab))
    
  end function get_slab_id

!==============================================================================  
! reset_tallies
!==============================================================================
  subroutine reset_tallies()
    ! declare local variables
    integer     :: i    ! loop counter
    
    do i=1,geo%nsubslabs
      ! reset coll and track with tally_reset
      call tally_reset(tal(i))
    end do
    
  end subroutine reset_tallies

!==============================================================================
! bank_tallies
!==============================================================================
  subroutine bank_tallies()
    implicit none
    ! declare local vairables
    integer     :: i    ! loop counter
    
    do i=1, geo%nsubslabs
      ! call bank tally
      call bank_tally(tal(i))
    end do
    
  end subroutine bank_tallies

!==============================================================================  
! print_tallies
!==============================================================================
  subroutine print_tallies()
    ! declare local variables
    integer             :: i    ! loop counter for slabs
    
    ! print geometry
    write(*,'(/,"The geometry of the slab is:",/)')
    write(*,'("    Slab Length",T20,"   Num of Slabs",T40,"Sub-slab Length")')
    write(*,'(F15.4,T20,I15,T40,F15.4,/)') geo%len_slab, geo%nsubslabs, &
      & geo%len_subslab
    
    ! print material
    write(*,'(/,"The macroscopic cross-sections are:",/)')
    write(*,'("     Absorption",T20,"     Scattering",T40,"          Total")')
    write(*,'(F15.4,T20,F15.4,T40,F15.4,/)') mat%absxs, mat%scattxs, mat%totalxs
    
    ! print tally information by slab region
    write(*,'(/,"Slab Id",T10,"Coll",T35,"Track")')
    write(*,'("-------",T10,"----------------------",T35,   &
      &   "----------------------")')
    ! loop over subslabs
    do i=1,geo%nsubslabs
      ! perform statistics
      call perform_statistics(tal(i),npart,geo%len_subslab)
      
      write(*,'(I4,T10,F0.5," +/-",ES12.4,T35,F0.5," +/-",ES12.4)') i, &
        & tal(i)%cmean, tal(i)%cvar, tal(i)%smean, tal(i)%svar
      
      
            
    end do
    
  end subroutine print_tallies

!==============================================================================
end module execute
