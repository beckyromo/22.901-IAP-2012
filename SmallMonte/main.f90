program main

  use geometry, only: read_geometry, geometry_type
  use material, only: read_material, material_type
  use timing, only: timer_start, timer_stop, Timer, timer_get_value
  use execute, only: run_problem, print_tallies
  use global
  
  implicit none
  
  ! declare variables
  type(timer)         :: timer_run  ! timer



  ! read in number of particles to simulate
  write(*,*) 'Enter number of particles to simulate:'
  read(*,*) npart

  ! read in geometry - call read_geometry
  call read_geometry(geo)

  ! read in material - call read_material
  call read_material(mat)

  ! begin a timer
  call timer_start(timer_run)

  ! allocate the problem - call allocate_problem
  call allocate_problem()
  
  ! run the problem - call run_problem
  call run_problem()

  ! stop the timer
  call timer_stop(timer_run)

  ! print results - call print_tallies
  call print_tallies()

  ! get time elapsed
  write(*,'(/,/,"The elapsed time is:",T40,F9.4)') timer_run%elapsed
  write(*,'("The elapsed time is:",T40,ES9.2)') timer_run%elapsed
  
  ! free memory
  call free_memory()

  ! terminate program
  stop

end program
