program test_output
  use simsphere_mod, only: output, rnet, heat, evap, frveg, taf, xlai, rs, &
                           fco2, ccan, xlef, swave, ptime, atemp, ta, &
                           otemp, uten, uaf, q_fine, qa, qaf, f, fsub, psim,    &
                           psie, psig, caf, fglobal, flux_plant, eq
  use json_module
  use mod_testing, only: assert, initialize_tests, report_tests
  use iso_fortran_env, only: real64, error_unit
  implicit none

  type(json_core) :: json
  type(json_file) :: test_json
  type(json_value), pointer :: p, out
  logical :: found
  character(len=:), allocatable :: test_file
  integer :: error_cnt = 0
  character(len=6), parameter :: path = 'output'
  character(len=:), allocatable :: key

  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  ! Expected values
  ! 4 cases: I. rnet <= 0  --> produce something
  !          II. swave <= 0 --> produce something
  !          III. heat > 0 --> produce same as other cases
  !          IV. heat <= 0 --> produce same as other cases

  real(kind=real64), parameter :: ptime_exp = 1.0
  real(kind=real64), parameter :: swave_exp = 1.0
  real(kind=real64), parameter :: swave_II_exp = 0.0
  real(kind=real64), parameter :: rnet_exp = 1.0
  real(kind=real64), parameter :: rnet_I_exp = 0.0


  ! Buffer for looking up test variables in generated JSON output
  real(kind=real64) :: buf

  n = 1
  ntests = 7
  call initialize_tests(tests,ntests)

  ! Case I: rnet <= 0
  call output_init
  test_file = 'test_caseI.json'
  rnet = 0.0
  call output(json,out)
  call json % print(p,test_file)

  ! Read back in and check data written
  call input_init
  key = '.Time'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf,ptime_exp), key)
    n = n + 1
  end if
  key = '.Shortwave Flux/Wm-2'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf, swave_exp), key)
    n = n + 1
  end if
  key = '.Net Radiation/Wm-2'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf, rnet_I_exp), key)
    n = n + 1
  end if

  call json_destroy


  ! Case II: swave <= 0
  call output_init
  test_file = 'test_caseII.json'
  swave = 0.0
  call output(json,out)
  call json % print(p,test_file)

  call input_init
  key = '.Time'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf,ptime_exp), key//' swave == 0.0')
    n = n + 1
  end if
  key = '.Shortwave Flux/Wm-2'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf, swave_II_exp), key//' swave = 0.0')
    n = n + 1
  end if
  key = '.Net Radiation/Wm-2'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf, rnet_exp), key//' swave == 0.0')
    n = n + 1
  end if

  call json_destroy

  ! Case III: heat > 0
  call output_init
  test_file = 'test_caseIII.json'
  heat = 1.0
  call output(json,out)
  call json % print(p,test_file)

  call input_init
  key = '.Time'
  call test_json % get(path//key, buf, found)
  if ( found ) then
    tests(n) = assert(eq(buf,ptime_exp), key//' heat == 1.0')
    n = n + 1
  end if

  ! Case IV: heat <= 0
  call output_init
  test_file = 'test_caseIV.json'
  heat = 0.0
  call output(json,out)
  call json % print(p,test_file)

  call input_init
  key = '.Time'
  call test_json % get(path//key, buf, found)
  if (found) then
    tests(n) = assert(eq(buf,ptime_exp), key//' heat == 0.0')
    n = n + 1
  end if

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine output_init
    ! JSON initialization
    call json % initialize(compact_reals=.true., real_format='*')
    call json % create_object(p,'')
    call json % create_object(out,'output')
    call json % add(p,out) 
    rnet = 1.0
    heat = 1.0
    evap = 1.0
    frveg = 0.0
    taf = 1.0
    xlai = 1.0
    rs = 1.0
    fco2 = 1.0
    ccan = 1.0
    xlef = 1.0
    swave = 1.0
    ptime = 1.0
    atemp = 1.0
    ta = 1.0
    otemp = 1.0
    uten = 1.0
    uaf = 1.0
    q_fine = 1.0
    qa = 1.0
    qaf = 1.0
    f = 1.0
    fsub = 1.0
    psim = 1.0
    psie = 1.0
    psig = 1.0
    caf = 1.0
    fglobal = 1.0
    flux_plant = 1.0
    return
  end subroutine output_init

  subroutine input_init
    call test_json % initialize(compact_reals=.true., real_format='*')
    if (test_json % failed() ) then
      call test_json % print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if
    call test_json % load_file(filename = test_file)

    return
  end subroutine input_init

  subroutine json_destroy
    call json % destroy(p)
    call test_json % destroy()
    return
  end subroutine json_destroy

!  real(kind=real64) pure function get_value(json,path,key)
!    type(json_file), intent(in) :: json
!    character(len=1024), intent(in) :: path 
!    character(len=:), allocatable, intent(in) :: key
!    logical :: found
!    
!    call json % get(trim(path)//key, get_value, found)
!    
!  end function get_value

end program test_output
