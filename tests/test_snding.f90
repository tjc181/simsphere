program test_snding
  use simsphere_mod, only: vert_spacing, deltaz, ts, Celsius_to_Kelvin, gm,  &
                           grav, kts_to_metres, ntrp, atemp, tdif_s, aptemp, &
                           tscren, oshum, ahum, ps1, o_pot_tmp, tdif_50, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real, dimension(50) :: arg1
  real :: arg2

  ! Expected values
  real, parameter :: atemp_exp = 297.149994
  real, parameter :: aptemp_exp = 300.508942
  real, parameter :: o_pot_tmp_exp = 300.508942
  real, parameter :: tscren_exp = 297.149994
  real, parameter :: oshum_exp = 1.93754267E-02
  real, parameter :: ahum_exp = 1.42002506E-02
  real, parameter :: arg2_exp = 1.42002506E-02
  real, parameter :: ps1_exp = 967.0
  real, parameter :: tdif_50_exp = 3.35894775
  real, parameter :: tdif_s_exp = 2.85894775

  ! Initialize mod_testing
  n = 1
  ntests = 10
  call initialize_tests(tests,ntests)

  ! Initialize
  call start
  call snding_init
  call snding(arg1,arg2)

  tests(n) = assert(eq(atemp,atemp_exp), 'atemp')
  n = n + 1
  tests(n) = assert(eq(tscren,tscren_exp), 'tscren')
  n = n + 1
  tests(n) = assert(eq(oshum,oshum_exp), 'oshum')
  n = n + 1
  tests(n) = assert(eq(ahum,ahum_exp), 'ahum')
  n = n + 1
  tests(n) = assert(eq(arg2,arg2_exp), 'old_ahum')
  n = n + 1
  tests(n) = assert(eq(ps1,ps1_exp), 'ps1')
  n = n + 1
  tests(n) = assert(eq(aptemp,aptemp_exp), 'aptemp')
  n = n + 1
  tests(n) = assert(eq(o_pot_tmp,o_pot_tmp_exp), 'o_pot_tmp')
  n = n + 1
  tests(n) = assert(eq(tdif_50,tdif_50_exp), 'tdif_50')
  n = n + 1
  tests(n) = assert(eq(tdif_s,tdif_s_exp), 'tdif_s')
  n = n + 1

  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine snding_init
    ts = 1.0
    arg1 = 1.0
    arg2 = 0.0
    return
  end subroutine snding_init 

end program test_snding
