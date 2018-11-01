program test_average
  use simsphere_mod, only: average, eq
  use mod_testing, only: assert, initialize_tests, report_tests
  implicit none


  logical, dimension(:), allocatable :: tests
  logical :: test_failed
  integer :: n, ntests

  real :: avr_arg1, avr_arg2

  ! Expected values
  real, parameter :: avr_test_init1_exp = 20.0
  real, parameter :: avr_test_init_not1_exp = 20.0


  n = 1
  ntests = 2
  call initialize_tests(tests,ntests)

  call avr_init
  call average(avr_arg1, avr_arg2)
  tests(n) = assert(eq(avr_arg2,avr_test_init1_exp), 'avr_test_init1')
  n = n + 1

  call avr_init
  call average(avr_arg1, avr_arg2)
  tests(n) = assert(eq(avr_arg2,avr_test_init_not1_exp), 'avr_test_init_not1')
  n = n + 1


  test_failed = .false.
  call report_tests(tests,test_failed)
  if (test_failed) stop 1

contains
  subroutine avr_init
    avr_arg1 = 20.0
    avr_arg2 = 0.0
    return
  end subroutine avr_init

end program test_average
