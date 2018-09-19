program test_simsphere
  use simsphere_mod
  implicit none


  integer :: i
  real(kind=4) :: T_Obst_Hgt, T_zo_patch
  logical :: T_dual_regime
  
  logical :: splint_test, spline_test, start_test 
  logical :: transm_ftabs_test, transm_ftabs_test2
  logical :: transm_ftscatT_test, transm_ftscatT_test2
  logical :: transm_fbscatT_test, transm_fbscatT_test2
  logical :: advect_func_test
!  logical :: air_test
  logical :: cond_test
  logical :: stomfs_test

! splint_test variables
  integer, parameter :: splint_max_array = 50
  real, parameter :: splint_expected = 1.5
  real :: splint_arg1(splint_max_array), splint_arg2(splint_max_array)  
  real(kind=4), allocatable :: splint_arg3(:)
  integer :: splint_arg4, splint_arg5
  real :: splint_output

! spline_test variables
  integer, parameter :: spline_max_array = 50
  integer :: spline_arg3
  real, parameter :: spline_expected = 2.59807634
  real :: spline_arg1(spline_max_array), spline_arg2(spline_max_array)
  real :: spline_arg4, spline_arg5
  real, allocatable :: spline_output(:)

! transm_ftabsT variables
  real :: transm_ftabs_arg1, transm_ftabs_output
  real, parameter :: transm_ftabs_expected = 0.722923875
  real, parameter :: transm_ftabs_expected2 = 0.437107921

! transm_ftscatT variables
  real :: transm_ftscatT_arg1, transm_ftscatT_output
  real, parameter :: transm_ftscatT_expected = 0.553545594
  real, parameter :: transm_ftscatT_expected2 = 0.173223004 

! transm_fbscatT variables
  real :: transm_fbscatT_arg1, transm_fbscatT_output
  real, parameter :: transm_fbscatT_expected = 0.307550341
  real, parameter :: transm_fbscatT_expected2 = 0.304252803

! netrad_init_test variables
  real, parameter :: aepsi_expected = 0.804384410
  real, parameter :: aepsi_cloud_flag_expected = 1.02347386

! advect_test variables
  real, parameter :: advect_test_expected = 9.41291146E-05

! air_test variables
!  real, parameter :: air_test_expected = 0.0

! cond_test variables
  real, parameter :: cond_test_expected = 1.46884334
  real :: cond_test_output

! stomfs_test variables
  real, parameter :: stomfs_test_expected = 1.58197677
  real :: stomfs_test_output

! Set logical to control test execution
  start_test = .false.
  splint_test = .true.
  spline_test = .true.
  transm_ftabs_test = .true.
  transm_ftabs_test2 = .true.
  transm_ftscatT_test = .true.
  transm_ftscatT_test2 = .true.
  transm_fbscatT_test = .true.
  transm_fbscatT_test2 = .true.
  advect_func_test = .true.
  cond_test = .true.
  stomfs_test = .true.


! Initialize some test values
  T_Obst_Hgt = 0.0
  T_zo_patch = 0.0
  T_dual_regime = .false.

!
! splint_test
!
  if (splint_test) then
    do i = 1,splint_max_array
      splint_arg1(i) = 2*i
      splint_arg2(i) = 2*i
    end do
    splint_arg4 = 12
    if (.not. allocated(splint_arg3)) then
      allocate(splint_arg3(splint_arg4))
      do i = 1,splint_arg4
        splint_arg3(i) = 2*i
      end do
    end if
    splint_arg5 = 3

    splint_output=splint(splint_arg1, splint_arg2, splint_arg3, splint_arg4, splint_arg5)
    if (splint_output .ne. splint_expected) then
      write(*,*) 'splint_test: actual /= expected: ', splint_output, splint_expected
    else
      write(*,*) 'splint_test: OK'
    end if
    if (allocated(splint_arg3)) then
      deallocate(splint_arg3)
    end if
  end if

! 
! spline_test
!
  if (spline_test) then
    do i = 1,spline_max_array
      spline_arg1(i) = 2*i
      spline_arg2(i) = 2*i
    end do
    spline_arg3 = spline_max_array
    spline_arg4 = 1.0
    spline_arg5 = 2.5
    if (.not. allocated(spline_output)) then
      allocate(spline_output(spline_arg3))
    end if
    spline_output=spline(spline_arg1, spline_arg2, spline_arg3, spline_arg4, spline_arg5)
    if (spline_output(size(spline_output)) .ne. spline_expected) then
      write(*,*) 'spline_test: actual /= expected: ', spline_output, spline_expected
    else
      write(*,*) 'spline_test: OK'
    end if
    if (allocated(spline_output)) then
      deallocate(spline_output)
    end if
  end if

!
! transm_ftabs_test
!
! Test the "else" branch in ftabsT()

  if (transm_ftabs_test) then
    call ftabsT_init
    transm_ftabs_arg1 = 2.75401473
    transm_ftabs_output = ftabst(transm_ftabs_arg1)
    if (transm_ftabs_output /= transm_ftabs_expected) then
      write(*,*) 'transm_ftabs: arg2 actual /= expected: ', transm_ftabs_output, transm_ftabs_expected
    else
      write(*,*) 'transm_ftabs: OK'
    end if
  end if

!
! transm_ftabs_test2
!
! Test the "if" branch in ftabsT()

  if (transm_ftabs_test2) then
    call ftabsT_init
    transm_ftabs_arg1 = 11.0
    transm_ftabs_output = ftabst(transm_ftabs_arg1)
    if (transm_ftabs_output /= transm_ftabs_expected2) then
      write(*,*) 'transm_ftabs2: arg2 actual /= expected: ', transm_ftabs_output, transm_ftabs_expected2
    else
      write(*,*) 'transm_ftabs2: OK'
    end if
  end if

!
! transm_ftscatT_test
!
! Test the "else" branch in ftscatT()

  if (transm_ftscatT_test) then
    call ftscatT_init
    transm_ftscatT_arg1 = 2.75401473
    transm_ftscatT_output = ftscatT(transm_ftscatT_arg1)
    if (transm_ftscatT_output /= transm_ftscatT_expected) then
      write(*,*) 'transm_ftscatT: actual /= expected: ', transm_ftscatT_output, transm_ftscatT_expected
    else
      write(*,*) 'transm_ftscatT: OK'
    end if
  end if

!
! transm_ftscatT_test2
!
! Test the "if" branch in ftscatT()

  if (transm_ftscatT_test2) then
    call ftscatT_init
    transm_ftscatT_arg1 = 11.0
    transm_ftscatT_output = ftscatT(transm_ftscatT_arg1)
    if (transm_ftscatT_output /= transm_ftscatT_expected2) then
      write(*,*) 'transm_ftscatT2: actual /= expected: ', transm_ftscatT_output, transm_ftscatT_expected2
    else
      write(*,*) 'transm_ftscatT2: OK'
    end if
  end if

!
! transm_fbscatT_test
!
! Test the "else" branch in fbscatT()

  if (transm_fbscatT_test) then
    call fbscatT_init
    transm_fbscatT_arg1 = 2.75401473
    transm_fbscatT_output = fbscatT(transm_fbscatT_arg1)
    if (transm_fbscatT_output /= transm_fbscatT_expected) then
      write(*,*) 'transm_fbscatT: actual /= expected: ', transm_fbscatT_output, transm_fbscatT_expected
    else
      write(*,*) 'transm_fbscatT: OK'
    end if
  end if

!
! transm_fbscatT_test2
!
! Test the "if" branch in fbscatT()

  if (transm_fbscatT_test2) then
    call fbscatT_init
    transm_fbscatT_arg1 = 11.0
    transm_fbscatT_output = fbscatT(transm_fbscatT_arg1)
    if (transm_fbscatT_output /= transm_fbscatT_expected2) then
      write(*,*) 'transm_fbscatT2: actual /= expected: ', transm_fbscatT_output, transm_fbscatT_expected2
    else
      write(*,*) 'transm_fbscatT2: OK'
    end if
  end if

!
! advect_func_test
!
  if (advect_func_test) then
    call advect_init
    advgt=advect()
    if (advgt /= advect_test_expected) then
      write(*,*) 'advect_test: actual /= expected: ', advgt, advect_test_expected
    else
      write(*,*) 'advect_test: OK'
    end if
  end if   

!
! cond_test
!
  if (cond_test) then
    call cond_init
    cond_test_output = cond()
    if (cond_test_output /= cond_test_expected) then
      write(*,*) 'cond_test: actual /= expected: ', RKW, cond_test_expected
    else
      write(*,*) 'cond_test: OK'
    end if
  end if

!
! stomfs_test
!
  if (stomfs_test) then
    call stomfs_init
    call stomfs
    stomfs_test_output = fs
    if (stomfs_test_output /= stomfs_test_expected) then
      write(*,*) 'stomfs_test: actual /= expected: ',stomfs_test_output,stomfs_test_expected
    else
      write(*,*) 'stomfs_test: OK'
    end if
  end if

contains 
  subroutine ftabsT_init
    OMEGA = 3.13
    ABSTBL(9) = 0.718309104
    ABSTBL(10) = 0.707092881
    ABSTBL(46) = 0.437107921
    PS1 = 967.0
  end subroutine ftabsT_init
  
  subroutine ftscatT_init
    OMEGA = 3.13
    SCATBL(9) = 0.548576415
    SCATBL(10) = 0.527300596
    SCATBL(46) = 0.173223004
    PS1 = 967.0
  end subroutine ftscatT_init

  subroutine fbscatT_init
    OMEGA = 3.13
    BSCTBL(9) = 0.307897508
    BSCTBL(10) = 0.307446688
    BSCTBL(46) = 0.304252803
    PS1 = 967.0
  end subroutine fbscatT_init

  subroutine advect_init
    CF = 9.19953891E-05
    OTEMP = 295.149994
    VGD(5) = 8.41951942
    VGD(3) = 8.43165302
    VGD(1) = 8.44378662
    UGD(5) = 13.0658665
    UGD(3) = 9.05783463
    UGD(1) = 5.04980326
  end subroutine advect_init

  subroutine cond_init
    RKS = 6.60699987
    THV = 1.00000000
    THMAX = 0.338999987
    COSBYB = 2.78999996
  end subroutine cond_init

  subroutine stomfs_init
    sc = 1.0
    sol = 1.0
  end subroutine stomfs_init

end program
