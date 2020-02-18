#Contributing to Simsphere

##Source Locations

Toby Carlson's website at https://simsphere.ems.psu.edu is the
authoritative source for the Simsphere model.

https://github.com/tjc181/simsphere is the authoritative repository for
the Simsphere source code.  Clone the repository and submit pull requests
via Github.  Use the Github issue tracker to raise and resolve issues.

##Formatting and Style
Code is free-form Fortran.  Begin in column 1.

All variables explicitly declared.  Use "implicit none" in all units.

Statements in lower case.

Comments start with an exclamation mark in columnn 1.  Avoid comments
trailing a statement on the same line.

Use 2 spaces for indent.

```
program simsphere
  use simsphere_mod
  integer :: x, y

! Calculate the thing if x > 10
  if ( x > 10 ) then
    y = 20

  end if

end program simsphere
```

Terminate control blocks with distinct words:

```
end if
end program
end do
```

Use free-form style do loops:

```
do i = 0, NTRP
  x = i
end do
```

Use symbolic comparison operators: >, <, /=, ==, =<, <=

Pass data to other units.  Avoid globals whenever possible.  Use data
structures to organize flow of data in program.

Use pure functions whenever possible to reduce side effects.


