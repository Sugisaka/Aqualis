!=============================================================================================
! Project name: test4
! Project version: aaa
!---------------------------------------------------------------------------------------------
! Generated by Aqualis (algorithm and equation analyzer for lightwave simulation)
! Aqualis version: aaa
! Generated date: 2023/10/17 23:14:29
!=============================================================================================
program test4
implicit none
type testClass1
  double precision :: x1
end type testClass1
type(testClass1) :: ss
integer :: i001
double precision :: d001
complex(kind(0d0)) :: z001
integer,allocatable :: i1001(:)
integer :: i1001_size(1:1)=(/ -1 /)
i1001_size(1) = 4
allocate(i1001(1:i1001_size(1)))
i001 = 1
d001 = 2
z001 = 0
i1001(1) = 3
ss%x1 = 1.0d2
call func1(z001, d001, i001, i1001, i1001_size, ss)
print *, real(z001),aimag(z001)
i1001_size(1) = -1
deallocate(i1001)

contains

!=============================================================================================
! Subroutine name: func1
!  arg01 
!  arg02 
!  arg03 
!  arg04 
!  arg04_size 
!  arg06 
!=============================================================================================
subroutine func1(arg01, arg02, arg03, arg04, arg04_size, arg06)
  implicit none
  complex(kind(0d0)) :: arg01
  double precision :: arg02
  integer :: arg03
  integer,allocatable :: arg04(:)
  integer :: arg04_size(:)
  type(testClass1) :: arg06
  arg01 = arg02+arg03+arg04(1)+arg06%x1
  print *, real(arg01),aimag(arg01)
end subroutine func1

end program test4