program main
 use stdlib_kinds, only: int8
 use stdlib_hashmap_wrappers, only: fnv_1_hasher, key => key_type, other_type, set, get
 use stdlib_hashmaps, only: chaining_hashmap_type
 implicit none
 integer, parameter :: nentries = 25
 integer, parameter :: nelements = 2

 type dummy_type
  integer :: pos
  real :: myval(nelements)
 end type dummy_type

 integer :: i
 logical :: conflict, exists
 type(dummy_type) :: dummy


 type(other_type) :: other
 type(chaining_hashmap_type) :: map
 class(*), allocatable :: data


 write(*, '(a)')'Initialization of the map...'
 call map%init(fnv_1_hasher)

 write(*, '(a)')'Generating and storing the data...'
 do i = 1, nentries
  !Generate data
  dummy%pos = i
  call random_number(dummy%myval)

  !Storing data
  call set(other, dummy)
  call map%map_entry(key([transfer("dummy", 1_int8), transfer(i, 1_int8)]), other, conflict)
  if(conflict) error stop "Unable to map entry because of a key conflict"
 enddo

 write(*, '(a)')'Querying table info...'
 write(*, '(a,t40,i0)')'Number of buckets allocated: ',map%num_slots()
 write(*, '(a,t40,i0)')'Number of key-value pairs stored: ',map%entries()
 write(*, '(a,t40,i0)')'The worst case bucket depth is ',map%total_depth()


 write(*, '(a)')'Retrieving the data...'
 do i = 1, nentries
  call map%get_other_data(key([transfer("dummy", 1_int8), transfer(i, 1_int8)]), other, exists)
  if(.not.exists)write(*,'(a)')'Warning: missing key other'

  call get(other, data)

  select type (data)
   type is (dummy_type)
    print *, 'Other data % pos   = ', data%pos
    print *, 'Other data % myval = ', data%myval
   class default
    print *, 'Invalid data type in other'
  end select

 enddo

end program main
