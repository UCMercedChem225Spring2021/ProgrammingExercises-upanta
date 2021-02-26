        program test
                implicit none
                integer::i,j
                integer::m,n, index_inCol
!                integer::grab_length, grab_start_pos, grab_end_pos
                real,dimension(:,:), allocatable::matrix
                real,dimension(:), allocatable::col_lower

                
!                matrix(:,:) = 0
                m = 3
                n=3

                allocate(matrix(m,n))
                allocate(col_lower(m*(m+1)/2))
                col_lower = (/ 1,2,3,4,5,6 /)
                do j=1,n
                        do i=1,m
                                if (i.ge.j) then
                                        index_inCol = (j-1) * m + i - j*(j-1)/2
                                        write(*,*) index_incol, col_lower(index_inCol)
                                        matrix(i,j) = col_lower(index_inCol)
                                else
                                        matrix(i,j) = matrix(j,i)
                                endif
                        enddo
                enddo



!               count_in = m
!                grab_end_pos = 0
!                grab_start_pos = 1
!                grab_length = 0
!                do i=1, m
!
!                        grab_start_pos = (grab_length) + grab_start_pos
!                        grab_length = m - i + 1
!                        grab_end_pos = grab_end_pos + grab_length
!                        matrix(i:m:1, i) = transpose(col_lower(grab_start_pos:grab_end_pos))
!!                        write(*,*) matrix(i:m, i)
!                        write(*,*) grab_length, grab_start_pos, grab_end_pos
!                        write(*,*)' printing row', col_lower(grab_start_pos:grab_end_pos)
!
!!                        grab_start_pos = grab_start_pos + grab_length
!!                        grab_end_pos = grab_end_pos + grab_length 
!
!
!                endDo

                do i=1,m
                        write(*,*) matrix(i,1),matrix(i,2),matrix(i,3)
                endDo  

        endprogram test
