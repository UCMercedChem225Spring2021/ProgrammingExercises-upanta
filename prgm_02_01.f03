      Program prgm_02_01
!
!
!
      implicit none
      integer::i, n1, n2, delta
      real:: m, l, KE_mat, PIB_1D_T_Element

      real,parameter::pi=4.0*ATAN(1.0)
666 format(1X,'Kinetic energy matrix element ',I5,',',I5,' is ',F12.5,'.')
!
!     write(*,*)' Value of pi=', pi
!
!     Start by asking the user for the parameters.
!
      write(*,*)' What is the mass of the particle?'
      read(*,*) m
      write(*,*)' What is the length of the box?'
      read(*,*) l
      write(*,*)' What is the quantum number of the first eigenstate?'
      read(*,*) n1
      write(*,*)' What is the quantum number of the second eigenstate?'
      read(*,*) n2
!
!
      KE_mat = PIB_1D_T_Element(m, n1, n2, l) 
      write(*, 666) n1, n2, KE_mat

      End Program prgm_02_01





real  Function PIB_1D_T_Element(m, n1,n2,l)
      
      implicit none
      integer:: delta
      integer, intent(IN):: n1,n2
      real, intent(IN):: m, l
!      real, intent(OUT):: PIB_1D_T_Element
      
      real,parameter::pi=4.0*ATAN(1.0)
      if (n1.eq.n2) then
	delta=1
      else
	delta=0
      end if

      PIB_1D_T_Element = (n1 * n2 * pi**2 / (2* m * l**2)) * delta
      Return      
      End Function PIB_1D_T_Element
