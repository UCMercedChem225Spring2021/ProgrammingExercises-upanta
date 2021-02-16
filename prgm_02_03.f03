      Program prgm_02_03
!
!
!
      implicit none
      integer::i, n1, n2, delta
      real:: m, l, b, H, PIB_1D_Modified_Hamiltonian_Element

      real,parameter::pi=4.0*ATAN(1.0)
666 format(1X,'Hamiltonian matrix element ',I5,',',I5,' is ',F12.5,'.')
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
      H = PIB_1D_Modified_Hamiltonian_Element(m, b, n1, n2, l)

      write(*, 666) n1, n2, H

      End Program prgm_02_03





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


real Function PIB_1D_Modified_V_Element(b, n1, n2, L)
      real,parameter::pi=4.0*atan(1.0)
      integer, intent(IN):: n1,n2
      real, intent(IN)::b, L

      if (n1.ne.n2) then     
        V = (b*L/pi**2)*((((-1)**(n1-n2) - 1)/(n1-n2)**2) - (((-1)**(n1+n2) - 1)/(n1+n2)**2))
      else
        V=0
      end if

      PIB_1D_Modified_V_Element = V
      end function

real Function PIB_1D_Modified_Hamiltonian_Element(m, b, n1, n2, L)
      real:: V, T
      real, intent(IN):: m, b, L
      integer, intent(IN):: n1, n2

      V = PIB_1D_Modified_V_Element(b, n1, n2, L)
      T = PIB_1D_T_Element(m, n1, n2, L)

      H = V + T
      PIB_1D_Modified_Hamiltonian_Element = H
      end function
