        Program prgm_03_01
!
!       This program reads a file name from the command line, opens that
!       file, and loads a packed/linearized form of a square matrix. Then,
!       the packed matrix is expanded assuming a row-packed form and printed.
!       Finally, the packed matrix is expanded as a column-packed form and
!       printed.
!
!       The input file is expected to have the leading dimension (an integer
!       NDim) of the matrix on the first line. The next NDim*NDim lines each
!       have one real number each given.
!
        Implicit None
        Integer,Parameter::IIn=10
        Integer::IError,NDim,i,j
        Real,Dimension(:),Allocatable::Array_Input
        Real,Dimension(:,:),Allocatable::Matrix
        Character(Len=256)::FileName
!
!       Begin by reading the input file name from the command line. Then,
!       open the file and read the input array, Array_Input.
!
        Call Get_Command_Argument(1,FileName)
        Open(Unit=IIn,File=TRIM(FileName),Status='OLD',IOStat=IError)
        If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
        endIf
        Read(IIn,*) NDim
        Allocate(Array_Input(NDim*NDim),Matrix(NDim,NDim))
!
! *************************************************************************
!
        do i = 1,NDim*NDim
                read(IIn,*) Array_Input(i) 
        endDo
        close(IIn)

!
!
!       Convert Array_Input to Matrix and print the matrix.
!
        Write(*,*)' The matrix expanded according to a row-wise ', &
        'linear packed format:'
        Call Packed2Matrix_RowWise(NDim,NDim,Array_Input,Matrix)
        Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
        Write(*,*)' The matrix expanded according to a column-wise ', &
        'linear packed format:'
        Call Packed2Matrix_ColumnWise(NDim,NDim,Array_Input,Matrix)
        Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
!
        End Program prgm_03_01


        Subroutine Packed2Matrix_ColumnWise(M,N,ArrayIn,AMatOut)
!
!       This subroutine accepts an array, ArrayIn, that is M*N long. It then
!       takes those elements and converts them to the M-by-N matrix AMatOut
!       such that the elements in ArrayIn are interpreted as a packed
!       column-wise form of the matrix AMatOut.
!
        Implicit None
        Integer,Intent(In)::M,N
        Real,Dimension(N*M),Intent(In)::ArrayIn
        Real,Dimension(M,N),Intent(Out)::AMatOut
!
        Integer::i,j,k
!
!       Loop through the elements of AMatOut and fill them appropriately from
!       Array_Input.
!
!
! *************************************************************************
        do i=1,M
          do j=1,N
            
            AMatOut(i,j) = ArrayIn((j-1)*m + i)
          endDo
        endDo
!
!
        Return
        End Subroutine Packed2Matrix_ColumnWise

        Subroutine Packed2Matrix_RowWise(M,N,ArrayIn,AMatOut)
!
!       This subroutine accepts an array, ArrayIn, that is M*N long. It then
!       takes those elements and converts them to the M-by-N matrix AMatOut
!       such that the elements in ArrayIn are interpreted as a packed
!       row-wise form of the matrix AMatOut.
!
        Implicit None
        Integer,Intent(In)::M,N
        Real,Dimension(N*M),Intent(In)::ArrayIn
        Real,Dimension(M,N),Intent(Out)::AMatOut
!
        Integer::i,j,k
!
!       Loop through the elements of AMatOut and fill them appropriately from
!       Array_Input.
!
!
!
        do i=1,M
          do j=1,N
            AMatOut(i,j) = ArrayIn((i-1)*m + j)
          endDo
        endDo
!
        Return
        End Subroutine Packed2Matrix_RowWise



        Subroutine Print_Matrix_Full_Real(Mat, MDim, NDim)

        Implicit none
        integer, intent(IN):: MDim, NDim
        integer:: i,j
        real, dimension(MDim,NDim), Intent(IN)::Mat

9999    format(I6, 5x, 3(2x,f5.1))
9998    format(I6, 1x,10f15.6)
9997    format(1x,I6)
        write(*,*) (j, j=1, NDim) 
        do i=1, MDim
          write(*,9998) i, (Mat(i,j), j=1,NDim)
        endDo

        end subroutine
