module LIB_VTK_IO
!----------------------------------------------------------------------------------------------------------------------------------
implicit none
private

! functions for VTK XML
public:: VTK_INI_XML
public:: VTK_GEO_XML
public:: VTK_DAT_XML
public:: VTK_VAR_XML
public:: VTK_END_XML
public:: write_VTR
public:: PODIN_to_VTR
public:: PODOUT_to_VTR
public:: FIELD_to_PODIN

! portable kind-precision
public:: R16P, FR16P
public:: R8P,  FR8P
public:: R4P,  FR4P
public:: R_P,  FR_P
public:: I8P,  FI8P
public:: I4P,  FI4P
public:: I2P,  FI2P
public:: I1P,  FI1P
public:: I_P,  FI_P

!----------------------------------------------------------------------------------------------------------------------------------
! overloading of VTK_GEO_XML
interface VTK_GEO_XML
  module procedure VTK_GEO_XML_RECT_R8, & ! real(R8P) RectilinearGrid
                   VTK_GEO_XML_RECT_R4, & ! real(R4P) RectilinearGrid
                   VTK_GEO_XML_CLOSEP     ! closing tag "Piece" function
endinterface
! overloading of VTK_VAR_XML
interface VTK_VAR_XML
  module procedure VTK_VAR_XML_SCAL_R8, & ! real(R8P)    scalar
                   VTK_VAR_XML_SCAL_R4, & ! real(R4P)    scalar
                   VTK_VAR_XML_SCAL_I8, & ! integer(I8P) scalar
                   VTK_VAR_XML_SCAL_I4, & ! integer(I4P) scalar
                   VTK_VAR_XML_SCAL_I2, & ! integer(I2P) scalar
                   VTK_VAR_XML_SCAL_I1    ! integer(I1P) scalar
endinterface
!----------------------------------------------------------------------------------------------------------------------------------

!!Real precision definitions:
!!
integer, parameter:: R16P = selected_real_kind(33,4931) ! 33  digits, range $[\pm 10^{-4931}  ,\pm 10^{+4931}   -1]$
integer, parameter:: R8P  = selected_real_kind(15,307)  ! 15  digits, range $[\pm 10^{-307}~~ ,\pm 10^{+307}~~  -1]$
integer, parameter:: R4P  = selected_real_kind(6,37)    ! 6~~~digits, range $[\pm 10^{-37}~~~~,\pm 10^{+37}~~~~ -1]$
integer, parameter:: R_P  = R8P                         ! default real precision
!!Integer precision definitions:
!!
integer, parameter:: I8P  = selected_int_kind(18)       ! range $[-2^{63} ,+2^{63}  -1]$
integer, parameter:: I4P  = selected_int_kind(9)        ! range $[-2^{31} ,+2^{31}  -1]$
integer, parameter:: I2P  = selected_int_kind(4)        ! range $[-2^{15} ,+2^{15}  -1]$
integer, parameter:: I1P  = selected_int_kind(2)        ! range $[-2^{7}~~,+2^{7}~~ -1]$
integer, parameter:: I_P  = I4P                         ! default integer precision
!!
!!Besides the kind parameters there are also the format parameters useful for writing in a well-ascii-format numeric variables.
!!Also these parameters are public.
!!
!! Real output formats:
!!
character(10), parameter:: FR16P = '(E41.33E4)'         ! R16P  output format
character(10), parameter:: FR8P  = '(E23.15E3)'         ! R8P   output format
character(9),  parameter:: FR4P  = '(E14.6E2)'          ! R4P   output format
character(10), parameter:: FR_P  = '(E23.15E3)'         ! R\_P  output format
!! Integer output formats:
!!
character(5), parameter:: FI8P  = '(I21)'               ! I8P  output format
character(5), parameter:: FI4P  = '(I12)'               ! I4P  output format
character(4), parameter:: FI2P  = '(I7)'                ! I2P  output format
character(4), parameter:: FI1P  = '(I5)'                ! I1P  output format
character(5), parameter:: FI_P  = '(I12)'               ! I\_P output format
!!
!!\LIBVTKIO uses a small set of internal variables that are private (not accessible from the outside). The following are
!! private variables:
!!
integer(I4P), parameter:: maxlen       = 500         ! max number of characters os static string
character(1), parameter:: end_rec      = char(10)    ! end-character for binary-record finalize
integer(I4P), parameter:: f_out_ascii  = 0           ! ascii-output-format parameter identifier
integer(I4P), parameter:: f_out_binary = 1           ! binary-output-format parameter identifier
integer(I4P)::            f_out        = f_out_ascii ! current output-format (initialized to ascii format)
character(len=maxlen)::   topology                   ! mesh topology
integer(I4P)::            Unit_VTK                   ! internal logical unit
integer(I4P)::            Unit_VTK_Append            ! internal logical unit for raw binary XML append file
integer(I4P)::            N_Byte                     ! number of byte to be written/read
real(R8P)::               tipo_R8                    ! prototype of R8P real
real(R4P)::               tipo_R4                    ! prototype of R4P real
integer(I8P)::            tipo_I8                    ! prototype of I8P integer
integer(I4P)::            tipo_I4                    ! prototype of I4P integer
integer(I2P)::            tipo_I2                    ! prototype of I2P integer
integer(I1P)::            tipo_I1                    ! prototype of I1P integer
integer(I4P)::            ioffset                    ! offset pointer
integer(I4P)::            indent                     ! indent pointer
!----------------------------------------------------------------------------------------------------------------------------------

!!In the following chapters there is the API reference of all functions of \LIBVTKIO.
contains

    function GetUnit() result(Free_Unit)
    !--------------------------------------------------------------------------------------------------------------------------------
    !!The GetUnit function is used for getting a free logic unit. The users of \LIBVTKIO does not know which is
    !!the logical unit: \LIBVTKIO handels this information without boring the users. The logical unit used is safe-free: if the
    !!program calling \LIBVTKIO has others logical units used \LIBVTKIO will never use these units, but will choice one that is free.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P):: Free_Unit ! free logic unit
    integer(I4P):: n1        ! counter
    integer(I4P):: ios       ! inquiring flag
    logical(4)::   lopen     ! inquiring flag
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    !!The following is the code snippet of GetUnit function: the units 0, 5, 6, 9 and all non-free units are discarded.
    !!
    !(\doc)codesnippet
    Free_Unit = -1_I4P                                      ! initializing free logic unit
    n1=1_I4P                                                ! initializing counter
    do
    if ((n1/=5_I4P).AND.(n1/=6_I4P).AND.(n1/=9_I4P)) then
        inquire (unit=n1,opened=lopen,iostat=ios)           ! verify logic units
        if (ios==0_I4P) then
        if (.NOT.lopen) then
            Free_Unit = n1                                  ! assignment of free logic
            return
        endif
        endif
    endif
    n1=n1+1_I4P                                           ! updating counter
    enddo
    return
    !(doc/)codesnippet
    !!GetUnit function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope to public.
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction GetUnit

    function Upper_Case(string)
    !--------------------------------------------------------------------------------------------------------------------------------
    !!The Upper\_Case function converts the lower case characters of a string to upper case one. \LIBVTKIO uses this function in
    !!order to achieve case-insensitive: all character variables used within \LIBVTKIO functions are pre-processed by
    !!Uppper\_Case function before these variables are used. So the users can call \LIBVTKIO functions whitout pay attention of the
    !!case of the kwywords passed to the functions: calling the function VTK\_INI with the string \code{E_IO = VTK_INI('Ascii',...)}
    !!or with the string  \code{E_IO = VTK_INI('AscII',...)} is equivalent.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(IN):: string     ! string to be converted
    character(len=len(string))::   Upper_Case ! converted string
    integer::                      n1         ! characters counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    !!The following is the code snippet of Upper\_Case function.
    !!
    !(\doc)codesnippet
    Upper_Case = string
    do n1=1,len(string)
    select case(ichar(string(n1:n1)))
    case(97:122)
        Upper_Case(n1:n1)=char(ichar(string(n1:n1))-32) ! Upper case conversion
    endselect
    enddo
    return
    !(doc/)codesnippet
    !!Upper\_Case function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope
    !!to public.
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction Upper_Case

    function VTK_INI_XML(output_format,filename,mesh_topology,nx1,nx2,ny1,ny2,nz1,nz2) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !!The VTK\_INI\_XML function is used for initializing file. This function must be the first to be called.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none
    !--------------------------------------------------------------------------------------------------------------------------------
    character(*), intent(IN)::           output_format ! output format: ASCII or BINARY
    character(*), intent(IN)::           filename      ! file name
    character(*), intent(IN)::           mesh_topology ! mesh topology
    integer(I4P), intent(IN), optional:: nx1,nx2       ! initial and final nodes of x axis
    integer(I4P), intent(IN), optional:: ny1,ny2       ! initial and final nodes of y axis
    integer(I4P), intent(IN), optional:: nz1,nz2       ! initial and final nodes of z axis
    integer(I4P)::                       E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::              s_buffer      ! buffer string
    !--------------------------------------------------------------------------------------------------------------------------------

    topology = trim(mesh_topology)
    Unit_VTK=GetUnit()
    select case(trim(Upper_Case(output_format)))
    case('ASCII')
        f_out = f_out_ascii
        open(unit   = Unit_VTK,       &
            file   = trim(filename), &
            form   = 'FORMATTED',    &
            access = 'SEQUENTIAL',   &
            action = 'WRITE',        &
            iostat = E_IO)
        ! writing header of file
        write(unit=Unit_VTK,fmt='(A)', iostat=E_IO)'<?xml version="1.0"?>'
        write(unit=Unit_VTK,fmt='(A)', iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="BigEndian">'
        indent = 2
        select case(trim(topology))
            case('RectilinearGrid','StructuredGrid')
                write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//' WholeExtent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            case('UnstructuredGrid')
                write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//'>'
        endselect
        indent = indent + 2
    case('BINARY')
        f_out = f_out_binary
        open(unit       = Unit_VTK,         &
            file        = trim(filename),   &
            form        = 'unformatted',    &
            access      = 'stream',         &
            convert     = 'big_endian',     &
            action      = 'WRITE')
        ! writing header of file
        write(unit=Unit_VTK,                     iostat=E_IO)'<?xml version="1.0"?>'//end_rec
        write(unit=Unit_VTK,                     iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="BigEndian">'//end_rec
        indent = 2
        select case(trim(topology))
            case('RectilinearGrid','StructuredGrid')
                write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//' WholeExtent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            case('UnstructuredGrid')
                write(s_buffer,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//'>'
            endselect
        write(unit=Unit_VTK,                     iostat=E_IO)trim(s_buffer)//end_rec
        indent = indent + 2
        Unit_VTK_Append=GetUnit()
        ! opening the SCRATCH file used for appending raw binary data
        open(unit       = Unit_VTK_Append,      &
            ! file        = 'field_append.vtr',   & ! uncomment this for debugging purposes
            form        = 'unformatted',        &
            access      = 'stream',             &
            convert     = 'big_endian',         &
            status     = 'SCRATCH')
        ioffset = 0 ! initializing offset puntator
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_INI_XML

    function VTK_GEO_XML_RECT_R8(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving mesh; topology = RectilinearGrid (R8P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: nx1,nx2    ! initial and final nodes of x axis
    integer(I4P), intent(IN):: ny1,ny2    ! initial and final nodes of y axis
    integer(I4P), intent(IN):: nz1,nz2    ! initial and final nodes of z axis
    real(R8P),    intent(IN):: X(nx1:nx2) ! x coordinates
    real(R8P),    intent(IN):: Y(ny1:ny2) ! y coordinates
    real(R8P),    intent(IN):: Z(nz1:nz2) ! z coordinates
    integer(I4P)::             E_IO       ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer   ! buffer string
    integer(I4P)::             n1         ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            indent = indent + 2
            
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<Coordinates>'
            indent = indent + 2
            
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="X" format="ascii">'
            write(unit=Unit_VTK,fmt=FR8P,               iostat=E_IO)(X(n1),n1=nx1,nx2)
            
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Y" format="ascii">'
            write(unit=Unit_VTK,fmt=FR8P,               iostat=E_IO)(Y(n1),n1=ny1,ny2)
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Z" format="ascii">'
            write(unit=Unit_VTK,fmt=FR8P,               iostat=E_IO)(Z(n1),n1=nz1,nz2)
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            indent = indent - 2
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</Coordinates>'
        case(f_out_binary)
            write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            indent = indent + 2
            write(unit=Unit_VTK,                   iostat=E_IO)trim(s_buffer)//end_rec
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<Coordinates>'//end_rec
            indent = indent + 2
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="X" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (nx2-nx1+1)*sizeof(Tipo_R8)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'R8',(nx2-nx1+1)
            
            write(unit=Unit_VTK_Append)(X(n1),n1=nx1,nx2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Y" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (ny2-ny1+1)*sizeof(Tipo_R8)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R8',ny2-ny1+1
            
            write(unit=Unit_VTK_Append)(Y(n1),n1=ny1,ny2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Z" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (nz2-nz1+1)*sizeof(Tipo_R8)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R8',nz2-nz1+1
            
            write(unit=Unit_VTK_Append)(Z(n1),n1=nz1,nz2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            indent = indent - 2
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</Coordinates>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_GEO_XML_RECT_R8

    function VTK_GEO_XML_RECT_R4(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving mesh; topology = RectilinearGrid (R4P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: nx1,nx2    ! initial and final nodes of x axis
    integer(I4P), intent(IN):: ny1,ny2    ! initial and final nodes of y axis
    integer(I4P), intent(IN):: nz1,nz2    ! initial and final nodes of z axis
    real(R4P),    intent(IN):: X(nx1:nx2) ! x coordinates
    real(R4P),    intent(IN):: Y(ny1:ny2) ! y coordinates
    real(R4P),    intent(IN):: Z(nz1:nz2) ! z coordinates
    integer(I4P)::             E_IO       ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer   ! buffer string
    integer(I4P)::             n1         ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            indent = indent + 2
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<Coordinates>'
            indent = indent + 2
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="X" format="ascii">'
            write(unit=Unit_VTK,fmt=FR4P,               iostat=E_IO)(X(n1),n1=nx1,nx2)
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Y" format="ascii">'
            write(unit=Unit_VTK,fmt=FR4P,               iostat=E_IO)(Y(n1),n1=ny1,ny2)
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Z" format="ascii">'
            write(unit=Unit_VTK,fmt=FR4P,               iostat=E_IO)(Z(n1),n1=nz1,nz2)
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</DataArray>'
            indent = indent - 2
            write(unit=Unit_VTK,fmt='(A)',              iostat=E_IO)repeat(' ',indent)//'</Coordinates>'
        case(f_out_binary)
            write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
            indent = indent + 2
            write(unit=Unit_VTK,                   iostat=E_IO)trim(s_buffer)//end_rec
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<Coordinates>'//end_rec
            indent = indent + 2
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="X" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (nx2-nx1+1)*sizeof(Tipo_R4)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R4',nx2-nx1+1
            
            write(unit=Unit_VTK_Append)(X(n1),n1=nx1,nx2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Y" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (ny2-ny1+1)*sizeof(Tipo_R4)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R4',ny2-ny1+1
            
            write(unit=Unit_VTK_Append)(Y(n1),n1=ny1,ny2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            write(s_buffer,fmt='(I8)',             iostat=E_IO)ioffset
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Z" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = (nz2-nz1+1)*sizeof(Tipo_R4)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R4',nz2-nz1+1
            
            write(unit=Unit_VTK_Append)(Z(n1),n1=nz1,nz2)
            
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
            indent = indent - 2
            write(unit=Unit_VTK,                   iostat=E_IO)repeat(' ',indent)//'</Coordinates>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_GEO_XML_RECT_R4

    function VTK_GEO_XML_CLOSEP() result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for closing mesh block data.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P):: E_IO ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    indent = indent - 2
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Piece>'
        case(f_out_binary)
            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Piece>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_GEO_XML_CLOSEP

    function VTK_DAT_XML(var_location,var_block_action) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !!This function \MaiuscolettoBS{must} be called before saving the data related to geometric mesh. This function initializes
    !!the saving of data variables indicating the \emph{type} of variables that will be saved.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    character(*), intent(IN):: var_location     ! location of saving variables: CELL for cell-centered, NODE for node-centered
    character(*), intent(IN):: var_block_action ! variables block action: OPEN or CLOSE block
    integer(I4P)::             E_IO             ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            select case(trim(Upper_Case(var_location)))
                case('CELL')
                    select case(trim(Upper_Case(var_block_action)))
                        case('OPEN')
                            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<CellData>'
                            indent = indent + 2
                        case('CLOSE')
                            indent = indent - 2
                            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</CellData>'
                    endselect
                case('NODE')
                    select case(trim(Upper_Case(var_block_action)))
                        case('OPEN')
                            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<PointData>'
                            indent = indent + 2
                        case('CLOSE')
                            indent = indent - 2
                            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</PointData>'
                    endselect
            endselect
        case(f_out_binary)
            select case(trim(Upper_Case(var_location)))
                case('CELL')
                    select case(trim(Upper_Case(var_block_action)))
                        case('OPEN')
                            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<CellData>'//end_rec
                            indent = indent + 2
                        case('CLOSE')
                            indent = indent - 2
                            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</CellData>'//end_rec
                    endselect
                case('NODE')
                    select case(trim(Upper_Case(var_block_action)))
                        case('OPEN')
                            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<PointData>'//end_rec
                            indent = indent + 2
                        case('CLOSE')
                            indent = indent - 2
                            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</PointData>'//end_rec
                    endselect
            endselect
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_DAT_XML

    function VTK_VAR_XML_SCAL_R8(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (R8P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    real(R8P),    intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)(var(n1),n1=1,NC_NN)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_R8)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'R8',NC_NN
            
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
            
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_R8

    function VTK_VAR_XML_SCAL_R4(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (R4P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    real(R4P),    intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FR4P, iostat=E_IO)var
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_R4)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            
            write(unit=Unit_VTK_Append)N_Byte,'R4',NC_NN
            
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
            
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_R4

    function VTK_VAR_XML_SCAL_I8(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (I8P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    integer(I8P), intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FI8P, iostat=E_IO)var
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_I8)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'I8',NC_NN
    
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
    
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_I8

    function VTK_VAR_XML_SCAL_I4(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (I4P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    integer(I4P), intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FI4P, iostat=E_IO)var
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_I4)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'I4',NC_NN
    
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
    
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_I4

    function VTK_VAR_XML_SCAL_I2(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (I2P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    integer(I2P), intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FI2P, iostat=E_IO)var
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_I2)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'I2',NC_NN
    
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
    
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_I2

    function VTK_VAR_XML_SCAL_I1(NC_NN,varname,var) result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !! Function for saving scalar variable (I1P).
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
    character(*), intent(IN):: varname      ! variable name
    integer(I1P), intent(IN):: var(1:NC_NN) ! variable to be saved
    integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(len=maxlen)::    s_buffer     ! buffer string
    integer(I4P)::             n1           ! counter
    !--------------------------------------------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
            write(unit=Unit_VTK,fmt=FI1P, iostat=E_IO)var
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
        case(f_out_binary)
            write(s_buffer,fmt='(I8)', iostat=E_IO)ioffset
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="1" format="appended" offset="',trim(s_buffer),'">'//end_rec
            N_Byte  = NC_NN*sizeof(Tipo_I1)
            ioffset = ioffset + sizeof(Tipo_I4) + N_Byte
            write(unit=Unit_VTK_Append)N_Byte,'I1',NC_NN
    
            write(unit=Unit_VTK_Append)(var(n1),n1=1,NC_NN)
    
            write(unit=Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    endselect
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_VAR_XML_SCAL_I1

    function VTK_END_XML() result(E_IO)
    !--------------------------------------------------------------------------------------------------------------------------------
    !!This function is used to finalize the file opened. The \LIBVTKIO manages the file unit without the user's action.
    !--------------------------------------------------------------------------------------------------------------------------------

    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    integer(I4P)::              E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
    character(2)::              var_type  ! var\_type = R8,R4,I8,I4,I2,I1
    real(R8P),    allocatable:: v_R8(:)   ! R8 vector for IO in AppendData
    real(R4P),    allocatable:: v_R4(:)   ! R4 vector for IO in AppendData
    integer(I8P), allocatable:: v_I8(:)   ! I8 vector for IO in AppendData
    integer(I4P), allocatable:: v_I4(:)   ! I4 vector for IO in AppendData
    integer(I2P), allocatable:: v_I2(:)   ! I2 vector for IO in AppendData
    integer(I1P), allocatable:: v_I1(:)   ! I1 vector for IO in AppendData
    integer(I4P)::              N_v       ! vector dimension
    integer(I4P)::              n1        ! counter
    !--------------------------------------------------------------------------------------------------------------------------------
    select case(f_out)
        case(f_out_ascii)
            indent = indent - 2
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</'//trim(topology)//'>'
            write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'</VTKFile>'
        case(f_out_binary)
            indent = indent - 2
            write(unit  =Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</'//trim(topology)//'>'//end_rec
            write(unit  =Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<AppendedData encoding="raw">'//end_rec
            write(unit  =Unit_VTK,       iostat=E_IO)'_'
            endfile(unit=Unit_VTK_Append)
            rewind(unit =Unit_VTK_Append)
            do
                read(unit=Unit_VTK_Append,end=100)N_Byte,var_type,N_v
                select case(var_type)
                    case('R8')
                        allocate(v_R8(1:N_v))
                        read(unit =Unit_VTK_Append)(v_R8(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_R8(n1),n1=1,N_v)
                        deallocate(v_R8)
                    case('R4')
                        allocate(v_R4(1:N_v))
                        read(unit =Unit_VTK_Append)(v_R4(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_R4(n1),n1=1,N_v)
                        deallocate(v_R4)
                    case('I8')
                        allocate(v_I8(1:N_v))
                        read(unit =Unit_VTK_Append)(v_I8(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_I8(n1),n1=1,N_v)
                        deallocate(v_I8)
                    case('I4')
                        allocate(v_I4(1:N_v))
                        read(unit =Unit_VTK_Append)(v_I4(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_I4(n1),n1=1,N_v)
                        deallocate(v_I4)
                    case('I2')
                        allocate(v_I2(1:N_v))
                        read(unit =Unit_VTK_Append)(v_I2(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_I2(n1),n1=1,N_v)
                        deallocate(v_I2)
                    case('I1')
                        allocate(v_I1(1:N_v))
                        read(unit =Unit_VTK_Append)(v_I1(n1),n1=1,N_v)
                        write(unit=Unit_VTK)N_Byte,(v_I1(n1),n1=1,N_v)
                        deallocate(v_I1)
                endselect
            enddo
            100 continue
            write(unit=Unit_VTK,iostat=E_IO)end_rec
            write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</AppendedData>'//end_rec
            write(unit=Unit_VTK,iostat=E_IO)'</VTKFile>'//end_rec
            ! closing AppendData file
            close(unit=Unit_VTK_Append)
    endselect
    close(unit=Unit_VTK,iostat=E_IO)
    return
    !--------------------------------------------------------------------------------------------------------------------------------
    endfunction VTK_END_XML
  
    subroutine write_VTR(formOut, fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite,&
                        uWriteNorm,vWriteNorm,wWriteNorm,fileName)
    !subroutine write_VTR(fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite)
    !call this subroutine as
    !   call write_VTR(fcounter,nnx,nny,nnz,xArr,yArr,zArr)
    
    character(*), intent(IN):: formOut  !
    character(*), intent(IN):: fileName !
    integer(I4P), intent(IN):: fcounter !
    integer(I4P), intent(IN):: nnx      ! initial and final nodes of x axis
    integer(I4P), intent(IN):: nny      ! initial and final nodes of y axis
    integer(I4P), intent(IN):: nnz      ! initial and final nodes of z axis
    real(R8P),    intent(IN):: xArr(:)  ! x coordinates
    real(R8P),    intent(IN):: yArr(:)  ! y coordinates
    real(R8P),    intent(IN):: zArr(:)  ! z coordinates
    
    real(I4P),    intent(IN):: uWrite(:)
    real(I4P),    intent(IN):: vWrite(:)
    real(I4P),    intent(IN):: wWrite(:)
    !real(I4P),    intent(IN):: tWrite(:)
    !real(I4P),    intent(IN):: phiWrite(:)
    
    real(I4P), allocatable, intent(IN):: uWriteNorm(:)
    real(I4P), allocatable, intent(IN):: vWriteNorm(:)
    real(I4P), allocatable, intent(IN):: wWriteNorm(:)
    !real(I4P),    intent(IN):: tWriteNorm(:)
    !real(I4P),    intent(IN):: phiWriteNorm(:)
    
    
    !--------------------------------------------------------------------------------------------------------------------------------
    !! this is where read_write calls functions from the LIB_VTK_IO.f90 library to run for converting from .vtk format to .vtr format
    !--------------------------------------------------------------------------------------------------------------------------------
    

    !truncName = buffer(5:index(trim(adjustl(buffer)),'.field')-1)
    !write(fchar,'(I4)') fcounter
    !truncName = buffer(5:index(trim(adjustl(buffer)),'.field')-1)
    !write(truncName,*) truncName,fcounter
    !print*, trim(adjustl(truncName))//fchar//'.vtr'
    
    character fchar*40
    integer(kind = 4) :: iErr = 0 !To store the output of each VTK_LIB_IO command
        
    write(fchar, '(I4)')fcounter
    iErr = VTK_INI_XML(output_format = trim(adjustl(formOut)), filename = trim(adjustl(fileName))//'.vtr', &
		    mesh_topology = 'RectilinearGrid', nx1=1, nx2=nnx, ny1=1, ny2=nny, nz1=1, nz2=nnz)
        
    iErr = VTK_GEO_XML(nx1=1, nx2=nnx, ny1=1, ny2=nny, nz1=1, nz2=nnz, X=xArr, Y=yArr, Z=zArr)
    
    iErr = VTK_DAT_XML(var_location = 'node', var_block_action = 'OPEN')
    
    iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'u', var = uWrite)
    iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'v', var = vWrite)
    iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'w', var = wWrite)
    !iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'tprime', var = tWrite)
    !iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'phiprime', var = phiWrite)
    
!--------------------------------------------------------------------------------------------------------------------------------
! Write the normals
!--------------------------------------------------------------------------------------------------------------------------------
    
    if(allocated(uWriteNorm))then
        iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'uprimenorm', var = uWriteNorm)
    end if
    
    if(allocated(uWriteNorm))then
        iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'vprimenorm', var = vWriteNorm)
    end if
    
    if(allocated(uWriteNorm))then
        iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'wprimenorm', var = wWriteNorm)
    end if
    
    !iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'tprimenorm', var = tWriteNorm)
    !iErr = VTK_VAR_XML(NC_NN = nnx*nny*nnz, varname = 'phiprimenorm', var = phiWriteNorm)
    
    iErr = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
    
    iErr = VTK_GEO_XML()
    
    iErr = VTK_END_XML()
        !Binary output issue is here
	    !VTK_END_XML begins on Line 3460
    return
    endsubroutine write_VTR

    subroutine PODIN_to_VTR(formOut)

        implicit none
        
        !#### Flags getting passed in
        character*10, intent(IN):: formOut

        !#### Variables dimensions
        character (len=100) :: Buffer                           !A character Buffer to read input argument
        character (len=10)  :: xd,yd
        character*20 :: fileName
        integer :: i,j,k,keff,fcounter                          !Counter variables
        integer :: iz									        !counter variables
        integer :: stat                                         !End of line variable
        integer :: UnitNum                                      !UnitNum
        !#### GRID PARAMETERS
        integer :: nnx, nny, nnz      					        !x, y, and z grid dimensions 
        integer :: nxy                					        !Number of grid points in a horizontal plane
        real    :: xl, yl, zl         	  					    ! x, y, and z domain sizes
        real    :: dx, dy, dz            					    ! x, y, and z grid lengths
        integer :: nscalars 							        !Number of scalars

        !####Number of z levels used
        !integer(kind = I4P) :: nnzused                     
 
        real(kind = R8P), dimension(:), allocatable :: xArr, yArr,zArr		                              !Array of x and y locations on the grid	                            						
        real, allocatable, dimension(:,:,:)         :: u,v,w,t,e,p,q,c,phi                                !Parameter dimension      
        real, allocatable, dimension(:,:,:)         :: up, wp, vp, tp, phip
        real, allocatable, dimension(:)             :: TMean, phiMean
        real, allocatable, dimension(:)             :: varwp,varup,varvp,vartp,varphip
        real, allocatable, dimension(:,:)           :: velMean

        !Plane Arrays in the format to be written into vec files
        real(kind= R4P), dimension(:), allocatable  :: uWrite,vWrite,wWrite,tWrite,phiWrite     
                            	
        real    :: ugtop, ugbot, vgtop, vgbot
        real    :: dtdzf, divgls, fcor, amonin, utau
        real    :: time_start, dt, z0
        
        character*40 :: fchar
        integer :: count
        
        real(R4P), allocatable :: uWriteNorm(:)
        real(R4P), allocatable :: vWriteNorm(:)
        real(R4P), allocatable :: wWriteNorm(:)
        
    !begin looping through all availible POD_in files
    count = 0
    fcounter = 0
    open(unit=10,file="FileList.txt")
    read(10,*,iostat=stat) buffer
    
    do while (stat .eq. 0)
        
        fileName = buffer(1:index(trim(adjustl(buffer)),'.PODIN')-1)
        
        fcounter = fcounter + 1
        write(fchar,'(I4)') fcounter
        open(unit=12,file=trim(adjustl(buffer)))
        ! eventually this will change to:
        ! open(unit=10,file="POD_"//trim(adjustl(fchar))//".PODIN")
        
        ! quickly read through the header of the file
        read(12,'(a)') Buffer, Buffer, Buffer, Buffer, Buffer
        
        ! read in the grid size
        read(12,'(I4)') nnx
        read(12,'(I4)') nny
        read(12,'(I4)') nnz
        
        if(fcounter == 1) then
            allocate(xArr(nnx))
            allocate(yArr(nny))
            allocate(zArr(nnz))        
            allocate(uWrite(nnx*nny*nnz))
            allocate(vWrite(nnx*nny*nnz))
            allocate(wWrite(nnx*nny*nnz))
        end if
        
        do k=1,nnz
            do j=1,nny
                do i=1,nnx
                    read(12,*) xArr(i), yArr(j), zArr(k), uWrite((k-1)*nnx*nny+(j-1)*nnx+i), vWrite((k-1)*nnx*nny+(j-1)*nnx+i), wWrite((k-1)*nnx*nny+(j-1)*nnx+i)
                enddo
            enddo
        enddo
        
        !convert this PODIN file to *.VTR
        call write_VTR(formOut,fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite,uWriteNorm,vWriteNorm,wWriteNorm,fileName)
        
        close(12)
        read(10,*,iostat=stat)buffer
        
    enddo      
    
    close(12)
    close(10)
    
    endsubroutine PODIN_to_VTR

    subroutine PODOUT_to_VTR(formOut)

        implicit none
        
        !#### Flags getting passed in
        character*10, intent(IN):: formOut

        !#### Variables dimensions
        character (len=100) :: Buffer                           !A character Buffer to read input argument
        character (len=10)  :: xd,yd
        character (len=100) :: fileName
        
        integer :: i,j,k,keff,fcounter                          !Counter variables
        integer :: iz									        !counter variables
        integer :: stat                                         !End of line variable
        integer :: UnitNum                                      !UnitNum
        !#### GRID PARAMETERS
        integer :: nnx, nny, nnz      					        !x, y, and z grid dimensions 
        integer :: nxy                					        !Number of grid points in a horizontal plane
        real    :: xl, yl, zl         	  					    ! x, y, and z domain sizes
        real    :: dx, dy, dz            					    ! x, y, and z grid lengths
        integer :: nscalars 							        !Number of scalars                     
 
        real(kind = R8P), dimension(:), allocatable :: xArr, yArr,zArr		                              !Array of x and y locations on the grid	                            						
        real, allocatable, dimension(:,:,:)         :: u,v,w,t,e,p,q,c,phi                                !Parameter dimension      
        real, allocatable, dimension(:,:,:)         :: up, wp, vp, tp, phip
        real, allocatable, dimension(:)             :: TMean, phiMean
        real, allocatable, dimension(:)             :: varwp,varup,varvp,vartp,varphip
        real, allocatable, dimension(:,:)           :: velMean

        !Plane Arrays in the format to be written into vec files
        real(kind= R4P), dimension(:), allocatable  :: uWrite,vWrite,wWrite,tWrite,phiWrite     
                            	
        real    :: ugtop, ugbot, vgtop, vgbot
        real    :: dtdzf, divgls, fcor, amonin, utau
        real    :: time_start, dt, z0
        
        character*40 :: fchar
        integer :: count
        
        real(R4P), allocatable :: uWriteNorm(:)
        real(R4P), allocatable :: vWriteNorm(:)
        real(R4P), allocatable :: wWriteNorm(:)
        
        ! character*10        :: modes
        ! used for reading old file format title: PODM*_output*.PODOUT
        
    !begin looping through all availible POD_in files
    
    !modes = buffer(6:index(trim(adjustl(buffer)),'_output')-1)
    ! used for reading old file format title: PODM*_output*.PODOUT
    
    open(unit=10,file="FileList.txt")
    read(10,*,iostat=stat)buffer
    
    do while (stat .eq. 0)
        
        fileName = buffer(1:index(trim(adjustl(buffer)),'.PODOUT')-1)
        
        !write(*,*)"POD_M"//trim(adjustl(fchar))//".PODOUT"
        !open(unit=10,file="POD_M"//trim(adjustl(fchar))//".PODOUT")
        open(unit=12,file=trim(adjustl(buffer)))
        
        !read and open based on old title format
        !write(*,*)"POD_M"//trim(adjustl(modes))//"_output"//trim(adjustl(fchar))//".PODOUT"
        !open(unit=10,file="POD_M"//trim(adjustl(modes))//"_output"//trim(adjustl(fchar))//".PODOUT")
        
        ! quickly read through the header of the file
        read(12,'(a)') Buffer, Buffer, Buffer, Buffer, Buffer
        
        ! read in the grid size
        read(12,'(I4)') nnx
        read(12,'(I4)') nny
        read(12,'(I4)') nnz
        
        if(.not.(allocated(xArr)) .or. .not.(allocated(yArr)) .or. .not.(allocated(zArr)) .or. &
               .not.(allocated(uWrite)) .or. .not.(allocated(vWrite)) .or. .not.(allocated(wWrite))) then
            
            allocate(xArr(nnx))
            allocate(yArr(nny))
            allocate(zArr(nnz))        
            allocate(uWrite(nnx*nny*nnz))
            allocate(vWrite(nnx*nny*nnz))
            allocate(wWrite(nnx*nny*nnz))
            
        end if
        
        do k=1,nnz
            do j=1,nny
                do i=1,nnx
                    read(12,*) xArr(i), yArr(j), zArr(k), uWrite((k-1)*nnx*nny+(j-1)*nnx+i), vWrite((k-1)*nnx*nny+(j-1)*nnx+i), wWrite((k-1)*nnx*nny+(j-1)*nnx+i)
                enddo
            enddo
        enddo
        
        !convert this PODOUT file to *.VTR
        call write_VTR(formOut,fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite,uWriteNorm,vWriteNorm,wWriteNorm,fileName)
        
        close(12)
        read(10,*,iostat=stat)buffer
        
    end do
    
    endsubroutine PODOUT_to_VTR
    
    subroutine FIELD_to_PODIN
    
        implicit none

        !#### Real precision definitions:
        integer, parameter:: R16P = selected_real_kind(33,4931) ! 33 digits, range $[\pm 10^{-4931}  ,\pm 10^{+4931}   -1]$
        integer, parameter:: R8P  = selected_real_kind(15,307)  ! 15 digits, range $[\pm 10^{-307}~~ ,\pm 10^{+307}~~  -1]$
        integer, parameter:: R4P  = selected_real_kind(6,37)    ! 6  digits, range $[\pm 10^{-37}~~~~,\pm 10^{+37}~~~~ -1]$
        integer, parameter:: R_P  = R8P                         ! default real precision

        !#### Integer precision definitions:

        integer, parameter:: I8P  = selected_int_kind(18)       ! range $[-2^{63} ,+2^{63}  -1]$
        integer, parameter:: I4P  = selected_int_kind(9)        ! range $[-2^{31} ,+2^{31}  -1]$
        integer, parameter:: I2P  = selected_int_kind(4)        ! range $[-2^{15} ,+2^{15}  -1]$
        integer, parameter:: I1P  = selected_int_kind(2)        ! range $[-2^{7}~~,+2^{7}~~ -1]$
        integer, parameter:: I_P  = I4P                         ! default integer precision

        !#### Variables dimensions
        character (len=100) :: Buffer                           !A character Buffer to read input argument
        character (len=10)  :: xd,yd
        integer :: i,j,k,keff,fcounter                          !Counter variables
        integer :: iz									        !counter variables
        integer :: stat                                         !End of line variable
        integer :: UnitNum                                      !UnitNum
        !#### GRID PARAMETERS
        integer :: nnx, nny, nnz      					        !x, y, and z grid dimensions 
        integer :: nxy                					        !Number of grid points in a horizontal plane
        real    :: xl, yl, zl         	  					    ! x, y, and z domain sizes
        real    :: dx, dy, dz            					    ! x, y, and z grid lengths
        integer :: nscalars 							        !Number of scalars
                    
        real(kind = I4P), dimension(:), allocatable :: xArr, yArr,zArr		                              !Array of x and y locations on the grid	                            						
        real, allocatable, dimension(:,:,:)         :: u,v,w,t,e,p,q,c,phi                                !Parameter dimension      

        !Plane Arrays in the format to be written into vec files
        real(kind= R4P), dimension(:), allocatable  :: uWrite,vWrite,wWrite,tWrite,phiWrite     
                            	
        real    :: ugtop, ugbot, vgtop, vgbot
        real    :: dtdzf, divgls, fcor, amonin, utau
        real    :: time_start, dt, z0
        real    :: ugal, vgal

        !#### Output file name
        Character (len=100) :: WriteFileName     

        !!#### Call Create fieldfiles subroutine
        !  call CreateFieldPodInFileList
  
        !#### Start the reading file and write to .vec file here
          call getarg(1,Buffer)
          fcounter = 0
  

          open(unit=10,file="FileList.txt")
          read(10,*,iostat=stat) Buffer
  
          open(unit=11,file=buffer,form="unformatted")
            read(11) time_start, nnx, nny, nnz, xl, yl, zl
            read(11) dt, z0, utau
            read(11) divgls, fcor, ugtop, ugbot, vgtop, vgbot
          close(11)
  
          dx = xl/nnx
          dy = yl/nny
          dz = zl/nnz                                 
  
          !#### Calculate x, y and z locations of the grid
          allocate(xArr(nnx))
          allocate(yArr(nny))
          allocate(zArr(nnz))
  
          do i=1,nnx
              xArr(i) = dble(i-1)*dx
              yArr(i) = dble(i-1)*dy
          end do
          do k=1,nnz
              zArr(k) = dble(k-1)*dz
          end do
  
          allocate(uWrite(nnx*nny*nnz))
          allocate(vWrite(nnx*nny*nnz))
          allocate(wWrite(nnx*nny*nnz)) 
          allocate(tWrite(nnx*nny*nnz)) 
          allocate(phiWrite(nnx*nny*nnz)) 
  
          allocate(u(nnx,nny,nnz))
          allocate(v(nnx,nny,nnz))
          allocate(w(nnx,nny,nnz))
          allocate(t(nnx,nny,nnz))
          allocate(e(nnx,nny,nnz))
          allocate(p(nnx,nny,nnz))
          allocate(phi(nnx,nny,nnz))

          print*,'allocated all memory'
          !write(*,*) 'allocated all memory'

  
        !####################################################################################################################################################
        !!!!Looping through the file name if stat = 0

        do while (stat .eq. 0)                            
  
          fcounter = fcounter + 1
  
          open(unit=11,file=Buffer,form="unformatted")
          read(11) time_start, nnx, nny, nnz, xl, yl, zl
          read(11) dt, z0, utau
          read(11) divgls, fcor, ugtop, ugbot, vgtop, vgbot
  
          !print*,'time start is ', time_start
          !print*,'time step is', dt
          !print*,'X-grid dimension: ', nnx
          !print*,'Y-grid dimension: ', nny
          !print*,'Z-grid dimension: ', nnz

          nxy = nnx*nny
 
          ugal = (max(0.,max(ugtop,ugbot))+min(0.,min(ugtop,ugbot)))*0.5
          vgal = (max(0.,max(vgtop,vgbot))+min(0.,min(vgtop,vgbot)))*0.5
  
              do iz=1,nnz
                read(11) u(:,:,iz), v(:,:,iz), w(:,:,iz), e(:,:,iz), p(1:nnx,1:nny,iz)          
              end do
      
          close(11)
  
        u = u + ugal
        v = v + vgal

        write(*,*) 'file counter is ',fcounter

  
        !####################################################################################################################################################
        !Calculate uWrite, vWrite, and wWrite with option of tWrite and phiWrite 
            do k=1,nnz
                do j = 1, nny
                    do i = 1, nnx
                    uWrite((k-1)*nnx*nny+(j-1)*nnx+i) = u(i,j,k)
                    end do
                end do
            end do

    
            do k=1,nnz
                do j = 1, nny
                    do i = 1, nnx
                    vWrite((k-1)*nnx*nny+(j-1)*nnx+i) = v(i,j,k)
                    end do
                end do
            end do
     
            do k=1,nnz
                do j = 1, nny
                    do i = 1, nnx
                    wWrite((k-1)*nnx*nny+(j-1)*nnx+i) = w(i,j,k)
                    end do
                end do
            end do
  
        !   do k=1,nnz
        !     do j = 1, nny
        !        do i = 1, nnx
        !           tWrite((k-1)*nnx*nny+(j-1)*nnx+i) = t(i,j,k)
        !        end do
        !     end do
        !   end do
        !
        !   do k=1,nnz
        !     do j = 1, nny
        !        do i = 1, nnx
        !           phiWrite((k-1)*nnx*nny+(j-1)*nnx+i) = phi(i,j,k)
        !        end do
        !     end do
        !   end do
        !####################################################################################################################################################
  
  
        !####################################################################################################################################################
        !!!Calculate uWriteNorm, vWriteNorm, and wWriteNorm
        !      do k=1,nnz
        !        do j = 1, nny
        !           do i = 1, nnx
        !              uWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = upnorm(i,j,k)
        !           end do
        !        end do
        !      end do
        !   
        !      do k=1,nnz
        !        do j = 1, nny
        !           do i = 1, nnx
        !              vWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = vpnorm(i,j,k)
        !           end do
        !        end do
        !      end do
        !   
        !      do k=1,nnz
        !        do j = 1, nny
        !           do i = 1, nnx
        !              wWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = wpnorm(i,j,k)
        !           end do
        !        end do
        !      end do
        !       
        !   do k=1,nnz
        !     do j = 1, nny
        !        do i = 1, nnx
        !           tWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = tpnorm(i,j,k)
        !        end do
        !     end do
        !   end do
        !
        !   do k=1,nnz
        !     do j = 1, nny
        !        do i = 1, nnx
        !           phiWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = phipnorm(i,j,k)
        !        end do
        !     end do
        !   end do
  
        !####################################################################################################################################################
  

            !Write the output file name
            UnitNum=20+fcounter
            Write(WriteFileName,'(a,I0,a)') 'POD_input', fcounter, '.podin' 
            open(unit=UnitNum,file=WriteFileName)

    
            !Write the comment line here
                write(UnitNum,'(a)',advance='no') 'Title: '
                write(UnitNum,'(a)') WriteFilename
                write(UnitNum,'(a)',advance='no') 'VARIABLES= "X pixel","Y pixel", "Z pixel", "U pixel", "V pixel", "W pixel", I='
                Write(UnitNum,'(i0)',advance='no') nnx
                write(UnitNum,'(a)',advance='no') ', J='
                Write(UnitNum,'(i0)',advance='no') nny
                Write(UnitNum,'(a)',advance='no') ', K='
                Write(UnitNum,'(i0)') nnz
                Write(UnitNum,'(a)') 'X,Y,Z: location of the grid point in x,y,z '
                write(UnitNum,'(a)') 'U,V,W: three dimensional velocity components'
                Write(UnitNum,'(a)') 'I,J,K: number of grid points in x,y,z domain'
                Write(UnitNum,'(i0)') nnx
                Write(UnitNum,'(i0)') nny
                Write(UnitNum,'(i0)') nnz

        

            do k=1,nnz         
                !Write x,y,u,v,and snr here
                do j=1,nny
                    do i=1,nnx
                    write(UnitNum,'(f10.7, a, f10.7, a, f10.7, a, f10.7, a, f10.7, a, f10.7)')  &
                                                xArr(i),' ', yArr(j), ' ', zArr(k), ' ', &
                                                uWrite((k-1)*nnx*nny+(j-1)*nnx+i), ' ',    &
                                                vWrite((k-1)*nnx*nny+(j-1)*nnx+i), ' ',    &
                                                wWrite((k-1)*nnx*nny+(j-1)*nnx+i)
                    end do
                end do         
            end do    

            close (UnitNum) 

  
        !####################################################################################################################################################
  
        read(10,*,iostat=stat) Buffer
        end do
        close(10)
        !####################################################################################################################################################
        !END READING FILES
    
    end subroutine
    
endmodule LIB_VTK_IO

module CREATE_LIST
public:: CreateFileList
public:: CreateFileListRPODOUT
public:: CreateFieldPodInFileList

contains
    
    subroutine CreateFileList(low,high,incr,fileName,extension)

        implicit none
        
        integer, intent(IN)      :: low,high,incr     ! the lower and upper address ranges used to populate the list of files
        character*20, intent(IN) :: fileName
        character*10, intent(IN) :: extension
        integer                  :: i

        open(unit=9,file="FileList.txt")
        
        do i=low,high,incr
            write(9,'(a,i0,a)') trim(adjustl(fileName)), i ,trim(adjustl(extension))
        end do
        
        close(9)

    end subroutine
    
    subroutine CreateFileListRPODOUT(low,high,incr,fileName,extension,mode)

        implicit none
        
        integer, intent(IN)      :: low,high,incr     ! the lower and upper address ranges used to populate the list of files
        character*20, intent(IN) :: fileName
        character*10, intent(IN) :: extension
        integer, intent(IN)      :: mode
        integer                  :: i

        open(unit=9,file="FileList.txt")
        
        do i=low,high,incr
            write(9,'(a,i0,a,i0,a)') trim(adjustl(fileName)),mode,'_output',i ,trim(adjustl(extension))
        end do
        
        close(9)

    end subroutine
    
    subroutine CreateFileListPODOUTSingle(low,high,incr,fileName,extension,modes)

        implicit none
        
        integer, intent(IN)      :: low,high,incr     ! the lower and upper address ranges used to populate the list of files
        character*20, intent(IN) :: fileName
        character*10, intent(IN) :: extension
        integer, intent(IN)      :: modes
        integer                  :: i,j

        open(unit=9,file="FileList.txt")
        
        do i=low,high,1
            write(9,'(a,i0,a)') trim(adjustl(fileName)),i,trim(adjustl(extension))
        end do
        
        close(9)

    end subroutine

    subroutine CreateFieldPodInFileList()

        implicit none
    
        character(len=30):: fieldname
        integer          :: i

          open(unit=9,file="FileList.txt")
          do i=960,970,10
          !do i=10,960,10
            write(9,'(a,i0,a)') 'test', i ,'.field'
          end do
          close(9)

    end subroutine
    
endmodule CREATE_LIST

program read_write_3d_filter
use LIB_VTK_IO
use CREATE_LIST

!This program reads the files that contain the 3d data (velocity, pressure, scalars) in the ".field" files and puts them into a tecplot file for visualization.
implicit none

character *100 :: buffer  ! A character buffer to read input argument
character *10  :: formOut ! The form the user chooses to output the data, as determined by input_VTR_POD and input_ASCII_BINARY
character *40  :: fchar
character *7   :: process
character*20   :: clow,chigh,cincr,filename
character*10 :: extension

integer      :: low,high,incr     ! the lower and upper address ranges used to populate the list of files
integer :: input 

!character truncName*50, fchar*40
integer :: i,j,k,fcounter !Counter variables
integer :: iz

! GRID PARAMETERS
integer :: nnx, nny, nnz    ! x, y, and z grid dimensions 
integer :: nxy              ! number of grid points in a horizontal plane
integer :: nscalars         !Number of scalars
integer :: stat
integer(I4P):: N, mode

real :: xl, yl, zl          ! x, y, and z domain sizes
real :: dx, dy, dz          ! x, y, and z grid lengths
real :: ugtop, ugbot, vgtop, vgbot
real :: divgls, fcor, amonin, utau
real :: time_start, dt, z0
real :: ugal, vgal
real :: pi = 3.14159265358979

real(kind=8), allocatable, dimension(:)  :: xArr, yArr,zArr !Array of x and y locations on the grid
real(kind=4), allocatable, dimension(:)  :: uWrite,vWrite,wWrite,tWrite,phiWrite !Plane Arrays in the format to be written into vtr files
real(kind=4), allocatable, dimension(:)  :: uWriteNorm,vWriteNorm,wWriteNorm,tWriteNorm,phiWriteNorm !Plane Arrays in the format to be written into vtr files
real(I4P),    allocatable, dimension(:,:):: Uf, UTU, UfTrans

real, allocatable, dimension(:,:,:) :: u,v,w,t,e,p,q,c,phi
real, allocatable, dimension(:,:,:) :: up, wp, vp, tp, phip
real, allocatable, dimension(:,:,:) :: upnorm, wpnorm, vpnorm, tpnorm, phipnorm
real, allocatable, dimension(:,:)   :: velMean
real, allocatable, dimension(:)     :: TMean, phiMean,varwp,varup,varvp,vartp,varphip
real(I4P)                               :: meanUf

nscalars = 6
N=0

!--------------------------------------------------------------------------------------------------------------------------------
! Ask the user what kind of data they want to output
!--------------------------------------------------------------------------------------------------------------------------------

open(unit=65,file="commands.txt")
	
	! determine output mode
	read(65,FI4P) input
		! input = 1 -> *.field to *.VTR
        ! input = 2 -> *.field to *.PODIN
		! input = 3 -> *.PODIN to *.VTR
		! input = 4 -> *.PODOUT to *.VTR
		
		if(input == 1) then
			process = 'FIELD'
			fileName = 'test'
			extension = '.field'
		
        else if(input == 2) then
			process = 'cPODIN'
			fileName = 'test'
			extension = '.field'
            
		else if(input == 3) then
			process = 'PODIN'
			fileName = 'POD_input'
			extension = '.PODIN'
		
		else if(input == 4) then
			process = 'PODOUT'
			fileName = 'POD_M'
			extension = '.PODOUT'
            
        else if(input == 5) then
			process = 'rPODOUT'
			fileName = 'POD_M'
			extension = '.PODOUT'
		end if
	
	
	! determine output type
	read(65,FI4P) input
		! input = 1 -> ASCII
		! input = 2 -> binary
	
		if(input == 1) then
			formOut = 'ascii'
		else if (input == 2) then
			formOut = 'binary'
		end if
	
	! get lower range
	read(65,FI4P) low
		write(clow,'(i0)') low
	
	! get upper range
	read(65, FI4P) high
		write(chigh,'(i0)') high
	
	!get step increment
	read(65, FI4P) incr
		write(cincr,'(i0)') incr
        
    ! read the mode if applicable
    if(trim(adjustl(process))=="rPODOUT") then
        read(65, FI4P) mode
    end if

close(65)

if(trim(adjustl(process))=="rPODOUT") then
    call CreateFileListRPODOUT(low,high,incr,fileName,extension,mode)
else
    call CreateFileList(low,high,incr,fileName,extension)
end if

!--------------------------------------------------------------------------------------------------------------------------------
! Populate field list
!--------------------------------------------------------------------------------------------------------------------------------

fcounter = 0
open(unit=75,file="FileList.txt")
read(75,*,iostat=stat) buffer

if(trim(adjustl(process)) == "PODIN") then
    call PODIN_to_VTR(formOut)
else if(trim(adjustl(process))=="rPODOUT") then
    call PODOUT_to_VTR(formOut)
else if(trim(adjustl(process)) == "PODOUT") then
    call PODOUT_to_VTR(formOut)
else if(trim(adjustl(process)) == "cPODIN") then
    call FIELD_to_PODIN
else if(trim(adjustl(process)) == "FIELD") then
    
    open(unit=50,file=buffer,form="unformatted")
    read(50) time_start, nnx, nny, nnz, xl, yl, zl
    read(50) dt, z0, utau
    read(50) divgls, fcor, ugtop, ugbot, vgtop, vgbot
    close(50)
    dx = xl/nnx
    dy = yl/nny
    dz = zl/nnz

    ! calculate x, y and z locations of the grid
    allocate(xArr(nnx))
    allocate(yArr(nny))
    allocate(zArr(nnz))
    do i=1,nnx
        xArr(i) = dble(i-1)*dx
        yArr(i) = dble(i-1)*dy
    end do
    do k=1,nnz
        zArr(k) = dble(k-1)*dz
    end do

    allocate(uWrite(nnx*nny*nnz))
    allocate(vWrite(nnx*nny*nnz))
    allocate(wWrite(nnx*nny*nnz)) 
    allocate(tWrite(nnx*nny*nnz)) 
    allocate(phiWrite(nnx*nny*nnz)) 

    !allocate(uWriteNorm(nnx*nny*nnz))
    !allocate(vWriteNorm(nnx*nny*nnz))
    !allocate(wWriteNorm(nnx*nny*nnz)) 
    !!allocate(tWriteNorm(nnx*nny*nnz)) 
    !!allocate(phiWriteNorm(nnx*nny*nnz))

    allocate(u(nnx,nny,nnz))
    allocate(v(nnx,nny,nnz))
    allocate(w(nnx,nny,nnz))
    allocate(t(nnx,nny,nnz))
    allocate(e(nnx,nny,nnz))
    allocate(p(nnx,nny,nnz))
    allocate(phi(nnx,nny,nnz))

    allocate(up(nnx,nny,nnz))
    allocate(vp(nnx,nny,nnz))
    allocate(wp(nnx,nny,nnz))
    allocate(tp(nnx,nny,nnz))
    allocate(phip(nnx,nny,nnz))

    allocate(upnorm(nnx,nny,nnz))
    allocate(vpnorm(nnx,nny,nnz))
    allocate(wpnorm(nnx,nny,nnz))
    allocate(tpnorm(nnx,nny,nnz))
    allocate(phipnorm(nnx,nny,nnz))

    allocate(velMean(nnz,3))
    allocate(TMean(nnz))
    allocate(phiMean(nnz))
    allocate(varwp(nnz))
    allocate(varup(nnz))
    allocate(varvp(nnz))
    allocate(vartp(nnz))
    allocate(varphip(nnz))

    write(*,*) 'allocated all memory'

    !*****************************************************BEGIN READING FILES

    do while (stat .eq. 0) 
        
        fileName = buffer(1:index(trim(adjustl(buffer)),'.field')-1)
        
        fcounter = fcounter + 1

        open(unit=50,file=buffer,form="unformatted")
        read(50) time_start, nnx, nny, nnz, xl, yl, zl
        read(50) dt, z0, utau
        read(50) divgls, fcor, ugtop, ugbot, vgtop, vgbot

        !print*,'time start is ', time_start
        !print*,'time step is', dt
    
        nxy = nnx*nny
        
        ugal = (max(0.,max(ugtop,ugbot))+min(0.,min(ugtop,ugbot)))*0.5
        vgal = (max(0.,max(vgtop,vgbot))+min(0.,min(vgtop,vgbot)))*0.5

        do iz=1,nnz
             read(50) u(:,:,iz), v(:,:,iz), w(:,:,iz), e(:,:,iz), p(1:nnx,1:nny,iz)
        end do
    
        close(50)

        u = u + ugal
        v = v + vgal
        
        ! calculate flow angle at every location
        do k = 1,nnz
           do j= 1,nny
             do i=1,nnx
        !	flowangle(i,j,k,fcounter)=atan(v(i,j,k)/u(i,j,k))*(180.0/pi)
	        phi(i,j,k)=atan(v(i,j,k)/u(i,j,k))*(180.0/pi)
             enddo
           enddo
        enddo
        
!--------------------------------------------------------------------------------------------------------------------------------
! I think this is where it takes the mean out
!--------------------------------------------------------------------------------------------------------------------------------
        !! I think this is where it takes the mean out
        !do iz=1,nnz
        !   velMean(iz,1) = sum(u(:,:,iz))/real(nxy)
        !   velMean(iz,2) = sum(v(:,:,iz))/real(nxy)
        !   velMean(iz,3) = sum(w(:,:,iz))/real(nxy)
        !   !TMean(iz) = sum(t(:,:,iz))/real(nxy)
        !   phiMean(iz) = sum(phi(:,:,iz))/real(nxy)
        !   !write(*,*) iz, ' ',  velmean(iz,1)!, ' ', countCond(1,1)/real(nxy)
        !end do
        
        write(*,*) 'file counter is ',fcounter

        !! calculate up,vp,wp
        !do k = 1,nnz
        !   varwp(k)=0.0
        !   varup(k)=0.0
        !   varvp(k)=0.0
        !   vartp(k)=0.0
        !   varphip(k)=0.0
        !   do j= 1,nny
        !     do i=1,nnx
	       ! up(i,j,k)=u(i,j,k)-velMean(k,1)
	       ! vp(i,j,k)=v(i,j,k)-velMean(k,2)
	       ! wp(i,j,k)=w(i,j,k)-velMean(k,3)
	       ! !tp(i,j,k)=t(i,j,k)-TMean(k)
	       ! phip(i,j,k)=phi(i,j,k)-phiMean(k)
	       ! varwp(k)=varwp(k)+(wp(i,j,k)*wp(i,j,k))/(nxy)
	       ! varup(k)=varup(k)+(up(i,j,k)*up(i,j,k))/(nxy)
	       ! varvp(k)=varvp(k)+(vp(i,j,k)*vp(i,j,k))/(nxy)
	       ! !vartp(k)=vartp(k)+(tp(i,j,k)*tp(i,j,k))/(nxy)
	       ! varphip(k)=varphip(k)+(phip(i,j,k)*phip(i,j,k))/(nxy)
        !     enddo
        !   enddo
        !enddo
        
        !do k = 1,nnz
        !   do j= 1,nny
        !     do i=1,nnx
	       ! upnorm(i,j,k)=up(i,j,k)/sqrt(varup(k))
	       ! vpnorm(i,j,k)=vp(i,j,k)/sqrt(varvp(k))
	       ! wpnorm(i,j,k)=wp(i,j,k)/sqrt(varwp(k))
	       ! !tpnorm(i,j,k)=tp(i,j,k)/sqrt(vartp(k))
	       ! phipnorm(i,j,k)=phip(i,j,k)/sqrt(varphip(k))
        !
        !     enddo
        !   enddo
        !enddo

!--------------------------------------------------------------------------------------------------------------------------------
! convert u to the proper VTR format and plug into uWrite
!--------------------------------------------------------------------------------------------------------------------------------
       do k=1,nnz
         do j = 1, nny
            do i = 1, nnx
               uWrite((k-1)*nnx*nny+(j-1)*nnx+i) = u(i,j,k)
            end do
         end do
       end do
       
       do k=1,nnz
         do j = 1, nny
            do i = 1, nnx
               vWrite((k-1)*nnx*nny+(j-1)*nnx+i) = v(i,j,k)
            end do
         end do
       end do
       
       do k=1,nnz
         do j = 1, nny
            do i = 1, nnx
               wWrite((k-1)*nnx*nny+(j-1)*nnx+i) = w(i,j,k)
            end do
         end do
       end do

    !   do k=1,nnz
    !     do j = 1, nny
    !        do i = 1, nnx
    !           tWrite((k-1)*nnx*nny+(j-1)*nnx+i) = tp(i,j,k)
    !        end do
    !     end do
    !   end do
    !
    !   do k=1,nnz
    !     do j = 1, nny
    !        do i = 1, nnx
    !           phiWrite((k-1)*nnx*nny+(j-1)*nnx+i) = phip(i,j,k)
    !        end do
    !     end do
    !   end do


       !do k=1,nnz
       !  do j = 1, nny
       !     do i = 1, nnx
       !        uWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = upnorm(i,j,k)
       !     end do
       !  end do
       !end do
       !
       !do k=1,nnz
       !  do j = 1, nny
       !     do i = 1, nnx
       !        vWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = vpnorm(i,j,k)
       !     end do
       !  end do
       !end do
       !
       !do k=1,nnz
       !  do j = 1, nny
       !     do i = 1, nnx
       !        wWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = wpnorm(i,j,k)
       !     end do
       !  end do
       !end do

    !   do k=1,nnz
    !     do j = 1, nny
    !        do i = 1, nnx
    !           tWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = tpnorm(i,j,k)
    !        end do
    !     end do
    !   end do
    !
    !   do k=1,nnz
    !     do j = 1, nny
    !        do i = 1, nnx
    !           phiWriteNorm((k-1)*nnx*nny+(j-1)*nnx+i) = phipnorm(i,j,k)
    !        end do
    !     end do
    !   end do
    
        call write_VTR(formOut,fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite,uWriteNorm,vWriteNorm,wWriteNorm,fileName)

        write(*,*)''
        !##########################################################
        read(75,*,iostat=stat)buffer
    
    end do
else if(trim(adjustl(formOut)) == 'POD') then
        stop
        !--------------------------------------------------------------------------------------------------------------------------------
        ! in FORTRAN, arrays are written as someArray[#rows:#columns]
        !--------------------------------------------------------------------------------------------------------------------------------

        !if(.NOT. allocated(Uf)) then
        !    allocate(Uf(nnx*nny*nnz*3,150))
        !    allocate(UfTrans(150,nnx*nny*nnz*3))
        !end if
        !
        !N=N+1
        !call readField_POD(formOut,fcounter,nnx,nny,nnz,xArr,yArr,zArr,uWrite,vWrite,wWrite,uWriteNorm,vWriteNorm,wWriteNorm,Uf,N)
end if
close(75)
!**********************************************************END READING FILES


!if(trim(adjustl(formOut)) == 'binary' .OR. trim(adjustl(formOut)) == 'ascii') then
!    
!    write(*,*)'End Reading Files'
!    write(*,*)'Press any button to quit'
!    read(*,*)
!    stop
!    
!else if(trim(adjustl(formOut)) == 'POD') then
!    !--------------------------------------------------------------------------------------------------------------------------------
!    ! Generate the POD analysis here to plug into write_POD
!    !--------------------------------------------------------------------------------------------------------------------------------
!
!    write(*,*)'End Reading Files'
!    write(*,*)''
!    write(*,*)'Beginning POD decomposition'
!    allocate(UTU(N,N))
!    UfTrans = transpose(Uf)
!
!    write(*,*)"Generating UTU Matrix... Please wait... This calculation"
!    write(*,*)"may take awhile..."
!
!    UTU = matmul(UfTrans,Uf)
!    
!    call writeMatrix(150,150,UTU)
!
!    write(*,*)"UTU array successfully calculated"
!    meanUf = 0
!    meanUf = sum (Uf(:,2))
!
!    write(*,*)'Press Enter to quit.'
!    read(*,*)
!end if
write(*,*)'End Reading Files'

end program read_write_3d_filter
