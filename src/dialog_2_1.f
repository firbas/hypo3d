c
      subroutine dialog_2_1
     >(endit,prt,scan_depth,i0,maxIter,rms_on_net,loc_write,rp,i_menu)
c
c*****************************************************************************
c
c  subroutine DIALOG_2_1
c
c-----------------------------------------------------------------------------
c
c  purpose:
c
c     show various menu, test answer, set appropriate flags
c
c-----------------------------------------------------------------------------
c
c  formal parameters:
c
c     logical     ENDIT       ...  end of iter. process           I/O
c     logical     PRT         ...  printed results                I/O
c     logical     SCAN_DEPTH  ...  scanned depth mode               I
c     integer     I0          ...  no. of iterations                I
c     integer     MAXITER     ...  max. number of iterations        I
c     logical     RMS_ON_NET  ...  rms_on_net mode                  I
c     logical     LOC_WRITE   ...  locfile written                I/O
c     logical     RP          ...  repeat of location             I/O
c     integer     I_MENU      ...  minimum time of datum8           O
c
c----------------------------------------------------------------------------
c
c  calling convention:
c
c     subroutine dialog_2_1 (endit,prt,scan_depth,i0,maxIter,rms_on_net,
c                            loc_write,rp,i_menu)
c
c----------------------------------------------------------------------------
c
c  programmed:  87-04  01.00  mw
c
c*****************************************************************************
c
c
c  ************
c  declarations
c  ************
c
         implicit none
c
c  formal parameters
c
         logical endit
         logical prt
         logical scan_depth
         integer i0
         integer maxIter
         logical rms_on_net
         logical rp
         logical loc_write
         integer i_menu
c
c  local parameters  ...  none
c
c
c  global parameters
c
         include 'pname.fi'
c
c  local variables
c
         character*1 answer
c
c  global variables
c
         logical       loc
         common /wloc/ loc
c
c  functions  ...  none
c
c
c  *******************
c  end of declarations
c  *******************
c
c=============================================================================
c
10       continue
c
         if (endit) then
            if (prt) then
               write (*,'(1x,a,": M e n u")') prog_name
               write (*,'(1x,a,
     >               ": End of location            -     ''E''  "/
     >        "         Continue location          -     ''C''  ",/
     >        "         Write loc-file             -     ''L'' [L]:_")')
     >         prog_name
            else
               write (*,'(1x,a,": M e n u")') prog_name
               write (*,'(1x,a,
     >               ": End of location            -     ''E''  "/
     >        "         Continue location          -     ''C''  ",/
     >        "         Write loc-file             -     ''L''  ")')
     >         prog_name
            endif
         else if (scan_depth .or. i0.eq.maxIter
     >      .or. rms_on_net .or. loc_write) then
c
c  not loc-file  writing
c
            if (prt) then
               write (*,'(1x,a,": M e n u")') prog_name
               write (*,'(1x,a,
     >               ": End of location            -     ''E''  "/
     >        "         Continue location          -     ''C'' [E]:_")')
     >         prog_name
            else
               write (*,'(1x,a,": M e n u")') prog_name
               write (*,'(1x,a,
     >               ": End of location            -     ''E''  "/
     >        "         Continue location          -     ''C''  ")')
     >         prog_name
            endif
         else
            write (*,'(1x,a,": M e n u")') prog_name
            write (*,'(1x,a,
     >           ": End of location            -     ''E''  "/
     >    "         Continue location          -     ''C'' [E]:_")')
     >      prog_name
         endif
c
c  read answer
c
         read (*,'(a)',end=10) answer

cc            call clear_display

cc            call CaseFold(answer)
         if(answer.eq.'l')answer='L'
         if(answer.eq.'e')answer='E'
         if(answer.eq.'c')answer='C'
         if(answer.eq.'p')answer='P'
c
c  set default values
c
         if (answer.eq.' ') then
            if (endit) then
c
c  default:   write locfile
c
               answer='L'
            else
c
c  default:   end of program
c
               answer='E'
            endif
         endif
c
c  test the answer
c
         if (answer.eq.'C') then
c
c  repeat of location
c
            rp=.true.
            i_menu=1
c
         else if (answer.eq.'L' .and. endit) then
c
c  set flags
c
            loc=.true.
            endit=.false.
            loc_write=.true.
            i_menu=2

         else if (answer.eq.'P' .and.
     >   (endit .or. scan_depth .or.
     >   i0.eq.maxIter .or. rms_on_net .or. loc_write) ) then
c
c  set flag for printed results
c
            prt=.true.
            i_menu=3

         else if (answer .eq. 'E') then
c
c  go to the next hypfile (if exist)
c
            i_menu=4
         else
c
c  show menu
c
            go to 10
         endif

         return
      end subroutine dialog_2_1
