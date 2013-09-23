       subroutine WRITE
      
      

c     Name:      WRITE input file
c     Purpose:   To write input data to a file.
c     Common:    TITLE,ELMDAT1,ELMDAT2,ISOMAT,LAMMAT,GEOM,MATCOD,LOAD,BC,CONTRL
c     Input:     Input data from the commons
c                
c     Output:    Data to the input file.
c     Called by:
c     Calls    : NODEIN,NODECALC,ERROR
c                                                                      |
c*******************************************************************************
      implicit     undefined(a-z)
      
      include      'n.par'
      include      'bc.com'
      include      'contrl.com'
      include      'elmdat.com'
      include      'geom.com'
      include      'io.com'
      include      'layup.com'
      include      'load.com'
      include      'loadi.com'
      include      'matcod.com'
      include      'matprp.com'
      include      'out.com'
      include      'save.com'
      include      'title.com'
      include      'units.com'
      include      'xedin.com'
      
      logical      OUTEXT
      
      character*1  ANS
      character*3  STAT
      character*7  DISPBC(MXNVAR),TRACBC(MXNVAR)
      character*10 OUTFILE
      
      integer      I,EDGENO,OUTFIL
      parameter(OUTFIL=3)
   
      data DISPBC/'u1','u2','w','1 Beta1','1 Beta2','1 Eta','2 Eta',
     &            '3 Beta1','3 Beta2','2 Beta1','2 Beta2','3 Eta',' '/,
     &     TRACBC/'N11','N12','Q1','1 M11','2 M12','1 S1','2 S1',
     &            '3 M11','3 M12','2 M11','2 M12', '3 S1',' '/

      
1     print 105
105   format(' Enter the name of the file to which you want to save',
     &       ' this input data set.')
      read(*,'(a)') OUTFILE
      inquire(file=OUTFILE,exist=OUTEXT)
      
      if (OUTEXT) then
         print 106, OUTFILE
106      format(/' The file "'a'" already exists.')
2        print 107, OUTFILE
107      format(' Do you want to write',
     &      ' over the existing "'a'" or',
     &      ' choose a different'/' file name ? ',
     &      ' Enter "w" to write or "n" to name a new file.')
         read(*,'(a)') ANS
         if (ANS .eq. 'n') then
               go to 1
             else if (ANS .eq. 'w') then
               STAT = 'old'
             else
               print 108
108            format(' Please answer "w" or "n".')
               go to 2
         end if
       else
         STAT = 'new'
      end if

      open (unit=OUTFIL, file= OUTFILE, status= STAT)
      
      write(OUTFIL,'(a)')     TITLE
      
      write(OUTFIL,100) UNIT
100   format(a,18x,':UNIT, "s" for SI, "e" for English')

      write(OUTFIL,110) THEORY
110   format(i1,18x,':THEORY;  1,2,3,4,5,6,7,8')
     
      write(OUTFIL,120) RIGID
120   format(l1,18x,':RIGID if TRUE, rigid',
     &       ' body displacements included')
      
      write(OUTFIL,130) TORORD
130   format(i1,18x,':TORORD; t/R order 0,1,or2')
      
      write(OUTFIL,140) OSHPU,OSHPW,OSHPD
140   format(i1,2(x,i1),14x,':OSHPU,OSHPW,OSHPD(integers);',
     &      'Order SHape function (U,W,D)')
     
      write(OUTFIL,150) NINT
150   format(i1,18x,':NINT (Number of INTegration points')

      write(OUTFIL,160) KSHORT
160   format(l1,18x,':KSHORT; if TRUE, quick alogorithm for',
     &       ' stiffness')
      
      write(OUTFIL,170) SHAPE
170   format(a,15x,':SHAPE(character*4); either CYLN, CONE, SPHR,',
     &       'PARB')
     
      if (SHAPE .eq. 'cyln') then
           write(OUTFIL,180) RAD,CYLEN
180        format(2(x,e12.5),2x,':Radius, Length')
         else if (SHAPE .eq. 'cone') then
           write(OUTFIL,190) R0,ALPHAD,CONEHT
190        format(3(x,e12.5),7x,':Ro,Alpha,Height')
         else if (SHAPE .eq. 'torr') then
           write(OUTFIL,200) RC,RPHI,PHI0D
200        format(3(x,e12.5),7x,':Rc,Rphi,Phi0')
         else if (SHAPE .eq. 'torp') then
           write(OUTFIL,201) RC,RPHI,XF,XS
201        format(4(x,e12.5),7x,':Rc,Rphi,Phi_top,Phi_bot')
         else if (SHAPE .eq. 'para') then
           write(OUTFIL,210) RAT0,OFFSET,XF,XS
210        format(4(x,e12.5),7x,':R0,Offset,Phi_top,Phi_bot')
         else if (SHAPE .eq. 'cyls') then
           write(OUTFIL,180) RAD
      end if
      
      write(OUTFIL,220) NUMEL
220   format(i2,17x,':NUMEL(integer); number of elements')

      do 10 I=1,NXEDI      
          write(OUTFIL,230) XNODEI(I),XEDGEI(I)
230       format(i2,x,e12.5,4x,':Node,X(Node)')
10    continue
                
      
      write(OUTFIL,240)  HOMOGN
240   format(l1,18x,':HOMOGN, if true material is homogeneous')
      
           
      write(OUTFIL,250) SYM
250   format(l1,18x,':SYM; if true laminate is symmetric')

      write(OUTFIL,260) NSTACK
260   format(i2,17x,':Number of stacks')
            
      do 20 I=1,NSTACK
          write(OUTFIL,270) I,MATNO(I),NPLY(I),PTHK(I),THETA(I)
270       format(3(x,i2),x,e12.5,x,f6.1,2x,':Stack No.,Material No.,',
     &           'NPLY,PTH,Theta')
20    continue

      
      write(OUTFIL,280) FCODE
280   format(l1,18x,':FCODE; if .TRUE. mechanical load data follows')
      if (FCODE) then
         write(OUTFIL,290) PZTOP,PZBOT
290      format(2(x,e12.5),2x,':Internal Pressure at edges 1 & 2')
         write(OUTFIL,300) N11,N22,M11,M22
300      format(2(x,e12.5),2x,':n11,n22,m11,m22')
      end if

      write(OUTFIL,310) TCODE
310   format(l1,18x,':TCODE; if .TRUE. thermal data follows')
      if (TCODE) then
         do 30 I=1,NTMPI
            write(OUTFIL,320) TNI(I),TII(I),TMI(I),TOI(I)
320         format(i2,3(x,e12.5),2x,':Node,Temp in.,',
     &             'Temp mid.,Temp out.')         
30       continue
      end if

      write(OUTFIL,330) MOIST
330   format(l1,18x,':MOIST; if .TRUE. moisture data follows')
      if (MOIST) then
         do 40 I=1,NMSTI
            write(OUTFIL,340) MNI(I),MII(I),MMI(I),MOI(I)
340         format(i2,3(x,e12.5),2x,':Node,Moist in.,',
     &             'Moist mid.,Moist out.')         
40       continue
      end if


      do 60 EDGENO=1,2
         write(OUTFIL,350)  BCTYPE(EDGENO),EDGENO
350      format(i1,18x,':BCTYPE(',i1,') 0:general 1:s.s 2:clamp',
     &                  ' 3:free 4:sym')
         if (BCTYPE(EDGENO) .eq. 0) then
            do 50 I=1,MXNBC
               if (BCCODE(EDGENO,I) .eq. 1) then
                  write(OUTFIL,360) I,BCCODE(EDGENO,I),UBC(EDGENO,I),
     &                              EDGENO,I,EDGENO,I,DISPBC(I)
360               format(i2,x,i1,x,e12.5,2x,':BCCODE(',i1,',',i2,
     &                   '), BC(',i1,',',i2,'):  ',a)
                 else if (BCCODE(EDGENO,I) .eq. 2) then
                  write(OUTFIL,360) I,BCCODE(EDGENO,I),TBC(EDGENO,I),
     &                              EDGENO,I,EDGENO,I,TRACBC(I)
               end if
50          continue
         end if
60    continue

      write(OUTFIL,370) BACK
370   format(l1,18x,':BACK; if true solve for displacements')

      write(OUTFIL,380) RSLT
380   format(l1,18x,':ReSuLT; if true solve for resultants')

      write(OUTFIL,390) PLTSWC
390   format(l1,18x,':PLoTSWitCh; if true plotting data files')

      write(OUTFIL,400) OTFILE
400   format(a,9x,':Output file name')
           
      close(OUTFIL)
      
      SAVED = .true.
      
      return
      end
      
      
