      subroutine INTRO
      
c     Name:      INTROduction
c     Purpose:   To print an introduction to the program
c     Input:     none
c                
c     Output:    An introdution output to the screen.
c     Called by: USRFRD,MODIFY
c     Calls    : 
c                                                                      |
c*******************************************************************************
 
 
      implicit     none
      
      include      'io.com'

      print 100
100   format(//'INTRODUCTION'//
     &' The program AXIHYG calculates the deformations of',
     &' axisymmetric, laminated'/' composite shells due to',
     &' changes in moisture',
     &' concentration and temperature. '/' The problem to be solved',
     &' must be compatible with the following assumptions:'//
     &'  1.  The material properties are linearly elastic and',
     &' independent'/
     &'      of temperature and moisture concentration.'/
     &'  2.  The shell''s geometry and material properties are',
     &' axisymmetric.'/
     &'  3.  The thickness of the shell is constant.'/
     &'  4.  Moisture concentration and temperature change are',
     &' axisymmetric and may'/
     &'      be either constant, or vary',
     &' linearly or quadratically through the'/,6x,'thickness.')
     
      print 105
105   format('  5.  Deflections, rotations and strains are small.'//
     &' The program is based on a higher order shell theory that',
     &' incorporates both'/
     &' transverse shear and normal strains.',
     &' The governing boundary value problem'/
     &' is one-dimensional',
     &' (axisymmetric) and is solved using a one-dimensional,'/ 
     &' displacement method finite element algorithm.  Further',
     &' information'/' regarding this code may be found in',
     &' "Hygrothermal Deformations of'/' Laminated Composite Shells"',
     &' and "A Higher Order Theory of the Hygrothermal'/' Behavior of',
     &' Composite Shells" by Doxsee and in "Hygrothermal'/
     &' Stresses and Strains in Axisymmetric Composite Shells"',
     &' and "Temperature'/' and Moisture Induced Deformations in',
     &' Fiber-reinforced Composite'/' Shells" by Doxsee and',
     &' Springer.'/)

      call WAIT

      print 110
110   format(
     &' Currently you are running the interactive user-friendly',
     &' interface "AXIHYGUF"'/
     &' which is used to create new "input data sets" or to load',
     &' and modify existing'/ 
     &' ones.  Once the input data set has been created, it may be',
     &' stored to a file.'/
     &' This file, if it is given the name "INPUT.DAT", becomes the',
     &' input file to the'/
     &' finite element program "AXIHYGFE".')
       
     
       call WAIT

      print 130
130   format('   User-Friendly Interface:'/
     &' The user-friendly interface is used',
     &' to either create new or to modify old'/' "input data',
     &' sets".  The "input data sets" are',
     &' created and modified in sections:'/)
     
      print 140
140   format('  1.  Title of the "input data set".'/
     &'  2.  Physical units to which the data refer (SI or English).'/
     &'  3.  Geometry of the shell (shape and dimensions of',
     &' the generator).'/
     &'  4.  Material properties (which are stored',
     &' in a different data base in the file'/
     &'      "MAT.DAT").'/
     &'  5.  Layup of the shell''s laminate.'/
     &'  6.  Hygrothermal loads applied to the shell.'/
     &'  7.  Boundary conditions at the two edges.'/
     &'  8.  Output selection (quantities for which to solve and',
     &' print).'/
     &'  9.  Nodal coordinates (positions of the edges',
     &' of the finite elements along'/
     &'      the generator).'/
     &' 10.  Solution techniques specification.'/)
    
      print 150
150   format(' These data can be viewed, modified or',
     &' stored at any point in the course of'/
     &' running the program.  Introductions and',
     &' instructions are provided at the'/
     &' beginning of key sections.  Figures which accompany',
     &' the code are sometimes'/' referenced.'/)

      call WAIT
      
      print 160
160   format(' Since a shell theory is the basis of the',
     &' analysis, the program solves for'/
     &' midsurface quantities from which',
     &' non-midsurface quantities can be calculated.'//
     &' The displacement of a point U(x1,x2,z) is',
     &' assumed to be in the form:'//
     &' U1(x1,x2,z) = u1(x1) +  z 1_beta_1(x1)',
     &'  +  z^2 2_beta_1(x1)  + z^3 3_beta_1(x1)'/
     &' U2(x1,x2,z) = u2(x1) +  z 1_beta_2(x1)',
     &'  +  z^2 2_beta_2(x1)  + z^3 3_beta_2(x1)'/
     &' U3(x1,x2,z) = w(x1)  +  z 1_eta(x1)   ',
     &'  +  z^2 2_eta(x1)     + z^3 3_eta(x1)'/)
      
      print 170
170   format(' where x1 is the meridional coordinate,',
     &' x2 is the circumferential coordinate,'/
     &' and z is the normal coordinate.',
     &'  The program solves for the midsurface'/
     &' displacements (u1, u2, w) and "rotations"',
     &' (n_beta_i,n_eta) from which the'/
     &' displacement of any point in the',
     &' shell can be calculated according to the'/
     &' above equations.  Midsurface strains and',
     &' curvature changes and stress'/
     &' resultants are also given as output.')
     
      call WAIT

      return
      end
      
      
