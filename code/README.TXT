                                   AxiHyg
               A Fortran77 program for calculating the deformation
                    of laminated composite shells subjected
                     to changes in temperature and moisture

                                 Version 1.0
                              December 31, 1987

                        Written by Lawrence E. Doxsee, Jr.
                              Stanford University


This program is a research tool in the development stage and is supplied
as is for the purpose of scientific collaboration.  No warranty,
expressed or implied, is given.


This package contains the following items:

* A disk containing the program.
* A reprint of  "Hygrothermal Deformations of Composite Shells"
* A short description of the input and output data for the program.
* A printout of a typical terminal session.


The program is provided on the enclosed floppy disk.  The disk contains
five ascii text files:

* The three Fortran77 source code files: ahfe.f, ahuf.f, ahlib.f
* A materials data file: MAT.DAT
* This file in ascii format: README.TXT


To create an executable version of the program, follow this procedure:

* Compile the three source codes separately with a Fortran77 compiler
  to create three object codes (for instance they might be called ahfe.o,
  ahuf.o, ahlib.o)
* Link the object files ahfe.o and ahlib.o to create the executable
  finite element program AxiHygFE.
* Link the object files ahuf.o and ahlib.o to create the executable
  user-friendly interface UsrFrd.


To run the programs:

* Make sure that data file MAT.DAT is in the same directory in which
  invoke the programs UsrFrd and AxiHygFE.
* Execute the program UsrFrd to specify the problem to be solved.  This
  program creates an input data set and saves this data set to an ascii 
  text file which can have any name.
* If the input data set was not saved to a file called INPUT.DAT in the
  previous step, rename the file containing the input data set to 
  INPUT.DAT.
* Execute the program AxiHygFE.  The output data is stored to several
  text files.


The author of the program would like to hear any comments concerning the
accuracy and usefulness of this program.  He would also like to be
informed of any weaknesses or programming errors in the code.  If you
have any comments please contact:

Lawrence E. Doxsee, Jr.
Rasna Corporation
2590 N. First Street, Suite 200
San Jose, CA 95131
USA
tel: (408) 922-7204 ext. 256
FAX: (408) 922-7256


Prof. George S. Springer
Dept. of Aeronautics and Astronautics
Stanford University
Stanford, CA 94305
USA
tel: (415) 723-4135
FAX: (415) 725-3377
