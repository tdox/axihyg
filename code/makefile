F77          = gfortran
CFLAGS       = -c
MORE_CFLAGS  =
LDFLAGS      =
LIBS         =

AXI_SOURCES      =             \
                  axi.f    \
                b1eb1.f    \
                b1eb2.f    \
                b1eb3.f    \
                b2eb2.f    \
               bigout.f    \
                   bn.f    \
                bn121.f    \
               bneout.f    \
               clearb.f    \
                clrfe.f    \
                clrke.f    \
                clrkf.f    \
               dimens.f    \
                  dot.f    \
               dspplt.f    \
                ehomo.f    \
                 elam.f    \
               engtoc.f    \
                 esub.f    \
               fedisp.f    \
               feedgt.f    \
               femech.f    \
                feout.f    \
               fethrm.f    \
                fgout.f    \
               fmechs.f    \
               formkf.f    \
               ftherm.f    \
               fthhom.f    \
               fthlam.f    \
                idien.f    \
                input.f    \
               intdat.f    \
                keout.f    \
               keshrt.f    \
                kesub.f    \
               kfassm.f    \
                kskyo.f    \
                 l121.f    \
               lamcof.f    \
               linshp.f    \
                ljout.f    \
               nodein.f    \
               output.f    \
               pltout.f    \
               posplt.f    \
               quashp.f    \
               rbmshp.f    \
               resplt.f    \
               result.f    \
               shpplt.f    \
               skylin.f    \
                solve.f    \
               stifro.f    \
                strro.f    \
               symtzr.f    \
               xnd121.f    \
                 xpos.f    \
               xwskro.f    \
               zeror2.f

AXI_OBJECTS    =  $(AXI_SOURCES:.f=.o )





USR_SOURCES      =             \
               usrfrd.f    \
               addmat.f    \
                  bcs.f    \
               chgmat.f    \
               create.f    \
                 dims.f    \
                geoms.f    \
               getmst.f    \
               gettmp.f    \
                 init.f    \
                intro.f    \
               layups.f    \
                loads.f    \
               lodats.f    \
               maters.f    \
               matlst.f    \
               matnot.f    \
               matwrt.f    \
                 mesh.f    \
                miwrt.f    \
               modify.f    \
               moisti.f    \
                nodes.f    \
               numels.f    \
                 outs.f    \
                phi12.f    \
                 quit.f    \
               shapes.f    \
               sltchs.f    \
               stkini.f    \
                tempi.f    \
               titles.f    \
                tiwrt.f    \
               unitss.f    \
               viewmt.f    \
                write.f    \
               xedges.f    \
                yesno.f

USR_OBJECTS    =  $(USR_SOURCES:.f=.o )



LIB_SOURCES      =             \
               altobt.f    \
                bcwrt.f    \
                bcset.f    \
               bcunit.f    \
               checkx.f    \
                error.f    \
               geowrt.f    \
                inred.f    \
                inwrt.f    \
                lamtm.f    \
               laywrt.f    \
               matred.f    \
               mstwrt.f    \
               otqwrt.f    \
               sltwrt.f    \
               thetin.f    \
               thrset.f    \
               thtint.f    \
               tmpwrt.f    \
               untset.f    \
                 wait.f    \
               wrtmat.f    \
               xedwrt.f    \
               xeintp.f

LIB_OBJECTS    =  $(LIB_SOURCES:.f=.o )


axi:  $(AXI_OBJECTS) $(LIB_OBJECTS)
	$(F77) -v $(LDFLAGS) -o axi $(AXI_OBJECTS) $(LIB_OBJECTS) $(LIBS)


usrfrd: $(USR_OBJECTS) $(LIB_OBJECTS)
	$(F77) -o usrfrd $(LDFLAGS) $(USR_OBJECTS) $(LIB_OBJECTS) $(LIBS)



axi.o    : axi.f io.par contrl.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) axi.f

altobt.o     : altobt.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) altobt.f

 b1eb1.o     :  b1eb1.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  b1eb1.f

 b1eb2.o     :  b1eb2.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  b1eb2.f

 b1eb3.o     :  b1eb3.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  b1eb3.f

 b2eb2.o     :  b2eb2.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  b2eb2.f

bigout.o     : bigout.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) bigout.f

    bn.o     :     bn.f io.par     n.par      contrl.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)     bn.f

 bn121.o     :  bn121.f io.par     n.par      contrl.com elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  bn121.f

bneout.o     : bneout.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) bneout.f

clearb.o     : clearb.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) clearb.f

 clrfe.o     :  clrfe.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  clrfe.f

 clrke.o     :  clrke.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  clrke.f

 clrkf.o     :  clrkf.f n.par      elmdat.com kfsky.com  kbig.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  clrkf.f

dimens.o     :  dimens.f pi.par    geom.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) dimens.f

   dot.o     :    dot.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)    dot.f

dspplt.o     :    io.par n.par contrl.com elmdat.com out.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) dspplt.f

 ehomo.o     :  ehomo.f n.par      contrl.com elmdat.com hommat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  ehomo.f

  elam.o     :   elam.f n.par      contrl.com elmdat.com lamscf.com layup.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   elam.f

engtoc.o     : engtoc.f io.par    
	$(F77) $(CFLAGS) $(MORE_CFLAGS) engtoc.f

  esub.o     :   esub.f n.par      matcod.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   esub.f

fedisp.o     : fedisp.f n.par      bc.com     elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) fedisp.f

feedgt.o     : feedgt.f n.par      bc.com     elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) feedgt.f

femech.o     : femech.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) femech.f

 feout.o     :  feout.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  feout.f

fethrm.o     : fethrm.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) fethrm.f

 fgout.o     :  fgout.f n.par      elmdat.com kfsky.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  fgout.f

fmechs.o     : fmechs.f n.par      elmdat.com load.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) fmechs.f

formkf.o     : formkf.f n.par      contrl.com elmdat.com geom.com   int.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) formkf.f

ftherm.o     : ftherm.f io.par     n.par      elmdat.com load.com   matcod.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) ftherm.f

fthhom.o     : fthhom.f io.par     n.par      contrl.com elmdat.com hommat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) fthhom.f

fthlam.o     : fthlam.f n.par      contrl.com elmdat.com lamecf.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) fthlam.f

 idien.o     :  idien.f n.par      elmdat.com bc.com     contrl.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  idien.f

 input.o     :  input.f n.par      bc.com     elmdat.com out.com   
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  input.f

intdat.o     : intdat.f n.par      int.com   
	$(F77) $(CFLAGS) $(MORE_CFLAGS) intdat.f

 keout.o     :  keout.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  keout.f

keshrt.o     : keshrt.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) keshrt.f

 kesub.o     :  kesub.f n.par      contrl.com elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  kesub.f

kfassm.o     : kfassm.f n.par      elmdat.com kfsky.com  kbig.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) kfassm.f

 kgout.o     :  kgout.f n.par      elmdat.com kbig.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  kgout.f

 kskyo.o     :  kskyo.f io.par     n.par      elmdat.com kfsky.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  kskyo.f

kstest.o     : kstest.f io.par     n.par      elmdat.com kfsky.com  kbig.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) kstest.f

  l121.o     :   l121.f n.par      contrl.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   l121.f

lamcof.o     : lamcof.f n.par      io.par     pi.par     contrl.com layup.com  \
                        matprp.com lamecf.com lamscf.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) lamcof.f

linshp.o     : linshp.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) linshp.f

 ljout.o     :  ljout.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  ljout.f

nodein.o     : nodein.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) nodein.f

output.o     : output.f n.par      bc.com     contrl.com elmdat.com geom.com   \
                        hommat.com io.com     layup.com  load.com   matcod.com \
                        matprp.com out.com    strses.com title.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) output.f

pltout.o     : pltout.f contrl.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) pltout.f

posplt.o     : posplt.f io.par     n.par      elmdat.com out.com   
	$(F77) $(CFLAGS) $(MORE_CFLAGS) posplt.f

quashp.o     : quashp.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) quashp.f

rbmshp.o     : rbmshp.f n.par     
	$(F77) $(CFLAGS) $(MORE_CFLAGS) rbmshp.f

resplt.o     : resplt.f io.par     n.par      contrl.com elmdat.com strses.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) resplt.f

result.o     : result.f n.par      contrl.com elmdat.com int.com    out.com    \
                        strses.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) result.f

shpplt.o     : shpplt.f io.par     n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) shpplt.f

skylin.o     : skylin.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) skylin.f

 solve.o     :  solve.f n.par      contrl.com elmdat.com kfsky.com  out.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  solve.f

stifro.o     : stifro.f io.par    
	$(F77) $(CFLAGS) $(MORE_CFLAGS) stifro.f

 strro.o     :  strro.f io.par    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  strro.f

symtzr.o     : symtzr.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) symtzr.f

xnd121.o     : xnd121.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) xnd121.f

  xpos.o     :   xpos.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   xpos.f

xwskro.o     : xwskro.f io.par    
	$(F77) $(CFLAGS) $(MORE_CFLAGS) xwskro.f

zeror2.o     : zeror2.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) zeror2.f


#$(AXI_SOURCES) : $$(@F)or
#	sed -f mactosg.sed $? > $@


usrfrd.o     : usrfrd.f io.com    
	$(F77) $(CFLAGS) $(MORE_CFLAGS) usrfrd.f



addmat.o     : addmat.f n.par      matprp.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) addmat.f

   bcs.o     :    bcs.f n.par      bc.com     elmdat.com io.com    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)    bcs.f

chgmat.o     : chgmat.f n.par      matprp.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) chgmat.f

create.o     : create.f save.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) create.f

  dims.o     :   dims.f geom.com    units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   dims.f

 geoms.o     :  geoms.f io.com    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  geoms.f

 getmst.o     :  getmst.f    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  getmst.f

 gettmp.o     :  gettmp.f units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  gettmp.f

init.o     : init.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  init.f

 intro.o     :  intro.f io.com   
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  intro.f


layups.o     : layups.f n.par      io.com     layup.com  units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) layups.f

 loads.o     :  loads.f n.par      contrl.com elmdat.com io.com     loadi.com  \
                        units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  loads.f

lodats.o     : lodats.f save.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) lodats.f

maters.o     : maters.f n.par      io.com     matprp.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) maters.f

matlst.o     : matlst.f n.par      matprp.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) matlst.f

matnot.o     : matnot.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS) matnot.f

matwrt.o     : matwrt.f n.par      matprp.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) matwrt.f

  mesh.o     : mesh.f   n.par elmdat.com loadi.com xedin.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) mesh.f

 miwrt.o     :  miwrt.f n.par      elmdat.com loadi.com  units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  miwrt.f

modify.o     : modify.f io.com     save.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) modify.f

moisti.o     : moisti.f n.par      elmdat.com io.com     load.com   loadi.com  \
                        units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) moisti.f

 nodes.o     :  nodes.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  nodes.f

numels.o     : numels.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) numels.f

  outs.o     :   outs.f contrl.com io.com    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   outs.f

 phi12.o     : phi12.f geom.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) phi12.f

  quit.o     :   quit.f save.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   quit.f

shapes.o     : shapes.f geom.com   units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) shapes.f

sltchs.o     : sltchs.f n.par      contrl.com elmdat.com geom.com   io.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) sltchs.f

stkini.o     : stkini.f n.par      layup.com  matprp.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) stkini.f

 tempi.o     :  tempi.f n.par      elmdat.com io.com     load.com   loadi.com  \
                        units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  tempi.f

titles.o     : titles.f title.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) titles.f

 tiwrt.o     :  tiwrt.f n.par      elmdat.com loadi.com  units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  tiwrt.f

unitss.o     : unitss.f units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) unitss.f

viewmt.o     : viewmt.f n.par      io.com     matprp.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) viewmt.f

 write.o     :  write.f n.par      bc.com     contrl.com elmdat.com geom.com   \
                        io.com     layup.com  load.com   loadi.com  matcod.com \
                        matprp.com out.com    save.com   title.com  units.com  \
                        xedin.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  write.f

xedges.o     : xedges.f n.par      elmdat.com io.com     units.com  xedin.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) xedges.f

 yesno.o     :  yesno.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  yesno.f


#$(USR_SOURCES) : $$(@F)or
#	sed -f mactosg.sed $? > $@







 bcwrt.o     :  bcwrt.f n.par      bc.com     elmdat.com io.com     units.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  bcwrt.f

 bcset.o     :  bcset.f n.par      bc.com     elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  bcset.f

bcunit.o     : bcunit.f n.par      units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) bcunit.f

checkx.o     : checkx.f pi.par     geom.com  
	$(F77) $(CFLAGS) $(MORE_CFLAGS) checkx.f

 error.o     :  error.f io.com    
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  error.f

geowrt.o     : geowrt.f geom.com   units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) geowrt.f

 inred.o     :  inred.f n.par      bc.com     contrl.com elmdat.com geom.com   \
                        hommat.com io.com     layup.com  load.com   loadi.com  \
                        matcod.com matprp.com title.com  units.com  xedin.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  inred.f

 inwrt.o     :  inwrt.f n.par      bc.com     contrl.com elmdat.com hommat.com \
                        io.com     layup.com  load.com   loadi.com  matcod.com \
                        matprp.com title.com  units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  inwrt.f

 lamtm.o     :  lamtm.f n.par      layup.com  matprp.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS)  lamtm.f

laywrt.o     : laywrt.f n.par      layup.com  units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) laywrt.f

matred.o     : matred.f n.par      matprp.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) matred.f

mstwrt.o     : mstwrt.f n.par      elmdat.com load.com   loadi.com  units.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) mstwrt.f

otqwrt.o     : otqwrt.f contrl.com io.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) otqwrt.f

sltwrt.o     : sltwrt.f n.par      contrl.com io.com     elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) sltwrt.f

thetin.o     : thetin.f n.par      io.com     elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) thetin.f

thrset.o     : thrset.f n.par      contrl.com elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) thrset.f

thtint.o     : thtint.f n.par      elmdat.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) thtint.f

tmpwrt.o     : tmpwrt.f n.par      elmdat.com loadi.com  load.com   units.com
	$(F77) $(CFLAGS) $(MORE_CFLAGS) tmpwrt.f

untset.o     : untset.f units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) untset.f

  wait.o     :   wait.f
	$(F77) $(CFLAGS) $(MORE_CFLAGS)   wait.f

wrtmat.o     : wrtmat.f n.par      matprp.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) wrtmat.f

xedwrt.o     : xedwrt.f n.par      elmdat.com units.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) xedwrt.f

xeintp.o     : xeintp.f n.par      elmdat.com xedin.com 
	$(F77) $(CFLAGS) $(MORE_CFLAGS) xeintp.f


#$(LIB_SOURCES) : $$(@F)or
#	sed -f mactosg.sed $? > $@

