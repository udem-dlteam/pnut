// pnut-options: -DSH_SAVE_VARS_WITH_SET -DALLOW_RECURSIVE_MACROS -URT_COMPACT
/*
 * A R4RS repl compiled with the Ribbit Scheme Compiler. This C file includes
 * a bytecode interpreter for the Ribbit Virtual Machine (RVM) and a garbage collector.
 *
 * For more details about ribbit, see the repository: https://github.com/udem-dlteam/ribbit
 *
 * Author : Leonard Oest O'Leary (github.com/leo-ard)
 */

// This repl has been tested against R4RS compliance tests that can be found here :
//  https://github.com/udem-dlteam/ribbit/tree/7360c7d78bd12eef5d2bb1fa54e851e78b121a33/src/tests/01-r4rs

// To run the repl efficiently using pnut compile it using :
//
//  $ gcc -Dsh -DSUPPORT_INCLUDE -DSH_SAVE_VARS_WITH_SET -DOPTIMIZE_LONG_LINES pnut.c -o pnut-sh.exe
//
// Then compile this file using pnut :
//
//  $ ./pnut-sh.exe repl.c > repl.sh
//
// For optimal performances, use ksh to run the repl :
//
//  $ ksh repl.sh
//
//  Alternatively, you can compile this file using gcc and run it directly :
//
//  $ gcc repl.c -o repl.exe
//  $ ./repl.exe
//

// Standard C definitions
#ifdef PNUT_CC
typedef long FILE;
#define O_RDONLY 0
#define O_WRONLY 1
#else
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#endif

#ifndef NULL
#define NULL 0
#endif

#define true (1)
typedef char bool;

// Ribbit's bytecode
char* input = "RD?naeloob,?xelpmoc,cc/llac,?citebahpla-rahc,<,dnuor,dna,etouq,gnirts,raaaac,radddc,raaddc,?=>ic-gnirts,esle,>,!rdc-tes,?qe,oludom,tsil>-rotcev,htgnel-gnirts,?tsil,fer-gnirts,-,nruter,esac,rddddc,certel,gnicilps-etouqnu,!tes,?rebmun,daol,rdadac,ro,rac,tneitouq,?orez,xam,/,?evitisop,?=>ic-rahc,ecaps,roolf,?tcaxe,?laer,?=>gnirts,rdaddc,raddac,adbmal,gniliec,rdaadc,elif-tuptuo-htiw-llac,fi,mcl,tropxe,gnirtsbus,!tes-gnirts,?evitagen,+,raaadc,!rac-tes,*,etouqnu,?=<ic-rahc,enifed,?=ic-gnirts,etouqisauq,elif-tupni-htiw-llac,?tcaxeni,rdc,!tes-rotcev,rddadc,rddaac,nigeb,radaac,rotcev-ekam,tel,etacnurt,rdaaac,?=gnirts,?=<rahc,htgnel-rotcev,=<,raadac,?lanoitar,_,?=<gnirts,fer-rotcev,=>,bat,?ddo,noitaunitnoc-tnerruc-htiw-llac,nim,?=>rahc,gnirts-ekam,?=<ic-gnirts,regetni>-rahc,?ciremun-rahc,radadc,dnoc,=,dneppa-gnirts,trop-tuptuo-esolc,cossa,dcg,qmem,?>ic-gnirts,hcae-rof,redniamer,?<ic-gnirts,?<gnirts,?=rahc,?=ic-rahc,gnirts>-lobmys,?>ic-rahc,?>gnirts,rebmem,?neve,vmem,?ecapsetihw-rahc,elif-tuptuo-nepo,lave,?>rahc,?esac-rewol-rahc,raadc,?esac-reppu-rahc,?trop-tuptuo,?trop-tupni,elif-tupni-nepo,trop-tupni-esolc,tsil>-gnirts,raaac,raddc,rdddac,?<ic-rahc,?<rahc,gnirts>-rebmun,rebmun>-gnirts,enilwen,raac,esacnwod-rahc,qssa,vssa,?regetni,?vqe,fer-tsil,trop-tupni-tnerruc,radac,dneppa,rotcev,?erudecorp,radc,rdddc,sba,trop-tuptuo-tnerruc,rotcev>-tsil,ylppa,esrever,lobmys>-gnirts,rdadc,rdaac,esacpu-rahc,rddac,rahc-etirw,?gnirts,gnirts>-tsil,?rotcev,etirw,?rahc,?tcejbo-foe,?lauqe,rahc-keep,rahc>-regetni,?lobmys,pam,htgnel,daer,,,,,ton,,,yalpsid,rdac,rddc,rahc-daer,tsil,,?llun,,,,?riap,,,snoc,,,,,,,,,;8V1k!T1)li%zAmk!TH7%lYAl_Im^[$Kl7(lYAlbAmZJl^)li&AmZBlb~YHl^{i$ZCl^{!V18V1kAmZ9kAmYJlZJl^99k~YHl^YAkAmPliW#y]J7$kWmi&:nHniW%ai&kk{!@#niVK`^}'!W%Rm:nkw)iW)l!U9)l_,mYU9ma?l_>l^~Fl^}'!UG)l^8UGnUmlb?l`^)l`~Bm_>l_~Fl_})!V)8;mVma_8;mVmaUm`l~Z4l_cYUGnka_?lb>la1nYV)ofd?lbCmai$>l`^~Fl_}+!V##n:nckiVE#nQla~i$#n:nckiVE#nQla~i$#n:nckiVE#nQla~i$#n:nckiVE#nQla~YU>lQla~Bmw*?la~BmiW)>la~YU>la_iVB}'!?#na_iW)#nk_iW)~BmiW%_}'!=1nb1nRm:nTng?lecw*iW)m~Fl?la>l`^})!UI8=nebb1nYUIqRm:nh4w2iW)m1nYUIqh1~BmiW%h1Cmh0eh-?lf?ldCmci$_`>la>l_~Fl_}/!V?7&ml_Im^[$Kl8U;nX,mb?l`X+ma>l_wVL8U;nX,mUmlb?l`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l`Z0l_wVJ~Bml_~i$8U;nX,mb?l`X+ma>l_wVL8U;nX,mUmlb?l`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l`Z0l_wVJ~Bml_~BmwTKZ8l^~Fl>l^8U;nX,mGmlb?l`YUJlwT)wVL88l^~Bml_~BmwT)>l^8UJl^8U;mX+ma>l_wW'~YKl^~SlFl^}'i${!UJ8U;m_wU1{!UD,mLnca_wT3})!1#nb`iVE8UIqgdLlCmbwS@LlaiVC`8V)oCmfbYBl`_`~YDl_?l`1nei$1neImYUDnCmCm?lddwTNCm?l`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l`~BmwTN^1nci$1ncYUDnCmNldwS2CmZ$lcwSLZ#la1ncCmZ$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl`>l_wTJ{YCmaKl5mi$>l^{wSI`Ol`~BmwTL^8UIqgdNldYCmai8YCm`iTE`Ol`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^Im^~^YDl^Ol`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc`YOlb`1nYV#meYUGnlc>l`CmCmNld?l`wT7`~Fl^Ol`~ImBmwT'_8UIqgdLlCmbwS@LlaiVC`8V)oCmfbYBl`_`~YDl_?l`1nei$1neImYUDnCmCm?lddwTNCm?l`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l`~BmwTN^1nci$1ncYUDnCmNldwS2CmZ$lcwSLZ#la1ncCmZ$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl`>l_wTJ{YCmaKl5mi$>l^{wSI`Ol`~BmwTL^8UIqgdNldYCmai8YCm`iTE`Ol`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^Im^~^YDl^Ol`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc`YOlb`1nYV#meYUGnlc>l`CmCmNld?l`wT7`~Fl^Ol`~Im^~^BmwTJ^1ncYV?lOla_~BmwT%^#ncOlaiVE~BmwU1^>l_~Fl_#nbYUGnk``iVF~YDl_})!V*)l^8V*l?l^~Fl^{!U@9&lCm`^8U@mCma>l_?l^~Fl^}'!UN)lk8>mYUNl?l_l~Fl^{!;#na_iVE}'!VKl!VIo!VEn!VFm!VBl!W)k!V'7&mZAlaZAl_[$Kl)lk)liVD~Fl_)ll)ll)liVD~Z=m__7,m?lb?l`~YS%m__>l`>l^~Fl_~Fl^}'i$}'!V27&m>la>l_[$Kl)lk)liVD~Fl_)ll7,m?lb?l`)ll~Em`^)liVD~Em__>l`>l^~Fl_~Fl^}'i$}'!S08LlZ'mYCm`jAj/z!T07(oi&ca_[$Kl8V&la7/oCmfZ2mb>lb`a_Gml`~Ema_}+i$})!U,8<lYS(m`^}'!S68<lYS+m`^}'!S+.mYV'ma_k}'!S(.mkYV'm`^}'!T&+mkYV'm`^}'!T:8<lYS'm`^}'!S?8<lZPm`^}']P.mYV2ma_k}'!S'.mkYV2m`^}'!SF8Gm`^}'!U08Ll^z!S78LlYV+m__Im^[&?l`>l_8LlYV+m__iW*~Bmi&_|!T/8V3n>lb`>l^})!U#8ElZ2m`>l^}'!U%(l^{!UL8V<liVAy!V<8V@llAmZ9kAmYS*m_Kl89liW$AmPl^{z!U=)li$8U=nca?l_AmDm>lb>l?l^AmDm>lbvS#8U=nca?l_AmDm>lb>l_~Sl^Z6m`>l^~Fl^})!U<7'ma^AmDm>lavCAmDm>lavR#AmDm>lavC)li$~Jl^8U<nb`?l^AmX'ma>l^AmDm>lavC~Fl^})!98ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~ImZEl`8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Im^[&?l`>l_8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~ImZEl`8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Z)k~Bmi&_|!J89m__-m>l_vLAmi$-m>l_vLAmImYU<naiJ?l^AmYJm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm`Ol^8Nm``~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n`iW&>l_AmDm>l_vE~YMl_Im^[&?l`>l_89m__-m>l_vLAmi$-m>l_vLAmImYU<naiJ?l^AmYJm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm`Ol^8Nm``~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n`iW&>l_AmDm>l_vE~YMl_Z)k~Bmi&_|]9-m>l_uIm^[%?l_>l^-m>l_uZ)k~Bmi&^z!T4)l^AmYS/l_X'l^ZKl^}'!V08V0l_8UMl_~Bmu>l^)l^~YHl^Ml^{!UM'l^8V0l_~BmvR0>l^8UMl_AmMl_~ZLl^)l^~YHl^YFl^{!UH8UHmaCm`^8UHmbCma^8UHmbCmat~BmvS;^8UHmbCmav0~BmvS9^8UHmbCmau~BmvS5^>lMl`~BmvS#^9&l_~BmvE^)li&~Bmk^>lMl_}'!UA8UAmCmbZ7lMl`_8LlZ&l`~ImImEmvD`8UAmCmbZ7lMl`_8LlZ&l`~ImIm^~^BmvL_8UAmCmbZ7lMl`_8LlZ&l`~Im^~^BmvK^>lYFl^}'!UO,mYUOla^)l^AmMlaYAl`~i$,mYUOla^)l^AmMlaYAl`~YGmiVOYS$l^~YDl^YAl_)li&AmMl_~BmvL^YUMl^{!A9%l`)l^~^^Z:l^YUAmi&_8V&lYUHm`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl`wT%AmMl_~BmvS'^5mYAl`wU1AmMl_~BmvJ^9%lYUAmLliVP`9(lYAl`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl`AmMl`~BmvS#^)li%AmMl`~BmvS;^)li$AmMl`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Im^[%?l_>l^9%l`)l^~^^Z:l^YUAmi&_8V&lYUHm`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl`wT%AmMl_~BmvS'^5mYAl`wU1AmMl_~BmvJ^9%lYUAmLliVP`9(lYAl`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl`AmMl`~BmvS#^)li%AmMl`~BmvS;^)li$AmMl`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Z1k~Bmi&^z!F)l^AmYUKm__Ml^Im^[%?l_>l^)l^AmYUKm__Ml^Z1k~Bmi&^z!T$)l^AmZBl_X'l^ZCl^}']))liW(y]1)liVGy!N-m>l_>l_Im^[&?l`>l_-m>l_>l_Z)k~Bmi&_|!S/)li$8V=l>l^AmYV$mi$^~?l^{]K#nti%YV9l^{!6)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Im^[%?l_>l^)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Z1k~Bmi&^z!V-8<l?l^{!UK8V$m`^}'!V%(l^{]B)li$8V.l>l^AmYV$mi$^~?l^{]C#nsi&YV>l^{!W(:nti%YV5k!VG:nsi&YV:kAmk!H+miW+^{!W+:npkk!V=8V.l^{!V.:nlkl!-:nlkm!UP:nlkn!V9:nlko!V>:nlkp!V5:nlkq!V::nlkr!U3iT=!SHiT=!T6iT=!T=)l^{!T28U?nal^[$Kl8U:m_YUCmYS-m``_Z*l`Z*l^)lk~Bmk_}'i$z!S-8U?nakKl7,m`^7,m__~Em__Z*l`Z*l^}'[$Kl7*m_YS)m__)l_~Bmk^}'i$z!U'8>mb^)l^~BmEmkbEmk`)lk~Bmk^GmYU:m`a_YUCm`^}'!S)0mYU:mYUCmb``^}']*)l^0m_k~Emk^{!S98U?na_Kl)l^)l_~Em__}'|!TB8U?na_Kl)l_)l^~Em__}'|!S;8<lZNl^{]N+mYU:mYUCmm`m^{!T..mk^{!T@.m_k{!TC+mk^{!S=8UFobi%_Kl8<lEm`^}'|!SC8UFobi%_Kl8<lEm__}'|!U*8UFobi%_Kl.m__}'|!U48UFobi%_Kl.m`^}'|!S18UFobi%_j3|!T#)li${!T<)li%{!TD8UCm`^}'!TA8U?na_iTD8UCm_l~Jl_|!TP8U?na_Kl0m`^}'0m_k~Jl_|!T*8U?n`lKl8U:m`^}'z!T-8U?n`kKl8>m`^}'z!UF)l`8UFo?ldX(m>lda>lb^~i$)l`8UFo?ldX(m>lda>lb^~`~Fla}+!U?)l_8U?n?lbX'm>lb`^~Fl`})!S:iU6!U67&lKl)l_AmYV6mQlc^AmYV(m>lc^?l?lKli${?l?lKli${!S*)li$9'mZ/mYUEmbiSPLl_iS*AmZ'mYUEmaiTE^~Fl>l_|!C)li&,mZ'mZ/mYUEmciSPLl`iCZ'mYUEmaiTE^~Fl>l_|!UE)li&,mYUEm?la_X%l>l_~Fl_}']'8V;m`^}'!V;:nlks]7)l^8ElUmvC>l^~ZFl^{!P)l^8ElGmvC>l^~ZHl^{]F)li$.mvRP>l^~Em>l_vR5{]H)li$.mvSB>l^~Em>l_vS'{]L/lYS,miVM>l^{!S4)li$.mvR/>l^~Em>l_vR${!U59Fl_)l^~^ZHl^{!T?8<lZ=m`^}'!T(8<lYS#m`^}'!S#9ImYPl`YPl^}']=9<mYPl`YPl^}'!S%8S&mYPl`YPl^}'!S88<lZ<m`^}'!SE8<lZIm`^}']I.m>l_>l_}']<.m>l`>l^}'!S&93m`^}'].9(l^z!SJ9(lYV+mk^{!SO8V3nb`>l^})!S>92m`>l^}'!SD(l^{!VJj/!VLi,!W'9(l^{!U;i5!V/)l`8V/nCmca`Gml^~Em_k})!V+8V/ni&`^}'!S.)li$8S.m?la_)l^~YGm>l__>l_~Fl_}']6j5]5)li$95m?la_)l^~Z3m>l__>l_~Fl_}']O)li$9Om?l`^)l_~YGm>l`^~Fl_}'!S,jM]M)li$9Mm?l`^)l_~Z3m>l`^~Fl_}'!V,)l^8V,mGml`?l^~Em`k}'!V38V(maYV,m`^})]2'lYV,m`^}'!V4)l_8V4mCma>l_?l^~Fl^}']&8V4mi&^{]/7%l_[$Kl)li&7)l?l_)l^~Jl?l_,mX*lCm?la?l_>l^~Fl^>l^~Fl^{i$z!B)lk8>mYBl?l_l~Fl^{!5)l^z!TM9+l?l^{!U.9+l>l^{!T99?l?l^{!U-9?l>l^{!SN9$l?l^{!S39$l>l^{!T59Gl?l^{!T,9Gl>l^{!T88Ol>l^{!TG90l?l^{!SB90l>l^{!SM9#l?l^{!SK9#l>l^{!SG9@l?l^{!U/9@l>l^{]+87l?l^{]?87l>l^{]$9,l?l^{]G9,l>l^{]088l>l^{]#98l?l^{]@98l>l^{],(l>l^{]8'l>l^{]>8Ol?l^{!O88l?l^{!7(l?l^{!8'l?l^{!U)8V$m`^}'!T+8V(m`^}'!SP(l^{!TE'l^{!,#nk`^}'!G)li$)li$)li$)li$8Gm>la>l_~YGm?la?l_~YGmQlaQl_~YU>l`)li$~BmpQl_~YU>l_)l^~^Bm`^}'!<+mi$^{]:7'l^)li$0m_k~^X'l?l^~BmvP>l^)li$~Jl^>lc[$Kl)l_)li$7.ndUm`YU:mch/?l`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li$~Jl^{[&Kl)li$0mvR%^~i$)li$0mvR%^~EmvR/^~Em_vR${['Kl)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR${i$i$i$i$Im^[&?l`>l_7'l^)li$0m_k~^X'l?l^~BmvP>l^)li$~Jl^>lc[$Kl)l_)li$7.ndUm`YU:mch/?l`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li$~Jl^{[&Kl)li$0mvR%^~i$)li$0mvR%^~EmvR/^~Em_vR${['Kl)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR${i$i$i$i$u~Bmi&_|];#nnYBl_^X&mi&`#nnYBl_^:nkX'mi&GmbkvP~Emk`[$Kl)l^7-m_`~Emak:nkbUm_vR%)l^7-m_`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i$Im^[&?l`>l_#nnYBl_^X&mi&`#nnYBl_^:nkX'mi&GmbkvP~Emk`[$Kl)l^7-m_`~Emak:nkbUm_vR%)l^7-m_`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i$u~Bmi&_|!V8#nmYS0l_i${]%7&miVN_[$Kl)l^!VN:nkiVN^YV8l^7+m?la_)l^~YGm`?l^>l_~Fl_}'i${!VN?li#!S$(l^{!U&'l^{](#noYBl_^{]A8UEmYV7l_iE{!L8V&lYUEm_iS5{!UE)li&#nkYUEm?la_X%l>l_~Fl_}'!V7'l^{!V&#nnYBl_^{!E#nqk^{!S5'l^{Amk!B)lk8>mYBl?l_l~Fl^{!U$7&m`_[$Kl3l^3l^7+m?la?l^~Fl^)li$~Bma^?l^~Fl^}'i${!U7iT;!T;iSA!SAj4!TIj4]48<lYU>l^{!3+mi&^{!U(+m`^}']3+m`^+m>l`>l^~i$+m`^+m>l`>l^~YIl_~YIl^}']EYUBlt]DYUBls!U8+mi$_)l^~^Bmi%^{!IYUBlq]-YUBll!KYUBlo!MYUBln!DYUBlm!/YUBlk!UB4l)li$+mbQl^~YU>l^{{AmkAmkAmkAmkAmkAmkAmkAmkAmkAmk!):nlkt!2:nlkv.!4:nlkv/!U>:nlkv0!':nlkv1!(:nlkv2!::nlkv3!V(:nlkv4!V$:nlkv5!V6:nlkv6!+:nlkv7!.:nlkv8!>:nlkv9!U::nlkv;!UC:nlkv<!V@:nlkv=!W#:nnm:nk:nki&vCvR3!VC:nki&wS@!VDGmlk!W*:nqkvC!VA:nnq:nk:nk:nk:nk:nk:nki&vLvJvR#vS&vR#vK!W$:nnl:nki&vC!W&:nk:nk:nk:nki&:nk:nki&vEvE:nk:nki&vS#vS#:nk:nki&vS;v0:nk:nki&vS5u!VO:nnl:nki&vR#!VP:nqkvF!VH:nk:nk:nk:nki&:nk:nki&v0wTO:nk:nki&twS<:nk:nki&vCwT>:nk:nki&ux9!VM:nk:nk:nk:nk:nki&v0v.utvC!0:nlkv:!*:nlkuy\0"; // RVM code that prints HELLO!

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s++;
  }
}

// === Rib definitions & tagging ===
//  - Ribs are tagged with the lowest bit set to 0
//  - Numbers are tagged with the lowest bit set to 1
struct rib {      // a Rib object
  long field0;
  long field1;
  long field2;
};
typedef long obj; // a tagged value
typedef long num; // a number

// Ribs are of size 4 for tagging

#define RIB_NB_FIELDS 4
#define RIB(x) ((struct rib *)(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

#define UNTAG(x) ((x) >> 1)
#define NUM(x) ((num)(UNTAG((num)(x))))

#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))

// Ribs macro accessor
#define CAR(x) RIB(x)->field0
#define CDR(x) RIB(x)->field1
#define TAG(x) RIB(x)->field2

// VM definitions
#define TOS CAR(stack)
#define NUM_0 (TAG_NUM(0))

#define INSTR_AP 0
#define INSTR_SET 1
#define INSTR_GET 2
#define INSTR_CONST 3
#define INSTR_IF 4
#define INSTR_HALT 5

#define PAIR_TAG TAG_NUM(0)
#define CLOSURE_TAG TAG_NUM(1)
#define SYMBOL_TAG TAG_NUM(2)
#define STRING_TAG TAG_NUM(3)
#define SINGLETON_TAG TAG_NUM(5)

// the only three roots allowed
obj stack = NUM_0;
obj pc = NUM_0;
obj FALSE = NUM_0;

// True and NIL are hidden inside FALSE
#define TRUE (CAR(FALSE))
#define NIL (CDR(FALSE))

// Temp values that can be used to shield
//  pointers from the evil GC
#define TEMP1 CAR(TRUE)
#define TEMP2 CDR(TRUE)
#define TEMP3 CAR(NIL)
#define TEMP4 CDR(NIL)

// global, but not a root, referenced
obj symbol_table = NUM_0;

// === GC ===
const long max_nb_objs = 100000;
const long rib_nb_fields = 4;
const long space_size = (max_nb_objs * rib_nb_fields);
obj *heap_start, *heap_mid, *heap_end;
obj *alloc;
obj *alloc_limit;
obj *scan;

// We use NULL as a broken heart for the GC
#define GC_COPIED_OBJ ((obj)NULL)

void init_heap() {
  heap_start = malloc(sizeof(long) * (space_size + 1));

  // make sure heap_start is even (for pnut)
  if (((long)heap_start) & 1 == 1) {
    heap_start++;
  }

  if (!heap_start) {
    exit(7);
  }

  heap_mid = heap_start + (space_size >> 1);
  heap_end = heap_start + space_size;
  alloc = ((obj *)(heap_start));
  alloc_limit = heap_mid;
  stack = (((((obj)(0)) << 1) | 1));
}

// Copy an object to the to_space
obj copy(obj o) {
  obj *ptr, field0, copy;
  // we sometime reference rib that are allocated in BSS,
  // we do not want to copy those
  if (IS_RIB(o)) {
    ptr = (long*)RIB(o);
    field0 = ptr[0];

    if (field0 == GC_COPIED_OBJ) {
      copy = ptr[1]; // copied, get new address
    } else {
      copy = TAG_RIB(alloc);
      *ptr++ = GC_COPIED_OBJ; // ptr points to CDR
      *alloc++ = field0;
      *alloc++ = *ptr++; // ptr points to TAG
      *alloc++ = *ptr;
      *alloc++ = NUM_0; // empty slot for alignment (pnut)

      ptr[-1] = copy; // set forward ptr. Since it points to TAG, ptr[-1]
                      // rewrites the CDR
    }
    return copy;
  }
  return o;
}

// Stop and copy GC
void gc() {
  // swap to_space and from_space
  obj* to_space;
  if (alloc_limit == heap_mid){
    to_space = heap_mid;
    alloc_limit = heap_end;
  }
  else{
    to_space = heap_start;
    alloc_limit = heap_mid;
  }

  alloc = to_space;

  // root: stack
  stack = copy(stack);

  // root: pc
  pc = copy(pc);

  // root: false
  FALSE = copy(FALSE);

  // scan the to_space to pull all live references
  scan = to_space;
  while (scan != alloc) {
    *scan = copy(*scan); // use scan pointer
    scan++;
  }
}

// === Ribs allocation and stack manipulation ===
obj pop() {
  obj x = CAR(stack);
  stack = CDR(stack);
  return x;
}

// Ribs are allocated using this push function
void push2(obj car, obj tag) {
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;
  *alloc++ = NUM_0; // make sure a rib is always aligned (for pnut)

  stack = TAG_RIB((struct rib *)(alloc - RIB_NB_FIELDS));

  if (alloc == alloc_limit) {
    gc();
  }
}

/**
 * Allocate a rib that is not kept on the stack (can be linked
 * from anywhere). The car and cdr can be live references to other
 * ribs.
 */
struct rib *alloc_rib(obj car, obj cdr, obj tag) {
  obj old_stack, allocated;

  push2(car, cdr); // tag is set
  old_stack = CDR(stack);
  allocated = stack;

  CDR(allocated) = TAG(allocated);
  TAG(allocated) = tag;

  stack = old_stack;

  return RIB(allocated);
}

struct rib *alloc_rib2(obj car, obj cdr, obj tag) {
  obj old_stack, allocated;
  push2(car, tag); // tag is set
  old_stack = CDR(stack);
  allocated = stack;

  CDR(allocated) = cdr;

  stack = old_stack;

  return RIB(allocated);
}


// === Utility functions ===

struct rib *list_tail(struct rib *lst, num i) {
  while (i--) {
    lst = RIB(lst->field1);
  }
  return lst;
}

struct rib *inst_tail(struct rib *lst, num i){
  while (i--) {
    lst = RIB(lst->field2);
  }
  return lst;
}

obj list_ref(struct rib *lst, num i) {
  return list_tail(lst, i)->field0;
}

obj get_opnd(obj o) {
  struct rib* return_value;
  if (IS_NUM(o)) {
    return_value = RIB(list_tail(RIB(stack), NUM(o)));
  }
  else{
    return_value = RIB(o);
  }
  return return_value->field0;
}

obj get_cont() {
  obj s = stack;

  while (!NUM(TAG(s))) {
    s = CDR(s);
  }

  return s;
}

obj lst_length(obj list) {
  long l = 0;

  while (IS_RIB(list) && NUM(TAG(list)) == 0) {
    ++l;
    list = CDR(list);
  }

  return TAG_NUM(l);
}

// === Bytecode (RIBN) reading ===
#define ENCODING_SIZE  (92)
#define HALF_ENCODING_SIZE ENCODING_SIZE/2

int pos = 0; // Position in the input buffer
char get_byte() { return input[pos++]; }

num get_code() {
  num x = get_byte() - 35;
  return x < 0 ? 57 : x;
}

num get_int(num n) {
  num x = get_code();
  n *= HALF_ENCODING_SIZE;
  if (x < HALF_ENCODING_SIZE){
    return n+x;
  }
  else{
    return get_int(n+x-HALF_ENCODING_SIZE);
  }
}

// === Symbol table ===
struct rib *symbol_ref(num n) { return RIB(list_ref(RIB(symbol_table), n)); }

struct rib *create_sym(obj name) {
  struct rib *list = alloc_rib(name, lst_length(name), STRING_TAG);
  struct rib *sym = alloc_rib(FALSE, TAG_RIB(list), SYMBOL_TAG);
  struct rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
  return root;
}

void build_sym_table() {
  num n = get_int(0);
  obj accum;
  char c;

  while (n > 0) {
    n--;
    symbol_table = TAG_RIB(create_sym(NIL));
  }

  accum = NIL;

  while (1) {
    c = get_byte();

    if (c == 44) {
      symbol_table = TAG_RIB(create_sym(accum));
      accum = NIL;
      continue;
    }

    if (c == 59)
      break;

    accum = TAG_RIB(alloc_rib(TAG_NUM(c), TAG_RIB(accum), PAIR_TAG));
  }

  symbol_table = TAG_RIB(create_sym(accum));
}

void set_global(obj c) {
  CAR(CAR(symbol_table)) = c;
  symbol_table = CDR(symbol_table);
}


// === Decoding of the bytecode ===
//int weights[6] = {20, 30, 0, 10, 11, 4};
int weights[6];
void init_weights(){
  weights[0] = 20;
  weights[1] = 30;
  weights[2] = 0;
  weights[3] = 10;
  weights[4] = 11;
  weights[5] = 4;
}

void decode() {
  obj n;
  int d, op;
  num x;
  struct rib *c;

  while (1) {
    x = get_code();
    n = x;
    op = -1;

    while (n > 2 + (d = weights[++op])) {
      n -= d + 3;
    }

    if (x > 90) {
      op = INSTR_IF;
      n = pop();
    } else {
      if (!op) {
        push2(NUM_0, NUM_0);
      }

      if (n >= d) {
        if (n == d){
          n = TAG_NUM(get_int(0));
        }
        else{
          n = TAG_RIB(symbol_ref(get_int(n - d - 1)));
        }
      } else {
        if (op < 3){
          n = TAG_RIB(symbol_ref(n));
        }
        else{
          n = TAG_NUM(n);
        }
      }

      if (op > 4) {
        n = TAG_RIB(
            alloc_rib(TAG_RIB(alloc_rib2(n, NUM_0, pop())), NIL, CLOSURE_TAG));
        if (stack == NUM_0) {
          break;
        }
        op = INSTR_CONST;
      } else if (op > 0) {
        op--;
      } else {
        op = 0;
      }
    }

    c = alloc_rib(TAG_NUM(op), n, 0);
    c->field2 = TOS;
    TOS = TAG_RIB(c);
  }

  pc = TAG(CAR(n));
}

// === Conversion functions ===
char* scm2str(obj s) {
    int i = 0;
    int length = (int) ((num)((((num)(((struct rib *)(s))->field1)) >> 1)));
    obj current = ((struct rib *)(s))->field0;
    char* str = malloc(length + 1);

    for (i = 0; i < length; i++) {
        str[i] = (char) ((num)((((num)(((struct rib *)(current))->field0)) >> 1)));
        current = ((struct rib *)(current))->field1;
    }

    str[length] = '\0';

    return str;
}

obj bool2scm(bool x) {
  if (x){
    return CAR(FALSE);
  }
  else{
    return FALSE;
  }
}

// === Primitives ===
#define PRIM1() x = pop()
#define PRIM2()                                                                \
  y = pop();                                                               \
  PRIM1()
#define PRIM3()                                                                \
  z = pop();                                                               \
  PRIM2()

// Primitives table for the VM
obj prim(int no) {
  obj x, y, z, new_rib, arg;
  int file;
  char* buffer = malloc(1);
  int success, bytes_read, num_args;
  char* filename;
  if (no == 0){ // (##rib x y z) - creation of a rib
    new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    PRIM3();
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
  }
  else if (no == 1) // (##close-input/output-port x) - close a file descriptor
  {
    PRIM1();
    close(NUM(x));
  }
  else if (no == 2) // (##write x fd) - write a byte to a file
  {
    PRIM2();
    buffer[0] = (char) NUM(x);
    success = write(NUM(y), buffer, 1);
    if (success != 1) {
      perror("Cannot write to file.");
    }
    push2(TRUE, PAIR_TAG);
  }
  else if (no == 3) // (##read x) - read a byte from a file
  {
    PRIM1();
    bytes_read = read(NUM(x), buffer, 1);
    if (!bytes_read) push2(NIL, PAIR_TAG);
    else push2(TAG_NUM(buffer[0]), PAIR_TAG);
  }
  else if (no == 4) // (##open-output-file x) - open a file for writing
  {
    PRIM1();
    filename = scm2str(x);
    file = open(filename, O_WRONLY);
    if (file < 0){
      push2(FALSE, PAIR_TAG);
    }
    else{
      push2(TAG_NUM(file), PAIR_TAG);
    }
    free((void *) filename);
  }
  else if(no == 5) // (##open-input-file x) - open a file for reading
  {
    PRIM1();
    filename = scm2str(x);
    file = open(filename, O_RDONLY);
    if (file < 0){
      push2(FALSE, PAIR_TAG);
    }
    else{
      push2(TAG_NUM(file), PAIR_TAG);
    }
    free((void*) filename);
  }
  else if(no == 6) // (##stdin) - get the file descriptor for stdin
  {
    push2(TAG_NUM(1), PAIR_TAG);
  }
  else if(no == 7) // (##stdout) - get the file descriptor for stdout
  {
    push2(TAG_NUM(0), PAIR_TAG);
  }
  else if(no == 8) // (##apply x y) - apply a function to a list of arguments
  {
    PRIM2();
    num_args = 0;
    TEMP1 = x; // save x for the gc
    arg = TAG_RIB(y);
    while (arg != NIL) {
      push2(arg, PAIR_TAG); // make sure the arg doesn't get GC'd
      arg = CAR(stack);
      CAR(stack) = CAR(arg);
      arg = TAG_RIB(CDR(arg));
      num_args++;
    }
    push2(TAG_NUM(num_args), PAIR_TAG);
    x = TEMP1; // retrive x from possibly GC'd
    return TAG_RIB(x);
  }
  else if(no == 9) // (##id x) - identity
  {
    PRIM1();
    push2(x, PAIR_TAG);
  }
  else if(no == 10) // (##arg1 x) - pop a value from the stack
  {
    pop();
  }
  else if(no == 11) // (##arg2 x)
  {
    x = pop();
    pop();
    push2(x, PAIR_TAG);
  }
  else if(no == 12) // (##close) - creates a closure
  {
    x = CAR(TOS);
    y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
  }
  else if(no == 13) // (##rib? x) - check if x is a rib
  {
    PRIM1();
    push2(bool2scm(IS_RIB(x)), PAIR_TAG);
  }
  else if(no == 14) // (##field0 x) - get the first field of a rib
  {
    PRIM1();
    push2(CAR(x), PAIR_TAG);
  }
  else if(no == 15) // (##field1 x) - get the second field of a rib
  {
    PRIM1();
    push2(CDR(x), PAIR_TAG);
  }
  else if(no == 16) // (##field2 x) - get the third field of a rib
  {
    PRIM1();
    push2(TAG(x), PAIR_TAG);
  }
  else if(no == 17) // (##field0-set! x y) - set the first field of a rib
  {
    PRIM2();
    push2(CAR(x) = y, PAIR_TAG);
  }
  else if(no == 18) // (##field1-set! x y) - set the second field of a rib
  {
    PRIM2();
    push2(CDR(x) = y, PAIR_TAG);
  }
  else if(no == 19) // (##field2-set! x y) - set the third field of a rib
  {
    PRIM2();
    push2(TAG(x) = y, PAIR_TAG);
  }
  else if(no == 20) // (##eqv? x y) - check if x and y are equal
  {
    PRIM2();
    push2(bool2scm(x == y), PAIR_TAG);
  }
  else if(no == 21) // (##< x y) - check if x is less than y
  {
    PRIM2();
    push2(bool2scm(NUM(x) < NUM(y)), PAIR_TAG);
  }
  else if(no == 22) // (##+ x y) - add x and y
  {
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
  }
  else if(no == 23) // (##- x y) - subtract x and y
  {
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
  }
  else if(no == 24) // (##* x y) - multiplies x and y
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
  }
  else if(no == 25) // (##quotient x y) - integer division
  {
    PRIM2();
    // To avoid division by zero bug with ksh :
    //   https://github.com/ksh93/ksh/issues/770
    if (NUM(y) < 0){
      push2(TAG_NUM(-(NUM(x) / -NUM(y))), PAIR_TAG);
    }
    else{
      push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    }
  }
  else if(no == 26) // (##exit n) - exits the program
  {
    PRIM1();
    exit(NUM(x));
  }
  else{
    exit(6); // illegal instruction
  }
  free(buffer);
  return TAG_NUM(0);
}


// === MAIN INTERPRETER LOOP ===
void run() {
  int i;
  num instr, nargs, nparams_vari, nparams, vari;
  bool jump;
  obj proc, s2, c2, k, new_pc, rest, p, x;
  struct rib* opnd;

  while (1) {
    instr = NUM(CAR(pc));
    //printf("instr : %d\n", instr);
    if (instr == INSTR_HALT) {
      exit(0);
    }
    else if(instr == INSTR_AP) {
      jump = TAG(pc) == NUM_0;
      proc = get_opnd(CDR(pc));
      //show_rib(proc, 0);
      while (1) {
        if (IS_NUM(CAR(proc))) {
          pop();
          proc=prim(NUM(CAR(proc)));

          if (IS_RIB(proc)) continue;

          if (jump) {
            // jump
            pc = get_cont();
            CDR(stack) = CAR(pc);
          }
          pc = TAG(pc);
        } else {
          nargs = NUM(pop());
          s2 = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));
          proc = CDR(s2);
          CAR(pc) = CAR(proc); // save the proc from the mighty gc


          nparams_vari = NUM(CAR(CAR(proc)));
          nparams = nparams_vari >> 1;
          vari = nparams_vari&1;
          if (vari ? nparams > nargs : nparams != nargs) {
            // printf("*** Unexpected number of arguments nargs: %d nparams: %d vari: %d\n", nargs, nparams, vari);
            putstr("Unexpected number of arguments\n");
            exit(1);
          }
          nargs-=nparams;
          if (vari){
            rest = NIL;
            for(i = 0; i < nargs; ++i){
              rest = TAG_RIB(alloc_rib(pop(), rest, s2));
              s2 = TAG(rest);
              TAG(rest) = PAIR_TAG;
            }
            s2 = TAG_RIB(alloc_rib(rest, s2, PAIR_TAG));
          }
          for (i = 0; i < nparams; ++i) {
            s2 = TAG_RIB(alloc_rib(pop(), s2, PAIR_TAG));
          }

          nparams = nparams + vari;
          c2 = TAG_RIB(list_tail(RIB(s2), nparams));

          if (jump) {
            k = get_cont();
            CAR(c2) = CAR(k);
            TAG(c2) = TAG(k);
          } else {
            CAR(c2) = stack;
            TAG(c2) = TAG(pc);
          }

          stack = s2;

          new_pc = CAR(pc);
          CAR(pc) = TAG_NUM(instr);
          pc = TAG(new_pc);
        }
        break;
      }
    }
    else if(instr == INSTR_SET){
      x = CAR(stack);
      if (IS_NUM(CDR(pc))){
        opnd = RIB(list_tail(RIB(stack), NUM(CDR(pc))));
      }
      else{
        opnd = RIB(CDR(pc));
      }
      opnd->field0 = x;
      stack = CDR(stack);
      pc = TAG(pc);
    }
    else if (instr == INSTR_GET){
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      pc = TAG(pc);
    }
    else if (instr == INSTR_CONST){
      push2(CDR(pc), PAIR_TAG);
      pc = TAG(pc);
    }
    else if (instr == INSTR_IF) { // if
      p = pop();
      if (p != FALSE) {
        pc = CDR(pc);
      } else {
        pc = TAG(pc);
      }
    }
    else{
      exit(6); // illegal instruction
    }
  }
}

void setup_stack() {
  obj first;
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG);

  first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(INSTR_HALT);
  CDR(first) = NUM_0;
  TAG(first) = PAIR_TAG;
}

void init() {
  init_weights();
  init_heap();

  FALSE = TAG_RIB(alloc_rib(TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            TAG_RIB(alloc_rib(NUM_0, NUM_0, SINGLETON_TAG)),
                            SINGLETON_TAG));

  build_sym_table();
  decode();

  set_global(
      TAG_RIB(alloc_rib(NUM_0, symbol_table, CLOSURE_TAG))); /* primitive 0 */
  set_global(FALSE);
  set_global(TRUE);
  set_global(NIL);

  setup_stack();

  run();
}

int main() { init(); }

// Debug procedures
// #ifdef DEBUG
// void show_rib(obj s, int depth){
//     if (depth > 3){
//         if (IS_RIB(s)){
//             printf("[Array]");
//             return;
//         }
//     }
//     if (IS_RIB(s)){
//       //printf("(%d)", s);
//         printf("[ ");
//         show_rib(CAR(s), depth+1);
//         printf(", ");
//         show_rib(CDR(s), depth+1);
//         printf(", ");
//         show_rib(TAG(s), depth+1);
//         printf(" ]");
//     }
//     else{
//         printf("%d", NUM(s));
//     }
// }
//
// void show_stack(){
//     obj itr = stack;
//     PRINTLN();
//     if (NUM(TAG(itr))){
//         printf("[]");
//         return;
//
//     }
//     printf("[ ");
//     int first = 0;
//     while(!NUM(TAG(itr))){
//         if (first){
//             printf(", ");
//         }
//         else{
//             first = 1;
//         }
//         show_rib(CAR(itr), 0);
//         itr = CDR(itr);
//     }
//     printf(" ]");
//
// }
// #endif
