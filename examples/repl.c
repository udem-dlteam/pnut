/*
 * A R4RS repl compiled with the Ribbit Scheme Compiler 
 *
 * For more details about ribbit, see the repository: https://github.com/udem-dlteam/ribbit
 *
 * Author : Léonard Oest O'leary (leo-ard)
 */

#ifndef PNUT_CC
#include <stdio.h>
#include <stdlib.h>
#endif



char* input = "RD?naeloob,?xelpmoc,cc/llac,?citebahpla-rahc,<,dnuor,dna,etouq,gnirts,raaaac,radddc,raaddc,?=>ic-gnirts,esle,>,!rdc-tes,?qe,oludom,tsil>-rotcev,htgnel-gnirts,?tsil,fer-gnirts,-,nruter,esac,rddddc,certel,gnicilps-etouqnu,!tes,?rebmun,daol,rdadac,ro,rac,tneitouq,?orez,xam,/,?evitisop,?=>ic-rahc,ecaps,roolf,?tcaxe,?laer,?=>gnirts,rdaddc,raddac,adbmal,gniliec,rdaadc,elif-tuptuo-htiw-llac,fi,mcl,tropxe,gnirtsbus,!tes-gnirts,?evitagen,+,raaadc,!rac-tes,*,etouqnu,?=<ic-rahc,enifed,?=ic-gnirts,etouqisauq,elif-tupni-htiw-llac,?tcaxeni,rdc,!tes-rotcev,rddadc,rddaac,nigeb,radaac,rotcev-ekam,tel,etacnurt,rdaaac,?=gnirts,?=<rahc,htgnel-rotcev,=<,raadac,?lanoitar,_,?=<gnirts,fer-rotcev,=>,bat,?ddo,noitaunitnoc-tnerruc-htiw-llac,nim,?=>rahc,gnirts-ekam,?=<ic-gnirts,regetni>-rahc,?ciremun-rahc,radadc,dnoc,=,dneppa-gnirts,trop-tuptuo-esolc,cossa,dcg,qmem,?>ic-gnirts,hcae-rof,redniamer,?<ic-gnirts,?<gnirts,?=rahc,?=ic-rahc,gnirts>-lobmys,?>ic-rahc,?>gnirts,rebmem,?neve,vmem,?ecapsetihw-rahc,elif-tuptuo-nepo,lave,?>rahc,?esac-rewol-rahc,raadc,?esac-reppu-rahc,?trop-tuptuo,?trop-tupni,elif-tupni-nepo,trop-tupni-esolc,tsil>-gnirts,raaac,raddc,rdddac,?<ic-rahc,?<rahc,gnirts>-rebmun,rebmun>-gnirts,enilwen,raac,esacnwod-rahc,qssa,vssa,?regetni,?vqe,fer-tsil,trop-tupni-tnerruc,radac,dneppa,rotcev,?erudecorp,radc,rdddc,sba,trop-tuptuo-tnerruc,rotcev>-tsil,ylppa,esrever,lobmys>-gnirts,rdadc,rdaac,esacpu-rahc,rddac,rahc-etirw,?gnirts,gnirts>-tsil,?rotcev,etirw,?rahc,?tcejbo-foe,?lauqe,rahc-keep,rahc>-regetni,?lobmys,pam,htgnel,daer,,,,,ton,,,yalpsid,rdac,rddc,rahc-daer,tsil,,?llun,,,,?riap,,,snoc,,,,,,,,,;8V1k!T1)li%zAmk!TH7%lYAl_Im^[$Kl7(lYAlbAmZJl^)li&AmZBlb~YHl^{i$ZCl^{!V18V1kAmZ9kAmYJlZJl^99k~YHl^YAkAmPliW#y]J7$kWmi&:nHniW%ai&kk{!@#niVK`^}'!W%Rm:nkw)iW)l!U9)l_,mYU9ma?l_>l^~Fl^}'!UG)l^8UGnUmlb?l`^)l`~Bm_>l_~Fl_})!V)8;mVma_8;mVmaUm`l~Z4l_cYUGnka_?lb>la1nYV)ofd?lbCmai$>l`^~Fl_}+!V##n:nckiVE#nQla~i$#n:nckiVE#nQla~i$#n:nckiVE#nQla~i$#n:nckiVE#nQla~YU>lQla~Bmw*?la~BmiW)>la~YU>la_iVB}'!?#na_iW)#nk_iW)~BmiW%_}'!=1nb1nRm:nTng?lecw*iW)m~Fl?la>l`^})!UI8=nebb1nYUIqRm:nh4w2iW)m1nYUIqh1~BmiW%h1Cmh0eh-?lf?ldCmci$_`>la>l_~Fl_}/!V?7&ml_Im^[$Kl8U;nX,mb?l`X+ma>l_wVL8U;nX,mUmlb?l`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l`Z0l_wVJ~Bml_~i$8U;nX,mb?l`X+ma>l_wVL8U;nX,mUmlb?l`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l`Z0l_wVJ~Bml_~BmwTKZ8l^~Fl>l^8U;nX,mGmlb?l`YUJlwT)wVL88l^~Bml_~BmwT)>l^8UJl^8U;mX+ma>l_wW'~YKl^~SlFl^}'i${!UJ8U;m_wU1{!UD,mLnca_wT3})!1#nb`iVE8UIqgdLlCmbwS@LlaiVC`8V)oCmfbYBl`_`~YDl_?l`1nei$1neImYUDnCmCm?lddwTNCm?l`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l`~BmwTN^1nci$1ncYUDnCmNldwS2CmZ$lcwSLZ#la1ncCmZ$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl`>l_wTJ{YCmaKl5mi$>l^{wSI`Ol`~BmwTL^8UIqgdNldYCmai8YCm`iTE`Ol`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^Im^~^YDl^Ol`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc`YOlb`1nYV#meYUGnlc>l`CmCmNld?l`wT7`~Fl^Ol`~ImBmwT'_8UIqgdLlCmbwS@LlaiVC`8V)oCmfbYBl`_`~YDl_?l`1nei$1neImYUDnCmCm?lddwTNCm?l`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l`~BmwTN^1nci$1ncYUDnCmNldwS2CmZ$lcwSLZ#la1ncCmZ$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl`>l_wTJ{YCmaKl5mi$>l^{wSI`Ol`~BmwTL^8UIqgdNldYCmai8YCm`iTE`Ol`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_YU:mYBl`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVE`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i$i$ak`iVEYU@mi&`~_UmYU:mYUNlaml~^Im^~^YDl^Ol`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc`YOlb`1nYV#meYUGnlc>l`CmCmNld?l`wT7`~Fl^Ol`~Im^~^BmwTJ^1ncYV?lOla_~BmwT%^#ncOlaiVE~BmwU1^>l_~Fl_#nbYUGnk``iVF~YDl_})!V*)l^8V*l?l^~Fl^{!U@9&lCm`^8U@mCma>l_?l^~Fl^}'!UN)lk8>mYUNl?l_l~Fl^{!;#na_iVE}'!VKl!VIo!VEn!VFm!VBl!W)k!V'7&mZAlaZAl_[$Kl)lk)liVD~Fl_)ll)ll)liVD~Z=m__7,m?lb?l`~YS%m__>l`>l^~Fl_~Fl^}'i$}'!V27&m>la>l_[$Kl)lk)liVD~Fl_)ll7,m?lb?l`)ll~Em`^)liVD~Em__>l`>l^~Fl_~Fl^}'i$}'!S08LlZ'mYCm`jAj/z!T07(oi&ca_[$Kl8V&la7/oCmfZ2mb>lb`a_Gml`~Ema_}+i$})!U,8<lYS(m`^}'!S68<lYS+m`^}'!S+.mYV'ma_k}'!S(.mkYV'm`^}'!T&+mkYV'm`^}'!T:8<lYS'm`^}'!S?8<lZPm`^}']P.mYV2ma_k}'!S'.mkYV2m`^}'!SF8Gm`^}'!U08Ll^z!S78LlYV+m__Im^[&?l`>l_8LlYV+m__iW*~Bmi&_|!T/8V3n>lb`>l^})!U#8ElZ2m`>l^}'!U%(l^{!UL8V<liVAy!V<8V@llAmZ9kAmYS*m_Kl89liW$AmPl^{z!U=)li$8U=nca?l_AmDm>lb>l?l^AmDm>lbvS#8U=nca?l_AmDm>lb>l_~Sl^Z6m`>l^~Fl^})!U<7'ma^AmDm>lavCAmDm>lavR#AmDm>lavC)li$~Jl^8U<nb`?l^AmX'ma>l^AmDm>lavC~Fl^})!98ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~ImZEl`8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Im^[&?l`>l_8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~ImZEl`8ULk89lLnQla?l`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi$-m>l_vLAmImYU<nai9?l^AmPm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_8U=n`i&>l_~YMl_8U=n`i&>l?l_~YDl_-m>l_vLAmYU<n`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Z)k~Bmi&_|!J89m__-m>l_vLAmi$-m>l_vLAmImYU<naiJ?l^AmYJm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm`Ol^8Nm``~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n`iW&>l_AmDm>l_vE~YMl_Im^[&?l`>l_89m__-m>l_vLAmi$-m>l_vLAmImYU<naiJ?l^AmYJm`>l^>l_~Em?l`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm`Ol^8Nm``~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n`iW&>l_AmDm>l_vE~YMl_Z)k~Bmi&_|]9-m>l_uIm^[%?l_>l^-m>l_uZ)k~Bmi&^z!T4)l^AmYS/l_X'l^ZKl^}'!V08V0l_8UMl_~Bmu>l^)l^~YHl^Ml^{!UM'l^8V0l_~BmvR0>l^8UMl_AmMl_~ZLl^)l^~YHl^YFl^{!UH8UHmaCm`^8UHmbCma^8UHmbCmat~BmvS;^8UHmbCmav0~BmvS9^8UHmbCmau~BmvS5^>lMl`~BmvS#^9&l_~BmvE^)li&~Bmk^>lMl_}'!UA8UAmCmbZ7lMl`_8LlZ&l`~ImImEmvD`8UAmCmbZ7lMl`_8LlZ&l`~ImIm^~^BmvL_8UAmCmbZ7lMl`_8LlZ&l`~Im^~^BmvK^>lYFl^}'!UO,mYUOla^)l^AmMlaYAl`~i$,mYUOla^)l^AmMlaYAl`~YGmiVOYS$l^~YDl^YAl_)li&AmMl_~BmvL^YUMl^{!A9%l`)l^~^^Z:l^YUAmi&_8V&lYUHm`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl`wT%AmMl_~BmvS'^5mYAl`wU1AmMl_~BmvJ^9%lYUAmLliVP`9(lYAl`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl`AmMl`~BmvS#^)li%AmMl`~BmvS;^)li$AmMl`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Im^[%?l_>l^9%l`)l^~^^Z:l^YUAmi&_8V&lYUHm`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl`wT%AmMl_~BmvS'^5mYAl`wU1AmMl_~BmvJ^9%lYUAmLliVP`9(lYAl`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl`AmMl`~BmvS#^)li%AmMl`~BmvS;^)li$AmMl`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Z1k~Bmi&^z!F)l^AmYUKm__Ml^Im^[%?l_>l^)l^AmYUKm__Ml^Z1k~Bmi&^z!T$)l^AmZBl_X'l^ZCl^}']))liW(y]1)liVGy!N-m>l_>l_Im^[&?l`>l_-m>l_>l_Z)k~Bmi&_|!S/)li$8V=l>l^AmYV$mi$^~?l^{]K#nti%YV9l^{!6)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Im^[%?l_>l^)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Z1k~Bmi&^z!V-8<l?l^{!UK8V$m`^}'!V%(l^{]B)li$8V.l>l^AmYV$mi$^~?l^{]C#nsi&YV>l^{!W(:nti%YV5k!VG:nsi&YV:kAmk!H+miW+^{!W+:npkk!V=8V.l^{!V.:nlkl!-:nlkm!UP:nlkn!V9:nlko!V>:nlkp!V5:nlkq!V::nlkr!U3iT=!SHiT=!T6iT=!T=)l^{!T28U?nal^[$Kl8U:m_YUCmYS-m``_Z*l`Z*l^)lk~Bmk_}'i$z!S-8U?nakKl7,m`^7,m__~Em__Z*l`Z*l^}'[$Kl7*m_YS)m__)l_~Bmk^}'i$z!U'8>mb^)l^~BmEmkbEmk`)lk~Bmk^GmYU:m`a_YUCm`^}'!S)0mYU:mYUCmb``^}']*)l^0m_k~Emk^{!S98U?na_Kl)l^)l_~Em__}'|!TB8U?na_Kl)l_)l^~Em__}'|!S;8<lZNl^{]N+mYU:mYUCmm`m^{!T..mk^{!T@.m_k{!TC+mk^{!S=8UFobi%_Kl8<lEm`^}'|!SC8UFobi%_Kl8<lEm__}'|!U*8UFobi%_Kl.m__}'|!U48UFobi%_Kl.m`^}'|!S18UFobi%_j3|!T#)li${!T<)li%{!TD8UCm`^}'!TA8U?na_iTD8UCm_l~Jl_|!TP8U?na_Kl0m`^}'0m_k~Jl_|!T*8U?n`lKl8U:m`^}'z!T-8U?n`kKl8>m`^}'z!UF)l`8UFo?ldX(m>lda>lb^~i$)l`8UFo?ldX(m>lda>lb^~`~Fla}+!U?)l_8U?n?lbX'm>lb`^~Fl`})!S:iU6!U67&lKl)l_AmYV6mQlc^AmYV(m>lc^?l?lKli${?l?lKli${!S*)li$9'mZ/mYUEmbiSPLl_iS*AmZ'mYUEmaiTE^~Fl>l_|!C)li&,mZ'mZ/mYUEmciSPLl`iCZ'mYUEmaiTE^~Fl>l_|!UE)li&,mYUEm?la_X%l>l_~Fl_}']'8V;m`^}'!V;:nlks]7)l^8ElUmvC>l^~ZFl^{!P)l^8ElGmvC>l^~ZHl^{]F)li$.mvRP>l^~Em>l_vR5{]H)li$.mvSB>l^~Em>l_vS'{]L/lYS,miVM>l^{!S4)li$.mvR/>l^~Em>l_vR${!U59Fl_)l^~^ZHl^{!T?8<lZ=m`^}'!T(8<lYS#m`^}'!S#9ImYPl`YPl^}']=9<mYPl`YPl^}'!S%8S&mYPl`YPl^}'!S88<lZ<m`^}'!SE8<lZIm`^}']I.m>l_>l_}']<.m>l`>l^}'!S&93m`^}'].9(l^z!SJ9(lYV+mk^{!SO8V3nb`>l^})!S>92m`>l^}'!SD(l^{!VJj/!VLi,!W'9(l^{!U;i5!V/)l`8V/nCmca`Gml^~Em_k})!V+8V/ni&`^}'!S.)li$8S.m?la_)l^~YGm>l__>l_~Fl_}']6j5]5)li$95m?la_)l^~Z3m>l__>l_~Fl_}']O)li$9Om?l`^)l_~YGm>l`^~Fl_}'!S,jM]M)li$9Mm?l`^)l_~Z3m>l`^~Fl_}'!V,)l^8V,mGml`?l^~Em`k}'!V38V(maYV,m`^})]2'lYV,m`^}'!V4)l_8V4mCma>l_?l^~Fl^}']&8V4mi&^{]/7%l_[$Kl)li&7)l?l_)l^~Jl?l_,mX*lCm?la?l_>l^~Fl^>l^~Fl^{i$z!B)lk8>mYBl?l_l~Fl^{!5)l^z!TM9+l?l^{!U.9+l>l^{!T99?l?l^{!U-9?l>l^{!SN9$l?l^{!S39$l>l^{!T59Gl?l^{!T,9Gl>l^{!T88Ol>l^{!TG90l?l^{!SB90l>l^{!SM9#l?l^{!SK9#l>l^{!SG9@l?l^{!U/9@l>l^{]+87l?l^{]?87l>l^{]$9,l?l^{]G9,l>l^{]088l>l^{]#98l?l^{]@98l>l^{],(l>l^{]8'l>l^{]>8Ol?l^{!O88l?l^{!7(l?l^{!8'l?l^{!U)8V$m`^}'!T+8V(m`^}'!SP(l^{!TE'l^{!,#nk`^}'!G)li$)li$)li$)li$8Gm>la>l_~YGm?la?l_~YGmQlaQl_~YU>l`)li$~BmpQl_~YU>l_)l^~^Bm`^}'!<+mi$^{]:7'l^)li$0m_k~^X'l?l^~BmvP>l^)li$~Jl^>lc[$Kl)l_)li$7.ndUm`YU:mch/?l`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li$~Jl^{[&Kl)li$0mvR%^~i$)li$0mvR%^~EmvR/^~Em_vR${['Kl)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR${i$i$i$i$Im^[&?l`>l_7'l^)li$0m_k~^X'l?l^~BmvP>l^)li$~Jl^>lc[$Kl)l_)li$7.ndUm`YU:mch/?l`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li$~Jl^{[&Kl)li$0mvR%^~i$)li$0mvR%^~EmvR/^~Em_vR${['Kl)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~i$)li$0mvS(^~i$)li$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR${i$i$i$i$u~Bmi&_|];#nnYBl_^X&mi&`#nnYBl_^:nkX'mi&GmbkvP~Emk`[$Kl)l^7-m_`~Emak:nkbUm_vR%)l^7-m_`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i$Im^[&?l`>l_#nnYBl_^X&mi&`#nnYBl_^:nkX'mi&GmbkvP~Emk`[$Kl)l^7-m_`~Emak:nkbUm_vR%)l^7-m_`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i$u~Bmi&_|!V8#nmYS0l_i${]%7&miVN_[$Kl)l^!VN:nkiVN^YV8l^7+m?la_)l^~YGm`?l^>l_~Fl_}'i${!VN?li#!S$(l^{!U&'l^{](#noYBl_^{]A8UEmYV7l_iE{!L8V&lYUEm_iS5{!UE)li&#nkYUEm?la_X%l>l_~Fl_}'!V7'l^{!V&#nnYBl_^{!E#nqk^{!S5'l^{Amk!B)lk8>mYBl?l_l~Fl^{!U$7&m`_[$Kl3l^3l^7+m?la?l^~Fl^)li$~Bma^?l^~Fl^}'i${!U7iT;!T;iSA!SAj4!TIj4]48<lYU>l^{!3+mi&^{!U(+m`^}']3+m`^+m>l`>l^~i$+m`^+m>l`>l^~YIl_~YIl^}']EYUBlt]DYUBls!U8+mi$_)l^~^Bmi%^{!IYUBlq]-YUBll!KYUBlo!MYUBln!DYUBlm!/YUBlk!UB4l)li$+mbQl^~YU>l^{{AmkAmkAmkAmkAmkAmkAmkAmkAmkAmk!):nlkt!2:nlkv.!4:nlkv/!U>:nlkv0!':nlkv1!(:nlkv2!::nlkv3!V(:nlkv4!V$:nlkv5!V6:nlkv6!+:nlkv7!.:nlkv8!>:nlkv9!U::nlkv;!UC:nlkv<!V@:nlkv=!W#:nnm:nk:nki&vCvR3!VC:nki&wS@!VDGmlk!W*:nqkvC!VA:nnq:nk:nk:nk:nk:nk:nki&vLvJvR#vS&vR#vK!W$:nnl:nki&vC!W&:nk:nk:nk:nki&:nk:nki&vEvE:nk:nki&vS#vS#:nk:nki&vS;v0:nk:nki&vS5u!VO:nnl:nki&vR#!VP:nqkvF!VH:nk:nk:nk:nki&:nk:nki&v0wTO:nk:nki&twS<:nk:nki&vCwT>:nk:nki&ux9!VM:nk:nk:nk:nk:nki&v0v.utvC!0:nlkv:!*:nlkuy\0"; // RVM code that prints HELLO!

// basic def. of a boolean
typedef char bool;

#define true (1)

// a tagged value
typedef long obj;

// a number
typedef long num;


// a rib obj
#define RIB_NB_FIELDS 3

struct rib {
  long fields[3];
};


// GC constants
struct rib *heap_start;
obj *alloc_limit;
#define MAX_NB_OBJS 100000000 // 48000 is minimum for bootstrap
#define SPACE_SZ (MAX_NB_OBJS * RIB_NB_FIELDS)
#define heap_bot ((obj *)(heap_start))
#define heap_mid (heap_bot + (SPACE_SZ))
#define heap_top (heap_bot + (SPACE_SZ << 1))
// end GC constants

#define EXIT_ILLEGAL_INSTR 6
#define EXIT_NO_MEMORY 7

#define UNTAG(x) ((x) >> 1)
#define RIB(x) ((struct rib *)(x))
#define NUM(x) ((num)(UNTAG((num)(x))))
#define IS_NUM(x) ((x)&1)
#define IS_RIB(x) (!IS_NUM(x))
#define TAG_RIB(c_ptr) (((obj)(c_ptr)))
#define TAG_NUM(num) ((((obj)(num)) << 1) | 1)

#define PRIM1() obj x = pop()
#define PRIM2()                                                                \
  obj y = pop();                                                               \
  PRIM1()
#define PRIM3()                                                                \
  obj z = pop();                                                               \
  PRIM2()

#define CAR(x) RIB(x)->fields[0]
#define CDR(x) RIB(x)->fields[1]
#define TAG(x) RIB(x)->fields[2]

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

// global, but not a root, referenced
obj symbol_table = NUM_0;

size_t pos = 0;



obj *alloc;
obj *alloc_limit;
obj *scan;

void init_heap() {
  heap_start = malloc(sizeof(obj) * (SPACE_SZ << 1));

  if (!heap_start) {
    exit(EXIT_NO_MEMORY);
  }

  alloc = heap_bot;
  alloc_limit = heap_mid;
  stack = NUM_0;
}

// NULL is a pointer (0) but would represent NULL
// so it is never present in an obj field, and
// cannot be a number because it is even. This
// saves a couple of bytes v.s having STACK
// as the broken heart value
#define GC_COPIED_OBJ ((obj)NULL)

void copy() {
  obj o = *scan;
  // we sometime reference rib that are allocated in BSS,
  // we do not want to copy those
  if (IS_RIB(o)) {
    obj *ptr = RIB(o)->fields;
    obj field0 = ptr[0];
    obj copy;

    if (field0 == GC_COPIED_OBJ) {
      copy = ptr[1]; // copied, get new address
    } else {
      copy = TAG_RIB(alloc);
      *ptr++ = GC_COPIED_OBJ; // ptr points to CDR
      *alloc++ = field0;
      *alloc++ = *ptr++; // ptr points to TAG
      *alloc++ = *ptr;
      ptr[-1] = copy; // set forward ptr. Since it points to TAG, ptr[-1]
                      // rewrites the CDR
    }
    *scan = copy; // overwrite to new address.
  }
  scan++;
}

void gc() {
#ifdef DEBUG_GC
  obj *from_space = (alloc_limit == heap_mid) ? heap_bot : heap_mid;

  size_t objc = alloc - from_space;
  printf("\t--GC %zu -> ", objc);
#endif

  // swap
  obj *to_space = (alloc_limit == heap_mid) ? heap_mid : heap_bot;
  alloc_limit = to_space + SPACE_SZ;

  alloc = to_space;

  // root: stack
  scan = &stack;
  copy();

  // root: pc
  scan = &pc;
  copy();

  // root: false
  scan = &FALSE;
  copy();

  // scan the to_space to pull all live references
  scan = to_space;
  while (scan != alloc) {
    copy();
  }

#ifdef DEBUG_GC

  objc = alloc - to_space;
  printf("%zu\n", objc);
  fflush(stdout);

#endif
}

obj pop() {
  obj x = CAR(stack);
  stack = CDR(stack);
  return x;
}

void push2(obj car, obj tag) {
  // default stack frame is (value, ->, NUM_0)
  *alloc++ = car;
  *alloc++ = stack;
  *alloc++ = tag;

  stack = TAG_RIB((struct rib *)(alloc - RIB_NB_FIELDS));

  if (alloc == alloc_limit) {
    gc();
  }
}

/**
 * Allocate a rib that is not kept on the stack (can be linked
 * from anywhere). The car and cdr can be live references to other
 * ribs.
 * @param car
 * @param cdr
 * @param tag
 * @return
 */
struct rib *alloc_rib(obj car, obj cdr, obj tag) {
  push2(car, cdr); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = TAG(allocated);
  TAG(allocated) = tag;

  stack = old_stack;

  return RIB(allocated);
}

struct rib *alloc_rib2(obj car, obj cdr, obj tag) {
  push2(car, tag); // tag is set
  obj old_stack = CDR(stack);
  obj allocated = stack;

  CDR(allocated) = cdr;

  stack = old_stack;

  return RIB(allocated);
}

unsigned char get_byte() { return input[pos++]; }

#define ENCODING_SIZE  (92)
#define HALF_ENCODING_SIZE ENCODING_SIZE/2

num get_code() {
  num x = get_byte() - 35;
  return x < 0 ? 57 : x;
}

num get_int(num n) {
  num x = get_code();
  n *= HALF_ENCODING_SIZE;
  return x < HALF_ENCODING_SIZE ? n + x : get_int(n + x - HALF_ENCODING_SIZE);
}

struct rib *list_tail(struct rib *lst, num i) {
  return (i == 0) ? lst : list_tail(RIB(lst->fields[1]), i - 1);
}

struct rib *inst_tail(struct rib *lst, num i){
  return (i == 0) ? lst : inst_tail(RIB(lst->fields[2]), i - 1);
}

obj list_ref(struct rib *lst, num i) { return list_tail(lst, i)->fields[0]; }

obj get_opnd(obj o) {
  return (IS_NUM(o) ? RIB(list_tail(RIB(stack), NUM(o))) : RIB(o))->fields[0];
}

obj get_cont() {
  obj s = stack;

  while (!NUM(TAG(s))) {
    s = CDR(s);
  }

  return s;
}

#define TRUE (CAR(FALSE))
#define NIL (CDR(FALSE))
// Temp values that can be used to shield 
//  pointers from the evil GC
#define TEMP1 CAR(TRUE)
#define TEMP2 CDR(TRUE)
#define TEMP3 CAR(NIL)
#define TEMP4 CDR(NIL)

char* scm2str(obj s) {
    int length = (int) NUM(CDR(s)); 
    obj current = CAR(s);
    char* str = malloc(length + 1);
    for (int i = 0; i < length; i++) {
        str[i] = (char) NUM(CAR(current));
        current = CDR(current);
    }

    str[length] = '\0';

    return str;
};

obj bool2scm(bool x) { return x ? CAR(FALSE) : FALSE; }

obj prim(int no) {
  switch (no) { case 0:
  {
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    PRIM3();
    CAR(new_rib) = x;
    CDR(new_rib) = y;
    TAG(new_rib) = z;
    push2(new_rib, PAIR_TAG);
    break;
    
  } 
case 1:
{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  fclose(file);
  break;
  }
case 2:
{
  PRIM2();
  FILE* file = (FILE*) ((long) y ^ 1);
  char buffer[1] = {(char) NUM(x)};
  int success = fwrite(buffer, 1, 1, file);
  if (success != 1) {
  perror("Cannot write to file.");
  }
  fflush(file);
  push2(TRUE, PAIR_TAG);
  break;
  }
case 3:
{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  char buffer[1];
  int bytes_read = fread(buffer, 1, 1, file);
  if (!bytes_read) push2(NIL, PAIR_TAG);
  else push2(TAG_NUM(buffer[0]), PAIR_TAG);
  break;
  }
case 4:
{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, "w");
  push2((long) file | 1, PAIR_TAG);
  free((void *) filename);
  break;
  }
case 5:
{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, "r");
  push2(file ? (long) file | 1 : FALSE, PAIR_TAG);
  free((void*) filename);
  break;
  }
case 6:
{
  FILE* file = fdopen(1, "w");
  push2((long) file | 1, PAIR_TAG);
  break;
  }
case 7:
{
  FILE* file = fdopen(0, "r");
  push2((long) file | 1, PAIR_TAG);
  break;
  }
case 8:
{
     PRIM2();
     TEMP1 = x; // save x for the gc 
     int num_args = 0;
     obj arg = TAG_RIB(y);
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
case 9:
  {
    PRIM1();
    push2(x, PAIR_TAG);
    break;
  } 
case 10:
  {
    pop();
    break;
  } 
case 11:
  {
    obj x = pop();
    pop();
    push2(x, PAIR_TAG);
    break;
  } 
case 12:
  {
    obj x = CAR(TOS);
    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    break;
  } 
case 13:
  {
    PRIM1();
    push2(bool2scm(IS_RIB(x)), PAIR_TAG);
    break;
  } 
case 14:
  {
    PRIM1();
    push2(CAR(x), PAIR_TAG);
    break;
  } 
case 15:
  {
    PRIM1();
    push2(CDR(x), PAIR_TAG);
    break;
  } 
case 16:
  {
    PRIM1();
    push2(TAG(x), PAIR_TAG);
    break;
  } 
case 17:
  { 
    PRIM2();
    push2(CAR(x) = y, PAIR_TAG);
    break;
  }
case 18:
  {
    PRIM2();
    push2(CDR(x) = y, PAIR_TAG);
    break;
  }
case 19:
  {
    PRIM2();
    push2(TAG(x) = y, PAIR_TAG);
    break;
  }
case 20:
  {
    PRIM2();
    push2(bool2scm(x == y), PAIR_TAG);
    break;
  }
case 21:
  {
    PRIM2();
    push2(bool2scm(NUM(x) < NUM(y)), PAIR_TAG);
    break;
  }
case 22:
  {
    PRIM2();
    push2(x + y - 1, PAIR_TAG);
    break;
  }
case 23:
  {
    PRIM2();
    push2(x - y + 1, PAIR_TAG);
    break;
  }
case 24:
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) * NUM(y))), PAIR_TAG);
    break;
  }
case 25:
  {
    PRIM2();
    push2(TAG_NUM((NUM(x) / NUM(y))), PAIR_TAG);
    break;
  }
case 26:
  {
    PRIM1();
    exit(NUM(x));
    break;
  }

  default: {
    exit(EXIT_ILLEGAL_INSTR);
  }
  }
  return TAG_NUM(0);
}

#ifdef DEBUG
void show_rib(obj s, int depth){
    if (depth > 3){
        if (IS_RIB(s)){
            printf("[Array]");
            return;
        }
    }
    if (IS_RIB(s)){
        printf("[ ");
        show_rib(CAR(s), depth+1);
        printf(", ");
        show_rib(CDR(s), depth+1);
        printf(", ");
        show_rib(TAG(s), depth+1);
        printf(" ]");
    }
    else{
        printf("%d", NUM(s));
    }
}

void show_stack(){
    obj itr = stack;
    PRINTLN();
    if (NUM(TAG(itr))){
        printf("[]");
        return;

    }
    printf("[ ");
    int first = 0;
    while(!NUM(TAG(itr))){
        if (first){
            printf(", ");
        }
        else{
            first = 1;
        }
        show_rib(CAR(itr), 0);
        itr = CDR(itr);
    }
    printf(" ]");

}

#endif

void run() {
#define ADVANCE_PC()                                                           \
  do {                                                                         \
    pc = TAG(pc);                                                              \
  } while (0)
  while (1) {
    num instr = NUM(CAR(pc));
    switch (instr) {
    default: { // error
      exit(EXIT_ILLEGAL_INSTR);
    }
    case INSTR_HALT: { // halt
      exit(0);
    }
    case INSTR_AP: // call or jump
    {
      bool jump = TAG(pc) == NUM_0;
#ifdef DEBUG_I_CALL
      printf(jump ? "--- jump " : "--- call ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      obj proc = get_opnd(CDR(pc));
      while (1) {
#define code CAR(proc)
          if (IS_NUM(code)) {
            pop();
            proc=prim(NUM(code));

            if (IS_RIB(proc)) continue;

            if (jump) {
              // jump
              pc = get_cont();
              CDR(stack) = CAR(pc);
            }
            pc = TAG(pc);
          } else {
            num nargs = NUM(pop());
            obj s2 = TAG_RIB(alloc_rib(NUM_0, proc, PAIR_TAG));
            proc = CDR(s2);
            CAR(pc) = CAR(proc); // save the proc from the mighty gc


            num nparams_vari = NUM(CAR(code));
            num nparams = nparams_vari >> 1;
            num vari = nparams_vari&1;
            if (vari ? nparams > nargs : nparams != nargs) {
                printf("*** Unexpected number of arguments nargs: %ld nparams: %ld vari: %ld\n", nargs, nparams, vari);
                exit(1);
            }
            nargs-=nparams;
            if (vari){
                obj rest = NIL;
                for(int i = 0; i < nargs; ++i){
                    rest = TAG_RIB(alloc_rib(pop(), rest, s2));
                    s2 = TAG(rest);
                    TAG(rest) = PAIR_TAG;
                }
                s2 = TAG_RIB(alloc_rib(rest, s2, PAIR_TAG));
            }
            for (int i = 0; i < nparams; ++i) {
              s2 = TAG_RIB(alloc_rib(pop(), s2, PAIR_TAG));
            }

            nparams = nparams + vari;
            obj c2 = TAG_RIB(list_tail(RIB(s2), nparams));

            if (jump) {
              obj k = get_cont();
              CAR(c2) = CAR(k);
              TAG(c2) = TAG(k);
            } else {
              CAR(c2) = stack;
              TAG(c2) = TAG(pc);
            }

            stack = s2;

            obj new_pc = CAR(pc);
            CAR(pc) = TAG_NUM(instr);
            pc = TAG(new_pc);
          }
          break;
      }
      break;
    }
#undef code
    case INSTR_SET: { // set
#ifdef DEBUG_I_CALL
      printf("--- set ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      obj x = CAR(stack);
      ((IS_NUM(CDR(pc))) ? list_tail(RIB(stack), NUM(CDR(pc))) : RIB(CDR(pc)))
          ->fields[0] = x;
      stack = CDR(stack);
      ADVANCE_PC();
      break;
    }
    case INSTR_GET: { // get
#ifdef DEBUG_I_CALL
      printf("--- get ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      push2(get_opnd(CDR(pc)), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_CONST: { // const
#ifdef DEBUG_I_CALL
      printf("--- const ");
      show_operand(CDR(pc));
      show_stack();
      PRINTLN();
#endif
      push2(CDR(pc), PAIR_TAG);
      ADVANCE_PC();
      break;
    }
    case INSTR_IF: { // if
#ifdef DEBUG_I_CALL
      printf("--- if");
      show_stack();
      PRINTLN();
#endif

      obj p = pop();
      if (p != FALSE) {
        pc = CDR(pc);
      } else {
        pc = TAG(pc);
      }
      break;
    }
    }
  }
#undef ADVANCE_PC
}

struct rib *symbol_ref(num n) { return RIB(list_ref(RIB(symbol_table), n)); }

obj lst_length(obj list) {
  size_t l = 0;

  while (IS_RIB(list) && NUM(TAG(list)) == 0) {
    ++l;
    list = CDR(list);
  }

  return TAG_NUM(l);
}

struct rib *create_sym(obj name) {
  struct rib *list = alloc_rib(name, lst_length(name), STRING_TAG);
  struct rib *sym = alloc_rib(FALSE, TAG_RIB(list), SYMBOL_TAG);
  struct rib *root = alloc_rib(TAG_RIB(sym), symbol_table, PAIR_TAG);
  return root;
}

void build_sym_table() {
  num n = get_int(0);

  while (n > 0) {
    n--;
    symbol_table = TAG_RIB(create_sym(NIL));
  }

  obj accum = NIL;

  while (1) {
    char c = get_byte();

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


void decode() {
  int weights[6] = {20, 30, 0, 10, 11, 4};

  obj n;
  int d;
  int op;

  while (1) {
    num x = get_code();
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
        n = (n == d) ? TAG_NUM(get_int(0))
                     : TAG_RIB(symbol_ref(get_int(n - d - 1)));
      } else {
        n = (op < 3) ? TAG_RIB(symbol_ref(n)) : TAG_NUM(n);
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

    struct rib *c = alloc_rib(TAG_NUM(op), n, 0);
    c->fields[2] = TOS;
    TOS = TAG_RIB(c);
  }

  pc = TAG(CAR(n));
}

void setup_stack() {
  push2(NUM_0, PAIR_TAG);
  push2(NUM_0, PAIR_TAG);

  obj first = CDR(stack);
  CDR(stack) = NUM_0;
  TAG(stack) = first;

  CAR(first) = TAG_NUM(INSTR_HALT);
  CDR(first) = NUM_0;
  TAG(first) = PAIR_TAG;
}


void init() {
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
