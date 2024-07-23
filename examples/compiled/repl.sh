#!/bin/sh
set -e -u

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}


unpack_escaped_string() {
  __buf="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${#__buf} + 1))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    case "$__buf" in
      '\'*)
        __buf="${__buf#?}" # Remove the current char from $__buf
        case "$__buf" in
          'a'*) __c=7 ;;
          'b'*) __c=8 ;;
          'f'*) __c=12 ;;
          'n'*) __c=10 ;;
          'r'*) __c=13 ;;
          't'*) __c=9 ;;
          'v'*) __c=11 ;;
          '\'*) __c=92 ;;
          '"'*) __c=34 ;;
          "'"*) __c=39 ;;
          '?'*) __c=63 ;;
          '$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes
          *) echo "invalid escape in string: $__buf"; exit 1 ;;
        esac
        __buf="${__buf#?}" # Remove the current char from $__buf
        ;;
      *)
        __c=$(LC_CTYPE=C printf "%d" "'${__buf%"${__buf#?}"}")
        __buf="${__buf#?}" # Remove the current char from $__buf
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0))
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2"
    : $(($1 = __addr))
  fi
  set -u
}

defstr __str_0 "RD?naeloob,?xelpmoc,cc/llac,?citebahpla-rahc,<,dnuor,dna,etouq,gnirts,raaaac,radddc,raaddc,?=>ic-gnirts,esle,>,!rdc-tes,?qe,oludom,tsil>-rotcev,htgnel-gnirts,?tsil,fer-gnirts,-,nruter,esac,rddddc,certel,gnicilps-etouqnu,!tes,?rebmun,daol,rdadac,ro,rac,tneitouq,?orez,xam,/,?evitisop,?=>ic-rahc,ecaps,roolf,?tcaxe,?laer,?=>gnirts,rdaddc,raddac,adbmal,gniliec,rdaadc,elif-tuptuo-htiw-llac,fi,mcl,tropxe,gnirtsbus,!tes-gnirts,?evitagen,+,raaadc,!rac-tes,*,etouqnu,?=<ic-rahc,enifed,?=ic-gnirts,etouqisauq,elif-tupni-htiw-llac,?tcaxeni,rdc,!tes-rotcev,rddadc,rddaac,nigeb,radaac,rotcev-ekam,tel,etacnurt,rdaaac,?=gnirts,?=<rahc,htgnel-rotcev,=<,raadac,?lanoitar,_,?=<gnirts,fer-rotcev,=>,bat,?ddo,noitaunitnoc-tnerruc-htiw-llac,nim,?=>rahc,gnirts-ekam,?=<ic-gnirts,regetni>-rahc,?ciremun-rahc,radadc,dnoc,=,dneppa-gnirts,trop-tuptuo-esolc,cossa,dcg,qmem,?>ic-gnirts,hcae-rof,redniamer,?<ic-gnirts,?<gnirts,?=rahc,?=ic-rahc,gnirts>-lobmys,?>ic-rahc,?>gnirts,rebmem,?neve,vmem,?ecapsetihw-rahc,elif-tuptuo-nepo,lave,?>rahc,?esac-rewol-rahc,raadc,?esac-reppu-rahc,?trop-tuptuo,?trop-tupni,elif-tupni-nepo,trop-tupni-esolc,tsil>-gnirts,raaac,raddc,rdddac,?<ic-rahc,?<rahc,gnirts>-rebmun,rebmun>-gnirts,enilwen,raac,esacnwod-rahc,qssa,vssa,?regetni,?vqe,fer-tsil,trop-tupni-tnerruc,radac,dneppa,rotcev,?erudecorp,radc,rdddc,sba,trop-tuptuo-tnerruc,rotcev>-tsil,ylppa,esrever,lobmys>-gnirts,rdadc,rdaac,esacpu-rahc,rddac,rahc-etirw,?gnirts,gnirts>-tsil,?rotcev,etirw,?rahc,?tcejbo-foe,?lauqe,rahc-keep,rahc>-regetni,?lobmys,pam,htgnel,daer,,,,,ton,,,yalpsid,rdac,rddc,rahc-daer,tsil,,?llun,,,,?riap,,,snoc,,,,,,,,,;8V1k!T1)li%zAmk!TH7%lYAl_Im^[\$Kl7(lYAlbAmZJl^)li&AmZBlb~YHl^{i\$ZCl^{!V18V1kAmZ9kAmYJlZJl^99k~YHl^YAkAmPliW#y]J7\$kWmi&:nHniW%ai&kk{!@#niVK\`^}'!W%Rm:nkw)iW)l!U9)l_,mYU9ma?l_>l^~Fl^}'!UG)l^8UGnUmlb?l\`^)l\`~Bm_>l_~Fl_})!V)8;mVma_8;mVmaUm\`l~Z4l_cYUGnka_?lb>la1nYV)ofd?lbCmai\$>l\`^~Fl_}+!V##n:nckiVE#nQla~i\$#n:nckiVE#nQla~i\$#n:nckiVE#nQla~i\$#n:nckiVE#nQla~YU>lQla~Bmw*?la~BmiW)>la~YU>la_iVB}'!?#na_iW)#nk_iW)~BmiW%_}'!=1nb1nRm:nTng?lecw*iW)m~Fl?la>l\`^})!UI8=nebb1nYUIqRm:nh4w2iW)m1nYUIqh1~BmiW%h1Cmh0eh-?lf?ldCmci\$_\`>la>l_~Fl_}/!V?7&ml_Im^[\$Kl8U;nX,mb?l\`X+ma>l_wVL8U;nX,mUmlb?l\`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l\`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l\`Z0l_wVJ~Bml_~i\$8U;nX,mb?l\`X+ma>l_wVL8U;nX,mUmlb?l\`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l\`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l\`Z0l_wVJ~Bml_~BmwTKZ8l^~Fl>l^8U;nX,mGmlb?l\`YUJlwT)wVL88l^~Bml_~BmwT)>l^8UJl^8U;mX+ma>l_wW'~YKl^~SlFl^}'i\${!UJ8U;m_wU1{!UD,mLnca_wT3})!1#nb\`iVE8UIqgdLlCmbwS@LlaiVC\`8V)oCmfbYBl\`_\`~YDl_?l\`1nei\$1neImYUDnCmCm?lddwTNCm?l\`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l\`~BmwTN^1nci\$1ncYUDnCmNldwS2CmZ\$lcwSLZ#la1ncCmZ\$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci\$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm\`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni\$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl\`>l_wTJ{YCmaKl5mi\$>l^{wSI\`Ol\`~BmwTL^8UIqgdNldYCmai8YCm\`iTE\`Ol\`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^Im^~^YDl^Ol\`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc\`YOlb\`1nYV#meYUGnlc>l\`CmCmNld?l\`wT7\`~Fl^Ol\`~ImBmwT'_8UIqgdLlCmbwS@LlaiVC\`8V)oCmfbYBl\`_\`~YDl_?l\`1nei\$1neImYUDnCmCm?lddwTNCm?l\`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l\`~BmwTN^1nci\$1ncYUDnCmNldwS2CmZ\$lcwSLZ#la1ncCmZ\$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci\$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm\`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni\$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl\`>l_wTJ{YCmaKl5mi\$>l^{wSI\`Ol\`~BmwTL^8UIqgdNldYCmai8YCm\`iTE\`Ol\`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^Im^~^YDl^Ol\`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc\`YOlb\`1nYV#meYUGnlc>l\`CmCmNld?l\`wT7\`~Fl^Ol\`~Im^~^BmwTJ^1ncYV?lOla_~BmwT%^#ncOlaiVE~BmwU1^>l_~Fl_#nbYUGnk\`\`iVF~YDl_})!V*)l^8V*l?l^~Fl^{!U@9&lCm\`^8U@mCma>l_?l^~Fl^}'!UN)lk8>mYUNl?l_l~Fl^{!;#na_iVE}'!VKl!VIo!VEn!VFm!VBl!W)k!V'7&mZAlaZAl_[\$Kl)lk)liVD~Fl_)ll)ll)liVD~Z=m__7,m?lb?l\`~YS%m__>l\`>l^~Fl_~Fl^}'i\$}'!V27&m>la>l_[\$Kl)lk)liVD~Fl_)ll7,m?lb?l\`)ll~Em\`^)liVD~Em__>l\`>l^~Fl_~Fl^}'i\$}'!S08LlZ'mYCm\`jAj/z!T07(oi&ca_[\$Kl8V&la7/oCmfZ2mb>lb\`a_Gml\`~Ema_}+i\$})!U,8<lYS(m\`^}'!S68<lYS+m\`^}'!S+.mYV'ma_k}'!S(.mkYV'm\`^}'!T&+mkYV'm\`^}'!T:8<lYS'm\`^}'!S?8<lZPm\`^}']P.mYV2ma_k}'!S'.mkYV2m\`^}'!SF8Gm\`^}'!U08Ll^z!S78LlYV+m__Im^[&?l\`>l_8LlYV+m__iW*~Bmi&_|!T/8V3n>lb\`>l^})!U#8ElZ2m\`>l^}'!U%(l^{!UL8V<liVAy!V<8V@llAmZ9kAmYS*m_Kl89liW\$AmPl^{z!U=)li\$8U=nca?l_AmDm>lb>l?l^AmDm>lbvS#8U=nca?l_AmDm>lb>l_~Sl^Z6m\`>l^~Fl^})!U<7'ma^AmDm>lavCAmDm>lavR#AmDm>lavC)li\$~Jl^8U<nb\`?l^AmX'ma>l^AmDm>lavC~Fl^})!98ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~ImZEl\`8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Im^[&?l\`>l_8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~ImZEl\`8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Z)k~Bmi&_|!J89m__-m>l_vLAmi\$-m>l_vLAmImYU<naiJ?l^AmYJm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n\`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm\`Ol^8Nm\`\`~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n\`iW&>l_AmDm>l_vE~YMl_Im^[&?l\`>l_89m__-m>l_vLAmi\$-m>l_vLAmImYU<naiJ?l^AmYJm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n\`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm\`Ol^8Nm\`\`~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n\`iW&>l_AmDm>l_vE~YMl_Z)k~Bmi&_|]9-m>l_uIm^[%?l_>l^-m>l_uZ)k~Bmi&^z!T4)l^AmYS/l_X'l^ZKl^}'!V08V0l_8UMl_~Bmu>l^)l^~YHl^Ml^{!UM'l^8V0l_~BmvR0>l^8UMl_AmMl_~ZLl^)l^~YHl^YFl^{!UH8UHmaCm\`^8UHmbCma^8UHmbCmat~BmvS;^8UHmbCmav0~BmvS9^8UHmbCmau~BmvS5^>lMl\`~BmvS#^9&l_~BmvE^)li&~Bmk^>lMl_}'!UA8UAmCmbZ7lMl\`_8LlZ&l\`~ImImEmvD\`8UAmCmbZ7lMl\`_8LlZ&l\`~ImIm^~^BmvL_8UAmCmbZ7lMl\`_8LlZ&l\`~Im^~^BmvK^>lYFl^}'!UO,mYUOla^)l^AmMlaYAl\`~i\$,mYUOla^)l^AmMlaYAl\`~YGmiVOYS\$l^~YDl^YAl_)li&AmMl_~BmvL^YUMl^{!A9%l\`)l^~^^Z:l^YUAmi&_8V&lYUHm\`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl\`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl\`wT%AmMl_~BmvS'^5mYAl\`wU1AmMl_~BmvJ^9%lYUAmLliVP\`9(lYAl\`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl\`AmMl\`~BmvS#^)li%AmMl\`~BmvS;^)li\$AmMl\`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Im^[%?l_>l^9%l\`)l^~^^Z:l^YUAmi&_8V&lYUHm\`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl\`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl\`wT%AmMl_~BmvS'^5mYAl\`wU1AmMl_~BmvJ^9%lYUAmLliVP\`9(lYAl\`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl\`AmMl\`~BmvS#^)li%AmMl\`~BmvS;^)li\$AmMl\`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Z1k~Bmi&^z!F)l^AmYUKm__Ml^Im^[%?l_>l^)l^AmYUKm__Ml^Z1k~Bmi&^z!T\$)l^AmZBl_X'l^ZCl^}']))liW(y]1)liVGy!N-m>l_>l_Im^[&?l\`>l_-m>l_>l_Z)k~Bmi&_|!S/)li\$8V=l>l^AmYV\$mi\$^~?l^{]K#nti%YV9l^{!6)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami\$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Im^[%?l_>l^)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami\$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Z1k~Bmi&^z!V-8<l?l^{!UK8V\$m\`^}'!V%(l^{]B)li\$8V.l>l^AmYV\$mi\$^~?l^{]C#nsi&YV>l^{!W(:nti%YV5k!VG:nsi&YV:kAmk!H+miW+^{!W+:npkk!V=8V.l^{!V.:nlkl!-:nlkm!UP:nlkn!V9:nlko!V>:nlkp!V5:nlkq!V::nlkr!U3iT=!SHiT=!T6iT=!T=)l^{!T28U?nal^[\$Kl8U:m_YUCmYS-m\`\`_Z*l\`Z*l^)lk~Bmk_}'i\$z!S-8U?nakKl7,m\`^7,m__~Em__Z*l\`Z*l^}'[\$Kl7*m_YS)m__)l_~Bmk^}'i\$z!U'8>mb^)l^~BmEmkbEmk\`)lk~Bmk^GmYU:m\`a_YUCm\`^}'!S)0mYU:mYUCmb\`\`^}']*)l^0m_k~Emk^{!S98U?na_Kl)l^)l_~Em__}'|!TB8U?na_Kl)l_)l^~Em__}'|!S;8<lZNl^{]N+mYU:mYUCmm\`m^{!T..mk^{!T@.m_k{!TC+mk^{!S=8UFobi%_Kl8<lEm\`^}'|!SC8UFobi%_Kl8<lEm__}'|!U*8UFobi%_Kl.m__}'|!U48UFobi%_Kl.m\`^}'|!S18UFobi%_j3|!T#)li\${!T<)li%{!TD8UCm\`^}'!TA8U?na_iTD8UCm_l~Jl_|!TP8U?na_Kl0m\`^}'0m_k~Jl_|!T*8U?n\`lKl8U:m\`^}'z!T-8U?n\`kKl8>m\`^}'z!UF)l\`8UFo?ldX(m>lda>lb^~i\$)l\`8UFo?ldX(m>lda>lb^~\`~Fla}+!U?)l_8U?n?lbX'm>lb\`^~Fl\`})!S:iU6!U67&lKl)l_AmYV6mQlc^AmYV(m>lc^?l?lKli\${?l?lKli\${!S*)li\$9'mZ/mYUEmbiSPLl_iS*AmZ'mYUEmaiTE^~Fl>l_|!C)li&,mZ'mZ/mYUEmciSPLl\`iCZ'mYUEmaiTE^~Fl>l_|!UE)li&,mYUEm?la_X%l>l_~Fl_}']'8V;m\`^}'!V;:nlks]7)l^8ElUmvC>l^~ZFl^{!P)l^8ElGmvC>l^~ZHl^{]F)li\$.mvRP>l^~Em>l_vR5{]H)li\$.mvSB>l^~Em>l_vS'{]L/lYS,miVM>l^{!S4)li\$.mvR/>l^~Em>l_vR\${!U59Fl_)l^~^ZHl^{!T?8<lZ=m\`^}'!T(8<lYS#m\`^}'!S#9ImYPl\`YPl^}']=9<mYPl\`YPl^}'!S%8S&mYPl\`YPl^}'!S88<lZ<m\`^}'!SE8<lZIm\`^}']I.m>l_>l_}']<.m>l\`>l^}'!S&93m\`^}'].9(l^z!SJ9(lYV+mk^{!SO8V3nb\`>l^})!S>92m\`>l^}'!SD(l^{!VJj/!VLi,!W'9(l^{!U;i5!V/)l\`8V/nCmca\`Gml^~Em_k})!V+8V/ni&\`^}'!S.)li\$8S.m?la_)l^~YGm>l__>l_~Fl_}']6j5]5)li\$95m?la_)l^~Z3m>l__>l_~Fl_}']O)li\$9Om?l\`^)l_~YGm>l\`^~Fl_}'!S,jM]M)li\$9Mm?l\`^)l_~Z3m>l\`^~Fl_}'!V,)l^8V,mGml\`?l^~Em\`k}'!V38V(maYV,m\`^})]2'lYV,m\`^}'!V4)l_8V4mCma>l_?l^~Fl^}']&8V4mi&^{]/7%l_[\$Kl)li&7)l?l_)l^~Jl?l_,mX*lCm?la?l_>l^~Fl^>l^~Fl^{i\$z!B)lk8>mYBl?l_l~Fl^{!5)l^z!TM9+l?l^{!U.9+l>l^{!T99?l?l^{!U-9?l>l^{!SN9\$l?l^{!S39\$l>l^{!T59Gl?l^{!T,9Gl>l^{!T88Ol>l^{!TG90l?l^{!SB90l>l^{!SM9#l?l^{!SK9#l>l^{!SG9@l?l^{!U/9@l>l^{]+87l?l^{]?87l>l^{]\$9,l?l^{]G9,l>l^{]088l>l^{]#98l?l^{]@98l>l^{],(l>l^{]8'l>l^{]>8Ol?l^{!O88l?l^{!7(l?l^{!8'l?l^{!U)8V\$m\`^}'!T+8V(m\`^}'!SP(l^{!TE'l^{!,#nk\`^}'!G)li\$)li\$)li\$)li\$8Gm>la>l_~YGm?la?l_~YGmQlaQl_~YU>l\`)li\$~BmpQl_~YU>l_)l^~^Bm\`^}'!<+mi\$^{]:7'l^)li\$0m_k~^X'l?l^~BmvP>l^)li\$~Jl^>lc[\$Kl)l_)li\$7.ndUm\`YU:mch/?l\`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li\$~Jl^{[&Kl)li\$0mvR%^~i\$)li\$0mvR%^~EmvR/^~Em_vR\${['Kl)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR\${i\$i\$i\$i\$Im^[&?l\`>l_7'l^)li\$0m_k~^X'l?l^~BmvP>l^)li\$~Jl^>lc[\$Kl)l_)li\$7.ndUm\`YU:mch/?l\`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li\$~Jl^{[&Kl)li\$0mvR%^~i\$)li\$0mvR%^~EmvR/^~Em_vR\${['Kl)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR\${i\$i\$i\$i\$u~Bmi&_|];#nnYBl_^X&mi&\`#nnYBl_^:nkX'mi&GmbkvP~Emk\`[\$Kl)l^7-m_\`~Emak:nkbUm_vR%)l^7-m_\`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i\$Im^[&?l\`>l_#nnYBl_^X&mi&\`#nnYBl_^:nkX'mi&GmbkvP~Emk\`[\$Kl)l^7-m_\`~Emak:nkbUm_vR%)l^7-m_\`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i\$u~Bmi&_|!V8#nmYS0l_i\${]%7&miVN_[\$Kl)l^!VN:nkiVN^YV8l^7+m?la_)l^~YGm\`?l^>l_~Fl_}'i\${!VN?li#!S\$(l^{!U&'l^{](#noYBl_^{]A8UEmYV7l_iE{!L8V&lYUEm_iS5{!UE)li&#nkYUEm?la_X%l>l_~Fl_}'!V7'l^{!V&#nnYBl_^{!E#nqk^{!S5'l^{Amk!B)lk8>mYBl?l_l~Fl^{!U\$7&m\`_[\$Kl3l^3l^7+m?la?l^~Fl^)li\$~Bma^?l^~Fl^}'i\${!U7iT;!T;iSA!SAj4!TIj4]48<lYU>l^{!3+mi&^{!U(+m\`^}']3+m\`^+m>l\`>l^~i\$+m\`^+m>l\`>l^~YIl_~YIl^}']EYUBlt]DYUBls!U8+mi\$_)l^~^Bmi%^{!IYUBlq]-YUBll!KYUBlo!MYUBln!DYUBlm!/YUBlk!UB4l)li\$+mbQl^~YU>l^{{AmkAmkAmkAmkAmkAmkAmkAmkAmkAmk!):nlkt!2:nlkv.!4:nlkv/!U>:nlkv0!':nlkv1!(:nlkv2!::nlkv3!V(:nlkv4!V\$:nlkv5!V6:nlkv6!+:nlkv7!.:nlkv8!>:nlkv9!U::nlkv;!UC:nlkv<!V@:nlkv=!W#:nnm:nk:nki&vCvR3!VC:nki&wS@!VDGmlk!W*:nqkvC!VA:nnq:nk:nk:nk:nk:nk:nki&vLvJvR#vS&vR#vK!W\$:nnl:nki&vC!W&:nk:nk:nk:nki&:nk:nki&vEvE:nk:nki&vS#vS#:nk:nki&vS;v0:nk:nki&vS5u!VO:nnl:nki&vR#!VP:nqkvF!VH:nk:nk:nk:nki&:nk:nki&v0wTO:nk:nki&twS<:nk:nki&vCwT>:nk:nki&ux9!VM:nk:nk:nk:nk:nki&v0v.utvC!0:nlkv:!*:nlkuy"
_input=$__str_0
# rib struct member declarations
readonly __field0=0
readonly __field1=1
readonly __field2=2
readonly __sizeof__rib=3
_stack=$(((0 << 1) | 1))
_pc=$(((0 << 1) | 1))
_FALSE=$(((0 << 1) | 1))
_symbol_table=$(((0 << 1) | 1))
_max_nb_objs=100000
_rib_nb_fields=4
_space_size=$((_max_nb_objs * _rib_nb_fields))
_heap_start=0
_heap_mid=0
_heap_end=0
_alloc=0
_alloc_limit=0
_scan=0
_init_heap() {
  _malloc _heap_start $((1 * (_space_size + 1)))
  if [ $((_heap_start & (1 == 1))) != 0 ] ; then
    : $(((_heap_start += 1) - 1))
  fi
  if [ $((!_heap_start)) != 0 ] ; then
    exit 7
  fi
  _heap_mid=$((_heap_start + (_space_size >> 1)))
  _heap_end=$((_heap_start + _space_size))
  _alloc=$_heap_start
  _alloc_limit=$_heap_mid
  _stack=$(((0 << 1) | 1))
}

: $((copy = field0 = ptr = o = 0))
_copy() { # o: $2
  set $@ $o $ptr $field0 $copy
  o=$2
  if [ $((!(o & 1))) != 0 ] ; then
    ptr=$o
    field0=$((_$((ptr + 0))))
    if [ $field0 = 0 ] ; then
      copy=$((_$((ptr + 1))))
    else
      copy=$_alloc
      : $((_$(((ptr += 1) - 1)) = 0))
      : $((_$(((_alloc += 1) - 1)) = field0))
      : $((_$(((_alloc += 1) - 1)) = _$(((ptr += 1) - 1))))
      : $((_$(((_alloc += 1) - 1)) = _$ptr))
      : $((_$(((_alloc += 1) - 1)) = (0 << 1) | 1))
      : $((_$((ptr + -1)) = copy))
    fi
    : $(($1 = copy))
    : $((__tmp = $1)) $((o = $3)) $((ptr = $4)) $((field0 = $5)) $((copy = $6)) $(($1 = __tmp))
    return
  fi
  : $(($1 = o))
  : $((__tmp = $1)) $((o = $3)) $((ptr = $4)) $((field0 = $5)) $((copy = $6)) $(($1 = __tmp))
}

: $((to_space = 0))
_gc() {
  set $@ $to_space
  if [ $_alloc_limit = $_heap_mid ] ; then
    to_space=$_heap_mid
    _alloc_limit=$_heap_end
  else
    to_space=$_heap_start
    _alloc_limit=$_heap_mid
  fi
  _alloc=$to_space
  _copy _stack $_stack
  _copy _pc $_pc
  _copy _FALSE $_FALSE
  _scan=$to_space
  while [ $_scan != $_alloc ] ; do
    _copy _$_scan $((_$_scan))
    : $(((_scan += 1) - 1))
  done
  : $((__tmp = $1)) $((to_space = $2)) $(($1 = __tmp))
}

: $((x = 0))
_pop() {
  set $@ $x
  x=$((_$((_stack + __field0))))
  _stack=$((_$((_stack + __field1))))
  : $(($1 = x))
  : $((__tmp = $1)) $((x = $2)) $(($1 = __tmp))
}

: $((tag = car = 0))
_push2() { # car: $2, tag: $3
  set $@ $car $tag
  car=$2
  tag=$3
  : $((_$(((_alloc += 1) - 1)) = car))
  : $((_$(((_alloc += 1) - 1)) = _stack))
  : $((_$(((_alloc += 1) - 1)) = tag))
  : $((_$(((_alloc += 1) - 1)) = (0 << 1) | 1))
  _stack=$((_alloc - 4))
  if [ $_alloc = $_alloc_limit ] ; then
    _gc __ 
  fi
  : $((__tmp = $1)) $((car = $4)) $((tag = $5)) $(($1 = __tmp))
}

: $((allocated = old_stack = tag = cdr = car = 0))
_alloc_rib() { # car: $2, cdr: $3, tag: $4
  set $@ $car $cdr $tag $old_stack $allocated
  car=$2
  cdr=$3
  tag=$4
  _push2 __ $car $cdr
  old_stack=$((_$((_stack + __field1))))
  allocated=$_stack
  : $((_$((allocated + __field1)) = _$((allocated + __field2))))
  : $((_$((allocated + __field2)) = tag))
  _stack=$old_stack
  : $(($1 = allocated))
  : $((__tmp = $1)) $((car = $5)) $((cdr = $6)) $((tag = $7)) $((old_stack = $8)) $((allocated = $9)) $(($1 = __tmp))
}

: $((allocated = old_stack = tag = cdr = car = 0))
_alloc_rib2() { # car: $2, cdr: $3, tag: $4
  set $@ $car $cdr $tag $old_stack $allocated
  car=$2
  cdr=$3
  tag=$4
  _push2 __ $car $tag
  old_stack=$((_$((_stack + __field1))))
  allocated=$_stack
  : $((_$((allocated + __field1)) = cdr))
  _stack=$old_stack
  : $(($1 = allocated))
  : $((__tmp = $1)) $((car = $5)) $((cdr = $6)) $((tag = $7)) $((old_stack = $8)) $((allocated = $9)) $(($1 = __tmp))
}

: $((i = lst = 0))
_list_tail() { # lst: $2, i: $3
  set $@ $lst $i
  lst=$2
  i=$3
  while [ $(((i -= 1) + 1)) != 0 ] ; do
    lst=$((_$((lst + __field1))))
  done
  : $(($1 = lst))
  : $((__tmp = $1)) $((lst = $4)) $((i = $5)) $(($1 = __tmp))
}

: $((i = lst = 0))
_inst_tail() { # lst: $2, i: $3
  set $@ $lst $i
  lst=$2
  i=$3
  while [ $(((i -= 1) + 1)) != 0 ] ; do
    lst=$((_$((lst + __field2))))
  done
  : $(($1 = lst))
  : $((__tmp = $1)) $((lst = $4)) $((i = $5)) $(($1 = __tmp))
}

: $((__t1 = i = lst = 0))
_list_ref() { # lst: $2, i: $3
  set $@ $lst $i $__t1
  lst=$2
  i=$3
  _list_tail __t1 $lst $i
  : $(($1 = _$((__t1 + __field0))))
  : $((__tmp = $1)) $((lst = $4)) $((i = $5)) $((__t1 = $6)) $(($1 = __tmp))
}

: $((__t1 = return_value = o = 0))
_get_opnd() { # o: $2
  set $@ $o $return_value $__t1
  o=$2
  if [ $((o & 1)) != 0 ] ; then
    _list_tail __t1 $_stack $((o >> 1))
    return_value=$__t1
  else
    return_value=$o
  fi
  : $(($1 = _$((return_value + __field0))))
  : $((__tmp = $1)) $((o = $3)) $((return_value = $4)) $((__t1 = $5)) $(($1 = __tmp))
}

: $((s = 0))
_get_cont() {
  set $@ $s
  s=$_stack
  while [ $((!(_$((s + __field2)) >> 1))) != 0 ] ; do
    s=$((_$((s + __field1))))
  done
  : $(($1 = s))
  : $((__tmp = $1)) $((s = $2)) $(($1 = __tmp))
}

: $((l = list = 0))
_lst_length() { # list: $2
  set $@ $list $l
  list=$2
  l=0
  while [ $((!(list & 1))) != 0 ] && [ $((_$((list + __field2)) >> 1)) = 0 ] ; do
    : $((l += 1))
    list=$((_$((list + __field1))))
  done
  : $(($1 = (l << 1) | 1))
  : $((__tmp = $1)) $((list = $3)) $((l = $4)) $(($1 = __tmp))
}

_pos=0
_get_byte() {
  : $(($1 = _$((_input + (_pos += 1) - 1))))
}

: $((__t1 = x = 0))
_get_code() {
  set $@ $x $__t1
  _get_byte __t1 
  x=$((__t1 - 35))
  : $(($1 = (x < 0) ? 57: x))
  : $((__tmp = $1)) $((x = $2)) $((__t1 = $3)) $(($1 = __tmp))
}

: $((x = n = 0))
_get_int() { # n: $2
  set $@ $n $x
  n=$2
  _get_code x 
  : $((n *= (92 / 2)))
  if [ $x -lt $((92 / 2)) ] ; then
    : $(($1 = n + x))
  else
    _get_int $1 $(((n + x) - (92 / 2)))
  fi
  : $((__tmp = $1)) $((n = $3)) $((x = $4)) $(($1 = __tmp))
}

: $((__t1 = n = 0))
_symbol_ref() { # n: $2
  set $@ $n $__t1
  n=$2
  _list_ref __t1 $_symbol_table $n
  : $(($1 = __t1))
  : $((__tmp = $1)) $((n = $3)) $((__t1 = $4)) $(($1 = __tmp))
}

: $((__t2 = __t1 = root = sym = list = name = 0))
_create_sym() { # name: $2
  set $@ $name $list $sym $root $__t1 $__t2
  name=$2
  _lst_length __t2 $name
  _alloc_rib list $name $__t2 $(((3 << 1) | 1))
  _alloc_rib sym $_FALSE $list $(((2 << 1) | 1))
  _alloc_rib root $sym $_symbol_table $(((0 << 1) | 1))
  : $(($1 = root))
  : $((__tmp = $1)) $((name = $3)) $((list = $4)) $((sym = $5)) $((root = $6)) $((__t1 = $7)) $((__t2 = $8)) $(($1 = __tmp))
}

: $((__t1 = c = accum = n = 0))
_build_sym_table() {
  set $@ $n $accum $c $__t1
  _get_int n 0
  while [ $n -gt 0 ] ; do
    : $(((n -= 1) + 1))
    _create_sym __t1 $((_$((_FALSE + __field1))))
    _symbol_table=$__t1
  done
  accum=$((_$((_FALSE + __field1))))
  while [ 1 != 0 ] ; do
    _get_byte c 
    if [ $c = 44 ] ; then
      _create_sym __t1 $accum
      _symbol_table=$__t1
      accum=$((_$((_FALSE + __field1))))
      continue
    fi
    if [ $c = 59 ] ; then
      break
    fi
    _alloc_rib __t1 $(((c << 1) | 1)) $accum $(((0 << 1) | 1))
    accum=$__t1
  done
  _create_sym __t1 $accum
  _symbol_table=$__t1
  : $((__tmp = $1)) $((n = $2)) $((accum = $3)) $((c = $4)) $((__t1 = $5)) $(($1 = __tmp))
}

: $((c = 0))
_set_global() { # c: $2
  set $@ $c
  c=$2
  : $((_$((_$((_symbol_table + __field0)) + __field0)) = c))
  _symbol_table=$((_$((_symbol_table + __field1))))
  : $((__tmp = $1)) $((c = $3)) $(($1 = __tmp))
}

defarr() { _malloc $1 $2; }

defarr _weights 6
_init_weights() {
  : $((_$((_weights + 0)) = 20))
  : $((_$((_weights + 1)) = 30))
  : $((_$((_weights + 2)) = 0))
  : $((_$((_weights + 3)) = 10))
  : $((_$((_weights + 4)) = 11))
  : $((_$((_weights + 5)) = 4))
}

: $((__t3 = __t2 = __t1 = c = x = op = d = n = 0))
_decode() {
  set $@ $n $d $op $x $c $__t1 $__t2 $__t3
  while [ 1 != 0 ] ; do
    _get_code x 
    n=$x
    op=-1
    while [ $n -gt $((2 + (d = _$((_weights + (op += 1)))))) ] ; do
      : $((n -= (d + 3)))
    done
    if [ $x -gt 90 ] ; then
      op=4
      _pop n 
    else
      if [ $((!op)) != 0 ] ; then
        _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
      fi
      if [ $n -ge $d ] ; then
        if [ $n = $d ] ; then
          _get_int __t1 0
          n=$(((__t1 << 1) | 1))
        else
          _get_int __t2 $(((n - d) - 1))
          _symbol_ref __t1 $__t2
          n=$__t1
        fi
      else
        if [ $op -lt 3 ] ; then
          _symbol_ref __t1 $n
          n=$__t1
        else
          n=$(((n << 1) | 1))
        fi
      fi
      if [ $op -gt 4 ] ; then
        _pop __t3 
        _alloc_rib2 __t2 $n $(((0 << 1) | 1)) $__t3
        _alloc_rib __t1 $__t2 $((_$((_FALSE + __field1)))) $(((1 << 1) | 1))
        n=$__t1
        if [ $_stack = $(((0 << 1) | 1)) ] ; then
          break
        fi
        op=3
      elif [ $op -gt 0 ] ; then
        : $(((op -= 1) + 1))
      else
        op=0
      fi
    fi
    _alloc_rib c $(((op << 1) | 1)) $n 0
    : $((_$((c + __field2)) = _$((_stack + __field0))))
    : $((_$((_stack + __field0)) = c))
  done
  _pc=$((_$((_$((n + __field0)) + __field2))))
  : $((__tmp = $1)) $((n = $2)) $((d = $3)) $((op = $4)) $((x = $5)) $((c = $6)) $((__t1 = $7)) $((__t2 = $8)) $((__t3 = $9)) $(($1 = __tmp))
}

: $((str = current = length = i = s = 0))
_scm2str() { # s: $2
  set $@ $s $i $length $current $str
  s=$2
  i=0
  length=$((_$((s + __field1)) >> 1))
  current=$((_$((s + __field0))))
  _malloc str $((length + 1))
  i=0
  while [ $i -lt $length ] ; do
    : $((_$((str + i)) = (_$((current + __field0)) >> 1)))
    current=$((_$((current + __field1))))
    : $(((i += 1) - 1))
  done
  : $((_$((str + length)) = __NUL__))
  : $(($1 = str))
  : $((__tmp = $1)) $((s = $3)) $((i = $4)) $((length = $5)) $((current = $6)) $((str = $7)) $(($1 = __tmp))
}

: $((x = 0))
_bool2scm() { # x: $2
  set $@ $x
  x=$2
  if [ $x != 0 ] ; then
    : $(($1 = _$((_FALSE + __field0))))
  else
    : $(($1 = _FALSE))
  fi
  : $((__tmp = $1)) $((x = $3)) $(($1 = __tmp))
}

: $((__t1 = filename = num_args = bytes_read = success = buffer = file = arg = new_rib = z = y = x = no = 0))
_prim() { # no: $2
  set $@ $no $x $y $z $new_rib $arg $file $buffer $success $bytes_read $num_args $filename $__t1
  no=$2
  _malloc buffer 1
  if [ $no = 0 ] ; then
    _alloc_rib __t1 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((0 << 1) | 1))
    new_rib=$__t1
    _pop z 
    _pop y 
    _pop x 
    : $((_$((new_rib + __field0)) = x))
    : $((_$((new_rib + __field1)) = y))
    : $((_$((new_rib + __field2)) = z))
    _push2 __ $new_rib $(((0 << 1) | 1))
  elif [ $no = 1 ] ; then
    _pop x 
    _close __ $((x >> 1))
  elif [ $no = 2 ] ; then
    _pop y 
    _pop x 
    : $((_$((buffer + 0)) = (x >> 1)))
    _write success $((y >> 1)) $buffer 1
    if [ $success != 1 ] ; then
      defstr __str_1 "Cannot write to file."
      _perror __ $__str_1
    fi
    _push2 __ $((_$((_FALSE + __field0)))) $(((0 << 1) | 1))
  elif [ $no = 3 ] ; then
    _pop x 
    _read bytes_read $((x >> 1)) $buffer 1
    if [ $((!bytes_read)) != 0 ] ; then
      _push2 __ $((_$((_FALSE + __field1)))) $(((0 << 1) | 1))
    else
      _push2 __ $(((_$((buffer + 0)) << 1) | 1)) $(((0 << 1) | 1))
    fi
  elif [ $no = 4 ] ; then
    _pop x 
    _scm2str filename $x
    _open file $filename 1
    if [ $file -lt 0 ] ; then
      _push2 __ $_FALSE $(((0 << 1) | 1))
    else
      _push2 __ $(((file << 1) | 1)) $(((0 << 1) | 1))
    fi
    _free __ $filename
  elif [ $no = 5 ] ; then
    _pop x 
    _scm2str filename $x
    _open file $filename 0
    if [ $file -lt 0 ] ; then
      _push2 __ $_FALSE $(((0 << 1) | 1))
    else
      _push2 __ $(((file << 1) | 1)) $(((0 << 1) | 1))
    fi
    _free __ $filename
  elif [ $no = 6 ] ; then
    _push2 __ $(((1 << 1) | 1)) $(((0 << 1) | 1))
  elif [ $no = 7 ] ; then
    _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
  elif [ $no = 8 ] ; then
    _pop y 
    _pop x 
    num_args=0
    : $((_$((_$((_FALSE + __field0)) + __field0)) = x))
    arg=$y
    while [ $arg != $((_$((_FALSE + __field1)))) ] ; do
      _push2 __ $arg $(((0 << 1) | 1))
      arg=$((_$((_stack + __field0))))
      : $((_$((_stack + __field0)) = _$((arg + __field0))))
      arg=$((_$((arg + __field1))))
      : $(((num_args += 1) - 1))
    done
    _push2 __ $(((num_args << 1) | 1)) $(((0 << 1) | 1))
    x=$((_$((_$((_FALSE + __field0)) + __field0))))
    : $(($1 = x))
    : $((__tmp = $1)) $((no = $3)) $((x = $4)) $((y = $5)) $((z = $6)) $((new_rib = $7)) $((arg = $8)) $((file = $9)) $((buffer = ${10})) $((success = ${11})) $((bytes_read = ${12})) $((num_args = ${13})) $((filename = ${14})) $((__t1 = ${15})) $(($1 = __tmp))
    return
  elif [ $no = 9 ] ; then
    _pop x 
    _push2 __ $x $(((0 << 1) | 1))
  elif [ $no = 10 ] ; then
    _pop __ 
  elif [ $no = 11 ] ; then
    _pop x 
    _pop __ 
    _push2 __ $x $(((0 << 1) | 1))
  elif [ $no = 12 ] ; then
    x=$((_$((_$((_stack + __field0)) + __field0))))
    y=$((_$((_stack + __field1))))
    _alloc_rib __t1 $x $y $(((1 << 1) | 1))
    : $((_$((_stack + __field0)) = __t1))
  elif [ $no = 13 ] ; then
    _pop x 
    _bool2scm __t1 $((!(x & 1)))
    _push2 __ $__t1 $(((0 << 1) | 1))
  elif [ $no = 14 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field0)))) $(((0 << 1) | 1))
  elif [ $no = 15 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field1)))) $(((0 << 1) | 1))
  elif [ $no = 16 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field2)))) $(((0 << 1) | 1))
  elif [ $no = 17 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field0)) = y)) $(((0 << 1) | 1))
  elif [ $no = 18 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field1)) = y)) $(((0 << 1) | 1))
  elif [ $no = 19 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field2)) = y)) $(((0 << 1) | 1))
  elif [ $no = 20 ] ; then
    _pop y 
    _pop x 
    _bool2scm __t1 $((x == y))
    _push2 __ $__t1 $(((0 << 1) | 1))
  elif [ $no = 21 ] ; then
    _pop y 
    _pop x 
    _bool2scm __t1 $(((x >> 1) < (y >> 1)))
    _push2 __ $__t1 $(((0 << 1) | 1))
  elif [ $no = 22 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((x + y) - 1)) $(((0 << 1) | 1))
  elif [ $no = 23 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((x - y) + 1)) $(((0 << 1) | 1))
  elif [ $no = 24 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((((x >> 1) * (y >> 1)) << 1) | 1)) $(((0 << 1) | 1))
  elif [ $no = 25 ] ; then
    _pop y 
    _pop x 
    if [ $((y >> 1)) -lt 0 ] ; then
      _push2 __ $(((-(((x >> 1) / -((y >> 1)))) << 1) | 1)) $(((0 << 1) | 1))
    else
      _push2 __ $(((((x >> 1) / (y >> 1)) << 1) | 1)) $(((0 << 1) | 1))
    fi
  elif [ $no = 26 ] ; then
    _pop x 
    exit $((x >> 1))
  else
    exit 6
  fi
  _free __ $buffer
  : $(($1 = (0 << 1) | 1))
  : $((__tmp = $1)) $((no = $3)) $((x = $4)) $((y = $5)) $((z = $6)) $((new_rib = $7)) $((arg = $8)) $((file = $9)) $((buffer = ${10})) $((success = ${11})) $((bytes_read = ${12})) $((num_args = ${13})) $((filename = ${14})) $((__t1 = ${15})) $(($1 = __tmp))
}

: $((__t2 = __t1 = opnd = x = p = rest = new_pc = k = c2 = s2 = proc = jump = vari = nparams = nparams_vari = nargs = instr = i = 0))
_run() {
  set $@ $i $instr $nargs $nparams_vari $nparams $vari $jump $proc $s2 $c2 $k $new_pc $rest $p $x $opnd $__t1 $__t2
  while [ 1 != 0 ] ; do
    instr=$((_$((_pc + __field0)) >> 1))
    if [ $instr = 5 ] ; then
      exit 0
    elif [ $instr = 0 ] ; then
      jump=$((_$((_pc + __field2)) == ((0 << 1) | 1)))
      _get_opnd proc $((_$((_pc + __field1))))
      while [ 1 != 0 ] ; do
        if [ $((_$((proc + __field0)) & 1)) != 0 ] ; then
          _pop __ 
          _prim proc $((_$((proc + __field0)) >> 1))
          if [ $((!(proc & 1))) != 0 ] ; then
            continue
          fi
          if [ $jump != 0 ] ; then
            _get_cont _pc 
            : $((_$((_stack + __field1)) = _$((_pc + __field0))))
          fi
          _pc=$((_$((_pc + __field2))))
        else
          _pop __t1 
          nargs=$((__t1 >> 1))
          _alloc_rib __t1 $(((0 << 1) | 1)) $proc $(((0 << 1) | 1))
          s2=$__t1
          proc=$((_$((s2 + __field1))))
          : $((_$((_pc + __field0)) = _$((proc + __field0))))
          nparams_vari=$((_$((_$((proc + __field0)) + __field0)) >> 1))
          nparams=$((nparams_vari >> 1))
          vari=$((nparams_vari & 1))
          if [ $((vari ? (nparams > nargs): (nparams != nargs))) != 0 ] ; then
            printf "*** Unexpected number of arguments nargs: %d nparams: %d vari: %d\n" $nargs $nparams $vari
            exit 1
          fi
          : $((nargs -= nparams))
          if [ $vari != 0 ] ; then
            rest=$((_$((_FALSE + __field1))))
            i=0
            while [ $i -lt $nargs ] ; do
              _pop __t2 
              _alloc_rib __t1 $__t2 $rest $s2
              rest=$__t1
              s2=$((_$((rest + __field2))))
              : $((_$((rest + __field2)) = (0 << 1) | 1))
              : $((i += 1))
            done
            _alloc_rib __t1 $rest $s2 $(((0 << 1) | 1))
            s2=$__t1
          fi
          i=0
          while [ $i -lt $nparams ] ; do
            _pop __t2 
            _alloc_rib __t1 $__t2 $s2 $(((0 << 1) | 1))
            s2=$__t1
            : $((i += 1))
          done
          nparams=$((nparams + vari))
          _list_tail __t1 $s2 $nparams
          c2=$__t1
          if [ $jump != 0 ] ; then
            _get_cont k 
            : $((_$((c2 + __field0)) = _$((k + __field0))))
            : $((_$((c2 + __field2)) = _$((k + __field2))))
          else
            : $((_$((c2 + __field0)) = _stack))
            : $((_$((c2 + __field2)) = _$((_pc + __field2))))
          fi
          _stack=$s2
          new_pc=$((_$((_pc + __field0))))
          : $((_$((_pc + __field0)) = (instr << 1) | 1))
          _pc=$((_$((new_pc + __field2))))
        fi
        break
      done
    elif [ $instr = 1 ] ; then
      x=$((_$((_stack + __field0))))
      if [ $((_$((_pc + __field1)) & 1)) != 0 ] ; then
        _list_tail __t1 $_stack $((_$((_pc + __field1)) >> 1))
        opnd=$__t1
      else
        opnd=$((_$((_pc + __field1))))
      fi
      : $((_$((opnd + __field0)) = x))
      _stack=$((_$((_stack + __field1))))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 2 ] ; then
      _get_opnd __t1 $((_$((_pc + __field1))))
      _push2 __ $__t1 $(((0 << 1) | 1))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 3 ] ; then
      _push2 __ $((_$((_pc + __field1)))) $(((0 << 1) | 1))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 4 ] ; then
      _pop p 
      if [ $p != $_FALSE ] ; then
        _pc=$((_$((_pc + __field1))))
      else
        _pc=$((_$((_pc + __field2))))
      fi
    else
      exit 6
    fi
  done
  : $((__tmp = $1)) $((i = $2)) $((instr = $3)) $((nargs = $4)) $((nparams_vari = $5)) $((nparams = $6)) $((vari = $7)) $((jump = $8)) $((proc = $9)) $((s2 = ${10})) $((c2 = ${11})) $((k = ${12})) $((new_pc = ${13})) $((rest = ${14})) $((p = ${15})) $((x = ${16})) $((opnd = ${17})) $((__t1 = ${18})) $((__t2 = ${19})) $(($1 = __tmp))
}

: $((first = 0))
_setup_stack() {
  set $@ $first
  _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
  _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
  first=$((_$((_stack + __field1))))
  : $((_$((_stack + __field1)) = (0 << 1) | 1))
  : $((_$((_stack + __field2)) = first))
  : $((_$((first + __field0)) = (5 << 1) | 1))
  : $((_$((first + __field1)) = (0 << 1) | 1))
  : $((_$((first + __field2)) = (0 << 1) | 1))
  : $((__tmp = $1)) $((first = $2)) $(($1 = __tmp))
}

: $((__t3 = __t2 = __t1 = 0))
_init() {
  set $@ $__t1 $__t2 $__t3
  _init_weights __ 
  _init_heap __ 
  _alloc_rib __t2 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((5 << 1) | 1))
  _alloc_rib __t3 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((5 << 1) | 1))
  _alloc_rib __t1 $__t2 $__t3 $(((5 << 1) | 1))
  _FALSE=$__t1
  _build_sym_table __ 
  _decode __ 
  _alloc_rib __t1 $(((0 << 1) | 1)) $_symbol_table $(((1 << 1) | 1))
  _set_global __ $__t1
  _set_global __ $_FALSE
  _set_global __ $((_$((_FALSE + __field0))))
  _set_global __ $((_$((_FALSE + __field1))))
  _setup_stack __ 
  _run __ 
  : $((__tmp = $1)) $((__t1 = $2)) $((__t2 = $3)) $((__t3 = $4)) $(($1 = __tmp))
}

_main() {
  _init __ 
}

# Character constants
readonly __NUL__=0
# Runtime library
_free() { # $2 = object to free
  __ptr=$(($2 - 1))          # Start of object
  __end=$((__ptr + _$__ptr)) # End of object
  while [ $__ptr -lt $__end ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
  done
  : $(($1 = 0))              # Return 0
}

# Unpack a Shell string into an appropriately sized buffer
unpack_line() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    __c=$(LC_CTYPE=C printf "%d" "'${__fgetc_buf%"${__fgetc_buf#?}"}")
    : $((_$__buffer = __c))
    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buffer += 1))              # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $((_$__buffer = 10))            # Line ends with newline
    : $((__buffer += 1))
  fi
  : $((_$__buffer = 0))               # Then \0
}

refill_buffer() { # $1: fd
  __fd=$1
  __buffer=$((__buffer_fd$__fd))

  IFS=
  if read -r __temp_buf <&$__fd ; then  # read next line into $__temp_buf
    __ends_with_eof=0
  else
    __ends_with_eof=1
  fi

  # Check that the buffer is large enough to unpack the line
  __buflen=$((__buflen_fd$__fd - 2)) # Minus 2 to account for newline and \0
  __len=${#__temp_buf}
  if [ $__len -gt $__buflen ]; then
    # Free buffer and reallocate a new one double the line size
    __buflen=$((__len * 2))
    _free __ $__buffer
    _malloc __buffer $__buflen
    : $((__buffer_fd$__fd = __buffer))
    : $((__buflen_fd$__fd = __buflen))
  fi
  unpack_line "$__temp_buf" $__buffer $__ends_with_eof
}

read_byte() { # $2: fd
  __fd=$2
  : $((__buffer=__buffer_fd$__fd))
  : $((__cursor=__cursor_fd$__fd))
  # The cursor is at the end of the buffer, we need to read the next line
  if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
    # Buffer has been read completely, read next line
    refill_buffer $__fd
    __cursor=0 # Reset cursor and reload buffer
    : $((__buffer=__buffer_fd$__fd))
    if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
      : $(($1 = -1)) # EOF
      return
    fi
  fi
  : $(($1 = _$((__buffer + __cursor))))
  : $((__cursor_fd$__fd = __cursor + 1))  # Increment cursor
}


# Convert a pointer to a C string to a Shell string.
# $__res is set to the result, and $__len is set to the length of the string.
pack_string() { # $1 = string address, $2 = end of string delimiter (default to null), $3 = max length (default to 100000000) 
  __addr=$1; 
  __max_len=100000000
  __delim=0
  __len=0
  __res=""
  if [ $# -ge 2 ] ; then __delim=$2   ; fi # Optional end of string delimiter
  if [ $# -ge 3 ] ; then __max_len=$3 ; fi # Optional max length
  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)        __char=$(printf "\\$(($__char/64))$(($__char/8%8))$(($__char%8))")
        __res="$__res$__char" ;;
    esac
  done
}

__state_fd0=0;
_malloc __buffer_fd0 1000   # Allocate buffer
: $((_$__buffer_fd0 = 0))   # Init buffer to ""
: $((__cursor_fd0 = 0))     # Make buffer empty
: $((__buflen_fd0 = 1000))  # Init buffer length
__state_fd1=1
__state_fd2=1
__state_fd3=-1
__state_fd4=-1
__state_fd5=-1
__state_fd6=-1
__state_fd7=-1
__state_fd8=-1
__state_fd9=-1

_open() { # $2: filename, $3: flags, $4: mode
  # Get available fd
  __fd=0
  while [ $__fd -lt 10 ]; do
    if [ $((__state_fd$__fd)) -lt 0 ]; then
      break
    fi
    : $((__fd += 1))
  done
  if [ $__fd -gt 9 ] ; then
    # Some shells don't support fd > 9
    echo "No more file descriptors available" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    _malloc __addr 1000                   # Allocate buffer
    : $((_$__addr = 0))                 # Init buffer to ""
    : $((__buffer_fd$__fd = __addr))    # Save buffer address
    : $((__cursor_fd$__fd = 0))         # Make buffer empty
    : $((__buflen_fd$__fd = 1000))      # Init buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    pack_string $2
    if [ $3 = 0 ] ; then
      case $__fd in
        0) exec 0< $__res ;; 1) exec 1< $__res ;; 2) exec 2< $__res ;;
        3) exec 3< $__res ;; 4) exec 4< $__res ;; 5) exec 5< $__res ;;
        6) exec 6< $__res ;; 7) exec 7< $__res ;; 8) exec 8< $__res ;;
        9) exec 9< $__res ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> $__res ;; 1) exec 1> $__res ;; 2) exec 2> $__res ;;
        3) exec 3> $__res ;; 4) exec 4> $__res ;; 5) exec 5> $__res ;;
        6) exec 6> $__res ;; 7) exec 7> $__res ;; 8) exec 8> $__res ;;
        9) exec 9> $__res ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> $__res ;; 1) exec 1>> $__res ;; 2) exec 2>> $__res ;;
        3) exec 3>> $__res ;; 4) exec 4>> $__res ;; 5) exec 5>> $__res ;;
        6) exec 6>> $__res ;; 7) exec 7>> $__res ;; 8) exec 8>> $__res ;;
        9) exec 9>> $__res ;;
      esac
    else
      echo "Unknow file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

_read() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    read_byte __byte $__fd
    if [ $__byte -lt 0 ] ; then
      break
    fi
    : $((_$((__buf + __i)) = __byte))
    : $((__i += 1))
  done
  : $(($1 = __i))
}

_write() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    : $((__byte = _$((__buf+__i))))
    printf \\$(($__byte/64))$(($__byte/8%8))$(($__byte%8)) >&$__fd
    : $((__i += 1))
  done
  : $(($1 = __count))
}

_close() { # $2: fd
  __fd=$2
  __buf=$((__buffer_fd$__fd))  # Get buffer
  _free __ $__buf              # Release buffer
  : $((__state_fd$__fd = -1))  # Mark the fd as closed
  case $__fd in
    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;
    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;
    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;
    9) exec 9<&- ;;
  esac
  : $(($1 = 0))
}

# Local variables
__=0

__code=0; # Success exit code
_main __code; exit $__code
