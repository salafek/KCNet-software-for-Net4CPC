?	Z3ENV  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?:??1§?????? ? E? ? H=? ? o.? ? K? ? A?	 ? C_? ????????#ޭ??                                         System : Net4CPC - W5100S
    Interface : 0FD20H
$ ??x?(7??!?	? ??!?* ???&|`??}??
Net4CPC Interface not found !$
At least KCNet Firmware-Version 1.3 required !$???r????
ü???
?3
?Ô
Ù
ý
?]
?+?!??a?i?y???!??a?i?x???????!??Q?Yѯ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????(z?????????(3????????????????????????????????= ͯ??_>-???2@ ??????????????????????????????????????!??Q?Yѯ?ʉ	????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????À??(z?????????(3????????????????????????????????= ͯ??_>-???2?	 ?????????????????????????????????????P  ?????????? ??_!??? ?????????P  ?r???????? ??_!???? ???????????!m
' ??????Net4CPC - W5100S Ethernet Interface
 $&.???!?> ?y>??y?xg?o|??????????g?????!?> ?y><?y?x???( ?????!?> ?y>??y?xg?xo
?/??????* W "???????    ??!?> ?y>??y?x??o?xg???!  ɯ)8?8?,????V?=(?????! ??????! ??{?????!???!?????!????? ?!????? ??????? ?????????????????	?? ??*???.?? ??! ?*?! ?
?!??!???g?_????!?
?!???!???g{?????!???!????
????? ,??? 4?????? ??? ??? ??W???????? ??W?????! ?! ?????! ?! ??Շ??W ????????̀?|?W}?_?͈?bk	?DM?x??B??0$??????x??RDM????????????BDM͈?????????????̈́?|?W}?_?͍?bk	?DM?|??B??0$??????|??RDM????????????BDM͍???????????!$??g?W#?_????͜???	???!$??gz?#{????????!(??g?W#?_?????????	???!(??gz?#{?????? ????W???!  ??B(
??????B ???????G#?O+????&??K<(	=?87?x=??(?7?Gz?(? y栲W? xY? ? ޯ? ?Zx???????W{??(?zz??~#?+zʉ?ÆW{? (?(?(z7??z???z?͢z???x?7(?zPY?????W{?_?(?(
?(?zzS???_z?{(#z?{(?9 ???#?!??g{?#z??????_z?(."?(.2?(.B{??W}?zÆ{??W>?z???>????(.???(&??(???@?&????W>?z(Y> ?z ?T??P??!??g??_ #?_??????W{??z -?? &?9(????>?z ????>?z ѷ??7????W>?z ???J?????W>?(z???? ?ͼ???!??g??#???????? ??ͼ?Wx?z7???x???B?0DMPY͢`i??R0?W??>?(>?z?(??  7?BK??@ ????W>? z??> ?z ???>???>?_z????PY???Wx?z?7??bk??B?0BK?q?@?????x???B?0DM??x?(3?????{??(?(? ??)???(???(
??? ???  7????@?&??@ ????W>??=>??6z??> ?z ??3Z? # ?????????x? ???  7?????W{? ?( ?( ?(z?c?????z?!(??g?W#?_????????????	?+N+F?? ? !???B0?????	????  7!??????bk??B0BK???????????	???ѷ??!(??gz?#{????????c??J?0???͔? ????????!? ~?(2 {?_#z?W+0?{?_#z?#W((?x?0?w ?#?'?d 
   {?0?w ?#???????? ~?08?:0#??W_?7(?+A~?0_?+?Abj]T)))?????{???????6.#? _Ϳ??6 ?????????~#?.7 ??{?87 ??????? ????	??????????? ????? ?? ?   ??????                                                                                                                               ?     	                                  WOL     INI                        	                              CP/M-Version 2+ required !
$Too many arguments !$No socket available !$Cannot open socket !$More TPA required !$WOL.INI not found (show help with -h) !$No accounts found in WOL.INI !$Account $ not found in WOL.INI !$ ### Account List ###
$ >M< .. more accounts
$ Select account number key: $ Send WOL packet $ Error!$ OK.$wol [-h|v] [-f] [[DIR|DU:]name]|[destination-MAC [network/mask]]

  Options:
   -h|v - show help or version and exit
     -f - Full Output
Parameter:
   name - Destination name defined in [DIR|DU:]WOL.INI
    MAC - ":"-delimited destination MAC address (hex.)
network - Network address, if MAC is not in local net (d.d.d.d)
   mask - Subnet mask of the other network (d.d.d.d)

   Wake-On-LAN  allows  a  computer to be turned on or
   woken up by a network message platform-independent.$###    WakeOnLan Client CP/M 2+     ###
             Version 1.1
         "KCNet TCP/IP-Stack"
### copyright 2009-2011 by >susowa< ###

$
           Show Help with -h$6 T]????>??#>
??#????(	?$(?5????????Eͬ ??????>:??#~ͱ##??????ͬ?ͬwͬ??  ?????!?5?*?^#V#"???! ͖??[	z?7??! ^#V#~??7??G?(D?GO???? (O~??$? #?͋?? ??K?? 	?????KZ~͝$(ͱ$( ??7?~???:??_?A?0?G#͋(ͱ$ ???#͋ ?{? 0?O???CZ#?invalid value for DIR|DU: !
$?0??
8????
?????o ??????#?:7 ??8??????!????(?!?/ ???invalid value for destination-MAC !
$?~#?/(?7 ?????!??d????~?/# ??!??d???!?5 ???invalid value for network !
$invalid value for ip-address !
$invalid value for subnet mask !
$? ~??(͖$(?-( ?a8?_#??7???invalid name, only "A-Z", "0-9", "-" allowed !
$?,??U6network-cable not connected !$!5?͔ͨ?????(??̬7???network not configured !$         	[ACCOUNT]xxx.xxx.xxx.xxx/xxx.xxx.xxx.xxx NAME=MAC=NET=?\?.?(???!8?;(?[(?'?????*!??	!??.?(??͝$ ?_w#?](????\ ?!?.??(???!8?;(?[???_?N(?M(?'??????*!??	!??.??(??͝$ ?_w#?=(?????!?(???!(???!N(\?!?6 \?.8(??!8͝$ ?_w#6 ??'0???*!??!???\?A?????*!???!???!/? ??!T4??O !???????.? ?????T!??7?? ???T!??7??
????*!?????
7??!59>>?/?#?= ?!/??? ??????ͬ:?W($>(??#!/????(> ??#!>ͽ>)??#> ??#?ͻ????>.??#:=!/?T!>?@?&!?f ?@!????W>? >? 
z??> ?z ?>?7?>?_z????? 2???? ?ܬ?T 1? ?#?8#?CZ?2?!? ??<?8$:??8	?ͬ?? ??!?"???
?>-? $#N#??(??!???V ???H ???F ???N#?]T  ??: ?/ ? ??C?ʹ0???0????:?7?:??7??ܬ?!???:?_7?:??7?:?=7??Nܬ?:?_?:??7?:??7?? 
?%?uܬ???ܬ?!???!?F??G?N???N ??ܬܟw?G?t?? ?K?? !?^? ??$ ?$?G"d?$?G"U?2T???KZ?##\??  ???G!???f(`!D ????8!>2T!D?? ?? ??!5???t? !T~?I?G6 iͬDͬrͬ͟??\??!?? ????0I?G͟?ͬ*U"X͖?2W?[X:W<2W! ???SX!T5??8:W?	 ?*U:WG> ??#>>??#>0???#><??#> ??##]Tͬ :?W ($> ??#??#????(?> ??#ͽ>/??#ͽ? ͟?:T??Ĭ͟?ͬ>??#͢#??? ??? ?18??:8???M ?_:T?(?{??#͟? O?0G:W?8?y??#*U ??R?# ??͟>? ???ܬ8#2=?v?ܬ8?U?ܬ?Ԭ:=??!?~??\??!??͟?# ?                                                                                                                                 ?g6#>
w?:?@???J????+"?2? 2? *? ??y#?]# $͂"?_"??ó!????+"?2? ??"*? ??y#?O#(߯ó!?????+":? ? ?2? >?2? ?_"??:? i?2? ?:? _?????+"?:? ?(?2? ?_">?:? B?##^#V+z?(s#r#^#V2? r+s ??+"͂" ?*? ~2? #"? *? +"? ?_"??:? ???????2? ?##^#V+z?(s#r#^#V:? ???+"??"?&!*? :? w#"? *? +"? ?????+" ????+"*? }?(+"? *? 6#"? ???"?[? ?X#?&!?!?"? ~2? #~2? #^#V?S? #^#V?S? #~2? #~2? #^#V?S? #"? ?*? #:? w?[? #s#r?[? #s#r#:? w#:? w?:? ??*? :? G?o#?[? ?e# ? ?>?2? *? "? :? ?g. ?<?"? ?!? ?o#?|? D???*? ):? ?G*? x?(?o#?[? ?j#? ?(??*? "? ?2? :? g. ?<?"? !? ?o#??:???8#?C#????K#?##??????Y ͍#X͍#????????͍#?? ͍#O?G?????]#?>?_#>?_#>͖#???>Ö#>Ö#?>?͖#??????? rr# ???????? ?ѷ????O? ?????ͨ#??$?>??$??????$??#????$??#??????O>??$????  B~?0?0?
0O?bk)8)88)8	8??#??{??W510????G???$"?#??8??$+"?#??????*?#?*?#͐$8*?#?S?#???????O?F?##?u$?(#s#r#(̓$?(y?(?x? ?̓$?u$?(????~?#w??????? (??	(????? ??	??|??}??ͱ$?Ý$?O???$?A8?[8??y???O??08?:8??y????a??{??_?????$????o??* o??* . : ???g??Ɛ'?@'???*%? j???	%??	??|??}??ͱ$?Ý$?O???$?A8?[8??y???O??08?:8??y????a??{??_?????$????o??* o??* . : ???g??Ɛ'?@'???*%