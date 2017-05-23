(*a*)
fun pert (e,[]) = false
    | pert (e,l) = e = hd(l) orelse pert(e,tl(l));
(*b*)
fun somat([]) = 0
    | somat(l) = hd(l) + somat(tl(l))
(*c*)
fun indlst(e,l) = if hd(l) = e then 0 else 1 + indlst(e,tl(l)); 
fun indlist(e,l) = if pert(e,l) then indlst(e,l) else ~1;
(*d*)
fun poslist(0, l) = hd(l)
    | poslist(i,l) = poslist(i-1,tl(l))
(*e*)
fun altlist(e,[],0) = []
    | altlist(e,l,0) = e::tl(l)
    | altlist(e,l,i) = hd(l)::altlist(e,tl(l),i-1);
(*f*)
fun conc([],l2) = l2
    | conc(l1,[]) = l1
    | conc(l1,l2) = hd(l1)::conc(tl(l1),l2);
(*g*)
fun inv2(l,0) = []
    | inv2(l,i) = poslist(i-1,l)::inv2(l,i-1); 
fun inv([]) = []
    | inv(l) =  inv2(l, length(l))
(*h*)
fun inselem(l,e,0) = e::l
    | inselem(l,e,i) = hd(l)::inselem(tl(l),e,i-1);
