(*A*)
fun poslista(t,0) = hd(t)
    | poslista(t,i) = poslista(tl(t),i-1);

fun postab(t,i,0) = poslista(hd(t),i)
    | postab(t,i,j) = postab(tl(t),i,j-1);

(*B*)
fun altlista([],e,0) = []
    | altlista(t,e,0) = e::tl(t)
    | altlista(t,e,i) = hd(t)::altlista(tl(t),e,i-1);

fun alttab(t,e, i, 0) = altlista(hd(t),e,i)::tl(t)
    | alttab(t,e, i, j) = hd(t)::alttab(tl(t),e,i,j-1);

(*C*)
fun transpose [] = []
| transpose ([]::_) = []
| transpose mat = (map hd mat)::(transpose(map tl mat));

(*D*)
fun inslin(t, 0, l) = l::t
    | inslin(t, i, l) = hd(t)::inslin(tl(t), i-1, l); 
