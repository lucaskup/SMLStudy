datatype leaf = Int of int | Str of string | Real of real;
datatype arvoper = soma | sub | mult | divis | expon;
datatype 'a arvbin = folha of 'a |
 nodo of arvoper *'a arvbin * 'a arvbin;

fun evalarv (folha(n)) = n |
 evalarv (nodo(oper, arvdir, arvesq)) =
 case oper of
 soma => (evalarv(arvdir)) + (evalarv(arvesq)) |
 sub => (evalarv(arvdir)) - (evalarv(arvesq)) |
 mult => (evalarv(arvdir)) * (evalarv(arvesq)) |
 divis => (evalarv(arvdir)) div (evalarv(arvesq));

val arv1 = nodo(mult,
nodo(sub,
 folha 3,
 folha 5),
 nodo(soma,
 folha 7,
 folha 2));

val arv2 = nodo(mult,
nodo(expon,
 folha (Int 3),
 folha (Int 5)),
 nodo(soma,
 folha (Int 7),
 folha (Str "x")));

(*A*)
fun preordlist(folha(n)) = [Int.toString n]
    | preordlist(nodo(oper,arvesq,arvdir)) =
        case oper of
             soma => ["somar"] @ (preordlist arvesq) @ (preordlist arvdir) |
             sub => ["subtrair"] @ (preordlist arvesq) @ (preordlist arvdir) |
             mult => ["multiplicar"] @ (preordlist arvesq) @ (preordlist arvdir) |
             divis => ["dividir"] @ (preordlist arvesq) @ (preordlist arvdir);

(*B*)
fun posordlist(folha(n)) = [Int.toString n]
    | posordlist(nodo(oper,arvesq,arvdir)) =
        case oper of
             soma => (posordlist arvesq) @ (posordlist arvdir) @ ["somar"] |
             sub => (posordlist arvesq) @ (posordlist arvdir) @ ["subtrair"] |
             mult => (posordlist arvesq) @ (posordlist arvdir) @ ["multiplicar"] |
             divis => (posordlist arvesq) @ (posordlist arvdir) @ ["dividir"];

(*C*)
fun simordlist(folha(n)) = [Int.toString n]
    | simordlist(nodo(oper,arvesq,arvdir)) =
        case oper of
             soma => (simordlist arvesq) @ ["somar"] @ (simordlist arvdir)  |
             sub => (simordlist arvesq) @ ["subtrair"] @ (simordlist arvdir) |
             mult => (simordlist arvesq) @ ["multiplicar"] @ (simordlist arvdir) |
             divis => (simordlist arvesq) @ ["dividir"] @ (simordlist arvdir);

(*D*)
val l = [ ("x", 10.0), ("y", ~3.1), ("a", 2.3) ];
fun dicVal(key,(k,value)::tail) = if key = k then value else dicVal(key,tail);


fun calc(folha(Real n),l) = n |
    calc(folha(Int n),l) = Real.fromInt n |
    calc(folha(Str n),l) = dicVal(n,l) |
 calc(nodo(oper, arvdir, arvesq),l) =
     case oper of
     soma => (calc(arvdir,l)) + (calc(arvesq,l)) |
     sub => (calc(arvdir,l)) - (calc(arvesq,l)) |
     mult => (calc(arvdir,l)) * (calc(arvesq,l)) |
     divis => (calc(arvdir,l)) / (calc(arvesq,l)) |
     expon => Math.pow(calc(arvdir,l),calc(arvesq,l));

(*E*)
datatype 'a bst = nod of 'a *'a bst * 'a bst|
 nil;

val arv3 = nod(5,
                nod(3,
                    nod(2,nil,nil),
                    nod(4,nil,nil)),
                nod(8,
                    nil,
                    nod(9,nil,nil)));

fun insere_abb(nil,v) = nod(v,nil,nil)
    | insere_abb(nod(x,bstLeft,bstRight),v) = if x = v then nod(x,bstLeft,bstRight) 
                                            else if v > x then nod(x,bstLeft,insere_abb(bstRight,v)) 
                                            else nod(x,insere_abb(bstLeft,v),bstRight);






