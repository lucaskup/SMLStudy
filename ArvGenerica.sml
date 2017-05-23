datatype arv = terminal of string * string list * real * int|
    intermediario of string * string * arv list;

val arv1 = intermediario("Mestrado","Par",[terminal("Tecnicas",["PC","IA"],3.2,2),
                                        terminal("Metodos",["Calculadora","PC"],7.3,2)])


(*A*)
fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);

fun varreListaArv([]) = []
    | varreListaArv(l) = isolate(total_recursos(hd(l)) @ varreListaArv(tl(l)))

and total_recursos(terminal(_,lista,_,_)) = isolate(lista)
    | total_recursos(intermediario(_,_,filhos)) = varreListaArv(filhos);


(*B*)

fun varreListaArv2([],_) = []
    | varreListaArv2(l,obj) = isolate(recursos_obj(hd(l),obj) @ varreListaArv2(tl(l),obj))
and recursos_obj(terminal(obj,lista,_,_),objetivo) = if obj = objetivo then isolate(lista) else []
    | recursos_obj(intermediario(obj,_,filhos),objetivo) = if obj = objetivo then varreListaArv(filhos) else varreListaArv2(filhos,objetivo);


(*C*)

fun somatorioLista([], _) = 0.0
    | somatorioLista(lista, f) = (f(hd(lista))) + somatorioLista(tl(lista),f);

fun custo_total(terminal(_,_,custo,_)) = custo
    | custo_total(intermediario(_,_,filhos)) = somatorioLista(filhos,custo_total);

(*D*)


fun custo_obj(terminal(obj,_,custo,_),objetivo) = if obj = objetivo then custo else 0.0
    | custo_obj(intermediario(obj,_,filhos),objetivo) = if obj = objetivo then somatorioLista(filhos,custo_total) else somatorioLista(filhos,(custo_b objetivo))
and custo_b obj arvore = custo_obj(arvore,obj);
