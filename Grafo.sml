type grafo = (string list) * ((string * string) list);
val G1:grafo = (["a","b","c","d","e","f","g","k"], [("b","a"), ("a","c"), ("c","g"), ("a","e"), ("e","b"),("e","d"), ("f","e"), ("g","f"),("d","g"),("b","k")]);

val G2:grafo = (["a","b","c","d"],
[("a","b"), ("b","c"), ("c","d"), ("d","a")]);

val G22:grafo = (["1","2","3","4"],
[("1","2"), ("2","3"), ("3","4"), ("4","1")]);

val G3:grafo = (["a","b","c","d","e","f"],
[("d","a"), ("d","b"), ("d","c"), ("e","a"), ("e","b"), ("e","c"),("f","a"), ("f","b"), ("f","c")]);

val G4:grafo = (["a","b","c","d","e"],
[("a","b"), ("a","c"), ("a","d"), ("a","e"), ("b","c"),("b","d"), ("b","e"), ("c","d"), ("c","e"),("d","e")]);

fun arestas (a,b) = b; 
fun vertices (a,b) = a;

(*A*)

fun connect_dir(lista,x,y) = 
    let 
        fun f n = (x,y) = n
    in 
        List.exists f lista
    end;

fun connect_interno(lista,visitados,x,y) = 
        connect_dir(lista,x,y) 
    orelse 
        let
            fun f n = connect_interno(lista,(x::visitados),n,y)
            fun extrai (a,_) = a = x
            fun segundo (a,b) = b
            fun equi a b = a = b
            fun naovisitado n = not (List.exists (equi n) visitados)
        in
            List.exists f (List.filter naovisitado (map segundo (List.filter extrai lista)))
        end;

fun conectado(G,n1,n2) = connect_interno((arestas G),[],n1,n2);

(*B*)
fun ciclo(G,n) = conectado(G,n,n);

(*C*)
fun grafo_ciclico(G) = 
    let 
        fun cicl n = ciclo(G,n)
    in
        List.exists cicl (vertices G)
    end;

(*D*)
fun caminho(G, n1, n2, []) = false
    | caminho(G, n1, n2, c::[]) = n1 = n2 andalso n1 = c
    | caminho(G, n1, n2, c) =
        let 
            fun equi a b = a = b
            val prim = hd(c)
            val seg = hd(tl(c))
        in
            List.exists (equi (prim,seg)) (arestas G) andalso caminho(G, seg, n2, tl(c))
        end;

(*E*)
fun chegouDestino(n2,lista,aresta) = 
    let 
        fun achouDestino x = x = n2
        fun achouDestinoLista x = achouDestino (List.last x)
        val resultado = List.find achouDestinoLista lista
    in 
        if resultado = NONE
        then expandeCaminho(n2,lista,aresta) 
        else [valOf resultado]
    end

and expandeCaminho(n2,lista,aresta) = 
    let 
        val head = hd(lista)
        fun concat x = head @ [x]
        fun extrai x (a,_) = a = x
        fun segundo (_,b) = b
        val expansao = map concat (map segundo (List.filter (extrai (List.last head)) (aresta)))
        
        val novaLista = tl(lista) @ expansao
    in
        chegouDestino(n2,novaLista,aresta)
    end;
    
fun caminho_mais_curto(G,n1,n2) = if conectado(G,n1,n2) then hd(chegouDestino(n2,[[n1]],(arestas G))) else [];

(*F*)
fun isolate [] = []
  | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs);
fun isCycle(lista) = hd(lista) = (List.last lista);
fun isHamiltonian(G,lista) = 
    let 
        val tamanho = (length (vertices G))
     in 
        tamanho = (length (isolate(lista))) andalso tamanho = (length (tl(lista)))
    end;

fun isHamiltonianCycle G l = isCycle(l) andalso isHamiltonian(G,l);

fun bfs(lista,aresta,0) = lista
    | bfs(lista,aresta,deep) = 
        let 

        fun concat x y = x @ [y]
        fun extrai x (a,_) = a = x
        fun segundo (_,b) = b
        fun expansao x = map (concat x) (map segundo (List.filter (extrai (List.last x)) (aresta)))
        val novaLista = List.concat (map expansao lista)


        in
            bfs(novaLista,aresta,deep-1)
        end;

fun hamiltoniano(G) =
    let 
        val aresta = arestas G
        val vertice = map (fn x => [x]) (vertices G)
        val deep = (length vertice) 
    in 
        List.exists (isHamiltonianCycle G) (bfs(vertice,aresta,deep))
    end;

(*G*)

fun isEulerian(G,lista) = 
    let 
        val tamanho = (length (arestas G))
     in 
        tamanho = (length (isolate(lista))) andalso tamanho = (length (tl(lista)))
    end;
fun isEulerianCycle G l = isCycle(l) andalso isEulerian(G,l);

fun euleriano(G) =
    let 
        val aresta = arestas G
        val vertice = map (fn x => [x]) (vertices G)
        val deep = (length vertice) 
    in 
        List.exists (isEulerianCycle G) (bfs(vertice,aresta,deep))
    end;

(*H*)
fun connect_bidir(lista,x,y) = 
    let 
        fun f n = (x,y) = n
        fun g n = (y,x) = n
    in 
        (List.exists f lista) orelse (List.exists g lista)
    end;

fun isk33(aresta,A,B,C,D,E,F) = 
    A <> B andalso A <> C andalso A <> D andalso A <> E andalso A <> F 
    andalso B <> C andalso B <> D andalso B <> E andalso B <> F
    andalso C <> D andalso C <> E andalso C <> F
    andalso D <> E andalso D <> F
    andalso E <> F
    andalso connect_bidir(aresta,D,A) andalso connect_bidir(aresta,D,B) andalso connect_bidir(aresta,D,C)
    andalso connect_bidir(aresta,E,A) andalso connect_bidir(aresta,E,B) andalso connect_bidir(aresta,E,C)
    andalso connect_bidir(aresta,F,A) andalso connect_bidir(aresta,F,B) andalso connect_bidir(aresta,F,C);

fun isk5(aresta,A,B,C,D,E) = 
    A <> B andalso A <> C andalso A <> D andalso A <> E
    andalso B <> C andalso B <> D andalso B <> E
    andalso C <> D andalso C <> E 
    andalso D <> E 
    andalso connect_bidir(aresta,A,B) andalso connect_bidir(aresta,A,C) andalso connect_bidir(aresta,A,D) andalso connect_bidir(aresta,A,E)
    andalso connect_bidir(aresta,B,C) andalso connect_bidir(aresta,B,D) andalso connect_bidir(aresta,B,E) 
    andalso connect_bidir(aresta,C,D) andalso connect_bidir(aresta,C,E)
    andalso connect_bidir(aresta,D,E); 


fun interleave x [] = [[x]]
| interleave x (h::t) =
    (x::h::t)::(List.map(fn l => h::l) (interleave x t));

fun permute nil = [[]]
| permute (h::t) = List.concat( List.map (fn l => interleave h l) (permute t));

fun planar(G) = 
    let
        val aresta = arestas G
        val vertice = vertices G
        val tamanho = length vertice
        fun k33 x = isk33(aresta,List.nth(x,0),List.nth(x,1),List.nth(x,2),List.nth(x,3),List.nth(x,4),List.nth(x,5))
        fun k5 x = isk5(aresta,List.nth(x,0),List.nth(x,1),List.nth(x,2),List.nth(x,3),List.nth(x,4))
        fun hasProhibitedGraph x = k33(x) orelse k5(x)
        val permutation = permute vertice
        fun take6 x = List.take(x,6)
    in
        tamanho < 5 
        orelse 
            if tamanho = 5 
            then  not(k5(vertice))
            else  not (List.exists (hasProhibitedGraph) (map take6 permutation))
    end;


(*I*)
fun dicVal(key,(k,value)::tail) = if key = k then value else dicVal(key,tail);

fun morfismo(ArestaX,ArestaY,Bijecao) = 
	let 
		fun aplicaBijecao (x,y) = (dicVal(x,Bijecao),dicVal(y,Bijecao))
		val arestaCandidata = map aplicaBijecao ArestaX
		fun igualdadeArestas([],[]) = true
			| igualdadeArestas([],_) = false
			| igualdadeArestas(_,[]) = false
			| igualdadeArestas(x::tailX,y::tailY) = x = y andalso igualdadeArestas(tailX,tailY)
	in 
		igualdadeArestas(arestaCandidata,ArestaY)
	end;
fun isomorfosCompleteTest arestasX verticesX arestasY verticesY = 
    let
        fun bijecaoV2([],[]) = []
			| bijecaoV2(x,y) = (hd(x),hd(y))::bijecaoV2(tl(x),tl(y))
        fun bijecaoV1 x = bijecaoV2(verticesX,x)

		val permutacao = permute verticesY
		val bijecaoVertice = map bijecaoV1 permutacao
		fun preservacaoarestas x = morfismo(arestasX,arestasY,x)
    in
		List.exists preservacaoarestas bijecaoVertice
        
    end;
	
	
fun isomorfos GX GY = 
    let
        val arestasX = arestas GX
        val arestasY = arestas GY
        val lengthArestaX = length arestasX
        val lengthArestaY = length arestasY


        val verticesX = vertices GX
        val verticesY = vertices GY
        val lengthVerticeX = length verticesX
        val lengthVerticeY = length verticesY

    in
        lengthArestaX = lengthArestaY andalso lengthVerticeX = lengthVerticeY 
		andalso (isomorfosCompleteTest arestasX verticesX arestasY verticesY)
    end;

