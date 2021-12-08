sig Node {
	edges : Node
}

sig Path {
	nodes : seq Node
}


/*
 * Some auxiliary functions and predicates
 */

pred even [n:Int] {
	rem[n, 2] = 0
}

fun srcIdxs [p:Path] : set Int {
	p.nodes.butlast.inds
}

fun nodeAt [p:Path, i:Int] : Node {
	p.nodes[i]
}

pred isEdge [p:Path, i:Int] {
	let j = add[i, 1] {
		nodeAt[p, i]->nodeAt[p, j] in edges
		or
		nodeAt[p, j]->nodeAt[p, i] in edges
	}
}

pred isEdgeAt [p:Path, i:Int, s:Node, d:Node] {
	//isEdge[p, i]
	let j = add[i, 1] {
		(nodeAt[p, i] = s && nodeAt[p, j] = d)
		or
		(nodeAt[p, i] = d && nodeAt[p, j] = s)
	}
}


/*
 * Restrictions of the problem at hand
 */

fact GraphFacts
{
	/* There are nodes */
	//#Node > 0

	/* Anti-reflexive: nodes aren't connected to themselves */
	//all n:Node | n->n not in edges
	no edges & iden

	/* Symmetric: nodes are connected both ways */
	//all a:Node, b:Node-a | a->b in edges <=> b->a in edges
	~edges in edges

	/* There are at least two (connected) nodes */
	some edges

	/* There's a path between each two nodes */
	all a:Node, b:Node-a | b in a.^edges
}

fact PathFacts
{
	all p:Path {
		/* Paths are either empty, or have at least 1 edge (2 nodes) */
		//(#p.nodes > 1 or p.nodes.isEmpty)

		/*
		 * All consecutive pairs of nodes in a path are edges of the
		 * graph
		 */
		all i:srcIdxs[p] {
			/* There's an edge at index i */
			isEdge[p, i]

			/* Each edge of the path appears only once */
			all s:Node, d:Node-s {
				isEdgeAt[p, i, s, d] => no j:srcIdxs[p]-i | isEdgeAt[p, j, s, d]
			}
		}

		/* Different paths have a different sequence of nodes */
		all p_:Path | p.nodes = p_.nodes => p = p_
	}
}


/*
 * The important predicates of the problem at hand
 */

pred isEulerPath [p:Path] {
	/* For all s->d edges */
	all s:Node, d:s.edges |

	/* There's exactly one index of p */
	one i:srcIdxs[p] |

	/* Such that s and d are consecutive, in whatever order (s->d or d->s) */
	isEdgeAt[p, i, s, d]
}

pred isEulerCircuit [p:Path] {
	isEulerPath[p]
	p.nodes.first = p.nodes.last
}


/*
 * Examples
 */

pred example1 {
	some a:Node,
	     b:Node-a,
	     c:Node-(a + b),
	     d:Node-(a + b + c),
	     e:Node-(a + b + c + d)
	{
		a->b in edges
		b->c in edges
		c->d in edges
		d->e in edges
		e->a in edges

		some p:Path | isEulerCircuit[p]
	}
}

pred example2 [n:Int] {
	#Node >= n
	some p:Path | isEulerCircuit[p]
}

run {
	example1
} for 6 but 1 Path
