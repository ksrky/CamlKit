type frag = {name: string; params: Syntax.id list; body: Syntax.exp}

type frags = frag list

val f : Core.Syntax.exp -> frags
