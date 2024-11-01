class Expr {
    constructor(type, ...args) {
        this.type = type;
        this.args = args;
    }
}

/**
 * lit := 
 * | Int
 * | Bool
 */
class Lit {
    constructor(type, value) {
        this.type = type;
        this.value = value;
    }
}

let varCounter = 1;

function varFresh() {
    return `t${varCounter++}`;
}

/**
 * type :=
 * | tvar
 * | tcon name
 * | tfun type -> type
 */
class Ty {
    constructor(type, ...args) {
        this.type = type;
        this.args = args;
    }
}

/**
 * Forall [vars] ty
 */
class Scheme {
    constructor(vars, ty) {
        this.vars = vars;
        this.ty = ty;
    }
}

/**
 * A mapping of variable to type
 * {
 *  var: var,
 *  type: type,
 * }
 */
class Assumption {
    constructor(_var, ty) {
        this.var = _var;
        this.ty = ty;
    }
}

/**
 * equality 
 *  type = type
 * explicit
 *  type = scheme
 * implicit
 *  type type monoset
 * 
 * The implicit instance constraint (τ1 ≤M τ2) expresses that
 * type τ1 should be an instance of the type scheme obtained
 * by generalizing type τ2 with the set of monomorphic
 * type variables M
 * 
 * The explicit instance constraint (τ ≤ σ) states that
 * type τ has to be a generic instance of the type scheme σ
 */
class Constr {
    constructor(type, ...args) {
        this.type = type;
        this.args = args;
    }
}

/**
 * subst -> type -> type
 */
function apply(s, ty) {
    switch (ty.type) {
        case 'TVar':
            return s.has(ty.args[0]) ? s.get(ty.args[0]) : ty;
        case 'TCon':
            return new Ty('TCon', ty.args[0], ty.args[1].map(t => apply(s, t)));
        case 'TFun':
            return new Ty('TFun', apply(s, ty.args[0]), apply(s, ty.args[1]));
        default:
            throw new Erra('Unknown type');
    }
}

function applyScheme(s, scheme) {
    if (scheme.vars.length === 0) {
        return new Scheme([], apply(s, scheme.ty));
    } else {
        const sPrime = scheme.vars.reduceRight((acc, _var) => {
            acc.delete(_var);
            return acc;
        }, new Map(s));
        return new Scheme(scheme.vars, apply(sPrime, scheme.ty));
    }
}

function applyConstr(s, constr) {
    switch (constr.type) {
        case 'Equality':
            return new Constr('Equality', apply(s, constr.args[0]), apply(s, constr.args[1]));
        case 'ImplInstance':
            const mPrime = constr.args[2].reduce((acc, _var) => {
                const ty = s.get(_var);
                if (ty) {
                    const vars = Array.from(freevars(apply(s, ty)));
                    return acc.concat(vars);
                } else {
                    return acc.concat(_var);
                }
            }, []);
            return new Constr('ImplInstance', apply(s, constr.args[0]), apply(s, constr.args[1]), mPrime);
        case 'ExplInstance':
            return new Constr('ExplInstance', apply(s, constr.args[0]), applyScheme(s, constr.args[1]));
        default:
            throw new Erra('Unknown constraint type');
    }
}

function applyConstrList(s, c) {
    return c.map(constr => applyConstr(s, constr));
}


/**
 * Composing substitution S1 and substitution S2 is
 * written as (S2 ◦ S1) and results in another substitution
 *
 * NOTE: Substitution composition is left biased
 * 
 * subst -> subst -> subst
 */
function compose(s1, s2) {
    const composed = new Map();
    for (let [key, value] of s2) {
        composed.set(key, apply(s1, value));
    }
    for (let [key, value] of s1) {
        if (!composed.has(key)) {
            composed.set(key, value);
        }
    }
    return composed;
}

class Erra extends Error {
    constructor(message) {
        super(message);
        this.name = 'Error';
    }
}



/**
 * Constraints collection
 *
 * The algorithm for gathering constraint and
 * assumptions is bottom - up, and the only
 * process which is top - down is the creation
 * of the monomorphic set M
 *
 * Every time a variable is used, a fresh type
 * variable β is generated and appended to the
 * assumption set, and there can be multiple
 * different assumption regarding the same
 * variable name
 *
 * Applications create a fresh type
 * variable β, merge and propagate the assumptions
 * and constraints collected by the subexpressions
 * while adding an equality constraint to ensure
 * that the type of the first expression matches
 * with the type of the second expression and β
 *
 * Lambda abstractions create a fresh type
 * variable β representing the bound variable
 * and add it to the monomorphic set M before
 * the collection of the subexpression
 *
 * An equality constraint is generated for each
 * type variable β in the assumption set that is
 * associated with the variable bound by the lambda,
 * and the assumptions concerning this variable
 * are removed from the set before being propagated
 *
 * Let expressions introduce polymorphism by
 * generating an implicit instance constraint,
 * using the monomorphic set M, for each type
 * variable β in the assumption set that is
 * associated with the variable bound by the let,
 * while removing the assumptions concerning this
 * variable from the set before being propagated
 *
 * Literals return their type without any
 * new constraint or assumption
 *
 * NOTE: Although the set of constrains is
 * not ordered, an implicit instance constraint
 * requires some constraints to be solved before
 * it becomes solvable
 *
 * NOTE: In this implementation the monomorphic set M
 * is propagated alongside the abstract syntax tree
 * traversal by the collect function, but it could be
 * also easily implemented as a separate function
 * 
 * monomorphic set -> expression -> [type, assumption_set, constraints]
 * 
 * A monomorphic set 
 */
function collect(m, expr) {
    switch (expr.type) {
        case 'Var':
            {
                /**
                 * Create a new fresh type variable for every variable
                 * Assume the type of variable is fresh type assumption {x: t0}
                 * return an empty constraint set
                 */
                const ty = new Ty('TVar', varFresh());
                return [ty, [new Assumption(expr.args[0], ty)], []];
            }
        case 'App':
            {
                const beta = varFresh();
                const [t1, a1, c1] = collect(m, expr.args[0]);
                const [t2, a2, c2] = collect(m, expr.args[1]);
                return [
                    new Ty('TVar', beta),
                    a1.concat(a2),
                    [
                        new Constr('Equality', t1, new Ty('TFun', t2, new Ty('TVar', beta)))
                    ].concat(c1).concat(c2)
                ];
            }
        case 'Abs':
            {
                /**
                 * params -> body
                 * 
                 * --- collecting constraints for params ---
                 * add the fresh variable to the monoset
                 */

                const betaAbs = varFresh();

                const [t, a, c] = collect(new Set(m).add(betaAbs), expr.args[1]);

                const [aPrime, cPrime] = a.reduce(([accA, accC], aPrime) => {
                    const { var: _var, ty: tPrime } = aPrime;
                    if (_var === expr.args[0]) {
                        return [accA, [
                            new Constr('Equality', tPrime, new Ty('TVar', betaAbs))
                        ].concat(accC)];
                    } else {
                        return [[new Assumption(_var, tPrime)].concat(accA), accC];
                    }
                }, [[], c]);

                console.log(aPrime, cPrime);

                return [
                    new Ty('TFun', new Ty('TVar', betaAbs), t),
                    aPrime,
                    cPrime
                ];
            }
        case 'Let':
            {
                const [t1, a1, c1] = collect(m, expr.args[1]);
                const [t2, a2, c2] = collect(m, expr.args[2]);
                const mPrime = Array.from(m);
                const [aPrime, cPrime] = a2.reduce(([accA, accC], aPrime) => {
                    const { var: _var, ty: tPrime } = aPrime;
                    if (_var === expr.args[0]) {
                        return [accA, [new Constr('ImplInstance', tPrime, t1, mPrime)].concat(accC)];
                    } else {
                        return [[new Assumption(_var, tPrime)].concat(accA), accC];
                    }
                }, [[], []]);
                return [t2, a1.concat(aPrime), c1.concat(c2).concat(cPrime)];
            }
        case 'Rec':
            {
                const tv = new Ty('TVar', varFresh());
                const [t1, a1, c1] = collect(m, new Expr('Abs', expr.args[0], expr.args[1]));
                const [t2, a2, c2] = collect(m, expr.args[2]);
                const mPrime = Array.from(m);
                const [aPrime, cPrime] = a2.reduce(([accA, accC], aPrime) => {
                    const { var: _var, ty: tPrime } = aPrime;
                    if (_var === expr.args[0]) {
                        return [accA, [new Constr('ImplInstance', tPrime, tv, mPrime)].concat(accC)];
                    } else {
                        return [[new Assumption(_var, tPrime)].concat(accA), accC];
                    }
                }, [[], []]);
                return [t2, a1.concat(aPrime), [new Constr('Equality', new Ty('TFun', tv, tv), t1)].concat(c1).concat(c2).concat(cPrime)];
            }
        case 'If':
            const [t1, a1, c1] = collect(m, expr.args[0]);
            const [t2, a2, c2] = collect(m, expr.args[1]);
            const [t3, a3, c3] = collect(m, expr.args[2]);
            return [t2, a1.concat(a2).concat(a3), [new Constr('Equality', t1, new Ty('TCon', 'Bool', []))].concat([new Constr('Equality', t2, t3)]).concat(c1).concat(c2).concat(c3)];
        case 'Tup':
            const [ts, a, c] = expr.args.reduce(([accT, accA, accC], e) => {
                const [tPrime, aPrime, cPrime] = collect(m, e);
                return [tPrime.concat(accT), accA.concat(aPrime), accC.concat(cPrime)];
            }, [[], [], []]);
            return [new Ty('TCon', ',', ts), a, c];
        case 'Lit':
            if (expr.args[0].type === 'Int') {
                return [new Ty('TCon', 'Int', []), [], []];
            } else if (expr.args[0].type === 'Bool') {
                return [new Ty('TCon', 'Bool', []), [], []];
            }
        default:
            throw new Erra(`Unknown expression type: ${expr.type}`);
    }
}

function freevars(ty) {
    switch (ty.type) {
        case 'TVar':
            return new Set([ty.args[0]]);
        case 'TCon':
            return ty.args[1].reduce((acc, t) => new Set([...acc, ...freevars(t)]), new Set());
        case 'TFun':
            return new Set([...freevars(ty.args[0]), ...freevars(ty.args[1])]);
        default:
            throw new Erra('Unknown type');
    }
}

function freevarsScheme(scheme) {
    if (scheme.vars.length === 0) {
        return freevars(scheme.ty);
    } else {
        const m = new Set(scheme.vars);
        return new Set([...freevars(scheme.ty)].filter(x => !m.has(x)));
    }
}

function activevars(constr) {
    switch (constr.type) {
        case 'Equality':
            return new Set([...freevars(constr.args[0]), ...freevars(constr.args[1])]);
        case 'ImplInstance':
            const mPrime = new Set(constr.args[2]);
            return new Set([...freevars(constr.args[0]), ...Array.from(mPrime).filter(x => freevars(constr.args[1]).has(x))]);
        case 'ExplInstance':
            return new Set([...freevars(constr.args[0]), ...freevarsScheme(constr.args[1])]);
        default:
            throw new Erra('Unknown constraint type');
    }
}

function activevarsList(c) {
    return c.reduce((acc, constr) => new Set([...acc, ...activevars(constr)]), new Set());
}

function instantiate(scheme) {
    if (scheme.vars.length === 0) {
        return scheme.ty;
    } else {
        const s = scheme.vars.reduce((acc, _var) => {
            acc.set(_var, new Ty('TVar', varFresh()));
            return acc;
        }, new Map());
        return apply(s, scheme.ty);
    }
}

function generalize(m, ty) {
    const mSet = new Set(m);
    const vars = Array.from(freevars(ty)).filter(x => !mSet.has(x));
    return new Scheme(vars, ty);
}

function mgu(t1, t2) {
    function bind(_var, ty) {
        if (ty.type === 'TVar' && ty.args[0] === _var) {
            return new Map();
        } else if (freevars(ty).has(_var)) {
            throw new Erra(`OccurFail(${_var}, ${ty})`);
        } else {
            return new Map([[_var, ty]]);
        }
    }

    if (t1.type === 'TVar') {
        return bind(t1.args[0], t2);
    } else if (t2.type === 'TVar') {
        return bind(t2.args[0], t1);
    } else if (t1.type === 'TCon' && t2.type === 'TCon' && t1.args[0] === t2.args[0]) {
        if (t1.args[1].length !== t2.args[1].length) {
            throw new Erra(`UnifyFail(${t1}, ${t2})`);
        } else {
            return t1.args[1].reduce((acc, t1, i) => compose(mgu(apply(acc, t1), apply(acc, t2.args[1][i])), acc), new Map());
        }
    } else if (t1.type === 'TFun' && t2.type === 'TFun') {
        const s1 = mgu(t1.args[0], t2.args[0]);
        const s2 = mgu(apply(s1, t1.args[1]), apply(s1, t2.args[1]));
        return compose(s2, s1);
    } else {
        throw new Erra(`UnifyFail(${JSON.stringify(t1, null, 2)}, ${JSON.stringify(t2, null, 2)})`);
    }
}

function solve(c) {
    if (c.length === 0) {
        return new Map();
    } else if (c[0].type === 'Equality') {
        const s = mgu(c[0].args[0], c[0].args[1]);
        return compose(solve(applyConstrList(s, c.slice(1))), s);
    } else if (c[0].type === 'ImplInstance') {
        const ftv = new Set([...freevars(c[0].args[1])].filter(x => !new Set(c[0].args[2]).has(x)));
        if (new Set([...ftv].filter(x => activevarsList(c.slice(1)).has(x))).size === 0) {
            return solve([new Constr('ExplInstance', c[0].args[0], generalize(c[0].args[2], c[0].args[1]))].concat(c.slice(1)));
        } else {
            return solve(c.slice(1).concat([c[0]]));
        }
    } else if (c[0].type === 'ExplInstance') {
        return solve([new Constr('Equality', c[0].args[0], instantiate(c[0].args[1]))].concat(c.slice(1)));
    } else {
        throw new Erra('Unknown constraint type');
    }
}

function envAux(gamma, a) {
    if (a.length === 0) {
        return [];
    } else {
        return a.reduce((acc, aPrime) => {
            const { var: _var, ty: ty } = aPrime;
            if (gamma.has(_var)) {
                return acc.concat(new Constr('ExplInstance', ty, gamma.get(_var)));
            } else {
                throw new Erra(`UnboundVar(${_var
                    })`);
            }
        }, []);
    }
}

function infer(gamma, e) {
    const [ty, a, c] = collect(new Set(), e);
    const cPrime = envAux(gamma, a);
    const s = solve(c.concat(cPrime));
    return [s, apply(s, ty)];
}

function main() {
    // Helper function to print test results
    function runTest(name, expr, expectedType) {
        const [subst, inferredType] = infer(new Map(), expr);
        console.log(`Test: ${name}`);
        console.log(`Inferred type: `, JSON.stringify(inferredType, null, 2));
        console.log(`Expected type: `, expectedType);
        console.log('---');
    }

    const letExpr = new Expr('Abs', 'x', new Expr('Var', 'x'));

    runTest('Let binding', letExpr, 'Int');
}

main()
