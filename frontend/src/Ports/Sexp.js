"use strict"

function toSexp(atom, list, e) {
    function go(e) {
        if (Array.isArray(e)) {
            return list(e.map(go))
        } else {
            return atom(e)
        }
    }
    return go(e)
}

exports._sexp = function(nothing, just, atom, list, s) {
    const res = window.sexp(s)
    if (res instanceof Error) {
        return nothing
    }
    return just(toSexp(atom, list, res))
}

exports._jsonParseArray = function(nothing, just, s) {
    try {
        const res = JSON.parse(s)
        if (Array.isArray(res)) {
            return just(res)
        }
        return nothing
    }
    catch (e) { return nothing }
}
