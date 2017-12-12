"use strict"

// Eff-ize a function, takes as input the function
function effizeF(f) {
    return function () {
        const args = arguments
        return function () {
            return f.apply(null, args)
        }
    }
}

// Eff-ize a method, takes as input the method name (as a string)
function effizeM(m) {
    return function () {
        const me = arguments[0]
        const args = Array.prototype.slice.call(arguments, 1)
        // console.log("Will call ", me, "'s method", m, " with arguments ", args)
        return function () {
            // console.log("About to call ", me, "'s method", m, " with arguments ", args)
            return me[m].apply(me, args)
        }
    }
}

// EffFn-ize a method, takes as input the method name (as a string)
function effFnM(m) {
    return function () {
        const me = arguments[0]
        const args = Array.prototype.slice.call(arguments, 1)
        return me[m].apply(me, args)
    }
}

function onFn(eventType, kArgs) {
    return function(self, callback) {
        return function() {
            return self.on(eventType, function() {
                if (kArgs.length != arguments.length) {
                    throw "FFI problem: for " + eventType + ", wrong kArgs length"
                }
                const args = {}
                for (var i in kArgs) {
                    args[kArgs[i]] = arguments[i]
                }
                callback(args)()
            })
        }
    }
}

function onEffFn(eventType, kArgs) {
    return function(self, callback) {
        return self.on(eventType, function() {
            if (kArgs.length != arguments.length) {
                throw "FFI problem: for " + eventType + ", wrong kArgs length"
            }
            const args = {}
            for (var i in kArgs) {
                args[kArgs[i]] = arguments[i]
            }
            // console.log(self)
            callback(args)()
        })
    }
}

exports._onCodeMirrorChange         = onEffFn("change",         ["instance", "changeObj"])
exports._onCodeMirrorCursorActivity = onEffFn("cursorActivity", ["instance"])

exports._codeMirror = CodeMirror

exports._addKeyMap       = effFnM("addKeyMap")
exports._clearTextMarker = effFnM("clear")
exports._getDoc          = effFnM("getDoc")
exports._getCursor       = effFnM("getCursor")
exports._getRange        = effFnM("getRange")
exports._getValue        = effFnM("getValue")
exports._hasFocus        = effFnM("hasFocus")
exports._lineCount       = effFnM("lineCount")
exports._markText        = effFnM("markText")
exports._scrollIntoView  = effFnM("scrollIntoView")
exports._setCursor       = effFnM("setCursor")
exports._setSize         = effFnM("setSize")
exports._setValue        = effFnM("setValue")
