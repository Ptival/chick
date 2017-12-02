const nodePath = "./node_modules"

requirejs.config({
  packages: [
    {
      name: 'codemirror',
      location: `${nodePath}/codemirror`,
      main: 'lib/codemirror'
    }
  ],
  paths: {
    "s-expression": `${nodePath}/s-expression-amd/index`,
    // "codemirror": `${nodePath}/codemirror/lib/codemirror`,
    // "codemirror/mode/mllike/mllike": `${nodePath}/codemirror/mode/mllike/mllike`,
    // "codemirror/lib/codemirror": `${nodePath}/codemirror/lib/codemirror`,
  },
  shim: {
    "codemirror/mode/mllike/mllike": { deps: ["codemirror"] },
  },
  waitSeconds: 0,
})

requirejs([
    "codemirror",
    "codemirror/mode/mllike/mllike",
    "s-expression",
], (CodeMirror, _, sexp) => {
    // console.log(`Binding CodeMirror`)
    window.CodeMirror = CodeMirror
    window.sexp = sexp
    require([
        "output/app",
    ])
})
