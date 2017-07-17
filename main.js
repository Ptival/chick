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
  },
  shim: {
    "codemirror/mode/mllike/mllike": { deps: ["codemirror"] },
  },
  waitSeconds: 0,
})

requirejs([
    "codemirror",
    "codemirror/mode/mllike/mllike",
], (CodeMirror, _) => {
    // console.log(`Binding CodeMirror`)
    window.CodeMirror = CodeMirror
    require([
        "output/app",
    ])
})
