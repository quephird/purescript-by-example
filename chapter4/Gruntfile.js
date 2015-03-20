module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],

    clean: ["tmp", "output"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],

    psc: {
      lib: {
        src: ["<%=libFiles%>"]
      },
      tests: {
        options: {
          module: ["Main"],
          main: true
        },
        src: ["tests/Tests.purs", "<%=libFiles%>"],
        dest: "tmp/tests.js"
      }
    },

    copy: [
      {
        expand: true,
        cwd: "output",
        src: ["**"],
        dest: "tmp/node_modules/"
      }, {
        src: ["js/index.js"],
        dest: "tmp/index.js"
      }
    ],

    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-copy");
  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("test", ["clean", "psc:tests", "execute:tests"]);
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make", "test"]);
};
