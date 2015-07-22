module.exports = function (grunt) {
    "use strict";

    grunt.loadNpmTasks('grunt-bower-concat');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-jslint');

    grunt.initConfig({
        bower_concat: {
            all: {
                cssDest: 'app/libs.css',
                dest: 'app/libs.js'
            }
        },
        concat: {
            script: {
                dest: 'app/script.js',
                src: ['source/script.js']
            }
        },
        copy: {
            main: {
                files: [{
                    cwd: 'source',
                    dest: 'app',
                    expand: true,
                    src: 'index.html'
                }]
            }
        },
        jslint: {
            main: {
                directives: {
                    browser: true,
                    predef: ['module']
                },
                src: ['Gruntfile.js', 'source/**/*.js']
            }
        },
        watch: {
            main: {
                files: ['**/*.css', '**/*.html', '**/*.js'],
                options: {
                    cwd: 'source',
                    spawn: false
                },
                tasks: ['dev']
            }
        }
    });

    grunt.registerTask('dev', ['copy', 'concat', 'watch']);
    grunt.registerTask('deps', ['bower_concat']);
    grunt.registerTask('live', ['jslint', 'copy', 'bower_concat', 'concat']);
};
