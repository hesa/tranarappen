module.exports = function (grunt) {
    'use strict';

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
                    dest: 'app',
                    expand: true,
                    src: 'background.jpg'
                }, {
                    cwd: 'bower_components/bootstrap-css-only',
                    dest: 'app',
                    expand: true,
                    src: 'fonts/*'
                }, {
                    cwd: 'bower_components/bootstrap-css-only/css',
                    dest: 'app',
                    expand: true,
                    src: 'bootstrap.css.map'
                }, {
                    cwd: 'source',
                    dest: 'app',
                    expand: true,
                    src: 'index.html'
                }, {
                    cwd: 'source',
                    dest: 'app',
                    expand: true,
                    src: 'style.css'
                }, {
                    cwd: 'source/templates',
                    dest: 'app/templates',
                    expand: true,
                    src: '*.html'
                }]
            }
        },
        jslint: {
            main: {
                directives: {
                    browser: true,
                    predef: ['angular', 'console', 'module']
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
