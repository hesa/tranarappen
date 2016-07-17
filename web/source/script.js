(function () {
    'use strict';

    var app = angular.module('coachassistant', ['com.2fdevs.videogular', 'jcs-autoValidate', 'ngSanitize', 'nya.bootstrap.select', 'ui.bootstrap', 'ui.router', 'ui.validate']),
        membersResolve = ['$http', '$q', function ($http, $q) {
            var deferred = $q.defer();

            $http.get('/api/0.0.0/members').success(function (result) {
                deferred.resolve(result.items);
            }).error(function (error) {
                deferred.reject(error);
            });

            return deferred.promise;
        }],
        teamsResolve = ['$http', '$q', function ($http, $q) {
            var deferred = $q.defer();

            $http.get('/api/0.0.0/teams').success(function (result) {
                deferred.resolve(result.items);
            }).error(function (error) {
                deferred.reject(error);
            });

            return deferred.promise;
        }],
        trainingPhasesResolve = ['$http', '$q', function ($http, $q) {
            var deferred = $q.defer();

            $http.get('/api/0.0.0/training-phases').success(function (result) {
                deferred.resolve(result.items);
            }).error(function (error) {
                deferred.reject(error);
            });

            return deferred.promise;
        }];

    app.service('lambdatrade', LambdatradeCommon);

    app.config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
        $urlRouterProvider.otherwise('/teams');

        $stateProvider.state('members', {
            controller: ['$http', '$uibModal', '$scope', 'members', 'teams', function ($http, $uibModal, $scope, members, teams) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/members').success(function (result) {
                        $scope.members.members = result.items;
                    });
                };

                $scope.members = {
                    addMember: function (addMemberFormController) {
                        $http.post('/api/0.0.0/members', { name: $scope.members.addMemberModel.addMemberName, teamUuid: $scope.members.addMemberModel.addMemberTeam ? $scope.members.addMemberModel.addMemberTeam.uuid : undefined }).success(function () {
                            $http.get('/api/0.0.0/members').success(function (result) {
                                $scope.members.members = result.items;
                                addMemberFormController.autoValidateFormOptions.resetForm();
                                delete $scope.members.addMemberModel.addMemberName;
                                delete $scope.members.addMemberModel.addMemberTeam;
                                document.getElementById('addMemberName').focus();
                            });
                        }).error(function (error) {
                            console.log(error);
                        });
                    },
                    addMemberModel: {},
                    members: members,
                    notUnique: function ($scope) {
                        var addMemberName = $scope.addMemberForm.addMemberName.$viewValue,
                            i;

                        if (addMemberName) {
                            for (i = 0; i < $scope.members.members.length; i = i + 1) {
                                if ($scope.members.members[i].name.toLowerCase() === addMemberName.toLowerCase()) {
                                    return false;
                                }
                            }
                        }

                        return true;
                    },
                    teams: teams,
                    viewMember: function (member) {
                        $uibModal.open({
                            backdrop: 'static',
                            controller: ['$uibModalInstance', '$scope', 'teams', 'videos', function ($uibModalInstance, $scope, teams, videos) {
                                var hackGetTeam = function (teamUuid) {
                                    var i;

                                    if (!teamUuid) {
                                        return;
                                    }

                                    for (i = 0; i < teams.length; i = i + 1) {
                                        if (teams[i].uuid === teamUuid) {
                                            return teams[i];
                                        }
                                    }
                                };

                                $scope.member = {
                                    cancel: function () {
                                        $uibModalInstance.close();
                                    },
                                    members: members,
                                    member: member,
                                    teams: teams,
                                    update: function () {
                                        $http.put('/api/0.0.0/members/' + $scope.member.member.uuid, { name: $scope.member.updateMemberModel.updateMemberName, teamUuid: $scope.member.updateMemberModel.updateMemberTeam.uuid }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateMemberModel: {
                                        updateMemberName: member.name,
                                        updateMemberTeam: hackGetTeam(member.teamUuid)
                                    },
                                    videos: videos
                                };

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                teams: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/teams').success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/videos/member/' + member.uuid).success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/member.html'
                        });
                    }
                };

                document.getElementById('addMemberName').focus();
            }],
            resolve: {
                members: membersResolve,
                teams: teamsResolve
            },
            templateUrl: 'templates/members.html',
            url: '/members'
        }).state('teams', {
            controller: ['$http', '$uibModal', '$scope', 'teams', function ($http, $uibModal, $scope, teams) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/teams').success(function (result) {
                        $scope.teams.teams = result.items;
                    });
                };

                $scope.teams = {
                    addTeam: function (addTeamFormController) {
                        $http.post('/api/0.0.0/teams', { name: $scope.teams.addTeamModel.addTeamName }).success(function () {
                            $http.get('/api/0.0.0/teams').success(function (result) {
                                $scope.teams.teams = result.items;
                                addTeamFormController.autoValidateFormOptions.resetForm();
                                delete $scope.teams.addTeamModel.addTeamName;
                                document.getElementById('addTeamName').focus();
                            });
                        }).error(function (error) {
                            console.log(error);
                        });
                    },
                    addTeamModel: {},
                    notUnique: function ($scope) {
                        var addTeamName = $scope.addTeamForm.addTeamName.$viewValue,
                            i;

                        if (addTeamName) {
                            for (i = 0; i < $scope.teams.teams.length; i = i + 1) {
                                if ($scope.teams.teams[i].name.toLowerCase() === addTeamName.toLowerCase()) {
                                    return false;
                                }
                            }
                        }

                        return true;
                    },
                    teams: teams,
                    viewTeam: function (team) {
                        $uibModal.open({
                            backdrop: 'static',
                            controller: ['$uibModalInstance', '$scope', 'members', 'videos', function ($uibModalInstance, $scope, members, videos) {
                                $scope.team = {
                                    cancel: function () {
                                        $uibModalInstance.close();
                                    },
                                    members: members,
                                    team: team,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/teams/' + $scope.team.team.uuid, { name: $scope.team.updateTeamModel.updateTeamName }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateTeamModel: {
                                        updateTeamName: team.name
                                    },
                                    videos: videos
                                };

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                members: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/members').success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/videos/team/' + team.uuid).success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/team.html'
                        });
                    }
                };

                document.getElementById('addTeamName').focus();
            }],
            resolve: {
                teams: teamsResolve
            },
            templateUrl: 'templates/teams.html',
            url: '/teams'
        }).state('trainingPhases', {
            // Semantically a copy of the teams state.
            controller: ['$http', '$uibModal', '$scope', 'trainingPhases', function ($http, $uibModal, $scope, trainingPhases) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/training-phases').success(function (result) {
                        $scope.trainingPhases.trainingPhases = result.items;
                    });
                };

                $scope.trainingPhases = {
                    addTrainingPhase: function (addTrainingPhaseFormController) {
                        $http.post('/api/0.0.0/training-phases', { name: $scope.trainingPhases.addTrainingPhaseModel.addTrainingPhaseName }).success(function () {
                            $http.get('/api/0.0.0/training-phases').success(function (result) {
                                $scope.trainingPhases.trainingPhases = result.items;
                                addTrainingPhaseFormController.autoValidateFormOptions.resetForm();
                                delete $scope.trainingPhases.addTrainingPhaseModel.addTrainingPhaseName;
                                document.getElementById('addTrainingPhaseName').focus();
                            });
                        }).error(function (error) {
                            console.log(error);
                        });
                    },
                    addTrainingPhaseModel: {},
                    notUnique: function ($scope) {
                        var addTrainingPhaseName = $scope.addTrainingPhaseForm.addTrainingPhaseName.$viewValue,
                            i;

                        if (addTrainingPhaseName) {
                            for (i = 0; i < $scope.trainingPhases.trainingPhases.length; i = i + 1) {
                                if ($scope.trainingPhases.trainingPhases[i].name.toLowerCase() === addTrainingPhaseName.toLowerCase()) {
                                    return false;
                                }
                            }
                        }

                        return true;
                    },
                    trainingPhases: trainingPhases,
                    viewTrainingPhase: function (trainingPhase) {
                        $uibModal.open({
                            backdrop: 'static',
                            controller: ['$uibModalInstance', '$scope', 'members', 'videos', function ($uibModalInstance, $scope, members, videos) {
                                $scope.trainingPhase = {
                                    cancel: function () {
                                        $uibModalInstance.close();
                                    },
                                    members: members,
                                    trainingPhase: trainingPhase,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/training-phases/' + $scope.trainingPhase.trainingPhase.uuid, { name: $scope.trainingPhase.updateTrainingPhaseModel.updateTrainingPhaseName }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateTrainingPhaseModel: {
                                        updateTrainingPhaseName: trainingPhase.name
                                    },
                                    videos: videos
                                };

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                members: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/members').success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/videos/training-phase/' + trainingPhase.uuid).success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/training-phase.html'
                        });
                    }
                };

                document.getElementById('addTrainingPhaseName').focus();
            }],
            resolve: {
                trainingPhases: trainingPhasesResolve
            },
            templateUrl: 'templates/training-phases.html',
            url: '/training-phases'
        }).state('video', {
            controller: ['$http', '$sce', '$scope', '$stateParams', function ($http, $sce, $scope, $stateParams) {
                $scope.videogular = {
                    preload: 'none',
                    sources: [ { src: $sce.trustAsResourceUrl('/api/0.0.0/videos/uuid/' + $stateParams.uuid + '/download'), type: 'video/webm' } ],
                    theme: {
                        url: 'https://www.videogular.com/styles/themes/default/latest/videogular.css'
                    }
                };

                $scope.removeVideo = function () {
                    $http.delete('/api/0.0.0/videos/uuid/' + $stateParams.uuid).success(function (result) {
                        window.history.back();
                    });
                };
            }],
            templateUrl: 'templates/video.html',
            url: '/videos/:uuid'
        }).state('videos', {
            controller: ['$http', '$sce', '$scope', '$stateParams', 'members', 'teams', 'trainingPhases', 'videos', function ($http, $sce, $scope, $stateParams, members, teams, trainingPhases, videos) {
                $scope.videos = {
                    filterMode: 'all',
                    members: members,
                    remove: function (uuid) {
                        $http.delete('/api/0.0.0/videos/uuid/' + uuid).success(function (result) {
                            refreshVideos(true, false); // Diff newValue and oldValue to force refresh
                        });
                    },
                    teams: teams,
                    trainingPhases: trainingPhases,
                    videos: videos
                };

                $scope.videos.selectedVideo = null;

                if (videos.length > 0) {
                    $scope.videos.selectedVideo = videos[0];
                }

                $scope.videos.selectedMember = null;
                $scope.videos.selectedTeam = null;
                $scope.videos.selectedTrainingPhase = null;

                $scope.videogular = {
                    preload: 'none',
                    theme: {
                        url: 'https://www.videogular.com/styles/themes/default/latest/videogular.css'
                    }
                };

                $scope.$watch('videos.selectedVideo', function (video) {
                    if (video) {
                        $scope.videogular.sources = [ { src: $sce.trustAsResourceUrl('/api/0.0.0/videos/uuid/' + video.uuid + '/download'), type: 'video/webm' } ];
                    }

                    return true;
                });

                var memberName = function (uuid) {
                    for (var i = 0; i < $scope.videos.members.length; i++) {
                        if ($scope.videos.members[i].uuid === uuid) {
                            return $scope.videos.members[i].name;
                        }
                    }

                    return 'N/A';
                };

                var addNamesToVideos = function () {
                    for (var i = 0; i < $scope.videos.videos.length; i++) {
                        $scope.videos.videos[i].memberName = memberName($scope.videos.videos[i].memberUuid);
                    }
                };

                // 1. Non-instructional
                // 2. By member
                // 3. By member and training phase
                // 4. By team
                // 5. By team and training phase
                // 6. By training phase
                // 7. Do nothing
                var refreshVideos = function (newValue, oldValue) {
                    if (newValue !== oldValue) {
                        if ($scope.videos.filterMode === 'all' && !$scope.videos.selectedTrainingPhase) { // Non-instructional
                            $http.get('/api/0.0.0/videos/non-instructional').success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'member' && $scope.videos.selectedMember && !$scope.videos.selectedTrainingPhase) { // By member
                            $http.get('/api/0.0.0/videos/member/' + $scope.videos.selectedMember.uuid).success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'member' && $scope.videos.selectedMember && $scope.videos.selectedTrainingPhase) { // By member and training phase
                            $http.post('/api/0.0.0/videos/member-and-training-phase?memberUuid=' + $scope.videos.selectedMember.uuid + '&trainingPhaseUuid=' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
                                $scope.videos.videos = result; // Abstraction leak
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'team' && $scope.videos.selectedTeam && !$scope.videos.selectedTrainingPhase) { // By team
                            $http.get('/api/0.0.0/videos/team/' + $scope.videos.selectedTeam.uuid).success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'team' && $scope.videos.selectedTeam && $scope.videos.selectedTrainingPhase) { // By team and training phase
                            $http.post('/api/0.0.0/videos/team-and-training-phase?teamUuid=' + $scope.videos.selectedTeam.uuid + '&trainingPhaseUuid=' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
                                $scope.videos.videos = result; // Abstraction leak
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'all' && $scope.videos.selectedTrainingPhase) { // By training phase
                            $http.get('/api/0.0.0/videos/training-phase/' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else {
                            addNamesToVideos();
                        }
                    }

                    return true;
                };

                addNamesToVideos();

                $scope.$watch('videos.filterMode', refreshVideos);
                $scope.$watch('videos.selectedMember', refreshVideos);
                $scope.$watch('videos.selectedTeam', refreshVideos);
                $scope.$watch('videos.selectedTrainingPhase', refreshVideos);
            }],
            resolve: {
                members: membersResolve,
                teams: teamsResolve,
                trainingPhases: trainingPhasesResolve,
                videos: ['$http', '$q', function ($http, $q) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/videos/non-instructional').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function () {
                        deferred.reject();
                    });

                    return deferred.promise;
                }]
            },
            templateUrl: 'templates/videos.html',
            url: '/videos'
        });
    }]);

    app.run(['$http', '$rootScope', '$state', '$window', 'bootstrap3ElementModifier', 'defaultErrorMessageResolver', function ($http, $rootScope, $state, $window, bootstrap3ElementModifier, defaultErrorMessageResolver) {
        $http({ url: '/api/user-info' }).success(function (data) {
            $http.defaults.headers.common['X-Instance'] = data.instances[0].id;
        });
        bootstrap3ElementModifier.enableValidationStateIcons(true);
        defaultErrorMessageResolver.getErrorMessages().then(function (errorMessages) {
            errorMessages.notUniqueError = 'En entitet med det namnet finns redan';
        });
        $rootScope.$on('$stateChangeError', function (event) {
            event.preventDefault();
            $state.go('teams');
        });
        $rootScope.$on('$viewContentLoaded', function () {
            $window.scrollTo(0, 0);
        });
        $rootScope.logout = function() {
            // <auth-service>
            logout($http);
            // </auth-service>
        };
    }]);
}());
