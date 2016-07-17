(function () {
    'use strict';

    var app = angular.module('coachassistant', ['com.2fdevs.videogular', 'jcs-autoValidate', 'ngSanitize', 'nya.bootstrap.select', 'ui.bootstrap', 'ui.router', 'ui.validate']),
        Coachassistant = ['$rootScope', function ($rootScope) {
            this.club = null;

            // Returns the club if set. Otherwise, fetches the club from the
            // session storage, if possible.
            this.getClub = function () {
                if (!this.club) {
                    if (sessionStorage.club) {
                        this.setClub(JSON.parse(sessionStorage.club));
                        return this.getClub();
                    }
                    // An else would provide better readability here, but that
                    // won't pass the JSLint check. The warning is
                    // "no-else-return".
                    return this.club;
                }
                // An else would provide better readability here, but that
                // won't pass the JSLint check. The warning is
                // "no-else-return".
                return this.club;
            };

            this.onClubSet = function (callback) {
                $rootScope.$on('coachassistant:clubSet', function (event, club) {
                    /*jslint unparam: true*/
                    callback(club);
                });
            };

            this.setClub = function (club) {
                this.club = club;
                sessionStorage.club = JSON.stringify(club);
                $rootScope.$broadcast('coachassistant:clubSet', club);
            };
        }],
        clubResolve = ['$q', 'coachassistant', function ($q, coachassistant) {
            var club = coachassistant.getClub(),
                deferred = $q.defer();

            if (club !== null) {
                deferred.resolve(club);
            } else {
                return deferred.reject();
            }

            return deferred.promise;
        }],
        membersResolve = ['$http', '$q', 'club', function ($http, $q, club) {
            var deferred = $q.defer();

            if (!club) {
                deferred.reject();
            } else {
                $http.get('/api/0.0.0/clubs/' + club.uuid + '/members').success(function (result) {
                    deferred.resolve(result.items);
                }).error(function (error) {
                    deferred.reject(error);
                });
            }

            return deferred.promise;
        }],
        teamsResolve = ['$http', '$q', 'club', function ($http, $q, club) {
            var deferred = $q.defer();

            if (!club) {
                deferred.reject();
            } else {
                $http.get('/api/0.0.0/clubs/' + club.uuid + '/teams').success(function (result) {
                    deferred.resolve(result.items);
                }).error(function (error) {
                    deferred.reject(error);
                });
            }

            return deferred.promise;
        }],
        trainingPhasesResolve = ['$http', '$q', 'club', function ($http, $q, club) {
            var deferred = $q.defer();

            if (!club) {
                deferred.reject();
            } else {
                $http.get('/api/0.0.0/clubs/' + club.uuid + '/training-phases').success(function (result) {
                    deferred.resolve(result.items);
                }).error(function (error) {
                    deferred.reject(error);
                });
            }

            return deferred.promise;
        }];

    app.service('coachassistant', Coachassistant);
    app.service('lambdatrade', LambdatradeCommon);

    app.controller('CoachassistantController', ['$scope', 'coachassistant', function ($scope, coachassistant) {
        $scope.coachassistant = {
            selectedClub: coachassistant.getClub()
        };

        coachassistant.onClubSet(function (club) {
            $scope.coachassistant.selectedClub = club;
        });
    }]);

    app.config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
        $urlRouterProvider.otherwise('/teams');

        $stateProvider.state('clubs', {
            controller: ['$http', '$scope', 'clubs', 'coachassistant', 'lambdatrade', function ($http, $scope, clubs, coachassistant, lambdatrade) {
                $scope.clubs = {
                    addClub: function (addClubFormController) {
                        $http.post('/api/0.0.0/clubs', { name: $scope.clubs.addClubModel.addClubName }).success(function () {
                            $http.get('/api/0.0.0/clubs').success(function (result) {
                                $scope.clubs.clubs = result.items;
                                addClubFormController.autoValidateFormOptions.resetForm();
                                delete $scope.clubs.addClubModel.addClubName;
                            });
                        }).error(function (error) {
                            console.log(error);
                        });
                    },
                    addClubModel: {},
                    clubs: clubs,
                    clubsChunks: [],
                    notUnique: function ($scope) {
                        var addClubName = $scope.addClubForm.addClubName.$viewValue,
                            i;

                        if (addClubName) {
                            for (i = 0; i < $scope.clubs.clubs.length; i = i + 1) {
                                if ($scope.clubs.clubs[i].name.toLowerCase() === addClubName.toLowerCase()) {
                                    return false;
                                }
                            }
                        }

                        return true;
                    },
                    selectedClub: coachassistant.getClub(),
                    setClub: function (club) {
                        coachassistant.setClub(club);
                    }
                };

                coachassistant.onClubSet(function (club) {
                    $scope.clubs.selectedClub = club;
                });

                $scope.$watch('clubs.clubs', function (clubs) {
                    $scope.clubs.clubsChunks = lambdatrade.arrayToChunks(clubs, 4);
                }, true);

                document.getElementById('addClubName').focus();
            }],
            resolve: {
                clubs: ['$q', '$http', function ($q, $http) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/clubs').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function (error) {
                        deferred.reject(error);
                    });

                    return deferred.promise;
                }]
            },
            templateUrl: 'templates/clubs.html',
            url: '/clubs'
        }).state('members', {
            controller: ['$http', '$uibModal', '$scope', 'club', 'coachassistant', 'members', 'teams', function ($http, $uibModal, $scope, club, coachassistant, members, teams) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/members').success(function (result) {
                        $scope.members.members = result.items;
                    });
                };

                $scope.members = {
                    addMember: function (addMemberFormController) {
                        $http.post('/api/0.0.0/clubs/' + $scope.members.selectedClub.uuid + '/members', { name: $scope.members.addMemberModel.addMemberName, teamUuid: $scope.members.addMemberModel.addMemberTeam ? $scope.members.addMemberModel.addMemberTeam.uuid : undefined }).success(function () {
                            $http.get('/api/0.0.0/clubs/' + $scope.members.selectedClub.uuid + '/members').success(function (result) {
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
                    selectedClub: club,
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
                            controller: ['$uibModalInstance', '$scope', 'coachassistant', 'teams', 'videos', function ($uibModalInstance, $scope, coachassistant, teams, videos) {
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
                                    selectedClub: coachassistant.getClub(),
                                    member: member,
                                    teams: teams,
                                    update: function () {
                                        $http.put('/api/0.0.0/clubs/' + $scope.member.selectedClub.uuid + '/members/' + $scope.member.member.uuid, { name: $scope.member.updateMemberModel.updateMemberName, teamUuid: $scope.member.updateMemberModel.updateMemberTeam.uuid }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateMemberModel: {
                                        updateMemberName: member.name,
                                        updateMemberTeam: hackGetTeam(member.teamUuid)
                                    },
                                    videos: videos
                                };

                                coachassistant.onClubSet(function () {
                                    $scope.member.selectedClub = club;
                                });

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                teams: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    if (!$scope.members.selectedClub) {
                                        deferred.reject();
                                    } else {
                                        $http.get('/api/0.0.0/clubs/' + $scope.members.selectedClub.uuid + '/teams').success(function (result) {
                                            deferred.resolve(result.items);
                                        }).error(function (error) {
                                            deferred.reject(error);
                                        });
                                    }

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    if (!$scope.members.selectedClub) {
                                        deferred.reject();
                                    } else {
                                        $http.get('/api/0.0.0/clubs/' + $scope.members.selectedClub.uuid + '/videos/member/' + member.uuid).success(function (result) {
                                            deferred.resolve(result.items);
                                        }).error(function (error) {
                                            deferred.reject(error);
                                        });
                                    }

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/member.html'
                        });
                    }
                };

                coachassistant.onClubSet(function (club) {
                    $scope.members.selectedClub = club;
                });

                document.getElementById('addMemberName').focus();
            }],
            resolve: {
                club: clubResolve,
                members: membersResolve,
                teams: teamsResolve
            },
            templateUrl: 'templates/members.html',
            url: '/members'
        }).state('teams', {
            controller: ['$http', '$uibModal', '$scope', 'club', 'coachassistant', 'teams', function ($http, $uibModal, $scope, club, coachassistant, teams) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/teams').success(function (result) {
                        $scope.teams.teams = result.items;
                    });
                };

                $scope.teams = {
                    addTeam: function (addTeamFormController) {
                        $http.post('/api/0.0.0/clubs/' + $scope.teams.selectedClub.uuid + '/teams', { name: $scope.teams.addTeamModel.addTeamName }).success(function () {
                            $http.get('/api/0.0.0/clubs/' + $scope.teams.selectedClub.uuid + '/teams').success(function (result) {
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
                    selectedClub: club,
                    teams: teams,
                    viewTeam: function (team) {
                        $uibModal.open({
                            backdrop: 'static',
                            controller: ['$uibModalInstance', '$scope', 'coachassistant', 'members', 'videos', function ($uibModalInstance, $scope, coachassistant, members, videos) {
                                $scope.team = {
                                    cancel: function () {
                                        $uibModalInstance.close();
                                    },
                                    members: members,
                                    selectedClub: coachassistant.getClub(),
                                    team: team,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/clubs/' + $scope.team.selectedClub.uuid + '/teams/' + $scope.team.team.uuid, { name: $scope.team.updateTeamModel.updateTeamName }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateTeamModel: {
                                        updateTeamName: team.name
                                    },
                                    videos: videos
                                };

                                coachassistant.onClubSet(function () {
                                    $scope.team.selectedClub = club;
                                });

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                members: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/clubs/' + $scope.teams.selectedClub.uuid + '/members').success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    if (!$scope.teams.selectedClub) {
                                        deferred.reject();
                                    } else {
                                        $http.get('/api/0.0.0/clubs/' + $scope.teams.selectedClub.uuid + '/videos/team/' + team.uuid).success(function (result) {
                                            deferred.resolve(result.items);
                                        }).error(function (error) {
                                            deferred.reject(error);
                                        });
                                    }

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/team.html'
                        });
                    }
                };

                coachassistant.onClubSet(function (club) {
                    $scope.teams.selectedClub = club;
                });

                document.getElementById('addTeamName').focus();
            }],
            resolve: {
                club: clubResolve,
                teams: teamsResolve
            },
            templateUrl: 'templates/teams.html',
            url: '/teams'
        }).state('trainingPhases', {
            // Semantically a copy of the teams state.
            controller: ['$http', '$uibModal', '$scope', 'club', 'coachassistant', 'trainingPhases', function ($http, $uibModal, $scope, club, coachassistant, trainingPhases) {
                var onModalClose = function () {
                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/training-phases').success(function (result) {
                        $scope.trainingPhases.trainingPhases = result.items;
                    });
                };

                $scope.trainingPhases = {
                    addTrainingPhase: function (addTrainingPhaseFormController) {
                        $http.post('/api/0.0.0/clubs/' + $scope.trainingPhases.selectedClub.uuid + '/training-phases', { name: $scope.trainingPhases.addTrainingPhaseModel.addTrainingPhaseName }).success(function () {
                            $http.get('/api/0.0.0/clubs/' + $scope.trainingPhases.selectedClub.uuid + '/training-phases').success(function (result) {
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
                    selectedClub: club,
                    trainingPhases: trainingPhases,
                    viewTrainingPhase: function (trainingPhase) {
                        $uibModal.open({
                            backdrop: 'static',
                            controller: ['$uibModalInstance', '$scope', 'coachassistant', 'members', 'videos', function ($uibModalInstance, $scope, coachassistant, members, videos) {
                                $scope.trainingPhase = {
                                    cancel: function () {
                                        $uibModalInstance.close();
                                    },
                                    members: members,
                                    selectedClub: coachassistant.getClub(),
                                    trainingPhase: trainingPhase,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/clubs/' + $scope.trainingPhase.selectedClub.uuid + '/training-phases/' + $scope.trainingPhase.trainingPhase.uuid, { name: $scope.trainingPhase.updateTrainingPhaseModel.updateTrainingPhaseName }).success(function () {
                                            $uibModalInstance.close();
                                        });
                                    },
                                    updateTrainingPhaseModel: {
                                        updateTrainingPhaseName: trainingPhase.name
                                    },
                                    videos: videos
                                };

                                coachassistant.onClubSet(function () {
                                    $scope.trainingPhase.selectedClub = club;
                                });

                                $scope.$on('modal.closing', function () {
                                    onModalClose();
                                });
                            }],
                            resolve: {
                                members: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    $http.get('/api/0.0.0/clubs/' + $scope.trainingPhases.selectedClub.uuid + '/members').success(function (result) {
                                        deferred.resolve(result.items);
                                    }).error(function (error) {
                                        deferred.reject(error);
                                    });

                                    return deferred.promise;
                                }],
                                videos: ['$q', '$http', function ($q, $http) {
                                    var deferred = $q.defer();

                                    if (!$scope.trainingPhases.selectedClub) {
                                        deferred.reject();
                                    } else {
                                        $http.get('/api/0.0.0/clubs/' + $scope.trainingPhases.selectedClub.uuid + '/videos/training-phase/' + trainingPhase.uuid).success(function (result) {
                                            deferred.resolve(result.items);
                                        }).error(function (error) {
                                            deferred.reject(error);
                                        });
                                    }

                                    return deferred.promise;
                                }]
                            },
                            size: 'sm',
                            templateUrl: '/templates/training-phase.html'
                        });
                    }
                };

                coachassistant.onClubSet(function (club) {
                    $scope.trainingPhases.selectedClub = club;
                });

                document.getElementById('addTrainingPhaseName').focus();
            }],
            resolve: {
                club: clubResolve,
                trainingPhases: trainingPhasesResolve
            },
            templateUrl: 'templates/training-phases.html',
            url: '/training-phases'
        }).state('video', {
            controller: ['$sce', '$scope', '$stateParams', 'club', function ($sce, $scope, $stateParams, club) {
                // Listen to changes to "club" if necessary.
                $scope.videogular = {
                    preload: 'none',
                    sources: [ { src: $sce.trustAsResourceUrl('/api/0.0.0/clubs/' + club.uuid + '/videos/uuid/' + $stateParams.uuid + '/download'), type: 'video/webm' } ],
                    theme: {
                        url: 'https://www.videogular.com/styles/themes/default/latest/videogular.css'
                    }
                };
            }],
            resolve: {
                club: clubResolve
            },
            templateUrl: 'templates/video.html',
            url: '/videos/:uuid'
        }).state('videos', {
            controller: ['$http', '$sce', '$scope', '$stateParams', 'club', 'members', 'teams', 'trainingPhases', 'videos', function ($http, $sce, $scope, $stateParams, club, members, teams, trainingPhases, videos) {
                $scope.videos = {
                    filterMode: 'all',
                    members: members,
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

                // Listen to changes to "club" if necessary.
                $scope.videogular = {
                    preload: 'none',
                    theme: {
                        url: 'https://www.videogular.com/styles/themes/default/latest/videogular.css'
                    }
                };

                $scope.$watch('videos.selectedVideo', function (video) {
                    if (video) {
                        $scope.videogular.sources = [ { src: $sce.trustAsResourceUrl('/api/0.0.0/clubs/' + club.uuid + '/videos/uuid/' + video.uuid + '/download'), type: 'video/webm' } ];
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
                            $http.get('/api/0.0.0/clubs/' + club.uuid + '/videos/non-instructional').success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'member' && $scope.videos.selectedMember && !$scope.videos.selectedTrainingPhase) { // By member
                            $http.get('/api/0.0.0/clubs/' + club.uuid + '/videos/member/' + $scope.videos.selectedMember.uuid).success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'member' && $scope.videos.selectedMember && $scope.videos.selectedTrainingPhase) { // By member and training phase
                            $http.post('/api/0.0.0/clubs/' + club.uuid + '/videos/member-and-training-phase?memberUuid=' + $scope.videos.selectedMember.uuid + '&trainingPhaseUuid=' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
                                $scope.videos.videos = result; // Abstraction leak
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'team' && $scope.videos.selectedTeam && !$scope.videos.selectedTrainingPhase) { // By team
                            $http.get('/api/0.0.0/clubs/' + club.uuid + '/videos/team/' + $scope.videos.selectedTeam.uuid).success(function (result) {
                                $scope.videos.videos = result.items;
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'team' && $scope.videos.selectedTeam && $scope.videos.selectedTrainingPhase) { // By team and training phase
                            $http.post('/api/0.0.0/clubs/' + club.uuid + '/videos/team-and-training-phase?teamUuid=' + $scope.videos.selectedTeam.uuid + '&trainingPhaseUuid=' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
                                $scope.videos.videos = result; // Abstraction leak
                                addNamesToVideos();
                            });
                        } else if ($scope.videos.filterMode === 'all' && $scope.videos.selectedTrainingPhase) { // By training phase
                            $http.get('/api/0.0.0/clubs/' + club.uuid + '/videos/training-phase/' + $scope.videos.selectedTrainingPhase.uuid).success(function (result) {
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
                club: clubResolve,
                members: membersResolve,
                teams: teamsResolve,
                trainingPhases: trainingPhasesResolve,
                videos: ['club', '$http', '$q', function (club, $http, $q) {
                    var deferred = $q.defer();

                    if (!club) {
                        deferred.reject();
                    }

                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/videos/non-instructional').success(function (result) {
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

    // <auth-service>
    initializeAuthService(app);
    // </auth-service>

    app.run(['$http', '$rootScope', '$state', '$window', 'bootstrap3ElementModifier', 'defaultErrorMessageResolver', function ($http, $rootScope, $state, $window, bootstrap3ElementModifier, defaultErrorMessageResolver) {
        bootstrap3ElementModifier.enableValidationStateIcons(true);
        defaultErrorMessageResolver.getErrorMessages().then(function (errorMessages) {
            errorMessages.notUniqueError = 'En entitet med det namnet finns redan';
        });
        $rootScope.$on('$stateChangeError', function (event) {
            event.preventDefault();
            $state.go('clubs');
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
