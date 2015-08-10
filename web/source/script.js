(function () {
    'use strict';

    var app = angular.module('coachassistant', ['jcs-autoValidate', 'nya.bootstrap.select', 'ui.bootstrap', 'ui.router', 'ui.validate']),
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
            controller: ['$http', '$modal', '$scope', 'club', 'coachassistant', 'members', 'teams', function ($http, $modal, $scope, club, coachassistant, members, teams) {
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
                        $modal.open({
                            backdrop: 'static',
                            controller: ['$modalInstance', '$scope', 'coachassistant', 'teams', function ($modalInstance, $scope, coachassistant, teams) {
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
                                        $modalInstance.close();
                                    },
                                    members: members,
                                    selectedClub: coachassistant.getClub(),
                                    member: member,
                                    teams: teams,
                                    update: function () {
                                        $http.put('/api/0.0.0/clubs/' + $scope.member.selectedClub.uuid + '/members/' + $scope.member.member.uuid, { name: $scope.member.updateMemberModel.updateMemberName, teamUuid: $scope.member.updateMemberModel.updateMemberTeam.uuid }).success(function () {
                                            $modalInstance.close();
                                        });
                                    },
                                    updateMemberModel: {
                                        updateMemberName: member.name,
                                        updateMemberTeam: hackGetTeam(member.teamUuid)
                                    }
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
                members: ['$q', '$http', 'club', function ($q, $http, club) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/members').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function (error) {
                        deferred.reject(error);
                    });

                    return deferred.promise;
                }],
                teams: ['$q', '$http', 'club', function ($q, $http, club) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/teams').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function (error) {
                        deferred.reject(error);
                    });

                    return deferred.promise;
                }]
            },
            templateUrl: 'templates/members.html',
            url: '/members'
        }).state('teams', {
            controller: ['$http', '$modal', '$scope', 'club', 'coachassistant', 'teams', function ($http, $modal, $scope, club, coachassistant, teams) {
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
                        $modal.open({
                            backdrop: 'static',
                            controller: ['$modalInstance', '$scope', 'coachassistant', 'members', function ($modalInstance, $scope, coachassistant, members) {
                                $scope.team = {
                                    cancel: function () {
                                        $modalInstance.close();
                                    },
                                    members: members,
                                    selectedClub: coachassistant.getClub(),
                                    team: team,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/clubs/' + $scope.team.selectedClub.uuid + '/teams/' + $scope.team.team.uuid, { name: $scope.team.updateTeamModel.updateTeamName }).success(function () {
                                            $modalInstance.close();
                                        });
                                    },
                                    updateTeamModel: {
                                        updateTeamName: team.name
                                    }
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
                teams: ['$q', '$http', 'club', function ($q, $http, club) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/teams').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function (error) {
                        deferred.reject(error);
                    });

                    return deferred.promise;
                }]
            },
            templateUrl: 'templates/teams.html',
            url: '/teams'
        }).state('trainingPhases', {
            // Semantically a copy of the teams state.
            controller: ['$http', '$modal', '$scope', 'club', 'coachassistant', 'trainingPhases', function ($http, $modal, $scope, club, coachassistant, trainingPhases) {
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
                        $modal.open({
                            backdrop: 'static',
                            controller: ['$modalInstance', '$scope', 'coachassistant', 'members', function ($modalInstance, $scope, coachassistant, members) {
                                $scope.trainingPhase = {
                                    cancel: function () {
                                        $modalInstance.close();
                                    },
                                    members: members,
                                    selectedClub: coachassistant.getClub(),
                                    trainingPhase: trainingPhase,
                                    update: function () {
                                        console.log();
                                        console.log();
                                        $http.put('/api/0.0.0/clubs/' + $scope.trainingPhase.selectedClub.uuid + '/training-phases/' + $scope.trainingPhase.trainingPhase.uuid, { name: $scope.trainingPhase.updateTrainingPhaseModel.updateTrainingPhaseName }).success(function () {
                                            $modalInstance.close();
                                        });
                                    },
                                    updateTrainingPhaseModel: {
                                        updateTrainingPhaseName: trainingPhase.name
                                    }
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
                trainingPhases: ['$q', '$http', 'club', function ($q, $http, club) {
                    var deferred = $q.defer();

                    $http.get('/api/0.0.0/clubs/' + club.uuid + '/training-phases').success(function (result) {
                        deferred.resolve(result.items);
                    }).error(function (error) {
                        deferred.reject(error);
                    });

                    return deferred.promise;
                }]
            },
            templateUrl: 'templates/training-phases.html',
            url: '/training-phases'
        });
    }]);

    app.run(['$rootScope', '$state', '$window', 'bootstrap3ElementModifier', 'defaultErrorMessageResolver', function ($rootScope, $state, $window, bootstrap3ElementModifier, defaultErrorMessageResolver) {
        bootstrap3ElementModifier.enableValidationStateIcons(true);
        defaultErrorMessageResolver.getErrorMessages().then(function (errorMessages) {
            errorMessages.notUniqueError = 'An entity with this name already exists';
        });
        $rootScope.$on('$stateChangeError', function (event) {
            event.preventDefault();
            $state.go('clubs');
        });
        $rootScope.$on('$viewContentLoaded', function () {
            $window.scrollTo(0, 0);
        });
    }]);
}());