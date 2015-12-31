describe('Coachassistant', function () {
    var clubNames = ['IK Nord', 'Bergsjö IF'];
    var addedNames = [];
    var clubsLink = element(by.css('a[ui-sref="clubs"]'));
    var teamsLink = element(by.css('a[ui-sref="teams"]'));
    var membersLink = element(by.css('a[ui-sref="members"]'));
    var trainingPhasesLink = element(by.css('a[ui-sref="trainingPhases"]'));

    browser.get('https://127.0.0.1/');

    it('should allow to create two clubs', function () {
        clubsLink.click();

        element.all(by.css('.list-group-item')).map(function (club) {
            return club.getText();
        }).then(function (clubTexts) {
            clubNames.forEach(function (clubName) {
                var modifier = 2;
                var modifiedName = clubName;

                while (true) {
                    if (clubTexts.indexOf(modifiedName) === -1) {
                        break;
                    } else {
                        modifiedName = clubName + modifier;
                        modifier++;
                    }
                }

                element(by.model('clubs.addClubModel.addClubName')).sendKeys(modifiedName);
                element(by.css('button[type="submit"]')).click();

                addedNames.push(modifiedName);
            });
        });

        element.all(by.css('.list-group-item')).map(function (club) {
            return club.getText();
        }).then(function (clubTexts) {
            addedNames.forEach(function (addedName) {
                expect(clubTexts.indexOf(addedName)).not.toEqual(-1);
            });
        });
    });

    it('should allow to select all clubs', function () {
        clubsLink.click();

        element.all(by.css('.list-group-item')).then(function (clubs) {
            clubs.forEach(function (club) {
                club.click().then(function () {
                    club.getText().then(function (clubText) {
                        element.all(by.css('a[ui-sref="clubs"] span')).then(function (elements) {
                            elements[0].getText().then(function (elementText) {
                                expect(elementText).toEqual('(' + clubText + ')');
                            });
                        });
                    });
                });
            });
        });
    });

    it('should allow to create a team', function () {
        teamsLink.click();

        teamName = 'P10';

        var modifiedName;

        element.all(by.repeater('team in teams.teams')).map(function (team) {
            return team.all(by.css('td:first-child')).first().getText();
        }).then(function (teamTexts) {
            var modifier = 2;
            modifiedName = teamName;

            while (true) {
                if (teamTexts.indexOf(modifiedName) === -1) {
                    break;
                } else {
                    modifiedName = teamName + '_' + modifier;
                    modifier++;
                }
            }

            element(by.model('teams.addTeamModel.addTeamName')).sendKeys(modifiedName);
            element(by.css('button[type="submit"]')).click();
        });

        element.all(by.repeater('team in teams.teams')).map(function (team) {
            return team.all(by.css('td:first-child')).first().getText();
        }).then(function (teamTexts) {
            expect(teamTexts.indexOf(modifiedName)).not.toEqual(-1);
        });
    });

    it('should allow to create a member (without a team)', function () {
        membersLink.click();

        memberName = 'Jon';

        var modifiedName;

        element.all(by.repeater('member in members.members')).map(function (member) {
            return member.all(by.css('td:first-child')).first().getText();
        }).then(function (memberTexts) {
            var modifier = 2;
            modifiedName = memberName;

            while (true) {
                if (memberTexts.indexOf(modifiedName) === -1) {
                    break;
                } else {
                    modifiedName = memberName + '_' + modifier;
                    modifier++;
                }
            }

            element(by.model('members.addMemberModel.addMemberName')).sendKeys(modifiedName);
            element(by.css('button[type="submit"]')).click();
        });

        element.all(by.repeater('member in members.members')).map(function (member) {
            return member.all(by.css('td:first-child')).first().getText();
        }).then(function (memberTexts) {
            expect(memberTexts.indexOf(modifiedName)).not.toEqual(-1);
        });
    });

    it('should allow to create a training phase', function () {
        trainingPhasesLink.click();

        trainingPhaseName = 'Armhävningar';

        var modifiedName;

        element.all(by.repeater('trainingPhase in trainingPhases.trainingPhases')).map(function (trainingPhase) {
            return trainingPhase.all(by.css('td:first-child')).first().getText();
        }).then(function (trainingPhaseTexts) {
            var modifier = 2;
            modifiedName = trainingPhaseName;

            while (true) {
                if (trainingPhaseTexts.indexOf(modifiedName) === -1) {
                    break;
                } else {
                    modifiedName = trainingPhaseName + '_' + modifier;
                    modifier++;
                }
            }

            element(by.model('trainingPhases.addTrainingPhaseModel.addTrainingPhaseName')).sendKeys(modifiedName);
            element(by.css('button[type="submit"]')).click();
        });

        element.all(by.repeater('trainingPhase in trainingPhases.trainingPhases')).map(function (trainingPhase) {
            return trainingPhase.all(by.css('td:first-child')).first().getText();
        }).then(function (trainingPhaseTexts) {
            expect(trainingPhaseTexts.indexOf(modifiedName)).not.toEqual(-1);
        });
    });
});
