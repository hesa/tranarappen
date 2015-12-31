describe('Coachassistant', function () {
    var clubNames = ['IK Nord', 'Bergsjö IF'];
    var addedNames = [];
    var clubsLink = element(by.css('a[ui-sref="clubs"]'));

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
});
