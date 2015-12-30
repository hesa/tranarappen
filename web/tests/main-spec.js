// Add a dummy test that tries to add a static club, "IK Nord", and counts that
// the number of existing clubs is incremented by one.
describe('Coachassistant', function () {
    it('allow for adding of clubs', function () {
        browser.get('https://127.0.0.1/');

        var numberOfClubsBefore = element.all(by.css('.list-group-item')).count();

        element(by.model('clubs.addClubModel.addClubName')).sendKeys('IK Nord');
        element(by.css('[type="submit"]')).click();

        var numberOfClubsAfter = element.all(by.css('.list-group-item')).count();

        numberOfClubsBefore.then(function (value) {
            expect(numberOfClubsAfter).toEqual(value + 1);
        });
    });
});
