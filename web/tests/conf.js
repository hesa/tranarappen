exports.config = {
    capabilities: {
        browserName: 'firefox'
    },
    seleniumAddress: 'http://localhost:4444/wd/hub',
    specs: ['main-spec.js']
};
