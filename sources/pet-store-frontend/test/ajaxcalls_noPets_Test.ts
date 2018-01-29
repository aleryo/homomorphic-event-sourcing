const expect = require('must');

import {fetchPets} from '../src-v1/ajaxcalls';
import {Pet} from '../src-v1/types';

const pact = require('pact');


describe('ajaxcalls Pact Contract', function() {

    let url = 'http://localhost';
    const port = 9001;

    // (1) Create the Pact object to represent your provider
    const provider = pact({
        consumer: 'fetchPets_noPets',
        provider: 'GET_pets',
        port: port,
        spec: 1,
        logLevel: "error",
        pactfileWriteMode: 'overwrite'
    });

    describe('with no pets in the server', function() {
        before((done) => {
            // (2) Start the mock server
            provider.setup()
            // (3) add interactions to the Mock Server, as many as required
                .then(() => {
                    provider.addInteraction({
                        // The 'state' field specifies a "Provider State"
                        state: 'i have no pets',
                        uponReceiving: 'a request for all pets',
                        withRequest: {
                            method: 'GET',
                            path: '/pets',
                            headers: {
                                'Accept': 'application/json'
                            }
                        },
                        willRespondWith: {
                            status: 200,
                            headers: {
                                'Content-Type': 'application/json'
                            },
                            body: {
                                tag: 'Pets',
                                pets: []
                            }
                        }
                    });
                })
                .then(() => done());
        });

        afterEach(() => provider.verify());

        after(() => provider.finalize());

        it('checks the fetching of no pets', function(done) {

            fetchPets((pets: Pet[]) => {
                expect(pets).to.have.length(0);
                done();
            }, url + ':' + port);
        });
    });
});
