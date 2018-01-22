const expect = require("must");

import {fetchPets} from '../src/ajaxcalls';
import {Pet} from '../src/types';

const pact = require('pact');


describe('ajaxcalls Pact Contract', function() {

    let url = 'http://localhost';
    const port = 9000;

    // (1) Create the Pact object to represent your provider
    const provider = pact({
        consumer: 'fetchPets',
        provider: 'GET_pets',
        port: port,
        spec: 1,
        pactfileWriteMode: 'overwrite'
    });

    before((done) => {
        // (2) Start the mock server
        provider.setup()
        // (3) add interactions to the Mock Server, as many as required
            .then(() => {
                provider.addInteraction({
                    // The 'state' field specifies a "Provider State"
                    state: 'i have a list of pets',
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
                        body: { pets: [
                            {
                                petName: 'Fifi',
                                petType: 'Dog'
                            },
                            {
                                petName: 'Minki',
                                petType: 'Cat'
                            }
                        ] }
                    }
                });
            })
            .then(() => done());
    });

    afterEach(() => provider.verify());

    after(() => provider.finalize());


    it('checks the fetching of the pets', function(done) {

        fetchPets((pets: Pet[]) => {
            expect(pets).to.have.length(2);
            expect(pets[0].name).to.eql('Fifi');
            expect(pets[0].species).to.eql('Dog');
            expect(pets[1].name).to.eql('Minki');
            expect(pets[1].species).to.eql('Cat');

            done();
        }, url + ':' + port);
    });

});
