var apolloClient = require("apollo-client")
var apolloLinkWS = require('apollo-link-ws')
var apolloCacheInMemory = require("apollo-cache-inmemory")
var gql = require('graphql-tag')
var GRAPHQL_URI = 'localhost:8080/ws/graphql';

//require("material-components-web-elm/dist/material-components-web-elm.js"); // from aforemny/material-components-web-elm
//require("material-components-web-elm/dist/material-components-web-elm.css");

const getClient = () => {
    var wsLink = new apolloLinkWS.WebSocketLink({
        uri: `ws://${GRAPHQL_URI}`,
        options: {
            reconnect: true,
        }
    });
    var client = new apolloClient.ApolloClient({
        link: wsLink,
        cache: new apolloCacheInMemory.InMemoryCache({
            addTypename: true
        })
    });
    return client;
};

document.addEventListener("DOMContentLoaded", function() {
    var app = Elm.Main.init({node: document.getElementById('main')})
    var client = getClient();

     app.ports.joinGame.subscribe(function(data) {
       /* Initiate subscription request */
        client.subscribe({
           query: gql`${data}`,
           variables: {}
         }).subscribe({
           next(resp) {
           var resp2 = { data: resp.data[Object.keys(resp.data)[0]] }
             app.ports.receivedGameEvent.send(resp2);
           },
           error(err) {
             console.log(err);
           }
         });
     });
//
//      app.ports.createTestSubscription.subscribe(function(data) {
//        /* Initiate subscription request */
//         getClient().subscribe({
//            query: gql`${data}`,
//            variables: {}
//          }).subscribe({
//            next(resp) {
//              console.log(resp)
//              var data = resp['data'][Object.keys(resp['data'])[0]];
//              console.log(data)
//              app.ports.receivedTestSubscriptionData.send(data);
//            },
//            error(err) {
//              console.log(err);
//            }
//          });
//      });
})