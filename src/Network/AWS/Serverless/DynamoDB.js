'use strict';

// module Network.AWS.Serverless.DynamoDB

const AWS = require('aws-sdk');
AWS.config.setPromisesDependency(require('bluebird'));

exports._documentClient = params => {
    return new AWS.DynamoDB.DocumentClient(params);
};

exports._getItem = async function (documentClient, params) {
    return documentClient.get(params).promise()
        .then(r => r.Item);
};

exports._putItem = async function (documentClient, params) {
    return documentClient.put(params).promise();
};
