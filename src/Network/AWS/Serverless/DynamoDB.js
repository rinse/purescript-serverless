'use strict';

// module Network.AWS.Serverless.DynamoDB

const AWS = require('aws-sdk');
AWS.config.setPromisesDependency(require('bluebird'));

exports._documentClient = params => {
    return new AWS.DynamoDB.DocumentClient(params);
};

exports._getItem = async function (documentClient, params) {
    return documentClient.get(params).promise()
};

exports._deleteItem = async function (documentClient, params) {
    return documentClient.delete(params).promise();
};

exports._putItem = async function (documentClient, params) {
    return documentClient.put(params).promise();
};

exports._queryItems = async function (documentClient, params) {
    return documentClient.query(params).promise();
};

exports._scanItems = async function (documentClient, params) {
    return documentClient.scan(params).promise();
};

exports._updateItem = async function (documentClient, params) {
    return documentClient.update(params).promise();
};
