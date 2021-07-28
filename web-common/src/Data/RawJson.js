/*eslint-env node*/
/*global exports gtag*/
'use strict';

exports._pretty = function (json) {
    return JSON.stringify(
        json,
        null,
        2
    );
};
