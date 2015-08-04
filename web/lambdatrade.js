/*
 * Commonality functionality written by Lambdatrade.
 *
 * Copyright © 2015, Lambdatrade AB.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

var LambdatradeCommon = function () {
    'use strict';

    this.arrayToChunks = function (array, length) {
        if (Object.prototype.toString.call(array) !== '[object Array]') {
            throw 'arrayToChunks: Not array';
        }
        if (length !== parseInt(length, 10)) {
            throw 'arrayToChunks: Not integer';
        }
        if (length < 1) {
            throw 'arrayToChunks: Not positive integer';
        }
        var chunks = [],
            clone = array.slice(0);
        while (clone.length > 0) {
            chunks.push(clone.splice(0, length));
        }
        return chunks;
    };
};
