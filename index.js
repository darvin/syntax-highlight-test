var request = require("request")
  , cheerio = require("cheerio")

var util =require("util");

module.exports.getLanguageSamples = function(callback) {
  request("http://softwaremaniacs.org/media/soft/highlight/test.html", function(err, res, body){
    if (err) throw err;
    var $ = cheerio.load(body);
    var table = $("#autotest");
    var result = {};
    table.children("tr").each(function(i, tr){
      var codeTag = $(tr).find("code");
      var code = codeTag.text();
      var name = codeTag.parent().parent().attr("class");
      var titleTag = $(tr).find("th");
      var title = titleTag['0'].children[0].data.replace(/\n/g, "");
      // console.error(util.inspect(titleTag, {  depth: 4 }));
      result[name] = {
          code:code
        , title:title
      };
    });
    callback(err, result);
  });

};