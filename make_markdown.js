var fs = require('fs')
  , path = require('path')
var getLanguageSamples = require('./').getLanguageSamples;

var OUTPUT_MARKDOWN = "readme.md";

getLanguageSamples(function(err, languageSamples){
  result = "# Syntax highlighting\n";
  for (var lang in languageSamples){
    result += "\n\n##"+languageSamples[lang].title+"\n\n```"+lang+"\n";
    result += languageSamples[lang].code;
    result += "\n```\n";
  }
  fs.writeFileSync(path.join(__dirname, OUTPUT_MARKDOWN), result);
  console.log(OUTPUT_MARKDOWN + " is written.");
});