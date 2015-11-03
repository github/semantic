function termFromJSON(json) {
	return new Term({
		extract: json.extract,
		unwrap: new Syntax(json.unwrap, function(x) {
			return termFromJSON(x);
		})
	});
}

function Term(object) {
	this.extract = object.extract;
	this.unwrap = object.unwrap;
	return this;
}

/// Term -> String -> DOM
function termToDOM(term, which, source) {
	return rangeAndSyntaxToDOM(term.range, term.unwrap, source, function(term) {
		return term.range;
	}, function(term) {
		return termToDOM(term, which, source);
	});
}
