function wrap(tagName, element) {
	if (element == null) {
		return null;
	}
	var node = document.createElement(tagName);
	node.appendChild(element);
	return node;
}

/// Range -> Syntax a -> String -> (a -> Range) -> (a -> DOM) -> DOM
function rangeAndSyntaxToDOM(range, syntax, source, getRange, recur) {
	recur = recur || function(term) {
		return rangeAndSyntaxToDOM(term.range, term.unwrap, source);
	}
	var element;
	if (syntax.leaf != null) {
		element = document.createElement("span");
		element.textContent = source.substr(range[0], range[1]);
	} else if (syntax.indexed != null || syntax.fixed != null) {
		var values = syntax.indexed || syntax.fixed;
		element = document.createElement("ul");
		var previous = range[0];
		for (i in values) {
			var child = values[i];
			if (child.pure == "") continue;
			var childRange = getRange(child);
			element.appendChild(document.createTextNode(source.substr(previous, childRange[0] - previous)));
			element.appendChild(wrap("li", recur(child)));
			previous = childRange[0] + childRange[1];
		}
		element.appendChild(document.createTextNode(source.substr(previous, range[0] + range[1] - previous)));
	} else if (syntax.keyed != null) {
		element = document.createElement("dl");
		var values = [];
		for (k in syntax.keyed.values) {
			if (syntax.keyed.values[k].pure == "") continue;
			values.push([k, syntax.keyed.values[k]]);
		}
		values.sort(function(a, b) {
			if (getRange(a[1])[0] < getRange(b[1])[0]) {
				return -1;
			} else if (getRange(a[1])[0] > getRange(b[1])[0]) {
				return 1;
			}
			return 0;
		});

		var previous = range[0];
		for (i in values) {
			var pair = values[i];
			var key = pair[0];
			var child = pair[1];
			var childRange = getRange(child);
			element.appendChild(document.createTextNode(source.substr(previous, childRange[0] - previous)));
			element.appendChild(wrap("dt", document.createTextNode(key)));
			element.appendChild(wrap("dd", recur(child)));
			previous = childRange[0] + childRange[1];
		}
		element.appendChild(document.createTextNode(source.substr(previous, range[0] + range[1] - previous)));
	}
	element["data-range"] = range;
	return element;
}
