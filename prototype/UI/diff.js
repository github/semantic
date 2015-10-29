function diffFromJSON(json) {
	if (json.pure != null) {
		return new Diff({
			pure: new Patch(json.pure)
		});
	}
	if (json.roll != null) {
		return new Diff({
			roll: {
				extract: json.roll.extract,
				unwrap: new Syntax(json.roll.unwrap, function(x) {
					return diffFromJSON(x);
				})
			}
		});
	}
}

function Diff(object) {
	if (object.pure != null) {
		this.pure = object.pure;
	}
	if (object.roll != null) {
		this.roll = object.roll;
	}
	return this;
}

// forall a b. Diff a -> (a -> b) -> Diff b
Diff.prototype.map = function(transform) {
	if (this.pure != null) {
		return new Diff({
			pure: transform(this.pure)
		});
	}
	if (this.roll != null) {
		return new Diff({
			roll: {
				extract: this.roll.extract,
				unwrap: this.roll.unwrap.map(function(x) {
					return x.map(transform);
				})
			}
		});
	}
}

// forall a. Diff a -> (Syntax a -> a) -> a
Diff.prototype.cata = function(transform) {
	if (this.pure != null) {
		return this.pure;
	}
	if (this.roll != null) {
		return transform(this.roll.unwrap.map(function(diff) {
			return diff.cata(transform);
		}))
	}
}

function wrap(tagName, element) {
	if (element == null) {
		return null;
	}
	var node = document.createElement(tagName);
	node.appendChild(element);
	return node;
}

/// String -> Syntax a -> Range -> (a -> Range) -> (a -> DOM) -> DOM
function rangeAndSyntaxToDOM(source, syntax, range, getRange, recur) {
	recur = recur || function(term) {
		return rangeAndSyntaxToDOM(source, term.unwrap, term.range);
	}
	var element;
	if (syntax.leaf != null) {
		element = document.createElement("span");
		element.textContent = source.substr(range[0], range[1]);
	} else if (syntax.indexed != null || syntax.fixed != null) {
		var values = syntax.indexed || syntax.fixed;
		elementA = document.createElement("ul");
		var previous = range[0];
		for (i in values) {
			var child = values[i];
			if (child.pure == "") continue;
			var childRange = getRange(child);
			if (childRange.before != null) {
				var beforeRange = childRange.before;
				element.appendChild(document.createTextNode(source.substr(previous, beforeRange[0] - previous)));
				element.appendChild(wrap("li", recur(child)));
				previous = beforeRange[0] + beforeRange[1];
			}
			if (childRange.after != null) {
				var afterRange = childRange.before;
				element.appendChild(document.createTextNode(source.substr(previous, afterRange[0] - previous)));
				element.appendChild(wrap("td", recur(child)));
				previous = afterRange[0] + afterRange[1];
			}
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

/// Diff -> String -> DOM
function diffToDOM(diff, source) {
	if (diff.pure != null) {
		return diff.pure;
	}

	function getRange(diff) {
		if (diff.pure != null) {
			return { "before": diff.pure.before.range, "after": diff.pure.after.range };
		}
		if (diff.roll != null) {
			return diff.roll.extract;
		}
	}
	return rollToDOM(source, diff.roll, getRange, function(diff) {
		return diffToDOM(diff, source);
	})
}

function rollToDOM(source, roll, getRangeFun, diffToDOMFun) {
	var syntax = roll.unwrap
	var range = roll.extract

	var elementA;
	var elementB;
	if (syntax.leaf != null) {
		elementA = document.createElement("span");
		elementA.textContent = source.substr(range.before[0], range.before[1]);
		elementB = document.createElement("span");
		elementB.textContent = source.substr(range.after[0], range.after[1]);
	} else if (syntax.indexed != null || syntax.fixed != null) {
		var values = syntax.indexed || syntax.fixed;
		elementA = document.createElement("ul");
		elementB = document.createElement("ul");
		var previousBefore = range.before[0];
		var previousAfter = range.before[0];
		for (i in values) {
			var child = values[i];
			if (child.pure == "") continue;

			var childRange = getRangeFun(child);
			if (childRange.before != null) {
				var beforeRange = childRange.before;

				var beforeAfterChild = diffToDOMFun(child)
				elementA.appendChild(document.createTextNode(source.substr(previousBefore, beforeRange[0] - previousBefore)));
				elementA.appendChild(wrap("li", beforeAfterChild.before));

				previousBefore = beforeRange[0] + beforeRange[1];
			}
			if (childRange.after != null) {
				var afterRange = childRange.after;

				elementB.appendChild(document.createTextNode(source.substr(previousAfter, afterRange[0] - previousAfter)));
				elementB.appendChild(wrap("li", beforeAfterChild.after));

				previousAfter = beforeRange[0] + beforeRange[1];
			}
		}
		var beforeText = source.substr(previousBefore, range.before[0] + range.before[1] - previousBefore);
		var afterText = source.substr(previousAfter, range.after[0] + range.after[1] - previousAfter);

		elementA.appendChild(document.createTextNode(beforeText));
		elementB.appendChild(document.createTextNode(afterText));
	} else if (syntax.keyed != null) {

	}

	return { "before": elementA, "after": elementB }
}
