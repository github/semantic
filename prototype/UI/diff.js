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
function termToDOM(source, syntax, extract, getRange, recur) {
	recur = recur || function(term) {
		return termToDOM(source, term.unwrap, term.extract, getRange);
	}
	var categories = extract.categories;
	var range = extract.range;
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

	for (index in categories) {
		element.classList.add('category-'+categories[index]);
	}

	return element;
}

/// Diff -> String -> DOM
function diffToDOM(diff, sources) {

	function getRange(diffOrTerm) {
		if (diffOrTerm.pure != null) {
			var beforeRange, afterRange;
			if (diffOrTerm.pure.before != null) {
				beforeRange = diffOrTerm.pure.before.extract.range
			}

			if (diffOrTerm.pure.after != null) {
				afterRange = diffOrTerm.pure.after.extract.range
			}

			if (beforeRange == null) {
				beforeRange = afterRange;
			}

			if (afterRange == null) {
				afterRange = beforeRange;
			}
			return { "before": beforeRange, "after": afterRange };
		}
		if (diffOrTerm.roll != null) {
			return { before: diffOrTerm.roll.extract.before.range, after: diffOrTerm.roll.extract.after.range };
		}
		if (diffOrTerm.extract != null) {
			return diffOrTerm.extract.range;
		}
	}

	if (diff.pure != null) {
		return pureToDOM(sources, diff.pure, getRange, function(diff) {
			return diffToDOM(diff, sources);
		})
	}

	return rollToDOM(sources, diff.roll, getRange, function(diff) {
		return diffToDOM(diff, sources);
	})
}

function pureToDOM(sources, patch, getRangeFun, diffToDOMFun) {
	var elementA, elementB;
	if (patch.before != null) {
		elementA = termToDOM(sources.before, patch.before.unwrap, patch.before.extract, getRangeFun);
		elementA.classList.add("delete");
		if (patch.after != null) {
			elementA.classList.add("replace");
		}
	}

	if (patch.after != null) {
		elementB = termToDOM(sources.after, patch.after.unwrap, patch.after.extract, getRangeFun);
		elementB.classList.add("insert");
		if (patch.before != null) {
			elementB.classList.add("replace");
		}
	}

	if (elementA == null) {
		elementA = elementB.cloneNode(true)
		elementA.classList.add("invisible")
	}

	if (elementB == null) {
		elementB = elementA.cloneNode(true)
		elementB.classList.add("invisible")
	}

	return { "before": elementA || "", "after": elementB || "" };
}

function rollToDOM(sources, rollOrTerm, getRangeFun, diffToDOMFun) {
	var syntax = rollOrTerm.unwrap
	var categories = {
		before: rollOrTerm.extract.before.categories,
		after: rollOrTerm.extract.after.categories
	}
	var range = {
		before: rollOrTerm.extract.before.range,
		after: rollOrTerm.extract.after.range
	}

	var elementA;
	var elementB;
	if (syntax.leaf != null) {
		elementA = document.createElement("span");
		elementA.textContent = sources.before.substr(range.before[0], range.before[1]);
		elementB = document.createElement("span");
		elementB.textContent = sources.after.substr(range.after[0], range.after[1]);
	} else if (syntax.indexed != null || syntax.fixed != null) {
		var values = syntax.indexed || syntax.fixed;
		elementA = document.createElement("ul");
		elementB = document.createElement("ul");
		var previousBefore = range.before[0];
		var previousAfter = range.after[0];
		for (i in values) {
			var child = values[i];
			if (child.pure == "") continue;

			var childRange = getRangeFun(child);
			var beforeAfterChild = diffToDOMFun(child)
			if (childRange.before != null) {
				var beforeRange = childRange.before;

				var beforeSeparator = sources.before.substr(previousBefore, beforeRange[0] - previousBefore);
				elementA.appendChild(document.createTextNode(beforeSeparator));
				var li = wrap("li", beforeAfterChild.before);
				if (beforeAfterChild.before.classList.contains("invisible")) {
					li.classList.add("invisible");
				}
				elementA.appendChild(li);

				previousBefore = beforeRange[0] + beforeRange[1];
			}
			if (childRange.after != null) {
				var afterRange = childRange.after;

				var afterSeparator = sources.after.substr(previousAfter, afterRange[0] - previousAfter);
				elementB.appendChild(document.createTextNode(afterSeparator));
				var li = wrap("li", beforeAfterChild.after);
				if (beforeAfterChild.after.classList.contains("invisible")) {
					li.classList.add("invisible");
				}
				elementB.appendChild(li);

				previousAfter = afterRange[0] + afterRange[1];
			}
		}
		var beforeText = sources.before.substr(previousBefore, range.before[0] + range.before[1] - previousBefore);
		var afterText = sources.after.substr(previousAfter, range.after[0] + range.after[1] - previousAfter);

		elementA.appendChild(document.createTextNode(beforeText));
		elementB.appendChild(document.createTextNode(afterText));
	} else if (syntax.keyed != null) {
		elementA = document.createElement("dl");
		elementB = document.createElement("dl");

		var befores = [];
		var afters = [];
		for (k in syntax.keyed.values) {
			if (syntax.keyed.values[k].pure == "") continue;
			var child = syntax.keyed.values[k];

			var ranges = getRangeFun(child);
			var doms = diffToDOMFun(child)
			if (ranges.before != null) {
				befores.push({ "key": k, "range": ranges.before, "child": doms.before });
			}
			if (ranges.after != null) {
				afters.push({ "key": k, "range": ranges.after, "child": doms.after });

			}
		}

		var sortByRange = function (array) {
			return function(a, b) {
				// if a is invisible, sort it based on its range vs. bs opposite range
				function sortByOppositeRange(el) {
					var opArray = array === befores ? afters : befores;
					var opEl = opArray.find(function (e) { return el.key === e.key; });
					var otherEl = el === a ? b : a;
					var opOtherEl = opArray.find(function (e) { return otherEl.key === e.key; });


					if (el.child.classList.contains("delete") && otherEl.child.classList.contains("insert")) {
						return -1;
					}
					if (el.child.classList.contains("insert") && otherEl.child.classList.contains("delete")) {
						return 1;
					}

					if (!el.child.classList.contains("invisible") && !otherEl.child.classList.contains("invisible")) {
						if (el.range[0] < otherEl.range[0]) {
							return -1;
						} else if (el.range[0] > otherEl.range[0]) {
							return 1;
						}
					}


					if (opEl.range[0] < opOtherEl.range[0]) {
						return -1;
					} else if (opEl.range[0] > opOtherEl.range[0]) {
						return 1;
					}


					return 0;
				}

				return sortByOppositeRange(a);
			}
		}

		befores.sort(sortByRange(befores));
		afters.sort(sortByRange(afters));

		var previousA = range.before[0];
		var previousB = range.after[0];

		zip(befores, afters, function (a, b) {
			var key = a.key
			var childElA = a.child
			var childRangeA = a.range
			var childElB = b.child
			var childRangeB = b.range

			var isFirst = elementA.childNodes.length == 0;

			function penultimateNode(el) {
				var nodes = Array.prototype.slice.call(el.childNodes).reverse();

				var lastVisibleNode;
				for (i in nodes) {
					var node = nodes[i];
					if (node.nodeType != 3 && node.classList.contains("invisible")) {
						var nextIndex = parseInt(i, 10) + 1;
						if (nextIndex < nodes.length) {
							var nextNode = nodes[nextIndex];
							if (nextNode.nodeType != 3 && !nextNode.classList.contains("invisible")) {
								return node;
							}
						}
					}
				}
			}

			if (isFirst || !childElA.classList.contains("invisible")) {
				var text = sources.before.substr(previousA, childRangeA[0] - previousA);

				var beforeNode = penultimateNode(elementA)
				if (beforeNode != null) {
					elementA.insertBefore(document.createTextNode(text), beforeNode);
				} else {
					elementA.appendChild(document.createTextNode(text));
				}
			}

			if (isFirst || !childElB.classList.contains("invisible")) {
				var text = sources.after.substr(previousB, childRangeB[0] - previousB);

				var beforeNode = penultimateNode(elementB)
				if (beforeNode != null) {
					elementB.insertBefore(document.createTextNode(text), beforeNode);
				} else {
					elementB.appendChild(document.createTextNode(text));
				}
			}

			var dtA = wrap("dt", document.createTextNode(key));
			elementA.appendChild(dtA);
			var ddA = wrap("dd", childElA)
			elementA.appendChild(ddA);
			if (childElA.classList.contains("invisible")) {
				dtA.classList.add("invisible");
				ddA.classList.add("invisible");
			}

			var dtB = wrap("dt", document.createTextNode(key));
			elementB.appendChild(dtB);
			var ddB = wrap("dd", childElB);
			elementB.appendChild(ddB);
			if (childElB.classList.contains("invisible")) {
				dtB.classList.add("invisible");
				ddB.classList.add("invisible");
			}


			if (isFirst || !childElA.classList.contains("invisible")) {
				previousA = childRangeA[0] + childRangeA[1]
			}
			if (isFirst || !childElB.classList.contains("invisible")) {
				previousB = childRangeB[0] + childRangeB[1]
			}

			var nextIndex = befores.indexOf(a) + 1;
			if (childElA.classList.contains("invisible") && nextIndex < afters.length) {
				var nextVisibleElement = afters[nextIndex]
				while (nextVisibleElement.child.classList.contains("invisible") && ++nextIndex < afters.length) {
					nextVisibleElement = afters[nextIndex]
				}
				if (nextVisibleElement == null) return;

				var beginningOfNextElement = nextVisibleElement.range[0];
				var text = sources.after.substr(previousB, beginningOfNextElement - previousB);
				var node = wrap("span", document.createTextNode(text));
				node.classList.add("invisible");
				elementA.appendChild(node);
			}

			var nextIndex = afters.indexOf(b) + 1;
			if (childElB.classList.contains("invisible") && nextIndex < befores.length) {
				var nextVisibleElement = befores[nextIndex]
				while (nextVisibleElement.child.classList.contains("invisible") && ++nextIndex < befores.length) {
					nextVisibleElement = befores[nextIndex]
				}
				if (nextVisibleElement == null) return;

				var beginningOfNextElement = nextVisibleElement.range[0];
				var text = sources.before.substr(previousA, beginningOfNextElement - previousA);
				var node = wrap("span", document.createTextNode(text));
				node.classList.add("invisible");
				elementB.appendChild(node);
			}
		});

		var textA = sources.before.substr(previousA, range.before[0] + range.before[1] - previousA);
		elementA.appendChild(document.createTextNode(textA));
		var textB = sources.after.substr(previousB, range.after[0] + range.after[1] - previousB);
		elementB.appendChild(document.createTextNode(textB));
	}

	for (index in categories.before) {
		elementA.classList.add('category-'+categories.before[index]);
	}

	for (index in categories.after) {
		elementB.classList.add('category-'+categories.after[index]);
	}

	return { "before": elementA, "after": elementB }
}


/// ([a], [b]) -> [(a, b)]
function zip (a, b, callback) {
	if (a.length > b.length) {
		return b.map(function(b, index) {
			return callback(a[index], b);
		});
	} else {
		return a.map(function(a, index) {
			return callback(a, b[index]) ;
		});
	}
}
