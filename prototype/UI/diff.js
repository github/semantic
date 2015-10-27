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

/// Diff -> String -> String -> DOM
function diffToDOM(diff, which, source) {
	if (diff.pure != null) {
		return diff.pure;
	}

	function getRange(diff) {
		if (diff.pure != null) {
			return diff.pure["data-range"];
		}
		if (diff.roll != null) {
			return diff.roll.extract[which];
		}
	}
	return rangeAndSyntaxToDOM(getRange(diff), diff.roll.unwrap, source, getRange, function(diff) {
		return diffToDOM(diff, which, source);
	})
}
